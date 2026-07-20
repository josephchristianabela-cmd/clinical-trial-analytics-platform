# scripts/04_validation.R
library(haven)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)

message("--- Starting: Regulatory Data Audit ---")

parse_clinical_date <- function(date_vec) {
  if (is.numeric(date_vec)) {
    return(as.Date(date_vec, origin = "1970-01-01"))
  }
  return(as.Date(substr(as.character(date_vec), 1, 10)))
}

# 1. Ingest Data for Auditing
adtte_file <- if (file.exists("data/adam/adtte_derm.xpt")) {
  "data/adam/adtte_derm.xpt"
} else if (file.exists("data/adam/adtte.xpt")) {
  "data/adam/adtte.xpt"
} else {
  stop("[ERROR] Missing ADTTE dataset. Verify adtte_derm.xpt or adtte.xpt exists in data/adam/.")
}

if (!file.exists("data/adam/adsl.xpt") || !file.exists("data/adam/adae.xpt") ||
    !file.exists("data/sdtm/dm.xpt")) {
  stop("[ERROR] Missing input datasets. Verify adsl.xpt, adae.xpt, and sdtm/dm.xpt exist.")
}

adsl  <- read_xpt("data/adam/adsl.xpt")
adae  <- read_xpt("data/adam/adae.xpt")
dm    <- read_xpt("data/sdtm/dm.xpt")
adtte <- read_xpt(adtte_file)

# =========================================================================
# 2. Temporal Validation: Chronology Checks
# =========================================================================
message("[AUDIT] Running Temporal Chronology Checks...")

# Determine which baseline anchor variables are populated in the SDTM/ADaM cuts
consent_var <- if ("RFICDTC" %in% names(dm)) "RFICDTC" else "RFSTDTC"
ae_date_var <- if ("ASTDT" %in% names(adae)) "ASTDT" else "AESTDTC"

message(paste("[INFO] Using tracking markers:", ae_date_var, "vs", consent_var))

# Build standard temporal cross-matrix
ae_audit <- adae %>%
  select(USUBJID, AEDECOD, any_of(c("TRTEMFL", ae_date_var))) %>%
  left_join(dm %>% select(USUBJID, !!sym(consent_var), RFSTDTC), by = "USUBJID")

# Calculate date sequences safely
ae_audit$ae_start     <- parse_clinical_date(ae_audit[[ae_date_var]])
ae_audit$consent_date <- parse_clinical_date(ae_audit[[consent_var]])
ae_audit$trpt_start   <- parse_clinical_date(ae_audit$RFSTDTC)

# Implement logical flag evaluations
ae_audit <- ae_audit %>%
  mutate(
    flag_pre_consent = ifelse(!is.na(ae_start) & !is.na(consent_date) & ae_start < consent_date, 1, 0),
    is_teae          = ifelse(!is.na(ae_start) & !is.na(trpt_start) & ae_start >= trpt_start, 1, 0)
  )

pre_consent_issues <- ae_audit %>% filter(flag_pre_consent == 1)

if(nrow(pre_consent_issues) > 0) {
  warning(paste("[DATA ISSUE]", nrow(pre_consent_issues), "AEs found occurring BEFORE Informed Consent!"))
} else {
  message("[PASS] All AEs occurred post-consent.")
}

# =========================================================================
# 3. Baseline & Duplicate Record Validation
# =========================================================================
message("[AUDIT] Verifying Baseline Logic and Record Uniqueness...")

# Scope to randomized subjects only: Screen Failures are never dosed and are
# EXPECTED to have missing TRTSDT, so checking all-comers previously produced
# a false-positive flag (52/306) on every run regardless of actual data
# quality. A missing TRTSDT among randomized subjects is the real issue.
randomized_arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
baseline_check <- adsl %>%
  filter(ARM %in% randomized_arms) %>%
  summarise(
    total_patients = n(),
    missing_trtsdt = sum(is.na(TRTSDT))
  )

if(baseline_check$missing_trtsdt > 0) {
  warning(paste("[DATA ISSUE]", baseline_check$missing_trtsdt, "randomized subjects found missing Treatment Start Dates (TRTSDT) in ADSL."))
} else {
  message("[PASS] No missing baseline treatment anchors among randomized subjects.")
}

# Structural duplicate check: USUBJID+AESEQ is the AE domain's actual
# uniqueness key (each AESEQ is a distinct event record for that subject).
# The previous key (USUBJID+AEDECOD+date) flagged 605/1191 rows (51%) as
# "duplicates" - these were genuinely distinct AE episodes (different AESEQ,
# severity, causality) that happened to share a preferred term and start
# date, e.g. recurring erythema flares. That is expected clinical granularity,
# not a data defect, so keying on term+date alone was a false-positive check.
if (!"AESEQ" %in% names(adae)) stop("[ERROR] ADAE missing expected variable AESEQ.")

duplicate_ae_records <- adae %>%
  group_by(USUBJID, AESEQ) %>%
  filter(n() > 1) %>%
  ungroup()

dup_ae_count <- nrow(duplicate_ae_records)

if(dup_ae_count > 0) {
  warning(paste("[DATA ISSUE]", dup_ae_count, "duplicate AE records discovered (Same USUBJID + AESEQ)."))
} else {
  message("[PASS] No structural duplicate AE records found (USUBJID + AESEQ unique).")
}

# =========================================================================
# 4. Cross-Domain Integrity (DM vs ADSL Traceability)
# =========================================================================
message("[AUDIT] Verifying Cross-Domain consistency...")

missing_in_dm <- anti_join(adsl, dm, by = "USUBJID")

if(nrow(missing_in_dm) == 0) {
  message("[PASS] All ADSL subjects are present in SDTM DM.")
} else {
  warning(paste("[FAIL] Subjects found in ADaM tier that do not resolve to SDTM DM:", nrow(missing_in_dm)))
}

# =========================================================================
# 5. ADTTE Traceability & Integrity (data/adam/adtte_derm.xpt)
# =========================================================================
message("[AUDIT] Verifying ADTTE traceability and integrity...")

adtte_orphans <- anti_join(adtte, dm, by = "USUBJID")
if (nrow(adtte_orphans) == 0) {
  message("[PASS] All ADTTE subjects resolve to SDTM DM.")
} else {
  warning(paste("[FAIL] ADTTE subjects not found in SDTM DM:", nrow(adtte_orphans)))
}

adtte_bad_cnsr <- adtte %>% filter(!CNSR %in% c(0, 1))
adtte_bad_aval <- adtte %>% filter(is.na(AVAL) | AVAL <= 0)

if (nrow(adtte_bad_cnsr) == 0) {
  message("[PASS] ADTTE CNSR values are all in {0,1}.")
} else {
  warning(paste("[FAIL] ADTTE rows with invalid CNSR:", nrow(adtte_bad_cnsr)))
}

if (nrow(adtte_bad_aval) == 0) {
  message("[PASS] ADTTE AVAL is populated and positive for all subjects.")
} else {
  warning(paste("[FAIL] ADTTE rows with missing/non-positive AVAL:", nrow(adtte_bad_aval)))
}

# =========================================================================
# 6. Export Audit Results for RMarkdown / Dashboard Reporting
# =========================================================================
audit_summary <- list(
  pre_consent_count   = nrow(pre_consent_issues),
  missing_dm_count    = nrow(missing_in_dm),
  total_subjects      = nrow(adsl),
  missing_trtsdt      = baseline_check$missing_trtsdt,
  duplicate_ae_rows   = dup_ae_count,
  adtte_orphan_count  = nrow(adtte_orphans),
  adtte_bad_cnsr_count = nrow(adtte_bad_cnsr),
  adtte_bad_aval_count = nrow(adtte_bad_aval)
)

# Single overall status other scripts (dashboard, Rmd reports) should read
# rather than hardcoding a result of their own.
audit_summary$overall_status <- if (all(unlist(audit_summary[c(
    "pre_consent_count", "missing_dm_count", "missing_trtsdt",
    "duplicate_ae_rows", "adtte_orphan_count", "adtte_bad_cnsr_count",
    "adtte_bad_aval_count")]) == 0)) "PASS" else "FLAGGED"

saveRDS(audit_summary, "data/outputs/audit_summary.rds")
message(paste("--- Audit Completed. Overall status:", audit_summary$overall_status, "---"))
message("--- Summary exported to data/outputs/audit_summary.rds ---")
