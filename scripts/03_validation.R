# scripts/03_validation.R 
library(haven)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)

message("--- Starting Phase: Week 3 Regulatory Data Audit ---")

# 1. Ingest Data for Auditing
if (!file.exists("data/adam/adsl.xpt") || !file.exists("data/adam/adae.xpt") || !file.exists("data/sdtm/dm.xpt")) {
  stop("[ERROR] Missing input datasets. Verify adsl.xpt, adae.xpt, and sdtm/dm.xpt exist.")
}

adsl <- read_xpt("data/adam/adsl.xpt")
adae <- read_xpt("data/adam/adae.xpt")
dm   <- read_xpt("data/sdtm/dm.xpt")

# Infrastructure setup
if(!dir.exists("data/outputs")) dir.create("data/outputs", recursive = TRUE)

# Robust date normalizer helper function
parse_clinical_date <- function(date_vec) {
  if (is.numeric(date_vec)) {
    return(as.Date(date_vec, origin = "1970-01-01"))
  }
  return(as.Date(substr(as.character(date_vec), 1, 10)))
}

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

# Leverage treatment date validation logic across ADSL
baseline_check <- adsl %>%
  summarise(
    total_patients = n(),
    missing_trtsdt = sum(is.na(TRTSDT))
  )

if(baseline_check$missing_trtsdt > 0) {
  warning(paste("[DATA ISSUE]", baseline_check$missing_trtsdt, "subjects found missing Treatment Start Dates (TRTSDT) in ADSL."))
} else {
  message("[PASS] No missing baseline treatment anchors discovered in ADSL.")
}

# AUDIT ADDITION: Track Exact Transactional Duplicates inside ADAE
duplicate_ae_records <- adae %>%
  group_by(USUBJID, AEDECOD, !!sym(ae_date_var)) %>%
  filter(n() > 1) %>%
  ungroup()

dup_ae_count <- nrow(duplicate_ae_records)

if(dup_ae_count > 0) {
  warning(paste("[DATA ISSUE]", dup_ae_count, "exact duplicate AE rows discovered (Same ID, Term, and Date)."))
} else {
  message("[PASS] No exact duplicate AE records found.")
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
# 5. Export Audit Results for RMarkdown Reporting
# =========================================================================
audit_summary <- list(
  pre_consent_count = nrow(pre_consent_issues),
  missing_dm_count  = nrow(missing_in_dm),
  total_subjects    = nrow(adsl),
  missing_trtsdt    = baseline_check$missing_trtsdt,
  duplicate_ae_rows = dup_ae_count
)

saveRDS(audit_summary, "data/outputs/audit_summary.rds")
message("--- Audit Completed: Summary exported to data/outputs/ ---")
