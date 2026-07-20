# scripts/02_derive_adtte.R
#
# Derives a traceable Time-to-Event (ADTTE) analysis dataset for the primary
# safety endpoint: time to first treatment-emergent dermatologic adverse
# event. This REPLACES the previous approach of pulling `adtte_onco` from
# pharmaverseadam, which is a pre-built oncology ADTTE (PARAMCD: OS/PFS/RSD)
# generated from an unrelated admiralonco example study. It shares no
# USUBJID space with the Xanomeline DM/AE/ADSL domains used elsewhere in
# this pipeline, so any endpoint/HR computed from it was not traceable to
# this study and did not represent a dermatologic outcome of any kind.
#
# Endpoint definition (ICH E9(R1) estimand, matches README Section "Statistical
# programming"):
#   Population    : Full Analysis Set (FAS) - randomized subjects only
#                   (TRT01P in Placebo / Xanomeline Low Dose / Xanomeline High
#                   Dose), i.e. Screen Failures are excluded. This resolves to
#                   n = 254 of 306 DM subjects.
#   Variable      : Days from treatment start (TRTSDT) to first treatment-
#                   emergent adverse event coded to SOC "SKIN AND SUBCUTANEOUS
#                   TISSUE DISORDERS" (AEBODSYS), TRTEMFL == "Y".
#   Intercurrent  : Treatment-Policy strategy - subjects without a qualifying
#   events          event are right-censored at EOSDT (End of Study Date),
#                   falling back to TRTEDT if EOSDT is missing.
#   Summary       : PARAMCD = "TTDE" / PARAM = "Time to First Dermatologic
#                   Adverse Event". CNSR = 0 (event) / 1 (censored), consistent
#                   with standard ADaM BDS-TTE convention.
#
# Traceability variables (SRCDOM/SRCVAR) are retained on the event rows so a
# reviewer can trace AVAL/ADT back to the exact source AE record.

suppressMessages({
  library(dplyr)
  library(haven)
})

message("--- Starting: Derive ADTTE (Time to First Dermatologic AE) ---")

if (!file.exists("data/adam/adsl.xpt") || !file.exists("data/adam/adae.xpt")) {
  stop("[ERROR] Missing data/adam/adsl.xpt or adae.xpt. Run 01_data_ingestion.R first.")
}

adsl <- read_xpt("data/adam/adsl.xpt")
adae <- read_xpt("data/adam/adae.xpt")

required_adsl_vars <- c("STUDYID", "USUBJID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT")
missing_vars <- setdiff(required_adsl_vars, names(adsl))
if (length(missing_vars) > 0) {
  stop(paste("[ERROR] ADSL is missing required variables for ADTTE derivation:",
             paste(missing_vars, collapse = ", "),
             "- this pipeline expects the standard CDISC Pilot ADSL schema and does not guess."))
}

if (!"AEBODSYS" %in% names(adae) || !"TRTEMFL" %in% names(adae) || !"ASTDT" %in% names(adae)) {
  stop("[ERROR] ADAE is missing AEBODSYS / TRTEMFL / ASTDT - cannot derive event dates.")
}

# ---------------------------------------------------------------------------
# 1. Full Analysis Set: randomized subjects only
# ---------------------------------------------------------------------------
fas_arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
adsl_fas <- adsl %>% filter(TRT01P %in% fas_arms)

message(paste("[INFO] FAS population:", nrow(adsl_fas), "of", nrow(adsl), "DM subjects",
              "(", nrow(adsl) - nrow(adsl_fas), "Screen Failures excluded)"))

# ---------------------------------------------------------------------------
# 2. First treatment-emergent dermatologic AE per subject (event source)
# ---------------------------------------------------------------------------
derm_events <- adae %>%
  filter(AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", TRTEMFL %in% "Y") %>%
  group_by(USUBJID) %>%
  summarise(
    EVENT_DT = min(ASTDT, na.rm = TRUE),
    SRCSEQ   = AESEQ[which.min(ASTDT)],
    .groups  = "drop"
  )

# ---------------------------------------------------------------------------
# 3. Assemble ADTTE (one row per FAS subject)
# ---------------------------------------------------------------------------
adtte_derm <- adsl_fas %>%
  select(STUDYID, USUBJID, TRT01P, TRT01A, AGE, SEX, TRTSDT, TRTEDT, EOSDT) %>%
  left_join(derm_events, by = "USUBJID") %>%
  mutate(
    CNSR     = if_else(is.na(EVENT_DT), 1, 0),
    CENS_DT  = coalesce(EOSDT, TRTEDT),
    ADT      = if_else(CNSR == 0, EVENT_DT, CENS_DT),
    STARTDT  = TRTSDT,
    AVAL     = as.numeric(ADT - STARTDT),
    AVAL     = if_else(AVAL < 1, 1, AVAL),   # floor same-day degenerate times at 1 day
    PARAMCD  = "TTDE",
    PARAM    = "Time to First Dermatologic Adverse Event",
    EVNTDESC = if_else(CNSR == 0, "Dermatologic Adverse Event", "End of Study / Last Known Date"),
    SRCDOM   = if_else(CNSR == 0, "AE", "DM"),
    SRCVAR   = if_else(CNSR == 0, "ASTDT", "EOSDT")
    # NOTE: TRT01P is intentionally left as character here. write_xpt() has no
    # native factor type and silently exports factor levels as integer codes,
    # which breaks on read-back (this bit us during testing). Analysis scripts
    # apply factor(TRT01P, levels = ...) themselves after reading the xpt.
  ) %>%
  select(STUDYID, USUBJID, TRT01P, TRT01A, AGE, SEX, PARAMCD, PARAM,
         STARTDT, ADT, AVAL, CNSR, EVNTDESC, SRCDOM, SRCVAR, SRCSEQ)

# ---------------------------------------------------------------------------
# 4. Sanity checks - fail loudly rather than export a broken dataset
# ---------------------------------------------------------------------------
if (nrow(adtte_derm) != 254) {
  warning(paste("[DATA ISSUE] Expected FAS n=254 per README; derived", nrow(adtte_derm)))
}
if (any(is.na(adtte_derm$AVAL)) || any(is.na(adtte_derm$ADT))) {
  stop("[ERROR] AVAL/ADT missing for one or more subjects after derivation.")
}
if (!all(adtte_derm$CNSR %in% c(0, 1))) {
  stop("[ERROR] CNSR contains values outside {0,1}.")
}
if (any(adtte_derm$AVAL <= 0)) {
  stop("[ERROR] Non-positive analysis time detected.")
}

event_summary <- adtte_derm %>% count(TRT01P, CNSR)
message("[INFO] Event/censor counts by arm (CNSR=0 is event):")
print(event_summary)

# ---------------------------------------------------------------------------
# 5. Export
# ---------------------------------------------------------------------------
if (!dir.exists("data/adam")) dir.create("data/adam", recursive = TRUE)
if (!dir.exists("data/outputs")) dir.create("data/outputs", recursive = TRUE)

# Export primary and alias datasets for downstream script compatibility
write_xpt(adtte_derm, "data/adam/adtte_derm.xpt")
write_xpt(adtte_derm, "data/adam/adtte.xpt")
saveRDS(event_summary, "data/outputs/adtte_derivation_log.rds")

message("[SUCCESS] Exported: adtte_derm.xpt and adtte.xpt to data/adam/")
message("--- ADTTE Derivation Complete ---")
