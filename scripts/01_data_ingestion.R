# scripts/01_data_ingestion.R
#
# Pulls raw CDISC Pilot (Xanomeline) domains from the pharmaverse test-data
# packages and exports them to the submission-style data/sdtm and data/adam
# tiers. This script ONLY ingests source domains; no derived analysis
# parameters are created here (see 02_derive_adtte.R for the TTE derivation).

if(!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
if(!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")

library(pharmaversesdtm)
library(pharmaverseadam)
library(haven)
library(purrr)

message("--- Starting Pharmaverse Extraction (Xanomeline CDISC Pilot) ---")

# All domains below are sourced from the SAME study (CDISCPILOT01 / Xanomeline
# Alzheimer's trial), preserving a single, traceable USUBJID space end to end.
domains <- list(
  sdtm = c("dm", "ae", "lb"),      # Tabulation tier
  adam = c("adsl", "adae")         # Analysis tier (ADTTE is derived, not sourced
                                    # pre-built — see 02_derive_adtte.R)
)

save_as_xpt <- function(domain_name, type) {
  if (!exists(domain_name)) {
    warning(paste("[WARN] Dataset object not found:", domain_name))
    return(invisible(NULL))
  }
  df <- get(domain_name)

  target_dir <- paste0("data/", type)
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

  write_xpt(df, paste0(target_dir, "/", domain_name, ".xpt"))
  message(paste("[SUCCESS] Exported:", domain_name, ".xpt to", target_dir))
}

walk(domains$sdtm, ~save_as_xpt(.x, "sdtm"))
walk(domains$adam, ~save_as_xpt(.x, "adam"))

message("--- Data Ingestion Complete ---")

# Silently snap the environment state
renv::snapshot(type = "all", prompt = FALSE, force = TRUE)
