# scripts/05_survival_analysis.R
#
# Fits the primary safety KM/Cox model on the traceable dermatologic-event
# ADTTE produced by 02_derive_adtte.R (data/adam/adtte_derm.xpt). This
# replaces the previous version, which read `adtte_onco` (an unrelated
# oncology ADTTE with no shared USUBJID space with this study) and silently
# fell back from a non-existent "TTDE" parameter to an oncology PFS endpoint.
# There is now exactly one PARAMCD ("TTDE") in the input file, so no
# parameter-guessing logic is needed.

library(haven)
library(dplyr)
library(survival)
library(survminer)

if (!file.exists("data/adam/adtte_derm.xpt")) {
  stop("[ERROR] Missing data/adam/adtte_derm.xpt. Run 02_derive_adtte.R first.")
}

adtte <- read_xpt("data/adam/adtte_derm.xpt")

if (!dir.exists("data/outputs")) dir.create("data/outputs", recursive = TRUE)
if (!dir.exists("reports/tfl_outputs")) dir.create("reports/tfl_outputs", recursive = TRUE)

# =========================================================================
# 1. Confirm endpoint identity before modeling anything
# =========================================================================
if (!all(adtte$PARAMCD == "TTDE")) {
  stop("[ERROR] Unexpected PARAMCD values in adtte_derm.xpt - expected only 'TTDE'.")
}
message("[INFO] Modeling parameter: TTDE (Time to First Dermatologic Adverse Event)")

fas_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
adtte <- adtte %>% mutate(TRT01P = factor(TRT01P, levels = fas_levels))

# =========================================================================
# 2. Fit Kaplan-Meier Curves
# =========================================================================
message("[MODELING] Estimating Kaplan-Meier Curves...")
km_fit <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = adtte)

# =========================================================================
# 3. Cox Proportional Hazards Model & Inference
# =========================================================================
message("[MODELING] Fitting Cox Proportional Hazards Regression...")
cox_model   <- coxph(Surv(AVAL, CNSR == 0) ~ TRT01P, data = adtte)
cox_summary <- summary(cox_model)

saveRDS(cox_summary, "data/outputs/cox_summary.rds")
message("[SUCCESS] Exported: Cox summary metrics to data/outputs/")

# =========================================================================
# 4. Export Static Text Report for Dossier Compliance
# =========================================================================
sink("reports/tfl_outputs/survival_analysis_results.txt")
cat("=========================================================================\n")
cat("CLINICAL TRIAL ANALYTICS PLATFORM - SURVIVAL ANALYSIS INFERENCE\n")
cat("Protocol: CDISCPILOT01 (Xanomeline) | Parameter: TTDE\n")
cat("Time to First Treatment-Emergent Dermatologic Adverse Event\n")
cat("Population: Full Analysis Set (FAS), n =", nrow(adtte), "\n")
cat("=========================================================================\n\n")
print(cox_summary)
sink()

# =========================================================================
# 5. Kaplan-Meier Plot
# =========================================================================
km_plot <- ggsurvplot(
  km_fit,
  data = adtte,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = "Set2",
  title = "KM Curve: Time to First Dermatologic Adverse Event",
  xlab = "Days from Treatment Start",
  legend.labs = fas_levels
)

png("reports/tfl_outputs/km_plot_dermatitis.png", width = 10, height = 7, units = "in", res = 300)
print(km_plot)
dev.off()

message("[SUCCESS] Generated KM curves and risk tables.")
message("--- Survival Analysis Complete ---")
