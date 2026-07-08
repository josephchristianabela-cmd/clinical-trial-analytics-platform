# scripts/04_survival_analysis.R
library(haven)
library(dplyr)
library(survival)
library(survminer)

# 1. Ingest Time-to-Event Data Tiers
if (!file.exists("data/adam/adtte.xpt")) {
  stop("[ERROR] Missing ADTTE dataset. Ensure phuse-org assets are downloaded.")
}

adtte <- read_xpt("data/adam/adtte.xpt")

# Create downstream artifact storage if missing
if(!dir.exists("data/outputs")) dir.create("data/outputs", recursive = TRUE)
if(!dir.exists("reports/tfl_outputs")) dir.create("reports/tfl_outputs", recursive = TRUE)
# =========================================================================
# 2. Filter for Target Parameter (Time to First Dermatologic Event)
# =========================================================================
surv_data <- adtte %>% 
  filter(PARAMCD == "TTDE")

# CHECK: If TRTP isn't there, it might be TRTA. Let's be safe:
if(!"TRTP" %in% names(surv_data)) {
  # If TRTP is missing, we use TRTA (Actual Treatment)
  surv_data <- surv_data %>% rename(TRTP = TRTA)
}

# Ensure Placebo is the reference group using TRTP
surv_data <- surv_data %>%
  mutate(TRTP = factor(TRTP, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")))

# =========================================================================
# 3. Fit Kaplan-Meier Curves
# =========================================================================
message("[MODELING] Estimating Kaplan-Meier Curves...")

# Use TRTP instead of ARM
km_fit <- survfit(Surv(AVAL, CNSR == 0) ~ TRTP, data = surv_data)

# =========================================================================
# 4. Cox Proportional Hazards Model & Inference
# =========================================================================
message("[MODELING] Fitting Cox Proportional Hazards Regression...")

# Use TRTP to match the KM fit and the data structure
cox_model <- coxph(Surv(AVAL, CNSR == 0) ~ TRTP, data = surv_data)
cox_summary <- summary(cox_model)

saveRDS(cox_summary, "data/outputs/cox_summary.rds")
message("[SUCCESS] Exported: Cox summary metrics to data/outputs/")

# =========================================================================
# 5. Export Static Text Report for Dossier Compliance
# =========================================================================
sink("reports/tfl_outputs/survival_analysis_results.txt")
cat("=========================================================================\n")
cat("CLINICAL TRIAL ANALYTICS PLATFORM - SURVIVAL ANALYSIS INFERENCE\n")
cat("Protocol: CDISCPilot1 | Parameter: Time to First Dermatologic Event\n")
cat("=========================================================================\n\n")
print(cox_summary)
sink()

# 6. Kaplan Meier Plot 
if(!requireNamespace("survminer", quietly = TRUE)) install.packages("survminer")
library(survminer)

# Create a professional KM Plot
km_plot <- ggsurvplot(
  km_fit, 
  data = surv_data,
  risk.table = TRUE,           # Adds the "Number at Risk" table at the bottom
  pval = TRUE,                 # Adds the Log-Rank test p-value
  conf.int = TRUE,             # Adds the confidence intervals
  palette = "Set2",
  title = "KM Curve: Time to First Dermatologic Event",
  legend.labs = c("Placebo", "Xanomeline Low", "Xanomeline High")
)

# Robustly export the combined plot and risk table matrix together
png("reports/tfl_outputs/km_plot_dermatitis.png", width = 10, height = 7, units = "in", res = 300)
print(km_plot)
dev.off()

message("[SUCCESS] Generated publication-grade KM curves and risk tables.")
message("--- Survival Analysis Engine Executed Successfully ---")