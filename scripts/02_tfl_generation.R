# scripts/02_tfl_generation.R 
library(haven)
library(dplyr)
library(tidyr)
library(gt) # For professional table formatting

message("--- Starting Phase: Week 2 TFL Generation Engine ---")

# 1. Ingest Analysis Tiers
if (!file.exists("data/adam/adsl.xpt") || !file.exists("data/adam/adae.xpt")) {
  stop("[ERROR] Missing input datasets in data/adam/. Ensure files are present.")
}

adsl <- read_xpt("data/adam/adsl.xpt")
adae <- read_xpt("data/adam/adae.xpt")

# Create infrastructure tracking layers if missing
if(!dir.exists("reports/tfl_outputs")) dir.create("reports/tfl_outputs", recursive = TRUE)
if(!dir.exists("data/outputs")) dir.create("data/outputs", recursive = TRUE)

# DYNAMIC ALIGNMENT: Handle Planned vs Actual treatment variable notation variants safely
standardize_treatment_var <- function(df) {
  if ("TRTP" %in% names(df)) {
    df <- df %>% rename(ARM = TRTP)
  } else if ("TRTA" %in% names(df)) {
    df <- df %>% rename(ARM = TRTA)
  }
  return(df)
}

adsl <- standardize_treatment_var(adsl)
adae <- standardize_treatment_var(adae)

# Ensure ARM is factored uniformly with Placebo as reference group
unique_arms <- unique(adsl$ARM)
target_levels <- intersect(c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"), unique_arms)

adsl <- adsl %>% mutate(ARM = factor(ARM, levels = target_levels))
adae <- adae %>% mutate(ARM = factor(ARM, levels = target_levels))

# ========================================================================= 
# 2. Table 1: Baseline Demographics (Professional gt Version)
# =========================================================================
message("[PROCESSING] Compiling Table 1: Baseline Demographics...")

# Calculate "Big N" per group for headers
big_n <- adsl %>% count(ARM, name = "N")

demo_table_raw <- adsl %>%
  group_by(ARM) %>%
  summarise(
    Mean_Age = mean(AGE, na.rm = TRUE),
    SD_Age   = sd(AGE, na.rm = TRUE),
    Min_Max  = paste0(min(AGE, na.rm = TRUE), " - ", max(AGE, na.rm = TRUE)),
    .groups  = "drop"
  ) %>%
  left_join(big_n, by = "ARM") %>%
  mutate(Column_Label = paste0(ARM, " (N=", N, ")"))

# Save the raw rds matrix so the Shiny app layout can parse it seamlessly
saveRDS(demo_table_raw, "data/outputs/demo_table.rds")

# Create a professional HTML table for the portfolio report layout
demo_table_gt <- demo_table_raw %>%
  select(Column_Label, Mean_Age, SD_Age, Min_Max) %>%
  gt() %>%
  tab_header(title = "Table 14.1: Summary of Baseline Demographics", subtitle = "Intent-to-Treat Population") %>%
  fmt_number(columns = c(Mean_Age, SD_Age), decimals = 1) %>%
  cols_label(
    Column_Label = "Treatment Arm", 
    Mean_Age = "Mean", 
    SD_Age = "SD", 
    Min_Max = "Min - Max"
  )

gtsave(demo_table_gt, "reports/tfl_outputs/table_demographics.html")


# =========================================================================
# 3. Figure 1 Data & Table 2: AE Incidence (Subject-Level Counts)
# =========================================================================
message("[PROCESSING] Computing AE Incidence (Subject-Level)...")

# Get total N per arm for percentage calculation
arm_totals <- adsl %>% count(ARM, name = "total_n")

ae_summary <- adae %>%
  group_by(ARM, AEDECOD) %>%
  summarise(n_subjects = n_distinct(USUBJID), .groups = "drop") %>% 
  left_join(arm_totals, by = "ARM") %>%
  mutate(pct = round((n_subjects / total_n) * 100, 1)) %>%
  rename(Event_Count = n_subjects) 

saveRDS(ae_summary, "data/outputs/ae_summary.rds")

# TABLE 2 (GT AE SUMMARY TABLE)
message("[PROCESSING] Generating Table 2: Adverse Event Summaries via 'gt'...")

table2_data <- ae_summary %>%
  mutate(Display_Value = paste0(Event_Count, " (", pct, "%)")) %>%
  select(ARM, AEDECOD, Display_Value) %>%
  pivot_wider(names_from = ARM, values_from = Display_Value, values_fill = "0 (0.0%)")

table2_gt <- table2_data %>%
  gt() %>%
  tab_header(
    title = "Table 14.3.1: Treatment-Emergent Adverse Events By Preferred Term",
    subtitle = "Safety Analysis Set"
  ) %>%
  cols_label(AEDECOD = "Medical Dictionary Preferred Term (AEDECOD)")

gtsave(table2_gt, "reports/tfl_outputs/table_adverse_events.html")


# =========================================================================
# 4. Listing 1: Serious Adverse Events
# =========================================================================
message("[PROCESSING] Generating Listing 1: Serious Adverse Events...")

# Find whichever analysis start date column exists in this ADaM cut
date_var <- intersect(c("ASTDTC", "ASTDT", "AESTDTC"), names(adae))[1]

serious_ae_listing <- adae %>%
  filter(AESER == "Y") %>%
  select(USUBJID, ARM, AEDECOD, AESEV, any_of(date_var))

write.csv(serious_ae_listing, "reports/tfl_outputs/serious_ae_listing.csv", row.names = FALSE)

# =========================================================================
# 5. Listing 2: Patient Disposition Listing
# =========================================================================
message("[PROCESSING] Generating Listing 2: Patient Disposition...")

disp_var <- intersect(c("EOSSTT", "DCDECOD", "DTHFL"), names(adsl))[1]

if (!is.na(disp_var)) {
  disposition_listing <- adsl %>%
    select(USUBJID, ARM, AGE, SEX, !!sym(disp_var)) %>%
    rename(Study_Status = !!sym(disp_var))
} else {
  disposition_listing <- adsl %>%
    select(USUBJID, ARM, AGE, SEX) %>%
    mutate(Study_Status = "Randomized")
}

write.csv(disposition_listing, "reports/tfl_outputs/patient_disposition_listing.csv", row.names = FALSE)

message("--- TFL Generation Engine Executed Successfully ---")
