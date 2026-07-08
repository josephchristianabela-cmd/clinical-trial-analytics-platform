# scripts/01_data_ingestion.R

# 1. Ensure core packages are available
if(!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
if(!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")

library(pharmaversesdtm) 
library(pharmaverseadam) 
library(haven)
library(purrr)

message("--- Starting Organized Pharmaverse Extraction ---")

# 2. Map domains to their explicit CDISC standard tiers
domains <- list(
  sdtm = c("dm", "ae", "lb"),          # Tabulation tier
  adam = c("adsl", "adae", "adtte")    # Analysis tier
)

# 3. Dynamic export function respecting regulatory folder tiering
save_as_xpt <- function(domain_name, type) {
  # The datasets exist in R directly as 'dm', 'adsl', etc.
  if (exists(domain_name)) {
    df <- get(domain_name)
    
    # Dynamically direct data into data/sdtm/ or data/adam/
    target_dir <- paste0("data/", type)
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
    
    write_xpt(df, paste0(target_dir, "/", domain_name, ".xpt"))
    message(paste("[SUCCESS] Exported:", domain_name, ".xpt to", target_dir))
  } else {
    warning(paste("[WARN] Dataset object not found:", domain_name))
  }
}

# 4. Run the decoupled submission pipeline loops
walk(domains$sdtm, ~save_as_xpt(.x, "sdtm"))
walk(domains$adam, ~save_as_xpt(.x, "adam"))

message("--- Data Ingestion Tiering Complete! ---")

# 5. Silently snap the environment state
renv::snapshot(type = "all", prompt = FALSE, force = TRUE)