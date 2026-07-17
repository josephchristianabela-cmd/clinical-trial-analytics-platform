**Clinical trial analytics platform.**

This platform aims to demonstrate end to end clinical trial data analytics using CDISC SDTM and ADaM standards.

The objective of this project is to demonstrate practical application of clinical data management, statistical programming, validation procedures, and reporting using open source CDISC datasets.

Programmes used: RStudio, Rshiny, R Markdown, CDISC & GitHub

**Dataset** 

The CDISC Pilot Study Dataset investigating the drug Xanomeline in patients with mild-to-moderate Alzheimer's disease is used. 
SDTM domains used (DM, AE, LB), ADaM datasets (ADSL, ADAE, ADTTE)
The workflow demonstrates traceability from SDTM domains to ADaM analysis datasets and final statistical outputs.

**Workflow**
Raw CDISC Data
      │
      ▼
SDTM Domains
      │
      ▼
Validation & Integrity Checks
      │
      ▼
ADaM Analysis Datasets
      │
      ▼
Statistical Programming
      │
      ▼
TFLs & Survival Analysis
      │
      ▼
Regulatory Reports

**Validation and integrity checks:**

A chronological audit was performed to verify adverse events did not occur prior to informed consent date, ensuring consistency.

A cohort audit was also performed to verify patients actually exist in the master demographics database.

**Statistical programmming:**

Informed by the ICH E9(R1) guidelines on estimands, Time-to-event analysis was performed, using ADaM time-to-event (ADTTE) structures, right-censored observations were handled via the censoring indicator (CNSR). 

A Cox proportional hazards model was fit to estimate hazard ratios and treatment effect sizes.

Kaplan–Meier curves were plotted; and dynamic summary tables used to report findings.

**Reproducibility**

To ensure a reproducible and automated workflow, GitHub is utilised.

