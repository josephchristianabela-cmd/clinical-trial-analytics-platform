**Clinical trial analytics platform.**

This platform aims to demonstrate end to end clinical trial data analytics using CDISC SDTM and ADaM standards.

The objective of this project is to demonstrate practical application of clinical data management, statistical programming, validation procedures, and reporting using open source CDISC datasets.

**Dataset**

The CDISC Pilot Study Dataset investigating the drug Xanomeline in patients with mild-to-moderate Alzheimer's disease is used. 

**Validation and integrity checks:**

A chronological audit was performed to verify adverse events did not occur prior to informed consent date, ensuring consistency.

A cohort audit was also performed to verify patients actually exist in the master demographics database.

**Statistical programmming:**

Informed by the ICH E9(R1) guidelines on estimands, Time-to-event analysis was performed, using ADaM time-to-event (ADTTE) structures, right-censored observations were handled via the censoring indicator (CNSR). 

A Cox proportional hazards model was fit to estimate hazard ratios and treatment effect sizes.

Kaplan–Meier curves were plotted; and dynamic summary tables used to report findings.

**Reproducibility**

To ensure a reproducible and automated workflow, GitHub is utilised.

