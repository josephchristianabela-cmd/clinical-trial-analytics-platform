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

**Data Validation:**

A chronological audit was performed to verify adverse events did not occur prior to informed consent date, ensuring consistency.

Subject verification and USUBJID uniqueness was performed by a cohort audit to verify patients actually exist in the master demographics database.

<img width="1162" height="576" alt="image" src="https://github.com/user-attachments/assets/ec045243-2e6d-43e5-a663-836176a62795" />


**Statistical programmming:**

Informed by the ICH E9(R1) guidelines on estimands:

Population: a Full Analysis Set (FAS) was used.

Emdpoint: Time-to-event, i.e  days from treatment start date (`TRTSDT`) to treatment end date (`TRTEDT`).

Intercurrent events: discontinuation 

Statistics: Hazard ratio calculated using Cox Proportional Hazards model.

Time-to-treatment-discontinuation was used as an indicator of safety and tolerability, using ADaM time-to-event (ADTTE) structures, right-censored observations were handled via the censoring indicator (CNSR). 

### Primary Model Outputs: Time-to-Treatment-Discontinuation
The regression engine evaluated the relative hazard of treatment discontinuation across study arms. The model completed with stable fit metrics across all three global statistical tests ($p = 0.001$):

| Treatment Arm | Hazard Ratio ($HR$) | 95% Confidence Interval | p-value |
| :--- | :---: | :---: | :---: |
| **Xanomeline Low Dose** | 1.55 | 1.15 – 2.10 | 0.005 |
| **Xanomeline High Dose** | 1.69 | 1.25 – 2.28 | < 0.001 |

*   **Model Discriminatory Power:** The framework achieved a Concordance index (C-index) of **0.588**.
*   **Clinical Interpretation:** Both active treatment arms show a statistically significant increase in the rate of discontinuation compared to placebo, indicating a clear, dose-dependent tolerability signal.

A Cox proportional hazards model was fit to estimate hazard ratios and treatment effect sizes.

Kaplan–Meier curves were plotted; and dynamic summary tables used to report findings.

**Reproducibility**

To ensure a reproducible and automated workflow, GitHub is utilised.

