# FMD Vaccine Effectiveness in Uruguay (2001)

This repository contains the R scripts used for the retrospective assessment of foot-and-mouth disease (FMD) vaccination effectiveness during the 2001 epidemic in Uruguay.  
The analysis is based on preparing epidemic data in an SIR framework and estimating the transmission rate parameter (β) using Generalized Linear Models with a complementary log-log link.

---
## 📂 Repository structure

R/ # R scripts
│ └── analysis_pipeline.R # runs data preparation, GLM fitting and figures

data/ # Example/simulated dataset
│ ├── simulated_fmd_dataset.csv

.gitignore # files ignored by Git

LICENSE # license file

README.md # project description

---

## ⚙️ Requirements
- R version ≥ 4.2  
- R packages: `tidyverse`, `lubridate`, `lme4`  

---

## ▶️ How to run

1. **Download or clone this repository**:
   ```bash
   git clone https://github.com/victoriairiarte/FMD_VaccineEffectiveness_Uruguay2001.git
   cd FMD_VaccineEffectiveness_Uruguay2001

2. Open R or RStudio in the project folder.

3. Install the required R packages (only the first time):
   install.packages(c("dplyr", "lubridate", "tidyverse", "broom", "lme4"))

4. Run the pipeline
   source("R/analysis_pipeline.R")

---

## 📊 Data
Due to restrictions, the original dataset from the 2001 epidemic in Uruguay cannot be shared.  
A **simulated dataset** with the same structure is provided in `/data` for demonstration purposes.  
Instructions for requesting access to the real data are provided in `data/README.md`.  

---

## 📖 Citation
If you use this code, please cite (subject to journal acceptance):   

Iriarte, M.V. et al. (2025). *Retrospective assessment of foot-and-mouth disease vaccination effectiveness in Uruguay during the 2001 epidemic*. npj Vaccines. DOI: [to be added]  

---

## 📜 License
This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.  

