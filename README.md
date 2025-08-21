# FMD Vaccine Effectiveness in Uruguay (2001)

This repository contains the R scripts used for the retrospective assessment of foot-and-mouth disease (FMD) vaccination effectiveness during the 2001 epidemic in Uruguay.  
The analysis is based on preparing epidemic data in an SIR framework and estimating the transmission rate parameter (β) using Generalized Linear Models with a complementary log-log link.

---

## 📂 Repository structure
R/ # Scripts used for data preparation, modelling and figures
│ ├── 01_data_prep.R
│ ├── 02_glm_model.R
│ └── 03_figures.R

data/ # Example/simulated dataset
│ └── README.md # Explains access to real data

---

## ⚙️ Requirements
- R version ≥ 4.2  
- R packages: `tidyverse`, `lubridate`, `lme4`  

---

## ▶️ How to run
1. Clone or download this repository.  
2. Run `R/01_data_prep.R` to prepare the dataset.  
3. Run `R/02_glm_model.R` to fit the GLM and estimate β.  
4. Run `R/03_figures.R` to reproduce the main figures from the manuscript.  

---

## 📊 Data
Due to restrictions, the original dataset from the 2001 epidemic in Uruguay cannot be shared.  
A **simulated dataset** with the same structure is provided in `/data` for demonstration purposes.  
Instructions for requesting access to the real data are provided in `data/README.md`.  

---

## 📖 Citation
If you use this code, please cite:  

Iriarte, M.V. et al. (2025). *Retrospective assessment of foot-and-mouth disease vaccination effectiveness in Uruguay during the 2001 epidemic*. npj Vaccines. DOI: [to be added]  

---

## 📜 License
This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.  

