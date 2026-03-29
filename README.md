# Replication Package

This directory contains all the materials necessary to replicate *Auditing the police: 
Experimental evidence of racial disparities in police accountability* (Stanford, 2027). The repository is organized into three main folders: scripts, data, and output. Below is a description of the structure and contents.

## Directory Structure

### 001 Scripts/
This folder contains R scripts used for data preparation, analysis, and figure generation.

- **01 pre-analysis/**: Scripts for initial data processing and cleaning.
  - `01 treatment assignment.R`: Assigns treatments for the experiment.
  - `02 general data clean.R`: General data cleaning procedures.
  - `03 map figure data clean.R`: Prepares data for map figures.
  - `04 wordcount response time figure data clean.R`: Prepares data for word count and response time figures.

- **02 analysis/**: Scripts for main analysis and output generation.
  - `01 Tables.R`: Generates tables for the study.
  - `02 Figures.R`: Generates figures for the study.

### 002 Data/
This folder contains all data files used in the study, divided into intermediate and final datasets.

- **01 intermediate data/**: Raw and processed data used in intermediate steps.
  - **agency data/**: Police agency data.
    - `LEMAS-Data-2016.dta`: Law Enforcement Management and Administrative Statistics data for 2016.
    - `LEMAS-Data-2020.dta`: Law Enforcement Management and Administrative Statistics data for 2020.
    - `leoka_yearly_1960_2020.rds`: Law Enforcement Officers Killed and Assaulted (LEOKA) data from 1960 to 2020.
  - **experiment data/**: Data related to the audit experiment.
    - `audit_identities.csv`: Identities used in audits.
    - `audit_names.csv`: Names used in audits.
    - `email_data.csv`: Email data from the experiment.
    - `selected_police_departments.csv`: List of selected police departments.
    - `SES.csv`: Socioeconomic status data.
  - **figure data/**: Data specifically for generating figures.
    - `census_pop_2020.csv`: 2020 census population data.
    - `state-hex-square-coords.csv`: Coordinates for state hexagons/squares.
    - `state-legal-status-201910.csv`: Legal status data by state as of October 2019.

- **02 final data/**: Cleaned and final datasets used for analysis.
  - `01_outgoing_email_data.csv`: Outgoing email data.
  - `02_all_department_data.csv`: Data for all departments.
  - `03_distinct_departments_data.csv`: Distinct departments data.
  - `04_main_data.csv`: Main analysis dataset.
  - `department_map_data.csv`: Data for department maps.
  - `response_time_data.csv`: Response time data.
  - `word_count_data.csv`: Word count data.

### 003 Output/
This folder contains the generated tables and figures from the analysis.

- **Figures/**: PNG files of the figures.
  - `fig_2_A.png`, `fig_2_B.png`: Figures 2A and 2B.
  - `fig_A1.png` to `fig_A6.png`: Appendix figures.

- **Tables/**: LaTeX files of the tables.
  - `table_1.tex` to `table_10.tex`: Main tables.
  - `table_A1.tex` to `table_A12.tex`: Appendix tables.

## Replication Instructions

To replicate the study:

1. Open the `Replication.Rproj` file in RStudio.
2. Run the scripts in `001 Scripts/` in order:
   - Start with pre-analysis scripts in `01 pre-analysis/`.
   - Then run analysis scripts in `02 analysis/`.
3. Outputs will be generated in `003 Output/`.

Ensure all required R packages are installed. Data files in `002 Data/` are provided as-is for replication.

## File Descriptions

The following table summarizes the goal and output of each script file in the `001 Scripts/` folder.

| File | Goal | Produces/Does |
|------|------|---------------|
| `01 treatment assignment.R` | Assign treatments to experimental units for the audit study. | Creates treatment assignments and integrates them into the dataset. |
| `02 general data clean.R` | Perform general cleaning and preprocessing on raw data. | Produces cleaned intermediate datasets ready for analysis. |
| `03 map figure data clean.R` | Prepare data specifically for generating map-based figures. | Outputs processed data for maps, such as department locations and overlays. |
| `04 wordcount response time figure data clean.R` | Clean and prepare data for word count and response time analyses and figures. | Generates datasets with word counts, response times, and related metrics. |
| `01 Tables.R` | Run statistical analyses and generate summary tables. | Produces LaTeX table files in `003 Output/Tables/`. |
| `02 Figures.R` | Create visualizations and plots for the study. | Generates PNG figure files in `003 Output/Figures/`. |