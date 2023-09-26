## Sizing up the Global Impact of Electronic Wages on Digital Payment Usage

### Getting Started

This repository contains the replication files for: 

> Allen, Jeffrey S. "Pay it forward (digitally): sizing up the global impact of electronic wages on digital payment usage." *Journal of Economics and Finance* (2023), https://doi.org/10.1007/s12197-023-09645-8. 

Users can either clone the repository or download the zip file. Both options are available under the green **Code** drop-down button.

The following execution scripts replicate the analysis in the paper: 

* `01_ewage_dataprep.R`
* `02_ewage_descriptive.R`
* `03_ewage_analysis.R`

Please see the additional notes about data access needs for running `01_ewage_dataprep.R` at the end of this welcome page. The Statistical Appendix referenced in the paper is located at: `output/Statistical_Appendix.pdf`.

### Folder Structure

```
|-- 01_ewage_dataprep.R     # prep data for analysis using R/prep_ewage_data.R
|-- 02_ewage_descriptive.R  # describe and summarize data using R/analyze_ewage.R  
|-- 03_ewage_analysis.R     # model and analyze data using R/analyze_ewage.R
|-- data                    # data produced by 01_ewage_dataprep.R
|-- data-raw                # data used in 01_ewage_dataprep.R (see notes below) 
|-- output                  # tables, figures, and Statistical Appendix
|-- R                       # backend functions that produce most of the analysis
|   |-- analyze_ewage.R     # analytical functions
|   |-- prep_ewage_data.R   # data preparation functions
```

### Built With

* dplyr
* ggplot2
* gridExtra
* haven
* lmtest
* magrittr
* margins
* readxl
* sampleSelection
* sandwich
* stargazer
* stringr
* survey
* tibble
* tidyr

### Data Sources

The analysis utilizes two sources from the World Bank's 2021 Global Financial Inclusion (Global Findex) Database (Demirguc-Kunt et al. 2022): 

* [Global Microdata](https://microdata.worldbank.org/index.php/catalog/4607) (see note about access below)
* Country Data via the [World Bank Databank](https://databank.worldbank.org/source/global-financial-inclusion)

Additionally, the analysis utilizes the World Bank's [Country and Lending Groups](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups) data. 

### Running the Data Preparation Routine

This repository does not make available the raw Findex microdata file. Thus, to 
run `01_ewage_dataprep.R`, users need to obtain the microdata from the World Bank 
at the link above. After obtaining the microdata, it is recommended that users
save the data as `data-raw/micro_world.dta`. However, the function `load_micro_data()` from `R/prep_ewage_data.R` can accept a different path. Currently, the data preparation functions are designed to process the `.dta`
microdata file and not the `.csv` file. 

Obtaining the microdata is not required for running `02_ewage_descriptive.R` or 
`03_ewage_analysis.R` because the repository makes available the curated output 
from `01_ewage_dataprep.R` as `data/ewage_microdata.rds`. 

### References

Demirguc-Kunt, Asli, Leora Klapper, Dorothe Singer, Saniya Ansar. 2022. The Global Findex Database 2021: Financial Inclusion, Digital Payments, and Resilience in the Age of COVID-19. Washington, DC: World Bank.
