# Annual Survey of Industries (ASI), 2015-16 to 2023-24

## Overview

At present, this repository hosts R scripts to:

1) standardise and recode variables of selected blocks across survey rounds, and save the cleaned and merged dataframes as Parquet files
2) reproduce summary statistics as reported in the ASI publications (to be done)

## Repository Structure

### `00_docs/`

Contains official ASI documentation obtained from the Ministry of Statistics and Programme Implementation (MoSPI) Microdata website ([https://microdata.gov.in/NADA/index.php/home](https://microdata.gov.in/NADA/index.php/home)).

### `01_data/`

Contains processed dataframes and derived outputs.

**Note:** This repository does not include the raw ASI data files. The scripts use the original `.dta` files, which must be downloaded separately by the user from the MoSPI Microdata website.

#### Required Raw Data Structure

After downloading the raw `.dta` files, place them in:

`01_data/01_data_input/01_raw_data/`

Organise the files by survey round as follows:

`01_data/01_data_input/01_raw_data/asiYYYY_YY/`

Within each round-specific folder, the files should follow this naming convention:

`asiYYYY_YY_blockX.dta`

**Note:** Replace `YYYY_YY` with the relevant survey year (e.g., `2023_24`) and `X` with the block letter (`A` to `J`).

### `02_scripts/`

Contains all the R scripts used to process the data files into survey-year-specific Parquet files.