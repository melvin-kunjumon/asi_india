
# cleaning environment ----

rm(list = ls())
gc()

# sourcing library ----

source("02_scripts/utils/00_library.R")
source("02_scripts/utils/01_standardise_asi.R")
source("02_scripts/utils/02_recode_asi.R")

# unzipping raw data ----

# optional - if the raw data files are already stored locally in the 01_raw_data folder, this step can be skipped

zip_file <- "01_data/01_data_input/01_raw_data.zip"
extract_dir <- "01_data/01_data_input"
raw_data_dir <- file.path(extract_dir, "01_raw_data")

if (!dir.exists(raw_data_dir)) {
  
  unzip(zip_file, exdir = extract_dir)
  
}

# reading files ----

asi_paths <- list(
  
  "2015_16" = "01_data/01_data_input/01_raw_data/asi2015_16",
  "2016_17" = "01_data/01_data_input/01_raw_data/asi2016_17",
  "2017_18" = "01_data/01_data_input/01_raw_data/asi2017_18",
  "2018_19" = "01_data/01_data_input/01_raw_data/asi2018_19",
  "2019_20" = "01_data/01_data_input/01_raw_data/asi2019_20",
  "2020_21" = "01_data/01_data_input/01_raw_data/asi2020_21",
  "2021_22" = "01_data/01_data_input/01_raw_data/asi2021_22",
  "2022_23" = "01_data/01_data_input/01_raw_data/asi2022_23",
  "2023_24" = "01_data/01_data_input/01_raw_data/asi2023_24"
  
)

asi_blocks <- c("blockA", "blockB", "blockC", "blockD", "blockE", "blockF", "blockG", "blockH", "blockI", "blockJ")
asi_files <- expand_grid(year = names(asi_paths), block = asi_blocks)

asi_list <- pmap(asi_files, function(year, block) {
  
  file_path <- file.path(asi_paths[[year]], paste0("asi", year, "_", block, ".dta"))
  
  read_dta(file_path)
  
})

names(asi_list) <- paste0("asi", asi_files$year, "_", asi_files$block)
rm(asi_files, asi_paths, asi_blocks)

# standardising the columns across rounds ----

asi_list <- imap(asi_list, standardise_asi)
asi_list <- split(asi_list, sub("_(block[A-J])$", "", names(asi_list)))

asi_list <- map(asi_list, function(df) {
  
  df <- reduce(df, left_join, by = "dsl_code")
  
  df <- df %>% 
    relocate(mult, .after = last_col())
  
})

# recoding the variables ----

asi_list <- map(asi_list, recode_asi)

# saving files ----

iwalk(asi_list, ~ {
  
  write_parquet(.x, file.path("01_data/01_data_input/02_clean_data", paste0(.y, ".parquet")))
  
})

# deleting extracted raw data ----

# optional - remove the extracted raw data if it was extracted from 01_raw_data.zip for this run

if (file.exists(zip_file)) {
  
  if (dir.exists(raw_data_dir)) {
    
    unlink(raw_data_dir, recursive = T, force = T)
    
  }
  
  macosx_dir <- file.path(extract_dir, "__MACOSX")
  
  if (dir.exists(macosx_dir)) {
    
    unlink(macosx_dir, recursive = T, force = T)
    
  }
  
}

# cleaning environment ----

rm(list = ls())
gc()
