
# setup -------------------------------------------------------------------
#kill button
rm(list = ls())

library(readxl);library(tidyverse); library(tidyr); library(dplyr)
library(janitor);library(expss); library(stringdist); library(writexl)
library(readr)

setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/')

#determine new unified colnames
new_colnames <- c(
  "LEAname", "AUN", "sitename", "siteID", "studentID", "studentfirstname", 
  "studentmiddlename", "studentlastname", "studentDOB", "studentaddress1", 
  "studentaddress2", "aptnum", "city", "state", "zip", "eligibility", 
  "addresscounty", "casenumber", "parentfirstname", "parentlastname", 
  "parentphone", "parentemail", "enrollbegin", "enrollend", "validationoutput"
)

# creating dataframes for the new DC-only files -----------------------------------------------------

#function to read csvs and standardize colnames for Dc Only
read_and_standardize_csvs <- function(folder_path, new_colnames) {
  file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  df_list <- lapply(file_list, function(file) {
    message("Reading full data from: ", file)
    
    # Try to read the file
    df <- tryCatch(read.csv(file, stringsAsFactors = FALSE), error = function(e) return(NULL))
    
    if (is.null(df)) return(NULL)
    
    # Keep only the first 25 columns
    df <- df[, 1:min(25, ncol(df))]
    
    # If fewer than 25 columns, pad with empty columns
    if (ncol(df) < 25) {
      missing_cols <- 25 - ncol(df)
      for (i in 1:missing_cols) {
        df[[paste0("V_missing_", i)]] <- NA
      }
    }
    
    # Rename columns to standard names
    colnames(df)[1:25] <- new_colnames
    
    #add filename column
    df$source_file <- basename(file) 
    
    return(df)
  })
  
  # Filter out any failed reads
  df_list <- Filter(Negate(is.null), df_list)
  
  # Combine into one data frame
  combined_df <- do.call(rbind, df_list)
  
  return(combined_df)
}


#DC-Only June files dataframe
#read in csv files with new colnames
june_data <- read_and_standardize_csvs("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) June files", new_colnames)
#filter to drop rows that are completely blank
june_data <- june_data %>%
  filter(!is.na(studentfirstname) & studentfirstname != "")
#rename AUN to AUN_original
june_data <- june_data %>%
  rename(AUN_original = AUN)
#create AUN copy column
june_data <- june_data %>%
  mutate(
    AUN = sub("^.*_1_(\\d{9})_.*\\.csv$", "\\1", source_file),     # extract 9-digit ID after '_1_'
    AUN = gsub("^(\\d{3})(\\d{2})(\\d{3})(\\d{1})$", "\\1-\\2-\\3-\\4", AUN)  # format it
  ) %>%
  relocate(AUN, .after = AUN_original)
#create usertype column
june_data$usertype <- "DC only"
#remove sourcefile column
june_data <- june_data %>%
  select(-source_file)


#DC-Only PASES files dataframe
pases_data <- read_and_standardize_csvs("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) PA-SES files", new_colnames)
#filter to drop rows that are completely blank
pases_data <- pases_data %>%
  filter(!is.na(studentfirstname) & studentfirstname != "")
#rename AUN to AUN_original
pases_data <- pases_data %>%
  rename(AUN_original = AUN)
#create AUN copy column
pases_data <- pases_data %>%
  mutate(
    AUN = sub("^x(\\d{9}).*", "\\1", source_file),  # extract 9 digits after 'x'
    AUN = gsub("^(\\d{3})(\\d{2})(\\d{3})(\\d{1})$", "\\1-\\2-\\3-\\4", AUN)  # format
  ) %>%
  relocate(AUN, .after = AUN_original)
#create usertype column
pases_data$usertype <- "DC only"
#remove sourcefile column
pases_data <- pases_data %>%
  select(-source_file)
#make siteID as character
pases_data$siteID <- as.character(pases_data$siteID)

# Full user files, load in and unify with colnames ------------------------

#Full User June file df
FU_june_data <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/ALLstudentfiles.rds")
#unified colnames for FU files
colnames(FU_june_data)[1:25] <- new_colnames
#add usertype column
FU_june_data$usertype <- "Full User"
#create AUN_original column for comnbining
FU_june_data <- FU_june_data %>%
  mutate(AUN_original = AUN) %>%
  relocate(AUN_original, .before = AUN)


#Full User PASES file df
FU_pases_data <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSprimerofiles/primerofiles.rds")
#unified colnames for FU files
colnames(FU_pases_data)[1:25] <- new_colnames
#add usertype column
FU_pases_data$usertype <- "Full User"
#create AUN_original column for comnbining
FU_pases_data <- FU_pases_data %>%
  mutate(AUN_original = AUN) %>%
  relocate(AUN_original, .before = AUN)



# combine Fulluser and DC only datasets for both filetypes ----------------

#all June files df
all_june_files <- bind_rows(june_data, FU_june_data)

# all PA-SES files df
all_pases_files <- bind_rows(pases_data, FU_pases_data)

#EXPORT (this takes a while, good thing we only have to do it once....right?)

library(data.table)
#june
fwrite(all_june_files, "//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/All final student files - FU + DC, PA-SES + June (for SQL conversion)/all_june_files.csv", showProgress = TRUE)
#pases
fwrite(all_pases_files, "//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/All final student files - FU + DC, PA-SES + June (for SQL conversion)/all_pases_files.csv", showProgress = TRUE)
