rm(list=ls())

library(readxl);library(tidyverse); library(tidyr); library(dplyr)
library(janitor);library(expss); library(stringdist); library(writexl)
library(readr)

setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/')


# messy, make dataframewith headers to check file structure (ignore) ------------------------------


#function to read in fileheaders and rename headers
read_csv_headers_rename <- function(folder_path, new_colnames) {
  file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  header_rows <- lapply(file_list, function(file) {
    message("Reading: ", file)
    
    header_line <- tryCatch(readLines(file, n = 1, warn = FALSE), error = function(e) NA)
    
    if (is.na(header_line)) return(NULL)
    
    split_header <- strsplit(header_line, ",")[[1]]
    
    # Pad with empty strings if fewer than 25
    if (length(split_header) < 25) {
      split_header <- c(split_header, rep("", 25 - length(split_header)))
    }
    
    # Still fail if somehow *more* than 25 columns
    if (length(split_header) != 25) {
      warning("Unexpected number of columns in file: ", file)
      return(NULL)
    }
    
    # Replace names with the standard list
    names_row <- setNames(as.list(new_colnames), new_colnames)
    return(as.data.frame(names_row, stringsAsFactors = FALSE))
  })
  
  header_rows <- Filter(Negate(is.null), header_rows)
  
  header_df <- do.call(rbind, header_rows)
  
  return(header_df)
}

new_colnames <- c(
  "LEAname", "AUN", "sitename", "siteID", "studentID", "studentfirstname", 
  "studentmiddlename", "studentlastname", "studentDOB", "studentaddress1", 
  "studentaddress2", "aptnum", "city", "state", "zip", "eligibility", 
  "addresscounty", "casenumber", "parentfirstname", "parentlastname", 
  "parentphone", "parentemail", "enrollbegin", "enrollend", "validationoutput"
)

#DC-Only June
june_folder_path <- "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) June files"
Juneheader_df <- read_csv_headers_rename(june_folder_path, new_colnames)
#DC-Only PA-SES
pases_folder_path <- "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) PA-SES files"
pasesheader_df <- read_csv_headers_rename(pases_folder_path, new_colnames)




# fixing individual cases (ignore) -------------------------------------------------



# Path to your fixed file
fixed_file <- "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) June files/SEBT2025_1_113385303_20250611-115252.csv"

# Read just that one file using the same logic
header_line <- readLines(fixed_file, n = 1, warn = FALSE)
line_clean <- gsub('"', '', header_line)
line_clean <- trimws(line_clean)
split_header <- strsplit(line_clean, ",")[[1]]

# Standardize to 25 columns
if (length(split_header) < 25) {
  split_header <- c(split_header, rep("", 25 - length(split_header)))
} else if (length(split_header) > 25) {
  split_header <- split_header[1:25]
}

# Build one-row data frame
new_row <- as.data.frame(setNames(as.list(new_colnames), new_colnames), stringsAsFactors = FALSE)
Juneheader_df <- rbind(Juneheader_df, new_row)



# creating dataframes for the new DC-only files -----------------------------------------------------

#function to read csvs and standardize colnames
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
    
    return(df)
  })
  
  # Filter out any failed reads
  df_list <- Filter(Negate(is.null), df_list)
  
  # Combine into one data frame
  combined_df <- do.call(rbind, df_list)
  
  return(combined_df)
}

#determine new colnames
new_colnames <- c(
  "LEAname", "AUN", "sitename", "siteID", "studentID", "studentfirstname", 
  "studentmiddlename", "studentlastname", "studentDOB", "studentaddress1", 
  "studentaddress2", "aptnum", "city", "state", "zip", "eligibility", 
  "addresscounty", "casenumber", "parentfirstname", "parentlastname", 
  "parentphone", "parentemail", "enrollbegin", "enrollend", "validationoutput"
)

#DC-Only June files dataframe
june_data <- read_and_standardize_csvs("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) June files", new_colnames)
#drop na rows (determined by missing/invalid AUN)
june_data <- june_data[grepl("^\\d{3}-\\d{2}-\\d{3}-\\d{1}$", june_data$AUN), ]
#create usertype column
june_data$usertype <- "DC only"

#Dc-Only PA-SES files dataframe
pases_data <- read_and_standardize_csvs("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Non-Full-Users student files/(NON FU) PA-SES files", new_colnames)
#fix AUn for CAI learning academy
pases_data$AUN[pases_data$AUN == "NoA-UN-#-"] <- "321-39-275-7"
#drop na rows (determined by missing/invalid AUN)
pases_data <- pases_data[grepl("^\\d{3}-\\d{2}-\\d{3}-\\d{1}$", pases_data$AUN), ]
#create usertype column
pases_data$usertype <- "DC only"

# cleaning/checking dataframe (ignore) ---------------------------------------------

length(unique(pases_data$AUN))
table(pases_data$AUN)
# Identify rows where AUN format is incorrect
bad_aun_rows <- !grepl("^\\d{3}-\\d{2}-\\d{3}-\\d{1}$", june_data$AUN)

# View the bad rows
june_data[bad_aun_rows, ]
table(bad_aun_rows)





# Full user files, load in and unify with colnames ------------------------

FU_june_data <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/ALLstudentfiles.rds")
FU_pases_data <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSprimerofiles/primerofiles.rds")

#unified colnames for FU files
colnames(FU_june_data)[1:25] <- new_colnames
colnames(FU_pases_data)[1:25] <- new_colnames

#clean FU df, remove invalid/missing AUN rows, add usertype column
FU_june_data <- FU_june_data[grepl("^\\d{3}-\\d{2}-\\d{3}-\\d{1}$", FU_june_data$AUN), ]
FU_june_data$usertype <- "Full User"

FU_pases_data <- FU_pases_data[grepl("^\\d{3}-\\d{2}-\\d{3}-\\d{1}$", FU_pases_data$AUN), ]
FU_pases_data$usertype <- "Full User"


# combine Fulluser and DC only datasets for both filetypes ----------------

#all June files df
all_june_files <- bind_rows(june_data, FU_june_data)

# all PA-SES files df
# error: site ID format -- Convert siteID to character in both dataframes
pases_data$siteID <- as.character(pases_data$siteID)
FU_pases_data$siteID <- as.character(FU_pases_data$siteID)
#now create df
all_pases_files <- bind_rows(pases_data, FU_pases_data)




