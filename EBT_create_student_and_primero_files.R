


# this file creates the student/SEBT and primero files for subsequent analysis
library(readxl);library(tidyverse); library(tidyr); library(dplyr)
library(janitor);library(expss); library(stringdist); library(writexl)


# File import/prep --------------------------------------------------------------------

## List all CSV files in the folder
primerofiles <- list.files(path = '//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Primero', pattern = "\\.csv$", full.names = TRUE)
studentfiles <- list.files(path = '//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Student upload/batch6', pattern = "\\.csv$", full.names = TRUE)

## Function to read CSV and convert all columns to character

standard_names<-c("sfa_name",   "aun9_3",  'school_name',  "school_code4",  "pasecureid",
                  "student_first_name",  "student_middle_name",  "student_last_name",  "student_dob",
                  "student_address1",  "student_address2",  "student_apt_no",  "student_city",
                  "student_state",  "student_zip",  "eligibility",  "county",
                  "case_number",  "parent_first_name",  "parent_last_name",  "parent_phone",  "parent_email",
                  "enrollment_begin",  "enrollment_end",  "validation_output")

## Function to read, convert to character, and rename columns by position
read_and_align <- function(file, standard_names) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df[] <- lapply(df, as.character)
  
  ####see if aun still looks right (in same column)  
  
  # Rename columns by position only if counts match
  if (ncol(df) == length(standard_names)) {
    names(df) <- standard_names
  } else {
    stop(paste("Column count mismatch in file:", file))
  }
  
  return(df)
}


## start merge -------------------------------------------------------------

## Read and combine files using standard column names
#setwd("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Primero")
#p <- lapply(primerofiles, function(f) read_and_align(f, standard_names)) %>%
# bind_rows() 



setwd("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Student upload")
s <- lapply(studentfiles, function(f) read_and_align(f, standard_names)) %>%
  bind_rows()


## Joining/matching --------------------------------------------------------

#rename with suffix for clarity
s2 <- s %>%
  rename_with(~ paste0("sebt_", .x))

#p2 <- p %>%
#rename_with(~ paste0("primero_", .x))


# Helper to clean and lowercase variables
clean_keys <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ iconv(as.character(.), from = "", to = "UTF-8", sub = "byte") %>%
                    trimws() %>%
                    tolower()))
}

## Clean both datasets
s2 <- clean_keys(s2, c("sebt_aun9_3", "sebt_pasecureid", "sebt_student_dob", "sebt_student_last_name", "sebt_student_first_name", "sebt_parent_first_name", "sebt_parent_last_name"))
#p2 <- clean_keys(p2, c("primero_aun9_3", "primero_pasecureid", "primero_student_dob", "primero_student_last_name", "primero_student_first_name", "primero_parent_first_name", "primero_parent_last_name"))


##drop any with missing AUN and/or missing secureID
s2 <- s2 %>%
  filter(!is.na(sebt_aun9_3), sebt_aun9_3 != "",
         !is.na(sebt_pasecureid), sebt_pasecureid != "")

p2 <- p2 %>%
  filter(!is.na(primero_aun9_3), primero_aun9_3 != "",
         !is.na(primero_pasecureid), primero_pasecureid != "")


#save the files to rds
#saveRDS(s2, file = "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch6.rds")
