rm(list=ls())

library(readxl);library(tidyverse); library(tidyr); library(dplyr)
library(janitor);library(expss)



# File import/prep --------------------------------------------------------------------

# List all CSV files in the folder
primerofiles <- list.files(path = '//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Dummy Schools/Primero', pattern = "\\.csv$", full.names = TRUE)
studentfiles <- list.files(path = '//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Dummy Schools/Student upload', pattern = "\\.csv$", full.names = TRUE)


# open all files into one data frame
col_types_def <- readr::cols(.default = col_character())
setwd("//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Dummy Schools/Primero")
p <- map_df(primerofiles, ~ read_csv(.x, col_types = col_types_def,show_col_types = TRUE)) %>% as.data.frame()

setwd("//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Dummy Schools/Student upload")
s <- map_df(studentfiles, ~ read_csv(.x, col_types = col_types_def,show_col_types = TRUE)) %>% as.data.frame()

# Function to read CSV and convert all columns to character

standard_names<-c("sfa_name",   "aun9_3",  'school_name',  "school_code4",  "pasecureid",
                             "student_first_name",  "student_middle_name",  "student_last_name",  "student_dob",
                             "student_address1",  "student_address2",  "student_apt_no",  "student_city",
                             "student_state",  "student_zip",  "eligibility",  "county",
                             "case_number",  "parent_first_name",  "parent_last_name",  "parent_phone",  "parent_email",
                             "enrollment_begin",  "enrollment_end",  "validation_output")

# Function to read, convert to character, and rename columns by position
read_and_align <- function(file, standard_names) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df[] <- lapply(df, as.character)
  
  # Rename columns by position only if counts match
  if (ncol(df) == length(standard_names)) {
    names(df) <- standard_names
  } else {
    stop(paste("Column count mismatch in file:", file))
  }
  
  return(df)
}


# Read and combine files using your standard column names
p <- lapply(primerofiles, function(f) read_and_align(f, standard_names)) %>%
  bind_rows()


s <- lapply(studentfiles, function(f) read_and_align(f, standard_names)) %>%
  bind_rows()


# Joining/matching --------------------------------------------------------

#rename with suffix for clarity
s <- s %>%
  rename_with(~ paste0("sebt_", .x))

p <- p %>%
  rename_with(~ paste0("primero_", .x))


# Helper to clean and lowercase matching keys
clean_keys <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ iconv(as.character(.), from = "", to = "UTF-8", sub = "byte") %>%
                    trimws() %>%
                    tolower()))
}

# Clean both datasets
s <- clean_keys(s, c("sebt_aun9_3", "sebt_pasecureid", "sebt_student_dob", "sebt_student_last_name", "sebt_student_first_name"))
p <- clean_keys(p, c("primero_aun9_3", "primero_pasecureid", "primero_student_dob", "primero_student_last_name", "primero_student_first_name"))


#drop any with missing AUN and/or missing secureID
s_filtered <- s %>%
  filter(!is.na(sebt_aun9_3), sebt_aun9_3 != "",
         !is.na(sebt_pasecureid), sebt_pasecureid != "")

p_filtered <- p %>%
  filter(!is.na(primero_aun9_3), primero_aun9_3 != "",
         !is.na(primero_pasecureid), primero_pasecureid != "")

# Perform the join
match1 <- full_join(s_filtered, p_filtered,
                    by = c("sebt_aun9_3" = "primero_aun9_3",
                           "sebt_pasecureid" = "primero_pasecureid"),
                    suffix = c("_sebt", "_primero"))


#AUN first: all matches/comparisons should be grouped by AUN (district) before anything! Matching p and s variables for each district -------


#Total student count difference between each district’s file A vs B (indicate which file has more/number missing cases) 
#Print() missing student info [List student detail (all column variables) for students included in district’s file A but not in file B, and vice versa.] --------


#Student eligibility: Number of ‘free’, paid’, and ‘reduced’ for each district for both p and s files 
#Total Count diff for each eligibility type between p and s --------


#PAsecureID?: print() student info for each case where PAsecureID < 10 -------------------------------------


#print students that don’t have the following variables match between s and p --------

# siteID -----------------------------------------------------------------


# parent guardian first and last ------------------------------------------


# student address ---------------------------------------------------------






