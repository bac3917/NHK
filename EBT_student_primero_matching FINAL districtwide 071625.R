rm(list=ls())

library(readxl);library(tidyverse); library(tidyr); library(dplyr)
library(janitor);library(expss); library(stringdist); library(writexl)


# Load the sebt & primero files in
p2 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSprimerofiles/primerofiles.rds")
s2 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/ALLstudentfiles.rds")

## Create unique IDs  --------------------------------------------------------

# Create join keys
p2 <- p2 %>%
  mutate(join_key1 = paste0(primero_aun9_3, "_",primero_pasecureid),
         join_key2 = paste0(primero_aun9_3, "_", primero_student_first_name, "_", primero_student_last_name, "_", primero_student_dob))

s2 <- s2 %>%
  mutate(join_key1 = paste0(sebt_aun9_3, "_", sebt_pasecureid ),
         join_key2 = paste0(sebt_aun9_3, "_", sebt_student_first_name, "_", sebt_student_last_name, "_", sebt_student_dob))

# Decide which key to use
p2 <- p2 %>%
  mutate(join_key = if_else( nchar(primero_pasecureid)>10 | nchar(primero_pasecureid<10) , join_key2,join_key1))
tabyl(duplicated(p2$join_key))

s2 <- s2 %>%
  mutate(join_key = if_else( nchar(sebt_pasecureid)>10 | nchar(sebt_pasecureid<10) , join_key2,join_key1))
tabyl(duplicated(s2$join_key))

# count number of missing columns
p2<-p2 %>% group_by(join_key) %>% mutate(missing_count = rowSums(is.na(across(everything()))), .groups = "drop")
s2<-s2 %>% group_by(join_key) %>% mutate(missing_count = rowSums(is.na(across(everything()))), .groups = "drop")

# Before joining, ensure each file has best unique set of cases
# if duplicated join_key select the row with least missing data
p_undup<-p2 %>% 
  group_by(join_key) %>%  slice_min(missing_count, with_ties = FALSE) %>% 
  ungroup()
#saveRDS(p_undup,"//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/p_undup.RDS")
s_undup<-s2 %>% 
  group_by(join_key) %>%  slice_min(missing_count, with_ties = FALSE) %>% 
  ungroup()
#saveRDS(s_undup,"//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/s_undup.RDS")

# Begin analysis here with p_undup and s_undup!

## use school names to match to rows in datasheet
library(readxl)
mds <- read_excel("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/NKH Master Data Sheet_local_copy.xlsx",
                          sheet = "Sheet1", skip = 3)
mds<-janitor::clean_names(mds)
mds$aun_2<-as.character(mds$aun_2)
mds<-mds %>% select(aun_2,sfa_name,schl_type)

# PURPLE COLUMN MEASURES START HERE --------
# analyses are based on records that match by `join_key` for unduplicated students
match1 <- inner_join(p_undup, s_undup, by = "join_key")
tabyl(duplicated(match1$join_key))
tabyl(duplicated(match1$join_key))


## CG - Total Student Count in PrimeroEdge File (this includes duplicate student records)
## CH - Total student count in student upload file (this includes duplicate student records)

        # Count students per AUN in each dataset using the UNIQUE student set
        p_counts <- p_undup %>%
          group_by(primero_aun9_3) %>%
          summarise(p_total = n(), .groups = "drop")
        
        s_counts <- s_undup %>%
          group_by(sebt_aun9_3) %>%
          summarise(s_total = n(), .groups = "drop")

        # Merge and compute difference
        student_counts_by_aun <- full_join(p_counts, s_counts,
                                           by = c("primero_aun9_3" = "sebt_aun9_3")) %>%
          rename(AUN = primero_aun9_3) %>%
          mutate(AUN=str_replace_all(AUN,"-","")) %>%
          mutate(across(c(p_total, s_total), ~replace_na(., 0)),
                 difference = p_total - s_total)
        
        student_counts_by_aun<-student_counts_by_aun %>% left_join(mds,by=c("AUN"="aun_2"))
        cg_ch<-student_counts_by_aun %>% select(sfa_name, AUN,s_total,p_total,difference) %>%
          mutate(AUN=as.numeric(AUN)) %>% arrange(AUN)
        write_xlsx(cg_ch,"//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/cg_ch.xlsx")

  
# find unmatched
# Students in p2 not matched in s2
unmatched_p2 <- anti_join(p2, s2, by = "join_key")

# Students in s2 not matched in p2
unmatched_s2 <- anti_join(s2, p2, by = "join_key")

#manytomany
# Count duplicates in each dataset
dups_p2 <- p2 %>% count(join_key) %>% filter(n > 1)
dups_s2 <- s2 %>% #make groupby...count(join_key) %>% filter(n > 1) %>% left_join(mds,by=c("AUN"="aun_2"))

# Check overlapping duplicate keys
many_to_many_keys <- inner_join(dups_p2, s2, by = "join_key")


#Total student count difference between each district’s file A vs B (indicate which file has more/number missing cases) 
#Print() missing student info [List student detail (all column variables) for students included in district’s file A but not in file B, and vice versa.] 

# Columns CJ - CL --------------
## CJ - Total FREE Student Count PASES File
## CK - Eligibility: Total FREE Student Count June File
## CL - Eligibility: Total FREE Student Count Diff (PASES vs June File)


# Student eligibility: Number of ‘free’, paid’, and ‘reduced’ for each district for both p and s files 
# Total Count diff for each eligibility type between p and s 

# Function: count all elig by AUN
count_eligibility <- function(df, aun_col, elig_col, prefix) {
  df %>%
    mutate(elig = tolower(.data[[elig_col]])) %>%
    filter(elig %in% c("free", "reduced", "paid")) %>%
    group_by(.data[[aun_col]], elig) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = elig, values_from = count, values_fill = 0) %>%
    rename_with(~ paste0(prefix, "_", .x), -all_of(aun_col)) %>%
    rename(AUN = all_of(aun_col))
}

# Count in both types
sebt_counts <- count_eligibility(match1, "sebt_aun9_3", "sebt_eligibility", "sebt")
primero_counts <- count_eligibility(match1, "sebt_aun9_3", "primero_eligibility", "primero")

# Merge and get diff
eligibility_counts <- full_join(sebt_counts, primero_counts, by = "AUN") %>%
  mutate(
    diff_free = sebt_free - primero_free,
    diff_paid = sebt_paid - primero_paid,
    diff_reduced = sebt_reduced - primero_reduced,
    AUN=str_replace_all(AUN,"-","")) %>% 
  arrange(AUN) %>% left_join(mds,by=c("AUN"="aun_2")) %>%
  mutate(AUN=as.numeric(AUN)) # need to match format in excel for successful `XLOOKUP`

setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/')
write_xlsx(eligibility_counts,"cj_cl.xlsx")

# Column CS: Matched Students - Site IDs Count Diff -----
school_code_mismatches <- match1 %>%
  filter(!is.na(sebt_school_code4), !is.na(primero_school_code4)) %>%
  filter(sebt_school_code4 != primero_school_code4) %>%
  group_by(sebt_aun9_3) %>%
  summarise(school_code_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3) %>% mutate(AUN=str_replace_all(AUN,"-","")) %>%
  arrange(AUN) %>% left_join(mds,by=c("AUN"="aun_2")) %>%
  mutate(AUN=as.numeric(AUN)) # need to match format in excel for successful `XLOOKUP`

setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/')
write_xlsx(school_code_mismatches,"cs.xlsx")


# Column CT ------
# - Parent/Guardian info: Count of matched student cases where June parent firstname ≠ PA-SES parent firstname AND June parent lastname ≠ PA-SES parent lastname

parent_name_mismatches <- match1 %>%
  filter(
    !is.na(sebt_parent_first_name), !is.na(primero_parent_first_name),
    !is.na(sebt_parent_last_name), !is.na(primero_parent_last_name)
  ) %>%
  filter(
    tolower(trimws(sebt_parent_first_name)) != tolower(trimws(primero_parent_first_name)) |
      tolower(trimws(sebt_parent_last_name)) != tolower(trimws(primero_parent_last_name))
  ) %>%
  group_by(sebt_aun9_3) %>%
  summarise(parent_name_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3) %>% mutate(AUN=str_replace_all(AUN,"-","")) %>%
  arrange(AUN) %>% left_join(mds,by=c("AUN"="aun_2")) %>%
  mutate(AUN=as.numeric(AUN)) # need to match format in excel for successful `XLOOKUP`

setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/')
write_xlsx(parent_name_mismatches,"ct.xlsx")

# Column CU  ---------------------------------------------------------
# Count Diff Matched Students - Count of matched student cases where PA-SES file student address ≠ June file student address
standardize_address <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    str_replace_all("\\bstreet\\b", "st") %>%
    str_replace_all("\\bavenue\\b", "ave") %>%
    str_replace_all("\\bdrive\\b", "dr") %>%
    str_replace_all("\\broad\\b", "rd") %>%
    str_replace_all("\\blane\\b", "ln") %>%
    str_replace_all("\\bcourt\\b", "ct") %>%
    str_replace_all("\\bplace\\b", "pl") %>%
    str_replace_all("\\bboulevard\\b", "blvd") %>%
    str_replace_all("\\.", "")
}


#similarity score
match1$sebt_student_address1<-iconv(match1$sebt_student_address1)
match1$primero_student_address1<-iconv(match1$primero_student_address1)
match1 <- match1 %>%
  mutate(
    addr_similarity = ifelse(
      !is.na(sebt_student_address1) & sebt_student_address1 != "" &
        !is.na(primero_student_address1) & primero_student_address1 != "",
      stringdist::stringsim(
        standardize_address(sebt_student_address1),
        standardize_address(primero_student_address1),
        method = "jw" #jarowinkler
      ), NA_real_))

# address mismatches (column CU)
address_mismatches <- match1 %>%
  filter(!is.na(addr_similarity)) %>%
  filter(addr_similarity < 0.9) %>%
  group_by(sebt_aun9_3) %>%
  summarise(address_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3) %>% 
  mutate(AUN=str_replace_all(AUN,"-","")) %>%
  arrange(AUN) %>% left_join(mds,by=c("AUN"="aun_2")) %>%
  mutate(AUN=as.numeric(AUN)) # need to match format in excel for successful `XLOOKUP`

setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/')
write_xlsx(address_mismatches,"cu.xlsx")


# Column CV case number -------------------------------------------------------------

case_number_mismatches <- match1 %>%
  filter(!is.na(sebt_case_number), !is.na(primero_case_number)) %>%
  filter(sebt_case_number != primero_case_number) %>%
  group_by(sebt_aun9_3) %>%
  summarise(case_number_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3)

# Column CW & CX --------------------------------

sIDcounts <- s_undup %>%
  mutate(
    s10digitID = case_when(
      is.na(sebt_pasecureid) ~ NA_character_,
      nchar(sebt_pasecureid) == 10 ~ "TRUE",
      TRUE ~ as.character(nchar(sebt_pasecureid))
    )   ) %>%
  mutate(under10chars=if_else(s10digitID=="TRUE",1,0)) %>%
  group_by(sebt_aun9_3) %>% 
  summarise(student_Under10=sum(under10chars))


pIDcounts <- p_undup %>%
  mutate(
    p10digitID = case_when(
      is.na(primero_pasecureid) ~ NA_character_,
      nchar(primero_pasecureid) == 10 ~ "TRUE",
      TRUE ~ as.character(nchar(primero_pasecureid))
    )   ) %>%
  mutate(under10chars=if_else(p10digitID=="TRUE",1,0)) %>%
  group_by(primero_aun9_3) %>% 
  summarise(primero_Under10=sum(under10chars))


temp<-sIDcounts %>% left_join(pIDcounts,by=c("sebt_aun9_3"="primero_aun9_3")) %>%
  rename(AUN=sebt_aun9_3) %>% 
  mutate(AUN=str_replace_all(AUN,"-","")) %>% 
  arrange(AUN) %>% left_join(mds,by=c("AUN"="aun_2")) %>%
  mutate(AUN=as.numeric(AUN)) # need to match format in excel for successful `XLOOKUP`

head(temp);tail(temp)
setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/')
write_xlsx(temp,"cw.xlsx")
temp %>% filter(str_detect(sfa_name,"First"))


# Column CY --------------------------------
# number of students per AUN

temp<-match1 %>% group_by(sebt_aun9_3) %>%
  summarise(n=n()) %>% mutate(AUN=str_replace_all(sebt_aun9_3,"-","")) %>%
  arrange(AUN) %>% left_join(mds,by=c("AUN"="aun_2")) %>%
  mutate(AUN=as.numeric(AUN)) # need to match format in excel for successful `XLOOKUP`
setwd('//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/import_to_master_data_sheet/')
write_xlsx(temp,"cy.xlsx")

# Columns CZ and DA ---------------


# alternatively put all vars for masterdatasheet into one file ------------------------------------

summarydata_all <- student_counts_by_aun %>%
  full_join(eligibility_counts, by = "AUN") %>%
  full_join(primero10digitID_summary, by = "AUN") %>%
  full_join(sebt10digitID_summary, by = "AUN") %>%
  full_join(school_code_mismatches, by = "AUN") %>%
  full_join(parent_name_mismatches, by = "AUN") %>%
  full_join(address_mismatches, by = "AUN") %>%
  full_join(case_number_mismatches, by = "AUN") %>%
  arrange(AUN) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))


#2 main outputs so far:
view(summarydata_all) #summary data to be put in master sheet
view(unmatched_ids) #list of students that could not be analyzed due to mismatched IDs


# export as excel file ----------------------------------------------------

output_folder_path <- "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher output"  # Change this to your actual path

## CHANGE BASED ON BATCH
filename <- "batch1_districtdata.xlsx" #change based on batch range (default should be soemthing like "district_data") MAKE SURE ITS .XLSX
## CHANGE BASED ON BATCH

output_folder <- file.path(output_folder_path, filename)
write_xlsx(summarydata_all, path = output_folder)
cat("File saved to:", output_folder, "\n")





# temp code to make studentfile.rds ---------------------------------------

#batch1 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch1.rds")
#batch2 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch2.rds")
#batch3 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch3.rds")
#batch4 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch4.rds")
#batch5 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch5.rds")
#batch6 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles/batch6.rds")


length(unique(p2$primero_aun9_3))  # number of unique AUNs


pattern <- "^\\d{3}-\\d{2}-\\d{3}-\\d{1}$"

valid_auns_primero <- unique(p2$primero_aun9_3)
nonmatched_auns_primero <- valid_auns_primero[!grepl(pattern, valid_auns_primero)]

length(nonmatched_auns_primero)

filtered_data <- combined_data[grepl(pattern, combined_data$sebt_aun9_3), ]


folder_path <- "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSstudentfiles"

rds_files <- list.files(folder_path, pattern = "^batch[1-6]\\.rds$", full.names = TRUE)


data_list <- lapply(rds_files, readRDS)


combined_data <- do.call(rbind, data_list)

saveRDS(filtered_data, file = file.path(folder_path, "ALLstudentfiles.rds"))


# Examine the Master Data Sheet ------
library(readxl)
m <- read_excel("//csc-profiles/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/NKH Master Data Sheet_local_copy.xlsx", 
                skip = 3)

m$missingaddresscount<-ifelse(is.na(m$`Matched Students -  Student Address Count Diff`),1,0)

# missing data far more likeley among Charter and PRivate LEAs
m %>% tabyl(`Schl Type` , missingaddresscount) %>% adorn_percentages()
