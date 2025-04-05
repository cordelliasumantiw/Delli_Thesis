library(dplyr)
library(readr)
library(stringr)

#COMBINE VILLAGE-LEVEL DATA FROM 2019-2021#
#Combine all 789 District Stunting Data
folder_stunting <- "C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/Datanya Deli All"
file_list <- list.files(path = folder_stunting, pattern = "*.csv", full.names = TRUE)

#Column Consistency
read_clean_csv <- function(file) {
  read_csv(file, col_types = cols(.default = "c"))
}

csv_combined <- file_list %>%
  lapply(read_clean_csv) %>%
  bind_rows()

#Change into Numerical Columns & Remove NAs
na_check_csv_comb <- sapply(csv_combined, function(x) sum(is.na(x)))
print(na_check_csv_comb)

csv_combined$`JUMLAH BALITA` <- as.numeric(gsub("[^0-9]", "", csv_combined$`JUMLAH BALITA`))
csv_combined$`JUMLAH PENDEK` <- as.numeric(gsub("[^0-9]", "", csv_combined$`JUMLAH PENDEK`))


csv_combined$`JUMLAH BALITA` <- as.numeric(csv_combined$`JUMLAH BALITA`)
csv_combined$`JUMLAH PENDEK` <- as.numeric(csv_combined$`JUMLAH PENDEK`)
csv_combined$`JUMLAH SANGAT PENDEK` <- as.numeric(csv_combined$`JUMLAH SANGAT PENDEK`)
csv_combined$`%` <- as.numeric(gsub("%", "", csv_combined$`%`))

str(csv_combined)

#----------------------------------------------------------------
#ADD THE DISTRICT AND YEAR#
#Read CSV and extract District name & Year; Add Total Stunting; Rename Column

read_csv_file_name <- function(file) {
  #extract the file name
  file_name <- basename(file)
  
  #extract the District name
  district_name <- str_extract(file_name, "(?<=_)[^_]+(?=_\\d{4})")
  
  #extract Year
  year <- str_extract(file_name, "\\d{4}")
  
  #read the CSV file
  df <- read_csv(file, col_types = cols(.default = "c")) %>%
    rename(
      `Total Children` = `JUMLAH BALITA`,
      `Short` = `JUMLAH PENDEK`,
      `Very Short` = `JUMLAH SANGAT PENDEK`,
      `Village` = `DESA`,
      `SubDist` = `KECAMATAN` 
    ) %>%
    mutate(District = district_name,
           Year = as.integer(year),
           `Total Children` = as.numeric(gsub("[^0-9]", "", `Total Children`)),
           `Short` = as.numeric(gsub("[^0-9]", "", `Short`)),
           `Very Short` = as.numeric(gsub("[^0-9]", "", `Very Short`)),
           `Total Stunting` = `Short` + `Very Short`
    ) %>%
    
    relocate(`Total Stunting`, .after = `Very Short`)
  
  return(df)
  
}

#Read and merge all CSV files
csv_combined <- file_list %>%
  lapply(read_csv_file_name) %>%
  bind_rows()

#CUMA BUAT CROSSCHECK Calculate Stuntng Rate for a Specific District and Year
speci_data <- csv_combined %>%
  filter(District == "SRAGEN", Year == "2020") %>%
  summarise(
    Total_Children = sum(`Total Children`, na.rm = TRUE),
    Total_Stunt = sum(`Total Stunting`, na.rm = TRUE),
    StuntRate = (sum(`Total Stunting`, na.rm = TRUE) / sum(`Total Children`, na.rm = TRUE)) * 100
  )

#CUMA BUAT CROSSCHECK If there are any NA values in the columns
na_check_csv_comb <- sapply(csv_combined, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#----------------------------------------------------------------
#AGGREGATE TO DISTRICT LEVEL#

#Convert to Numerical AGAIN
sum(is.na(csv_combined$`Total Children`))
sum(is.na(csv_combined$`%`))
csv_combined$`Total Children` <- as.numeric(csv_combined$`Total Children`)
csv_combined$`%` <- as.numeric(gsub("%", "", csv_combined$`%`))

str(csv_combined)

#Aggregate
aggregated_csv_combined <- csv_combined %>%
  group_by(Year, District) %>%
  summarise(
    Total_Children = sum(`Total Children`, na.rm = TRUE),
    Short = sum(Short, na.rm = TRUE),
    Very_Short = sum(`Very Short`, na.rm = TRUE),
    Total_Stunting = sum(`Total Stunting`, na.rm = TRUE),
    StuntRate = (sum(`Total Stunting`, na.rm = TRUE) / sum(`Total Children`, na.rm = TRUE)) * 100
  )
aggregated_csv_combined$StuntRate[is.na(aggregated_csv_combined$StuntRate) | is.nan(aggregated_csv_combined$StuntRate)] <- 0

#Buat Check Ada yg NA Value
na_check_csv_comb <- sapply(aggregated_csv_combined, function(x) sum(is.na(x)))
print(na_check_csv_comb)

str(aggregated_csv_combined)
head(aggregated_csv_combined)
#--------------------------------------------------------
#PANEL DATA CONSTRUCTION#
#1. Panel Data District FIX
subset_data <- aggregated_csv_combined %>%
  filter(is.finite(StuntRate))

stunt_panel <- subset_data %>%
  group_by(District) %>%
  filter(n_distinct(Year) == 3) %>%
  ungroup()
str(stunt_panel)

#2. Panel Data Village FIX
subset_data2 <- csv_combined %>%
  filter(is.finite(`%`))

#Buat Check Ada yg NA Value
na_check_csv_comb <- sapply(subset_data2, function(x) sum(is.na(x)))
print(na_check_csv_comb)

stunt_panel_vil <- subset_data2 %>%
  group_by(District) %>%
  filter(n_distinct(Year) == 3) %>%
  ungroup()
str(stunt_panel)
#----------------------------------------------------------------
#MAP OUT THE District level Data
