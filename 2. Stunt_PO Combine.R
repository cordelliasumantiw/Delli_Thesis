library(readr)
library(readxl)
library(tidyverse)
getwd()

#Combine PO Data with Stunting Data#
po_data <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/R files/Stunting Data Combined FIXXX/indonesia-palm-oil-spatial-v5-oil-palm-planted-area-2024-10-29-kabupaten.csv")

str(po_data)

na_check_csv_comb <- sapply(po_data, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#Standardize both dataset
#Set the 'District' columns to upper case letter
csv_combined$District <- toupper(csv_combined$District)
stunt_panel$District <- toupper(stunt_panel$District)
po_data$region <- toupper(po_data$region)

#Remove parentheses and re-arrange
po_data$region <- gsub("(.*) \\((.*)\\)", "\\2 \\1", po_data$region)

po_data <- po_data %>%
  mutate(region = case_when(
    region == "KOTA GUNUNG SITOLI" ~ "KOTA GUNUNGSITOLI",
    region == "LIMAPULUHKOTA" ~ "LIMA PULUH KOTA",
    region == "PANGKAJENE KEPULAUAN" ~ "PANGKAJENE DAN KEPULAUAN",
    region == "BANYUASIN" ~ "BANYU ASIN",
    region == "KOTA BANJARBARU" ~ "KOTA BANJAR BARU",
    region == "KOTA PADANG SIDEMPUAN" ~ "KOTA PADANGSIDIMPUAN",
    region == "TULANG BAWANG" ~ "TULANGBAWANG",
    TRUE ~ region
  ))

#Combine PO Data with Stunt at district-level
subset_data$District <- toupper(subset_data$District)

combined_stunt_po_dist <- merge(stunt_panel, po_data, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
stunt_po_dist <- combined_stunt_po_dist[, c("Year", "District", "Total_Children", "Short", "Very_Short", "Total_Stunting", "StuntRate", "oil_palm_planted_area_hectares")]
stunt_po_dist$oil_palm_planted_area_hectares[stunt_po_dist$District == "LOMBOK UTARA" & is.na(stunt_po_dist$oil_palm_planted_area_hectares)] <- 0

str(stunt_po_dist)
na_check_csv_comb <- sapply(stunt_po_dist, function(x) sum(is.na(x)))
print(na_check_csv_comb)
districts_na_dist <- stunt_po_dist %>% filter(is.na(oil_palm_planted_area_hectares))


#Combine PO with Stunt at village-level
combined_stunt_po_village <- merge(stunt_panel_vil, po_data, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
stunt_po_village <- combined_stunt_po_village[, c("Year", "District", "SubDist", "Village", "Total Children", "Short", "Very Short", "Total Stunting", "%", "oil_palm_planted_area_hectares")]

##Re-check Vill with NA
na_check_csv_comb <- sapply(stunt_po_village, function(x) sum(is.na(x)))
print(na_check_csv_comb)
districts_na_vil <- stunt_po_village %>% filter(is.na(oil_palm_planted_area_hectares))

##Replace the NAs
stunt_po_village$oil_palm_planted_area_hectares[
  stunt_po_village$District %in% c("LOMBOK UTARA", "KEPULAUAN TANIMBAR", "KOTA CIMAHI", "KOTA MAGELANG", "KOTA MATARAM", "KOTA PEKALONGAN", "KOTA SURAKARTA", "KOTA TEGAL") &
    is.na(stunt_po_village$oil_palm_planted_area_hectares)
] <- 0



#----------------------------------------------------------------
#Simple Regression District level
lm(stunt_po_dist$StuntRate~stunt_po_dist$oil_palm_planted_area_hectares, data = stunt_po_dist)
summary(lm(stunt_po_dist$StuntRate~stunt_po_dist$oil_palm_planted_area_hectares, data = stunt_po_dist))

#Table
dist_reg <- lm(stunt_po_dist$StuntRate~stunt_po_dist$oil_palm_planted_area_hectares, data = stunt_po_dist)
dist_reg_table <- tidy(dist_reg)

stargazer(dist_reg, title = "Dist Regression Results",
          dep.var.labels = "Stunting Rate",
          covariate.labels = "OP Planted Area (ha)",
          type = "text")


rm(firs_reg)

#Table
install.packages("broom")
install.packages("stargazer")
library(broom)
library(stargazer)
vil_reg <- lm(stunt_po_village$`%`~stunt_po_village$oil_palm_planted_area_hectares, data = stunt_po_village)
vil_reg_table <- tidy(vil_reg)

stargazer(vil_reg, title = "Village Regression Results",
          dep.var.labels = "Stunting Rate",
          covariate.labels = "OP Planted Area (ha)",
          type = "text")


#Simple Regression Village level
lm(stunt_po_village$`%`~stunt_po_village$oil_palm_planted_area_hectares, data = stunt_po_village)
summary(lm(stunt_po_village$`%`~stunt_po_village$oil_palm_planted_area_hectares, data = stunt_po_village))


#----------------------------------------------------------------
#Demographic and Health Survey Data
library(haven)

#PODES Survey
podes2019 <- read_dta
load("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/R files/Stunting Data Combined FIXXX/PODES/PODES/data/PODES2019.rda")




child5_dhs <- read_dta("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/R files/Stunting Data Combined FIXXX/DHS/IDKR71DT/IDKR71FL.DTA")
hh_dhs <- read_dta("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/R files/Stunting Data Combined FIXXX/DHS/IDHR71DT/IDHR71FL.DTA")
women_dhs <- read_dta("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/R files/Stunting Data Combined FIXXX/DHS/IDIR71DT/IDIR71FL.DTA")

install.packages("SUSENAS")
library(SUSENAS)
data("SUSENAS2020")
