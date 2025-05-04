library(readr)
library(readxl)
library(tidyverse)
library(plm)
getwd()

#Combine PO Data with Stunting Data#
po_data <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/1. Indonesia Stunting Data/R files/Stunting Data Combined FIXXX/indonesia-palm-oil-spatial-v5-oil-palm-planted-area-2024-10-29-kabupaten.csv")

str(po_data)

na_check_csv_comb <- sapply(po_data, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#Standardize both dataset
#Set the 'District' and 'Village' columns to upper case letter
csv_combined$District <- toupper(csv_combined$District)
stunt_panel$District <- toupper(stunt_panel$District)
po_data$region <- toupper(po_data$region)
stunt_panel_vil$District <- toupper(stunt_panel_vil$District)
stunt_panel_vil$Village <- toupper(stunt_panel_vil$Village)

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

#Convert from Hectares to Thousnads of Hectares
po_data$oil_palm_planted_area_hectares <- po_data$oil_palm_planted_area_hectares / 1000
po_data <- po_data %>%
  rename(OP_Area = oil_palm_planted_area_hectares)

#1. Combine PO Data with Stunt at district-level
subset_data$District <- toupper(subset_data$District)
stunt_panel$District <- toupper(stunt_panel$District)

#Full Stunt-PO by District
full_comb_stunt_po_dist <- merge(subset_data, po_data, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
full_stunt_po_dist <- full_comb_stunt_po_dist[, c("Year", "District", "Total_Children", "Short", "Very_Short", "Total_Stunting", "StuntRate", "OP_Area")]
full_stunt_po_dist$OP_Area[full_stunt_po_dist$District == "LOMBOK UTARA" & is.na(full_stunt_po_dist$OP_Area)] <- 0
##Replace the NAs
full_stunt_po_dist$OP_Area[
  full_stunt_po_dist$District %in% c("KEPULAUAN TANIMBAR", "KOTA CIMAHI", "KOTA MAGELANG", "KOTA MATARAM", "KOTA PEKALONGAN", "KOTA SURAKARTA", "KOTA TEGAL", "KOTABARU") &
    is.na(full_stunt_po_dist$OP_Area)
] <- 0

#Balanced Stunt-PO by District
combined_stunt_po_dist <- merge(stunt_panel, po_data, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
stunt_po_dist <- combined_stunt_po_dist[, c("Year", "District", "Total_Children", "Short", "Very_Short", "Total_Stunting", "StuntRate", "OP_Area")]
stunt_po_dist$OP_Area[stunt_po_dist$District == "LOMBOK UTARA" & is.na(stunt_po_dist$OP_Area)] <- 0

na_check_csv_comb <- sapply(stunt_po_dist, function(x) sum(is.na(x)))
print(na_check_csv_comb)
districts_na_dist <- stunt_po_dist %>% filter(is.na(OP_Area))


#2. Combine PO with Stunt at village-level
subset_data2$District <- toupper(subset_data2$District)
stunt_panel_vil$District <- toupper(stunt_panel_vil$District)

#Full Stunt-PO by Village
full_combined_stunt_po_village <- merge(subset_data2, po_data, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
full_stunt_po_village <- full_combined_stunt_po_village[, c("Year", "District", "SubDist", "Village", "Total Children", "Short", "Very Short", "Total Stunting", "%", "OP_Area")]
full_stunt_po_village <- full_stunt_po_village %>%
  rename(StuntRate = `%`)
##Replace the NAs
full_stunt_po_village$OP_Area[
  full_stunt_po_village$District %in% c("LOMBOK UTARA", "KEPULAUAN TANIMBAR", "KOTA CIMAHI", "KOTA MAGELANG", "KOTA MATARAM", "KOTA PEKALONGAN", "KOTA SURAKARTA", "KOTA TEGAL", "KOTABARU") &
    is.na(full_stunt_po_village$OP_Area)
] <- 0
str(stunt_po_village)

###Balanced Stunt-PO by Village
combined_stunt_po_village <- merge(stunt_panel_vil, po_data, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
stunt_po_village <- combined_stunt_po_village[, c("Year", "District", "SubDist", "Village", "Total Children", "Short", "Very Short", "Total Stunting", "%", "OP_Area")]
stunt_po_village <- stunt_po_village %>%
  rename(StuntRate = `%`)
##Replace the NAs
combined_stunt_po_village$OP_Area[
  combined_stunt_po_village$District %in% c("LOMBOK UTARA", "KEPULAUAN TANIMBAR", "KOTA CIMAHI", "KOTA MAGELANG", "KOTA MATARAM", "KOTA PEKALONGAN", "KOTA SURAKARTA", "KOTA TEGAL", "KOTABARU") &
    is.na(combined_stunt_po_village$OP_Area)
] <- 0

##Re-check Vill with NA
na_check_csv_comb <- sapply(stunt_po_village, function(x) sum(is.na(x)))
print(na_check_csv_comb)
districts_na_vil <- combined_stunt_po_village %>% filter(is.na(OP_Area))

#Change Villages name that have the exact same name
stunt_po_village <- stunt_po_village %>%
  group_by(Year, Village) %>%
  mutate(Village_code = paste0(Village, "_", row_number())) %>%
  ungroup()

#----------------------------------------------------------------

str(full_stunt_po_village)


#1. Simple Regression Village level
#Full Panel - Fixed Effects Reg
plm(StuntRate ~ OP_Area, data = full_stunt_po_village, model = "within", index = c("Village_code", "Year"))
summary(plm(StuntRate ~ OP_Area, data = full_stunt_po_village, model = "within", index = c("Village_code", "Year")))

#Balanced Panel - Fixed Effects Reg
plm(StuntRate ~ OP_Area, data = stunt_po_village, model = "within", index = c("Village", "Year"))
summary(plm(StuntRate ~ OP_Area, data = stunt_po_village, model = "within", index = c("Village", "Year")))

#Table
dist_reg <- lm(stunt_po_dist$StuntRate~stunt_po_dist$oil_palm_planted_area_hectares, data = stunt_po_dist)
dist_reg_table <- tidy(dist_reg)

stargazer(dist_reg, title = "Dist Regression Results",
          dep.var.labels = "Stunting Rate",
          covariate.labels = "OP Planted Area (ha)",
          type = "text")

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
lm(stunt_po_village$`%`~stunt_po_village$OP_Area, data = stunt_po_village)
summary(lm(stunt_po_village$`%`~stunt_po_village$OP_Area, data = stunt_po_village))

