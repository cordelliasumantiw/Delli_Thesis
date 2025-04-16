install.packages(c("ggplot2", "sf", "dplyr", "viridis"))
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(tidyr)

bound <- st_read("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/R files/Dist Boundaries/idn_admbnda_adm2_bps_20200401.shp")
bound$ADM2_EN <- toupper(bound$ADM2_EN)

str(bound)
str(po_data)

#PERCENTAGE CHANGE FROM 2019 TO 2021
#Separate both Datasets
po_data_2019 <- po_data %>% filter(year == 2019)
po_data_2021 <- po_data %>% filter(year == 2021)
po_data_1921 <- po_data_2019 %>%
  select(region, OP_Area) %>%
  rename(OP_Area_2019 = OP_Area) %>%
  left_join(po_data_2021 %>% select(region, OP_Area) %>% rename(OP_Area_2021 = OP_Area), by = "region")

#Percentage Change
po_data_1921 <- po_data_1921 %>%
  mutate(Change = ((OP_Area_2021 - OP_Area_2019) / OP_Area_2019) * 100)

#Change NAs to 0
po_data_1921 <- po_data_1921 %>%
  mutate(Change = ifelse(is.na(Change), 0, Change))

na_check_csv_comb <- sapply(po_data_1921, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#Merge the Change with Bound
bound <- bound %>%
  left_join(po_data_1921 %>% select(region, Change), by = c("ADM2_EN" = "region"))

na_check_csv_comb <- sapply(bound, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#CUMA Check the District names
dist_bound <- unique(bound$ADM2_EN)
dist_po_data <- unique(po_data$region)
dist_not_in_bound <- setdiff(dist_po_data, dist_bound)
dist_not_in_bound

#CUMA BUAT Change the District Names to be Align with the bound's
po_data <- po_data %>%
  mutate(region = case_when(
    region == "KOTA BAU-BAU" ~ "KOTA BAUBAU",
    region == "KARANGASEM" ~ "KARANG ASEM",
    region == "KEPULAUAN SIAU TAGULANDANG BIARO" ~ "SIAU TAGULANDANG BIARO",
    region == "KOTA KEPULAUAN TIDORE" ~ "KOTA TIDORE KEPULAUAN",
    region == "KOTABARU" ~ "KOTA BARU",
    region == "KOTA LUBUK LINGGAU" ~ "KOTA LUBUKLINGGAU",
    region == "MAHAKAM ULU" ~ "MAHAKAM HULU",
    region == "MOROTAI" ~ "PULAU MOROTAI",
    region == "MUKO-MUKO" ~ "MUKOMUKO",
    region == "KOTA SAWAHLUNTO" ~ "KOTA SAWAH LUNTO",
    TRUE ~ region
  ))

#Check the Data distribution
summary(bound$Change)

#Plot the Map!!!
#Classification based on Quantile CLassification
bound <- bound %>%
  mutate(Change_Class = cut(Change,
                            breaks = c(-Inf, 0, 1, 5, 20, Inf),
                            labels = c("No Change", "1% or below", "1% - 5%", "5% - 20%", ">20%"),
                            include.lowest = TRUE))

ggplot(data = bound) +
  geom_sf(aes(fill = Change_Class)) +
  scale_fill_manual(values = c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d"),
                    name = "% Change",
                    breaks = c("No Change", "1% or below", "1% - 5%", "5% - 20%", ">20%")) +
  labs(title = "Percent Change in Palm Oil Expansion: 2019 to 2021") +
  theme_minimal()
