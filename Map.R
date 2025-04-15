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

#PO Data 2019 and 2021
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

#Data distribution
summary(bound$Change)

#Plot the Map
ggplot(data = bound) +
  geom_sf(aes(fill = Change)) +
  scale_fill_viridis_c(option = "plasma", name = "% Change") +
  labs(title = "Percent Change in Palm Oil Expansion: 2019 to 2021") +
  theme_minimal()
