install.packages(c("ggplot2", "sf", "dplyr", "viridis"))
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(tidyr)

bound <- st_read("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/R files/Dist Boundaries/idn_admbnda_adm2_bps_20200401.shp")
bound$ADM2_EN <- toupper(bound$ADM2_EN)

#Calculate the Percentage Change in Years
str(po_data)
#Filter only for 2019, 2020, 2021
po_filter <- po_data

po_filter <- po_filter %>%
  pivot_wider(names_from = year, values_from = OP_Area)

#Calculate the Perct. Change
po_filter <- po_filter %>%
  mutate(
    change1920 = ((`2020` - `2019`) / `2019`) * 100,
    change2021 = ((`2021` - `2020`) / `2020`) * 100,
    change1921 = ((`2021` - `2019`) / `2019`) * 100,
  )
head(po_filter)

#Replace NaNs
po_filter <- po_filter %>%
  mutate(
    change1920 = ifelse(is.nan(change1920), 0, change1920),
    change2021 = ifelse(is.nan(change2021), 0, change2021),
    change1921 = ifelse(is.nan(change1921), 0, change1921)
  )

#Merge back
po_data_changes <- po_filter %>%
  left_join(po_filter %>%
              select(region, change1920, change2021, change1921),
            by = "region")
