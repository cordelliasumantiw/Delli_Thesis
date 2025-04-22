install.packages(c("ggplot2", "sf", "dplyr", "viridis", "newscale", "shadowtext", "patchwork", "extrafont"))
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(tidyr)
library(grid)
library(shadowtext)
library(patchwork)
library(extrafont)

font_import()
y

loadfonts()

bound <- st_read("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/R files/Dist Boundaries/idn_admbnda_adm2_bps_20200401.shp")
bound <- bound %>%
  select(Shape_Leng, Shape_Area, ADM2_EN, geometry)
bound$ADM2_EN <- toupper(bound$ADM2_EN)

str(bound)
str(po_data)

#---------------------------------------------------------
#Cumulative Area Columns
po_cumulative <- po_data %>%
  group_by(year) %>%
  summarise(total_area = sum(OP_Area, na.rm = TRUE)) %>%
  mutate(cumulative_area = cumsum(total_area))

#1. Plot the Total Area Annualy
ggplot(po_cumulative, aes(x = year, y = total_area)) +
  geom_line(color = "#076FA1", size = 1.75) +
  geom_point(size = 2.5,
             color = "#076FA1",
             stroke = 1) +
  labs(
    title = "Palm Oil Expansion from 2003-2022",
    x = "Year",
    y = "Palm Oil Area (1000ha)"
  ) +
  scale_x_continuous(
    breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021),
    labels = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019", "2021")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(po_cumulative$total_area), by = 1000)
  ) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length.y = unit(0, "mm"),
    axis.ticks.length.x = unit(2, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y = element_text(family = "Helvetica", size = 10),
    axis.text.x = element_text(family = "Helvetica", size = 10),
    plot.title = element_text(family = "Helvetica", size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(family = "Helvetica", size = 10),
    axis.title.y = element_text(family = "Helvetica", size = 10)
  )


#---------------------------------------------------------
#2. PERCENTAGE CHANGE FROM 2003 TO 2022
#Separate both Datasets
po_data_2003 <- po_data %>% filter(year == 2003)
po_data_2022 <- po_data %>% filter(year == 2022)
po_data_change <- po_data_2003 %>%
  select(region, OP_Area) %>%
  rename(OP_Area_2003 = OP_Area) %>%
  left_join(po_data_2022 %>% select(region, OP_Area) %>% rename(OP_Area_2022 = OP_Area), by = "region")

#Percentage Change
po_data_change <- po_data_change %>%
  mutate(Change = ((OP_Area_2022 - OP_Area_2003) / OP_Area_2003) * 100)

#Change NAs to 0
po_data_change <- po_data_change %>%
  mutate(Change = ifelse(is.na(Change), 0, Change))

na_check_csv_comb <- sapply(po_data_change, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#Merge the Change with Bound
bound <- bound %>%
  left_join(po_data_change %>% select(region, Change), by = c("ADM2_EN" = "region"))

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


#Plot the Map!!!
#Classification based on Quantile CLassification
bound <- bound %>%
  mutate(Change_Class = cut(Change,
                            breaks = c(-Inf, 0, 20, 40, 60, 80, 100, Inf),
                            labels = c("No Change", "1% 20%", "20% - 40%", "40% - 60%", "60% - 80%", "80% - 100%", ">100%"),
                            include.lowest = TRUE))

if (!"Change_Class" %in% names(bound)) {
  bound$Change_Class <- "No Data"
}

bound$Change_Class <- factor(bound$Change_Class, levels = c("No Change", "1% - 20%", "20% - 40%", 
                                                            "40% - 60%", "60% - 80%", "80% - 100%", 
                                                            ">100%", "No Data"))

ggplot(data = bound) +
  geom_sf(aes(fill = Change_Class)) +
  scale_fill_manual(values = c("No Change" = "#eff3ff", 
                               "1% - 20%" = "#c6dbef", 
                               "20% - 40%" = "#9ecae1", 
                               "40% - 60%" = "#6baed6", 
                               "60% - 80%" = "#4292c6", 
                               "80% - 100%" = "#2171b5", 
                               ">100%" = "#084594",
                               "No Data" = "#636363"),
                    name = "% Change",
                    breaks = c("No Change", "1% - 20%", "20% - 40%", "40% - 60%", "60% - 80%", "80% - 100%", ">100%", "No Data")) +
  labs(title = "Percentage Change in Palm Oil Expansion (2003-2022)") +
  theme_minimal(base_family = "Helvetica")
