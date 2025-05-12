#Palm Oil lagged data
#Year 2019
lag_2019 <- po_data %>%
  filter(year >= 2004 & year <= 2018) %>%
  select(year, region, OP_Area)
lag_2019 <- lag_2019 %>%
  pivot_wider(names_from = year, values_from = OP_Area, names_prefix = "OP_Area_")

#Divide it into 2 Categories (5 & 10 years before)
lag_2019_avg <- lag_2019 %>%
  rowwise() %>%
  mutate(
    avg_OP_first = mean(c_across(starts_with("OP_Area_") & matches("2014|2015|2016|2017|2018")), na.rm = TRUE),
    avg_OP_second = mean(c_across(starts_with("OP_Area_") & matches("2009|2010|2011|2012|2013")), na.rm = TRUE)
  ) %>%
  ungroup()

#Join the AVG. OP Data 2019 with the lags
po_2019 <- po_data %>%
  filter(year == 2019)
po_2019 <- po_2019 %>%
  left_join(lag_2019_avg, by = "region")
po_2019 <- po_2019 %>%
  select(year, region, OP_Area_2018:OP_Area_2004, avg_OP_first, avg_OP_second) %>%
  rename(
    po_lag1 = OP_Area_2018,
    po_lag2 = OP_Area_2017,
    po_lag3 = OP_Area_2016,
    po_lag4 = OP_Area_2015,
    po_lag5 = OP_Area_2014,
    po_lag6 = OP_Area_2013,
    po_lag7 = OP_Area_2012,
    po_lag8 = OP_Area_2011,
    po_lag9 = OP_Area_2010,
    po_lag10 = OP_Area_2009,
    po_lag11 = OP_Area_2008,
    po_lag12 = OP_Area_2007,
    po_lag13 = OP_Area_2006,
    po_lag14 = OP_Area_2005,
    po_lag15 = OP_Area_2004
  )

#-------------------------------
#Year 2020
lag_2020 <- po_data %>%
  filter(year >= 2005 & year <= 2019) %>%
  select(year, region, OP_Area)
lag_2020 <- lag_2020 %>%
  pivot_wider(names_from = year, values_from = OP_Area, names_prefix = "OP_Area_")

#Divide it into 2 Categories (5 & 10 years before)
lag_2020_avg <- lag_2020 %>%
  rowwise() %>%
  mutate(
    avg_OP_first = mean(c_across(starts_with("OP_Area_") & matches("2015|2016|2017|2018|2019")), na.rm = TRUE),
    avg_OP_second = mean(c_across(starts_with("OP_Area_") & matches("2010|2011|2012|2013|2014")), na.rm = TRUE)
  ) %>%
  ungroup()

#Join the OP Data 2020 with the lags
po_2020 <- po_data %>%
  filter(year == 2020)
po_2020 <- po_2020 %>%
  left_join(lag_2020_avg, by = "region")
po_2020 <- po_2020 %>%
  select(year, region, OP_Area_2019:OP_Area_2005, avg_OP_first, avg_OP_second) %>%
  rename(
    po_lag1 = OP_Area_2019,
    po_lag2 = OP_Area_2018,
    po_lag3 = OP_Area_2017,
    po_lag4 = OP_Area_2016,
    po_lag5 = OP_Area_2015,
    po_lag6 = OP_Area_2014,
    po_lag7 = OP_Area_2013,
    po_lag8 = OP_Area_2012,
    po_lag9 = OP_Area_2011,
    po_lag10 = OP_Area_2010,
    po_lag11 = OP_Area_2009,
    po_lag12 = OP_Area_2008,
    po_lag13 = OP_Area_2007,
    po_lag14 = OP_Area_2006,
    po_lag15 = OP_Area_2005
  )

#-------------------------------
#Year 2021
lag_2021 <- po_data %>%
  filter(year >= 2006 & year <= 2020) %>%
  select(year, region, OP_Area)
lag_2021 <- lag_2021 %>%
  pivot_wider(names_from = year, values_from = OP_Area, names_prefix = "OP_Area_")

#Divide it into 2 Categories (5 & 10 years before)
lag_2021_avg <- lag_2021 %>%
  rowwise() %>%
  mutate(
    avg_OP_first = mean(c_across(starts_with("OP_Area_") & matches("2016|2017|2018|2019|2020")), na.rm = TRUE),
    avg_OP_second = mean(c_across(starts_with("OP_Area_") & matches("2011|2012|2013|2014|2015")), na.rm = TRUE)
  ) %>%
  ungroup()

#Join the OP Data 2021 with the lags
po_2021 <- po_data %>%
  filter(year == 2021)
po_2021 <- po_2021 %>%
  left_join(lag_2021_avg, by = "region")
po_2021 <- po_2021 %>%
  select(year, region, OP_Area_2020:OP_Area_2006, avg_OP_first, avg_OP_second) %>%
  rename(
    po_lag1 = OP_Area_2020,
    po_lag2 = OP_Area_2019,
    po_lag3 = OP_Area_2018,
    po_lag4 = OP_Area_2017,
    po_lag5 = OP_Area_2016,
    po_lag6 = OP_Area_2015,
    po_lag7 = OP_Area_2014,
    po_lag8 = OP_Area_2013,
    po_lag9 = OP_Area_2012,
    po_lag10 = OP_Area_2011,
    po_lag11 = OP_Area_2010,
    po_lag12 = OP_Area_2009,
    po_lag13 = OP_Area_2008,
    po_lag14 = OP_Area_2007,
    po_lag15 = OP_Area_2006
  )

#---------------------------------------------------------
#Combine avg lags for year 2019-2021
po_lag <- bind_rows(po_2019, po_2020, po_2021)
po_lag <- po_lag %>%
  select(year, region, avg_OP_first, avg_OP_second)

po_lag <- po_lag %>%
  rename(Year = year, District = region)

#---------------------------------------------------------
#1.1 Combine with other Variablesss FIX (FULL Panel)
all_data_vil <- all_data_vil %>%
  left_join(po_lag, by = c("Year", "District"))

##Replace the NAs
all_data_vil <- all_data_vil %>%
  mutate(
    avg_OP_first = ifelse(is.na(avg_OP_first), 0, avg_OP_first),
    avg_OP_second = ifelse(is.na(avg_OP_second), 0, avg_OP_second)
  )

na_check_csv_comb <- sapply(all_data_vil, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#-------------
#1.2 Combine with other Variablesss FIX (BALANCED Panel)
bal_all_data_vil <- bal_all_data_vil %>%
  left_join(po_lag, by = c("Year", "District"))

##Replace the NAs
bal_all_data_vil <- bal_all_data_vil %>%
  mutate(
    avg_OP_first = ifelse(is.na(avg_OP_first), 0, avg_OP_first),
    avg_OP_second = ifelse(is.na(avg_OP_second), 0, avg_OP_second)
  )

na_check_csv_comb <- sapply(bal_all_data_vil, function(x) sum(is.na(x)))
print(na_check_csv_comb)
#----------------------------------------------------------------
deforest <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/spatial-metrics-indonesia-territorial_deforestation_kabupaten.csv")

#Remove parentheses and re-arrange
deforest$region <- toupper(deforest$region)
deforest$region <- gsub("(.*) \\((.*)\\)", "\\2 \\1", deforest$region)
deforest$deforestation_hectares <- deforest$deforestation_hectares / 1000
deforest <- deforest %>%
  rename(
    deforest = deforestation_hectares,
    Year = year,
    District = region)

#1.1 Combine with other Variablesss FIX (FULL)
all_data <- all_data_vil %>%
  left_join(deforest, by = c("Year", "District"))

all_data <- all_data %>%
  select(Year, District, SubDist, Village_code, `Total Children`, `Total Stunting`, StuntRate, OP_Area,
         Child_SupFeed, VitA, Compl_Imun, CleanWater, Sanitation, BKB, IntSerPost, Health_Insur,
         Poverty, FoodExp, Wom_AvgSch, NoElectricity, HealWork, avg_OP_first, avg_OP_second, deforest)


##Replace the NAs
all_data <- all_data %>%
  mutate(
    deforest = ifelse(is.na(deforest), 0, deforest)
  )


na_check_csv_comb <- sapply(all_data, function(x) sum(is.na(x)))
print(na_check_csv_comb)
na <- all_data %>% filter(is.na(deforest))


#1.2 Combine with other Variablesss FIX (BALANCED Panel)
bal_all_data <- bal_all_data_vil %>%
  left_join(deforest, by = c("Year", "District"))

bal_all_data <- bal_all_data %>%
  select(Year, District, SubDist, Village_code, `Total Children`, `Total Stunting`, StuntRate, OP_Area,
         Child_SupFeed, VitA, Compl_Imun, CleanWater, Sanitation, BKB, IntSerPost, Health_Insur,
         Poverty, FoodExp, Wom_AvgSch, NoElectricity, HealWork, avg_OP_first, avg_OP_second, deforest)


##Replace the NAs
bal_all_data <- bal_all_data %>%
  mutate(
    deforest = ifelse(is.na(deforest), 0, deforest)
  )


na_check_csv_comb <- sapply(bal_all_data, function(x) sum(is.na(x)))
print(na_check_csv_comb)
na <- bal_all_data %>% filter(is.na(deforest))
#----------------------------------------------------------------
#FE Regression
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun +
                          CleanWater + Sanitation + BKB + IntSerPost + Health_Insur +
                          Poverty + Wom_AvgSch + FoodExp + NoElectricity + HealWork + avg_OP_first + avg_OP_second + deforest | Village_code + Year, data = bal_all_data)
summary(stunting_model)
