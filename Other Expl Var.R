#COMBINE Explanatory Variables Village-level Data FROM 2019-2021#
install.packages("tools")
library(dplyr)
library(tidyr)
library(readr)
library(tools)
folder_services1 <- "C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Scrape Data/2019"
file_services1 <- list.files(path = folder_services1, pattern = "*.csv", full.names = TRUE)
folder_services2 <- "C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Scrape Data/2020"
file_services2 <- list.files(path = folder_services2, pattern = "*.csv", full.names = TRUE)
folder_services3 <- "C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Scrape Data/2021"
file_services3 <- list.files(path = folder_services3, pattern = "*.csv", full.names = TRUE)

#Column Consistency
read_clean_csv <- function(file) {
  read_csv(file, col_types = cols(.default = "c"))
}

csv_services1 <- file_services1 %>%
  lapply(read_clean_csv) %>%
  bind_rows()
csv_services2 <- file_services2 %>%
  lapply(read_clean_csv) %>%
  bind_rows()
csv_services3 <- file_services3 %>%
  lapply(read_clean_csv) %>%
  bind_rows()

#Add Year, Rename Columns
library(tools)
csv_services1 <- csv_services1 %>%
  rename(
    No. = Field1,
    BPS_ID = Phone,
    MoIA_ID = Phone1,
    Village = Field2,
    Wom_SupFeed = Field3,
    Wom_IFA = Field4,
    Child_SupFeed = Field5,
    IntSerPost = Field6,
    Wom_K4 = Field7,
    VitA = Field8,
    Compl_Imun = Field9,
    Zinc = Field10,
    TG_BBT = Field11,
    PostNatal_Care = Field12,
    Nutri_Couns = Field13,
    BKB = Field14,
    CleanWater = Field15,
    Sanitation = Field16,
    Parenting = Field17,
    EarlChildEdu = Field18,
    Health_Insur = Field19,
    KPM_FDS = Field20,
    HPK_BPNT = Field21,
    KRP_Vil = Field22
  ) %>%
  mutate(
    Village = toTitleCase(Village),
    Year = 2019
    )

csv_services2 <- csv_services2 %>%
  rename(
    No. = Field1,
    BPS_ID = Phone,
    MoIA_ID = Phone1,
    Village = Field2,
    Wom_SupFeed = Field3,
    Wom_IFA = Field4,
    Child_SupFeed = Field5,
    IntSerPost = Field6,
    Wom_K4 = Field7,
    VitA = Field8,
    Compl_Imun = Field9,
    Zinc = Field10,
    TG_BBT = Field11,
    PostNatal_Care = Field12,
    Nutri_Couns = Field13,
    BKB = Field14,
    CleanWater = Field15,
    Sanitation = Field16,
    Parenting = Field17,
    EarlChildEdu = Field18,
    Health_Insur = Field19,
    KPM_FDS = Field20,
    HPK_BPNT = Field21,
    KRP_Vil = Field22
  ) %>%
  mutate(
    Village = toTitleCase(Village),
    Year = 2020
  )

csv_services3 <- csv_services3 %>%
  rename(
    No. = Field1,
    BPS_ID = Phone,
    MoIA_ID = Phone1,
    Village = Field2,
    Wom_SupFeed = Field3,
    Wom_IFA = Field4,
    Child_SupFeed = Field5,
    IntSerPost = Field6,
    Wom_K4 = Field7,
    VitA = Field8,
    Compl_Imun = Field9,
    Zinc = Field10,
    TG_BBT = Field11,
    PostNatal_Care = Field12,
    Nutri_Couns = Field13,
    BKB = Field14,
    CleanWater = Field15,
    Sanitation = Field16,
    Parenting = Field17,
    EarlChildEdu = Field18,
    Health_Insur = Field19,
    KPM_FDS = Field20,
    HPK_BPNT = Field21,
    KRP_Vil = Field22
  ) %>%
  mutate(
    Village = toTitleCase(Village),
    Year = 2021
  )

head(csv_services2)

#Merge All the Services from Year 2019-2021; Change to Numeric; Select Columns
services_vil <- bind_rows(csv_services1, csv_services2, csv_services3)
str(services_vil)
services_vil <- services_vil %>%
  mutate(
    Wom_SupFeed = as.numeric(Wom_SupFeed),
    Wom_IFA = as.numeric(Wom_IFA),
    Child_SupFeed = as.numeric(Child_SupFeed),
    IntSerPost = as.numeric(IntSerPost),
    Wom_K4 = as.numeric(Wom_K4),
    VitA = as.numeric(VitA),
    Compl_Imun = as.numeric(Compl_Imun),
    Zinc = as.numeric(Zinc),
    TG_BBT = as.numeric(TG_BBT),
    PostNatal_Care = as.numeric(PostNatal_Care),
    Nutri_Couns = as.numeric(Nutri_Couns),
    BKB = as.numeric(BKB),
    CleanWater = as.numeric(CleanWater),
    Sanitation = as.numeric(Sanitation),
    Parenting = as.numeric(Parenting),
    EarlChildEdu = as.numeric(EarlChildEdu),
    Health_Insur = as.numeric(Health_Insur),
    Year = as.integer(Year)
  )
head(services_vil)
services_vil <- services_vil %>%
  select(Village, Year, Child_SupFeed, Compl_Imun, VitA, CleanWater, Sanitation, BKB, IntSerPost, Health_Insur)

#Village code
services_vil$Village <- toupper(services_vil$Village)
services_vil$Village_code <- toupper(services_vil$Village_code)

services_vil <- services_vil %>%
  group_by(Year, Village) %>%
  mutate(Village_code = paste0(Village, "_", row_number())) %>%
  ungroup()

full_stunt_po_village <- full_stunt_po_village %>%
  group_by(Year, Village) %>%
  mutate(Village_code = paste0(Village, "_", row_number())) %>%
  ungroup()
str(services_vil)

#----------------------------------------------------------------
#1.1 Combine Stunt, PO, Expl. Variables (Village-level FULL PANEL)
full_stunt_po_village$Village <- toupper(full_stunt_po_village$Village)

all_data_vil <- merge(full_stunt_po_village, services_vil,
                      by = c("Village_code", "Year"))

na_check_csv_comb <- sapply(all_data_vil, function(x) sum(is.na(x)))
print(na_check_csv_comb)
na <- all_data_vil %>% filter(is.na(Child_SupFeed))
rm(na)

#Fixed
all_data_vil <- all_data_vil %>%
  select(
    Year, District, SubDist, Village_code, `Total Children`, `Total Stunting`, `StuntRate`, OP_Area, Child_SupFeed, VitA, Compl_Imun, CleanWater, Sanitation, BKB, IntSerPost, Health_Insur
  )

#FE Regression
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun + CleanWater + Sanitation + BKB + IntSerPost + Health_Insur | Village_code + Year, data = all_data_vil)
summary(stunting_model)


#1.2 Combine Stunt, PO, Expl. Variables (Village-level BALANCED PANEL)
stunt_po_village$Village <- toupper(stunt_po_village$Village)

bal_all_data_vil <- merge(stunt_po_village, services_vil,
                      by = c("Village_code", "Year"))

na_check_csv_comb <- sapply(bal_all_data_vil, function(x) sum(is.na(x)))
print(na_check_csv_comb)
na <- bal_all_data_vil %>% filter(is.na(Child_SupFeed))
rm(na)

#Fixed
bal_all_data_vil <- bal_all_data_vil %>%
  select(
    Year, District, SubDist, Village_code, `Total Children`, `Total Stunting`, `StuntRate`, OP_Area, Child_SupFeed, VitA, Compl_Imun, CleanWater, Sanitation, BKB, IntSerPost, Health_Insur
  )

#FE Regression
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun + CleanWater + Sanitation + BKB + IntSerPost + Health_Insur | Village_code + Year, data = bal_all_data_vil)
summary(stunting_model)

#-----------------------------------------------------------
#2. Combine Stunt, PO, Expl. Variables (District-level)
#Aggregate all_data_vil to District data
all_data_dist <- all_data_vil %>%
  group_by(Year, District) %>%
  summarise(
    Total_Children = sum(`Total Children`, na.rm = TRUE),
    Total_Stunting = sum(`Total Stunting`, na.rm = TRUE),
    StuntRate = (sum(`Total Stunting`, na.rm = TRUE) / sum(`Total Children`, na.rm = TRUE)) * 100,
    OP_Area = mean(OP_Area, na.rm = TRUE),
    Child_SupFeed = mean(Child_SupFeed, na.rm = TRUE),
    VitA = mean(VitA, na.rm = TRUE),
    Zinc = mean(Zinc, na.rm = TRUE),
    Compl_Imun = mean(Compl_Imun, na.rm = TRUE),
    EarlChildEdu = mean(EarlChildEdu, na.rm = TRUE),
    Wom_SupFeed = mean(Wom_SupFeed, na.rm = TRUE),
    Wom_IFA = mean(Wom_IFA, na.rm = TRUE),
    Wom_K4 = mean(Wom_K4, na.rm = TRUE),
    CleanWater = mean(CleanWater, na.rm = TRUE),
    Sanitation = mean(Sanitation, na.rm = TRUE),
    IntSerPost = mean(IntSerPost, na.rm = TRUE),
    Health_Insur = mean(Health_Insur, na.rm = TRUE),
    PostNatal_Care = mean(PostNatal_Care, na.rm = TRUE),
    Nutri_Couns = mean(Nutri_Couns, na.rm = TRUE)
  )
all_data_dist$StuntRate[is.na(all_data_dist$StuntRate) | is.nan(all_data_dist$StuntRate)] <- 0

#Delete DEMAK Dist
all_data_dist <- all_data_dist %>%
  filter(District != "DEMAK")

#Buat Check Ada yg NA Value
na_check_csv_comb <- sapply(all_data_dist, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#CUMA BUAT CROSSCHECK Calculate Stuntng Rate for a Specific District and Year
speci_data <- all_data_vil %>%
  filter(District == "BLORA", Year == "2019") %>%
  summarise(
    Total_Children = sum(`Total Children`, na.rm = TRUE),
    Total_Stunt = sum(`Total Stunting`, na.rm = TRUE),
    StuntRate = (sum(`Total Stunting`, na.rm = TRUE) / sum(`Total Children`, na.rm = TRUE)) * 100,
    OP_Area = mean(OP_Area, na.rm = TRUE),
    Child_SupFeed = mean(Child_SupFeed, na.rm = TRUE),
    VitA = mean(VitA, na.rm = TRUE),
    Zinc = mean(Zinc, na.rm = TRUE),
    Compl_Imun = mean(Compl_Imun, na.rm = TRUE),
    EarlChildEdu = mean(EarlChildEdu, na.rm = TRUE),
    Wom_SupFeed = mean(Wom_SupFeed, na.rm = TRUE),
    Wom_IFA = mean(Wom_IFA, na.rm = TRUE),
    Wom_K4 = mean(Wom_K4, na.rm = TRUE),
    CleanWater = mean(CleanWater, na.rm = TRUE),
    Sanitation = mean(Sanitation, na.rm = TRUE),
    IntSerPost = mean(IntSerPost, na.rm = TRUE),
    Health_Insur = mean(Health_Insur, na.rm = TRUE),
    PostNatal_Care = mean(PostNatal_Care, na.rm = TRUE),
    Nutri_Couns = mean(Nutri_Couns, na.rm = TRUE)
  )
rm(speci_data)

#Regression with Variablesss
stunting_model_dist <- lm(`StuntRate`~
                       `OP_Area.x` + `Child_SupFeed` + VitA + Zinc + `Compl_Imun` + 
                       `EarlChildEdu` + `Wom_SupFeed` + `Wom_IFA` + Wom_K4 + 
                       CleanWater + Sanitation + `IntSerPost` + `Health_Insur` + 
                       `PostNatal_Care` + `Nutri_Couns`, data = all_data_dist)
summary(stunting_model_dist)

#----------------------------------------------------------------
#3. Additional District-level Variables FROM FSVA 2019-2021#
fsva2019 <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Food Security and Vulnerability Atlas/tabel_data (1).csv")
fsva2020 <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Food Security and Vulnerability Atlas/tabel_data (2).csv")
fsva2021 <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Food Security and Vulnerability Atlas/tabel_data (3).csv")

#Add Year, Rename, Select
#2019
fsva2019 <- fsva2019 %>%
  mutate(Year = 2019)
fsva2019 <- fsva2019 %>%
  rename(
    District = Wilayah,
    Poverty = Kemiskinan....,
    FoodExp = Pengeluaran.Pangan....,
    NoElectricity = Tanpa.Listrik....,
    Wom_AvgSch = Lama.Sekolah.Perempuan..tahun.,
    NoCleanWat = Tanpa.Air.Bersih....,
    HealWork = Rasio.Tenaga.Kesehatan,
    LifeExp = Angka.Harapan.Hidup..tahun.
  )
fsva2019 <- fsva2019 %>%
  select(Year, District, Poverty, LifeExp, FoodExp, NoElectricity, Wom_AvgSch, HealWork)

fsva2019$District <- sub(".*-\\s*(.*)", "\\1", fsva2019$District)
fsva2019$District <- toupper(fsva2019$District)

#2020
fsva2020 <- fsva2020 %>%
  mutate(Year = 2020)
fsva2020 <- fsva2020 %>%
  rename(
    District = Wilayah,
    Poverty = Kemiskinan....,
    FoodExp = Pengeluaran.Pangan....,
    NoElectricity = Tanpa.Listrik....,
    Wom_AvgSch = Lama.Sekolah.Perempuan..tahun.,
    NoCleanWat = Tanpa.Air.Bersih....,
    HealWork = Rasio.Tenaga.Kesehatan,
    LifeExp = Angka.Harapan.Hidup..tahun.
  )
fsva2020 <- fsva2020 %>%
  select(Year, District, Poverty, LifeExp, FoodExp, NoElectricity, Wom_AvgSch, HealWork)

fsva2020$District <- sub(".*-\\s*(.*)", "\\1", fsva2020$District)
fsva2020$District <- toupper(fsva2020$District)

#2021
fsva2021 <- fsva2021 %>%
  mutate(Year = 2021)
fsva2021 <- fsva2021 %>%
  rename(
    District = Wilayah,
    Poverty = Kemiskinan....,
    FoodExp = Pengeluaran.Pangan....,
    NoElectricity = Tanpa.Listrik....,
    Wom_AvgSch = Lama.Sekolah.Perempuan..tahun.,
    NoCleanWat = Tanpa.Air.Bersih....,
    HealWork = Rasio.Tenaga.Kesehatan,
    LifeExp = Angka.Harapan.Hidup..tahun.
  )
fsva2021 <- fsva2021 %>%
  select(Year, District, Poverty, LifeExp, FoodExp, NoElectricity, Wom_AvgSch, HealWork)

fsva2021$District <- sub(".*-\\s*(.*)", "\\1", fsva2021$District)
fsva2021$District <- toupper(fsva2021$District)

#fsva all combined
fsva <- bind_rows(fsva2019, fsva2020, fsva2021)
str(fsva)
head(fsva)

na_check_csv_comb <- sapply(fsva, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#Add POHUWATO DISTRICT Data in fsva Dataset
fsva <- rbind(fsva, 
              data.frame(Year = 2019, District = "POHUWATO", Poverty = 15.09, LifeExp = 68.58, FoodExp = 13.12, NoElectricity = 1.76, Wom_AvgSch = 8.44, HealWork = 1.94),
              data.frame(Year = 2020, District = "POHUWATO", Poverty = 14.88, LifeExp = 68.70, FoodExp = 12.83, NoElectricity = 1.13, Wom_AvgSch = 8.69, HealWork = 1.80),
              data.frame(Year = 2021, District = "POHUWATO", Poverty = 15.27, LifeExp = 68.74, FoodExp = 10.21, NoElectricity = 0.76, Wom_AvgSch = 8.72, HealWork = 1.66)
)

fsva <- fsva %>%
  mutate(District = case_when(
    District == "GUNUNGKIDUL" ~ "GUNUNG KIDUL",
    District == "JAKARTA TIMUR" ~ "KOTA JAKARTA TIMUR",
    District == "BANYUASIN" ~ "BANYU ASIN",
    District == "KOTA BANJARBARU" ~ "KOTA BANJAR BARU",
    District == "JAKARTA TIMUR" ~ "KOTA JAKARTA TIMUR",
    District == "JAKARTA PUSAT" ~ "KOTA JAKARTA PUSAT",
    District == "JAKARTA SELATAN" ~ "KOTA JAKARTA SELATAN",
    District == "JAKARTA UTARA" ~ "KOTA JAKARTA UTARA",
    District == "JAKARTA BARAT" ~ "KOTA JAKARTA BARAT",
    District == "KOTA PADANG SIDIMPUAN" ~ "KOTA PADANGSIDIMPUAN",
    District == "LABUHANBATU" ~ "LABUHAN BATU",
    District == "LABUHANBATU SELATAN" ~ "LABUHAN BATU SELATAN",
    District == "LABUHANBATU UTARA" ~ "LABUHAN BATU UTARA",
    District == "TULANG BAWANG" ~ "TULANGBAWANG",
    TRUE ~ District
  ))

head(fsva)
#----------------------------------------------------------------
#Combine FSVA with other Variablesss

#1. FULL Stunt-PO + Vill-contr Var + FSVA
all_data_vil <- merge(all_data_vil, fsva, by.x = c("Year", "District"), by.y = c("Year", "District"), all.x = TRUE)

na_check_csv_comb <- sapply(all_data_vil, function(x) sum(is.na(x)))
print(na_check_csv_comb)
na <- all_data_vil %>% filter(is.na(Poverty))

#FE Regression
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun +
                          CleanWater + Sanitation + BKB + IntSerPost + Health_Insur +
                          Poverty + Wom_AvgSch + FoodExp + NoElectricity + HealWork | Village_code + Year, data = all_data_vil)
summary(stunting_model)


library(plm)
stunting_model <- plm(StuntRate ~
                        OP_Area + Child_SupFeed + VitA + Compl_Imun +
                        CleanWater + Sanitation + BKB + IntSerPost + Health_Insur +
                        Poverty + Wom_AvgSch + FoodExp + NoElectricity + HealWork,
                      data = all_data_vil,
                      model = "within",
                      index = c("Village_code", "Year"))
summary(stunting_model)


all_data_dist <- merge(all_data_dist, fsva, by.x = c("Year", "District"), by.y = c("Year", "District"), all.x = TRUE)

all_data_dist <- all_data_dist %>%
  select(Year, District, Total_Children, Total_Stunting, StuntRate, Child_SupFeed, VitA, Zinc, Compl_Imun, EarlChildEdu, Wom_SupFeed, Wom_IFA, Wom_K4, CleanWater, Sanitation, IntSerPost, Health_Insur, PostNatal_Care, Nutri_Couns, Poverty, LifeExp, Wom_AvgSch, FoodExp, NoElectricity, HealWork, OP_Area)

#Regression with FSVA + Variablesss
all_data <- lm(`StuntRate`~
                       `OP_Area` + `Child_SupFeed` + VitA + Zinc + `Compl_Imun` + 
                       `EarlChildEdu` + `Wom_SupFeed` + `Wom_IFA` + Wom_K4 + 
                       CleanWater + Sanitation + `IntSerPost` + `Health_Insur` + 
                       `PostNatal_Care` + `Nutri_Couns` + `Poverty` + `LifeExp` + `Wom_AvgSch` + `FoodExp` + `NoElectricity` + `HealWork` , data = all_data_dist)
summary(all_data)

na_check_csv_comb <- sapply(all_data_dist, function(x) sum(is.na(x)))
print(na_check_csv_comb)

#----------------------------------------------------------------
#4. PO Data lag-15 years

#Year 2019
lag_2019 <- po_data %>%
  filter(year >= 2004 & year <= 2018) %>%
  select(year, region, OP_Area)
lag_2019 <- lag_2019 %>%
  pivot_wider(names_from = year, values_from = OP_Area, names_prefix = "OP_Area_")
#Join the OP Data 2019 with the lags
po_2019 <- po_data %>%
  filter(year == 2019)
po_2019 <- po_2019 %>%
  left_join(lag_2019, by = "region")
po_2019 <- po_2019 %>%
  select(year, region, OP_Area_2018:OP_Area_2004) %>%
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

#Year 2020
lag_2020 <- po_data %>%
  filter(year >= 2005 & year <= 2019) %>%
  select(year, region, OP_Area)
lag_2020 <- lag_2020 %>%
  pivot_wider(names_from = year, values_from = OP_Area, names_prefix = "OP_Area_")
#Join the OP Data 2020 with the lags
po_2020 <- po_data %>%
  filter(year == 2020)
po_2020 <- po_2020 %>%
  left_join(lag_2020, by = "region")
po_2020 <- po_2020 %>%
  select(year, region, OP_Area_2019:OP_Area_2005) %>%
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

#Year 2021
lag_2021 <- po_data %>%
  filter(year >= 2006 & year <= 2020) %>%
  select(year, region, OP_Area)
lag_2021 <- lag_2021 %>%
  pivot_wider(names_from = year, values_from = OP_Area, names_prefix = "OP_Area_")
#Join the OP Data 2021 with the lags
po_2021 <- po_data %>%
  filter(year == 2021)
po_2021 <- po_2021 %>%
  left_join(lag_2021, by = "region")
po_2021 <- po_2021 %>%
  select(year, region, OP_Area_2020:OP_Area_2006) %>%
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

#Combine lags for year 2019-2021
po_lag <- bind_rows(po_2019, po_2020, po_2021)

po_lag <- po_lag %>%
  rename(Year = year, District = region)

#Combine with other Variablesss FIX
all_data_vil <- all_data_vil %>%
  left_join(po_lag, by = c("Year", "District"))


#FIXED EFFECT REG
stunting_model <- plm(StuntRate ~
                        OP_Area.x + Child_SupFeed + VitA + Compl_Imun +
                        CleanWater + Sanitation + IntSerPost + Health_Insur +
                        Poverty + LifeExp + Wom_AvgSch + FoodExp + NoElectricity +
                        `po_lag1` + `po_lag2` + `po_lag3` + `po_lag4` + `po_lag5` +
                        `po_lag6` + `po_lag7` + `po_lag8` + `po_lag9` + `po_lag10`,
                      data = all_data_vil,
                      model = "within",
                      index = c("Village_code", "Year"))
summary(stunting_model)

#----------------------------------------------------------------


#NOT YET DONE - Combine FSVA + other Variablesss + deforest
deforest <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/spatial-metrics-indonesia-territorial_deforestation_kabupaten.csv")

#Remove parentheses and re-arrange
deforest$region <- toupper(deforest$region)
deforest$region <- gsub("(.*) \\((.*)\\)", "\\2 \\1", deforest$region)
deforest$deforestation_hectares <- deforest$deforestation_hectares / 1000

all_data <- merge(all_data_dist, deforest, by.x = c("Year", "District"), by.y = c("year", "region"), all.x = TRUE)
all_data_dist <- all_data_dist %>%
  select(Year, District, Total_Children, Total_Stunting, StuntRate, OP_Area, Child_SupFeed, VitA, Zinc, Compl_Imun, EarlChildEdu, Wom_SupFeed, Wom_IFA, Wom_K4, CleanWater, Sanitation, IntSerPost, Health_Insur, PostNatal_Care, Nutri_Couns, Poverty, LifeExp, Wom_AvgSch, FoodExp, NoElectricity, HealWork, deforestation_hectares)

#----------------------------------------------------------------
#Summary Statistics
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate", "Child_SupFeed", "VitA", "Compl_Imun", "CleanWater", "Sanitation", "BKB",
               "IntSerPost", "Health_Insur", "Poverty", "FoodExp", "NoElectricity", "Wom_AvgSch", "HealWork"),
  Mean = c(mean(all_data_vil$OP_Area), mean(all_data_vil$StuntRate), mean(all_data_vil$Child_SupFeed), mean(all_data_vil$VitA),
           mean(all_data_vil$Compl_Imun), mean(all_data_vil$CleanWater), mean(all_data_vil$Sanitation), mean(all_data_vil$BKB),
           mean(all_data_vil$IntSerPost), mean(all_data_vil$Health_Insur), mean(all_data_vil$Poverty), mean(all_data_vil$FoodExp),
           mean(all_data_vil$NoElectricity), mean(all_data_vil$Wom_AvgSch), mean(all_data_vil$HealWork)),
  SD = c(sd(all_data_vil$OP_Area), sd(all_data_vil$StuntRate), sd(all_data_vil$Child_SupFeed), sd(all_data_vil$VitA),
         sd(all_data_vil$Compl_Imun), sd(all_data_vil$CleanWater), sd(all_data_vil$Sanitation), sd(all_data_vil$BKB),
         sd(all_data_vil$IntSerPost), sd(all_data_vil$Health_Insur), sd(all_data_vil$Poverty), sd(all_data_vil$FoodExp),
         sd(all_data_vil$NoElectricity), sd(all_data_vil$Wom_AvgSch), sd(all_data_vil$HealWork)),
  min = c(min(all_data_vil$OP_Area), min(all_data_vil$StuntRate), min(all_data_vil$Child_SupFeed), min(all_data_vil$VitA),
          min(all_data_vil$Compl_Imun), min(all_data_vil$CleanWater), min(all_data_vil$Sanitation), min(all_data_vil$BKB),
          min(all_data_vil$IntSerPost), min(all_data_vil$Health_Insur), min(all_data_vil$Poverty), min(all_data_vil$FoodExp),
          min(all_data_vil$NoElectricity), min(all_data_vil$Wom_AvgSch), min(all_data_vil$HealWork)),
  max = c(max(all_data_vil$OP_Area), max(all_data_vil$StuntRate), max(all_data_vil$Child_SupFeed), max(all_data_vil$VitA),
          max(all_data_vil$Compl_Imun), max(all_data_vil$CleanWater), max(all_data_vil$Sanitation), max(all_data_vil$BKB),
          max(all_data_vil$IntSerPost), max(all_data_vil$Health_Insur), max(all_data_vil$Poverty), max(all_data_vil$FoodExp),
          max(all_data_vil$NoElectricity), max(all_data_vil$Wom_AvgSch), max(all_data_vil$HealWork)),
  N = c(length(all_data_vil$OP_Area), length(all_data_vil$StuntRate), length(all_data_vil$Child_SupFeed), length(all_data_vil$VitA),
        length(all_data_vil$Compl_Imun), length(all_data_vil$CleanWater), length(all_data_vil$Sanitation), length(all_data_vil$BKB),
        length(all_data_vil$IntSerPost), length(all_data_vil$Health_Insur), length(all_data_vil$Poverty), length(all_data_vil$FoodExp),
        length(all_data_vil$NoElectricity), length(all_data_vil$Wom_AvgSch), length(all_data_vil$HealWork))
)
print(sum_stats)
