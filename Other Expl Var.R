#COMBINE Explanatory Variables Village-level Data FROM 2019-2021#
install.packages("tools")
library(dplyr)
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
  select(Village, Year, Child_SupFeed, VitA, Zinc, Compl_Imun, EarlChildEdu, Wom_SupFeed, Wom_IFA, Wom_K4, CleanWater, Sanitation, IntSerPost, Health_Insur, PostNatal_Care, Nutri_Couns)

#Village code
services_vil$Village <- toupper(services_vil$Village)
services_vil$Village_code <- toupper(services_vil$Village_code)

services_vil <- services_vil %>%
  group_by(Year, Village) %>%
  mutate(Village_code = paste0(Village, "_", row_number())) %>%
  ungroup()
str(services_vil)
#----------------------------------------------------------------
#Combine Stunt, PO, Expl. Variables (Village-level)
all_data_vil <- left_join(stunt_po_village, services_vil, by = c("Village_code", "Year"))

##Replace the NAs
all_data_vil <- all_data_vil %>%
  mutate_at(vars(
    Child_SupFeed, VitA, Zinc, Compl_Imun, EarlChildEdu, Wom_SupFeed, Wom_IFA, Wom_K4, CleanWater, Sanitation, IntSerPost, Health_Insur, PostNatal_Care, Nutri_Couns
  ), ~ifelse(is.na(.), 0, .))

na_check_csv_comb <- sapply(all_data_vil, function(x) sum(is.na(x)))
print(na_check_csv_comb)
na <- all_data_vil %>% filter(is.na(Child_SupFeed))
rm(na)

#Fixed
all_data_vil <- all_data_vil %>%
  select(
    Year, District, SubDist, Village_code, `Total Children`, `Total Stunting`, `%`, OP_Area, Child_SupFeed, VitA, Zinc, Compl_Imun, EarlChildEdu, Wom_SupFeed, Wom_IFA, Wom_K4, CleanWater, Sanitation, IntSerPost, Health_Insur, PostNatal_Care, Nutri_Couns
  )

#Regression with Variablesss
stunting_model <- lm(`%`~
                       `OP_Area` + `Child_SupFeed` + VitA + Zinc + `Compl_Imun` + 
                       `EarlChildEdu` + `Wom_SupFeed` + `Wom_IFA` + Wom_K4 + 
                       CleanWater + Sanitation + `IntSerPost` + `Health_Insur` + 
                       `PostNatal_Care` + `Nutri_Couns`, data = all_data_vil)
summary(stunting_model)
head(all_data_vil)

#-----------------------------------------------------------
#Combine Stunt, PO, Expl. Variables (District-level)
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
                       `OP_Area` + `Child_SupFeed` + VitA + Zinc + `Compl_Imun` + 
                       `EarlChildEdu` + `Wom_SupFeed` + `Wom_IFA` + Wom_K4 + 
                       CleanWater + Sanitation + `IntSerPost` + `Health_Insur` + 
                       `PostNatal_Care` + `Nutri_Couns`, data = all_data_dist)
summary(stunting_model_dist)

#----------------------------------------------------------------
#Additional District-level Variables FROM FSVA 2019-2021#
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

#----------------------------------------------------------------
#Combine FSVA with other Variablesss
all_data_dist <- merge(all_data_dist, fsva, by.x = c("Year", "District"), by.y = c("Year", "District"), all.x = TRUE)

all_data_dist <- all_data_dist %>%
  select(Year, District, Total_Children, Total_Stunting, StuntRate, OP_Area, Child_SupFeed, VitA, Zinc, Compl_Imun, EarlChildEdu, Wom_SupFeed, Wom_IFA, Wom_K4, CleanWater, Sanitation, IntSerPost, Health_Insur, PostNatal_Care, Nutri_Couns, Poverty, LifeExp, Wom_AvgSch, FoodExp, NoElectricity, HealWork)

#Regression with FSVA + Variablesss
all_data <- lm(`StuntRate`~
                       `OP_Area` + `Child_SupFeed` + VitA + Zinc + `Compl_Imun` + 
                       `EarlChildEdu` + `Wom_SupFeed` + `Wom_IFA` + Wom_K4 + 
                       CleanWater + Sanitation + `IntSerPost` + `Health_Insur` + 
                       `PostNatal_Care` + `Nutri_Couns` + `Poverty` + `LifeExp` + `Wom_AvgSch` + `FoodExp` + `NoElectricity` + `HealWork` , data = all_data_dist)
summary(all_data)

na_check_csv_comb <- sapply(all_data_dist, function(x) sum(is.na(x)))
print(na_check_csv_comb)
