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

#----------------------------------------------------------------
#Combine Stunt, PO, Expl. Variables (Village-level)


#----------------------------------------------------------------
#Additional District-level Variables FROM FSVA 2019-2021#
fsva2019 <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Food Security and Vulnerability Atlas/tabel_data (1).csv")
fsva2020 <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Food Security and Vulnerability Atlas/tabel_data (2).csv")
fsva2021 <- read.csv("C:/Users/corde/OneDrive/Documents/國立台灣大學 NTU/Thesis/Data/THESIS DATA FIX/Food Security and Vulnerability Atlas/tabel_data (3).csv")

#Add Year, Rename, Select
fsva2019 <- fsva2019 %>%
  mutate(Year = 2019)
fsva2019 <- rename(
    District = Wilayah,
    Poverty = Kemiskinan....,
    FoodExp = Pengeluaran.Pangan....,
    NoElectricity = Tanpa.Listrik....,
    Wom_AvgSch = Lama.Sekolah.Perempuan..tahun.,
    NoCleanWat = Tanpa.Air.Bersih....,
    HealWork = Rasio.Tenaga.Kesehatan,
    LifeExp = Angka.Harapan.Hidup..tahun.
  )
colnames(fsva2019)

  select(Year, District, Poverty, LifeExpc, FoodExpn, NoElectricity, Wom_AvgSch, HealWork)
fsva2020 <- fsva2020 %>%
  mutate(Year = 2020) %>%
  rename(
    Poverty = Kemiskinan....,
    FoodExp = Pengeluaran.Pangan....,
    NoElectricity = Tanpa.Listrik....,
    Wom_AvgSch = Lama.Sekolah.Perempuan..tahun.,
    NoCleanWat = Tanpa.Air.Bersih....,
    HealWork = Rasio.Tenaga.Kesehatan,
    LifeExp = Angka.Harapan.Hidup..tahun.
  ) %>%
  select(Year, Poverty, LifeExp, FoodExp, NoElectricity, Wom_AvgSch, HealWork)
fsva2021 <- fsva2021 %>%
  mutate(Year = 2021) %>%
  rename(
    Poverty = Kemiskinan....,
    FoodExp = Pengeluaran.Pangan....,
    NoElectricity = Tanpa.Listrik....,
    Wom_AvgSch = Lama.Sekolah.Perempuan..tahun.,
    NoCleanWat = Tanpa.Air.Bersih....,
    HealWork = Rasio.Tenaga.Kesehatan,
    LifeExp = Angka.Harapan.Hidup..tahun.
  ) %>%
  select(Year, Poverty, LifeExp, FoodExp, NoElectricity, Wom_AvgSch, HealWork)
