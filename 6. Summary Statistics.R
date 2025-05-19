##Summary Statistics##

#1 Simple Reg.
#Full Panel
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate"),
  Mean = c(mean(full_stunt_po_village$OP_Area), mean(full_stunt_po_village$StuntRate)),
  SD = c(sd(full_stunt_po_village$OP_Area), sd(full_stunt_po_village$StuntRate)),
  min = c(min(full_stunt_po_village$OP_Area), min(full_stunt_po_village$StuntRate)),
  max = c(max(full_stunt_po_village$OP_Area), max(full_stunt_po_village$StuntRate)),
  N = c(length(full_stunt_po_village$OP_Area), length(full_stunt_po_village$StuntRate))
)
print(sum_stats)

#Balanced Panel
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate"),
  Mean = c(mean(stunt_po_village$OP_Area), mean(stunt_po_village$StuntRate)),
  SD = c(sd(stunt_po_village$OP_Area), sd(stunt_po_village$StuntRate)),
  min = c(min(stunt_po_village$OP_Area), min(stunt_po_village$StuntRate)),
  max = c(max(stunt_po_village$OP_Area), max(stunt_po_village$StuntRate)),
  N = c(length(stunt_po_village$OP_Area), length(stunt_po_village$StuntRate))
)
print(sum_stats)

#----------------------------------------------------------------
#2 Add Village level Controls
#Full Panel
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate", "Child_SupFeed", "VitA", "Compl_Imun", "CleanWater", "Sanitation", "BKB",
               "IntSerPost", "Health_Insur"),
  Mean = c(mean(all_data_vil$OP_Area), mean(all_data_vil$StuntRate), mean(all_data_vil$Child_SupFeed), mean(all_data_vil$VitA),
           mean(all_data_vil$Compl_Imun), mean(all_data_vil$CleanWater), mean(all_data_vil$Sanitation), mean(all_data_vil$BKB),
           mean(all_data_vil$IntSerPost), mean(all_data_vil$Health_Insur)),
  SD = c(sd(all_data_vil$OP_Area), sd(all_data_vil$StuntRate), sd(all_data_vil$Child_SupFeed), sd(all_data_vil$VitA),
         sd(all_data_vil$Compl_Imun), sd(all_data_vil$CleanWater), sd(all_data_vil$Sanitation), sd(all_data_vil$BKB),
         sd(all_data_vil$IntSerPost), sd(all_data_vil$Health_Insur)),
  min = c(min(all_data_vil$OP_Area), min(all_data_vil$StuntRate), min(all_data_vil$Child_SupFeed), min(all_data_vil$VitA),
          min(all_data_vil$Compl_Imun), min(all_data_vil$CleanWater), min(all_data_vil$Sanitation), min(all_data_vil$BKB),
          min(all_data_vil$IntSerPost), min(all_data_vil$Health_Insur)),
  max = c(max(all_data_vil$OP_Area), max(all_data_vil$StuntRate), max(all_data_vil$Child_SupFeed), max(all_data_vil$VitA),
          max(all_data_vil$Compl_Imun), max(all_data_vil$CleanWater), max(all_data_vil$Sanitation), max(all_data_vil$BKB),
          max(all_data_vil$IntSerPost), max(all_data_vil$Health_Insur)),
  N = c(length(all_data_vil$OP_Area), length(all_data_vil$StuntRate), length(all_data_vil$Child_SupFeed), length(all_data_vil$VitA),
        length(all_data_vil$Compl_Imun), length(all_data_vil$CleanWater), length(all_data_vil$Sanitation), length(all_data_vil$BKB),
        length(all_data_vil$IntSerPost), length(all_data_vil$Health_Insur))
)
print(sum_stats)

#Balanced Panel
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate", "Child_SupFeed", "VitA", "Compl_Imun", "CleanWater", "Sanitation", "BKB",
               "IntSerPost", "Health_Insur"),
  Mean = c(mean(bal_all_data_vil$OP_Area), mean(bal_all_data_vil$StuntRate), mean(bal_all_data_vil$Child_SupFeed), mean(bal_all_data_vil$VitA),
           mean(bal_all_data_vil$Compl_Imun), mean(bal_all_data_vil$CleanWater), mean(bal_all_data_vil$Sanitation), mean(bal_all_data_vil$BKB),
           mean(bal_all_data_vil$IntSerPost), mean(bal_all_data_vil$Health_Insur)),
  SD = c(sd(bal_all_data_vil$OP_Area), sd(bal_all_data_vil$StuntRate), sd(bal_all_data_vil$Child_SupFeed), sd(bal_all_data_vil$VitA),
         sd(bal_all_data_vil$Compl_Imun), sd(bal_all_data_vil$CleanWater), sd(bal_all_data_vil$Sanitation), sd(bal_all_data_vil$BKB),
         sd(bal_all_data_vil$IntSerPost), sd(bal_all_data_vil$Health_Insur)),
  min = c(min(bal_all_data_vil$OP_Area), min(bal_all_data_vil$StuntRate), min(bal_all_data_vil$Child_SupFeed), min(bal_all_data_vil$VitA),
          min(bal_all_data_vil$Compl_Imun), min(bal_all_data_vil$CleanWater), min(bal_all_data_vil$Sanitation), min(bal_all_data_vil$BKB),
          min(bal_all_data_vil$IntSerPost), min(bal_all_data_vil$Health_Insur)),
  max = c(max(bal_all_data_vil$OP_Area), max(bal_all_data_vil$StuntRate), max(bal_all_data_vil$Child_SupFeed), max(bal_all_data_vil$VitA),
          max(bal_all_data_vil$Compl_Imun), max(bal_all_data_vil$CleanWater), max(bal_all_data_vil$Sanitation), max(bal_all_data_vil$BKB),
          max(bal_all_data_vil$IntSerPost), max(bal_all_data_vil$Health_Insur)),
  N = c(length(bal_all_data_vil$OP_Area), length(bal_all_data_vil$StuntRate), length(bal_all_data_vil$Child_SupFeed), length(bal_all_data_vil$VitA),
        length(bal_all_data_vil$Compl_Imun), length(bal_all_data_vil$CleanWater), length(bal_all_data_vil$Sanitation), length(bal_all_data_vil$BKB),
        length(bal_all_data_vil$IntSerPost), length(bal_all_data_vil$Health_Insur))
)
print(sum_stats)

#----------------------------------------------------------------
#3 Add Village level & District level Controls
#Full Panel
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate", "Child_SupFeed", "VitA", "Compl_Imun", "CleanWater", "Sanitation", "BKB",
               "IntSerPost", "Health_Insur", "Poverty", "Wom_AvgSch", "FoodExp", "NoElectricity", "HealWork", "deforest", "emissions"),
  Mean = c(mean(all_data$OP_Area), mean(all_data$StuntRate), mean(all_data$Child_SupFeed), mean(all_data$VitA),
           mean(all_data$Compl_Imun), mean(all_data$CleanWater), mean(all_data$Sanitation), mean(all_data$BKB),
           mean(all_data$IntSerPost), mean(all_data$Health_Insur), mean(all_data$Poverty), mean(all_data$Wom_AvgSch),
           mean(all_data$FoodExp), mean(all_data$NoElectricity), mean(all_data$HealWork), mean(all_data$deforest), mean(all_data$emissions)),
  SD = c(sd(all_data$OP_Area), sd(all_data$StuntRate), sd(all_data$Child_SupFeed), sd(all_data$VitA),
         sd(all_data$Compl_Imun), sd(all_data$CleanWater), sd(all_data$Sanitation), sd(all_data$BKB),
         sd(all_data$IntSerPost), sd(all_data$Health_Insur), sd(all_data$Poverty), sd(all_data$FoodExp),
         sd(all_data$NoElectricity), sd(all_data$Wom_AvgSch), sd(all_data$HealWork), sd(all_data$deforest), sd(all_data$emissions)),
  min = c(min(all_data$OP_Area), min(all_data$StuntRate), min(all_data$Child_SupFeed), min(all_data$VitA),
          min(all_data$Compl_Imun), min(all_data$CleanWater), min(all_data$Sanitation), min(all_data$BKB),
          min(all_data$IntSerPost), min(all_data$Health_Insur), min(all_data$Poverty), min(all_data$FoodExp),
          min(all_data$NoElectricity), min(all_data$Wom_AvgSch), min(all_data$HealWork), min(all_data$deforest), min(all_data$emissions)),
  max = c(max(all_data$OP_Area), max(all_data$StuntRate), max(all_data$Child_SupFeed), max(all_data$VitA),
          max(all_data$Compl_Imun), max(all_data$CleanWater), max(all_data$Sanitation), max(all_data$BKB),
          max(all_data$IntSerPost), max(all_data$Health_Insur), max(all_data$Poverty), max(all_data$FoodExp),
          max(all_data$NoElectricity), max(all_data$Wom_AvgSch), max(all_data$HealWork), max(all_data$deforest), max(all_data$emissions)),
  N = c(length(all_data$OP_Area), length(all_data$StuntRate), length(all_data$Child_SupFeed), length(all_data$VitA),
        length(all_data$Compl_Imun), length(all_data$CleanWater), length(all_data$Sanitation), length(all_data$BKB),
        length(all_data$IntSerPost), length(all_data$Health_Insur), length(all_data$Poverty), length(all_data$FoodExp),
        length(all_data$NoElectricity), length(all_data$Wom_AvgSch), length(all_data$HealWork), length(all_data$deforest), length(all_data$emissions))
)
print(sum_stats)

#Balanced Panel
sum_stats <- data.frame(
  Variable = c("OP_Area", "StuntRate", "Child_SupFeed", "VitA", "Compl_Imun", "CleanWater", "Sanitation", "BKB",
               "IntSerPost", "Health_Insur", "Poverty", "Wom_AvgSch", "FoodExp", "NoElectricity", "HealWork", "deforest", "emissions"),
  Mean = c(mean(bal_all_data$OP_Area), mean(bal_all_data$StuntRate), mean(bal_all_data$Child_SupFeed), mean(bal_all_data$VitA),
           mean(bal_all_data$Compl_Imun), mean(bal_all_data$CleanWater), mean(bal_all_data$Sanitation), mean(bal_all_data$BKB),
           mean(bal_all_data$IntSerPost), mean(bal_all_data$Health_Insur), mean(bal_all_data$Poverty), mean(bal_all_data$Wom_AvgSch),
           mean(bal_all_data$FoodExp), mean(bal_all_data$NoElectricity), mean(bal_all_data$HealWork), mean(bal_all_data$deforest), mean(bal_all_data$emissions)),
  SD = c(sd(bal_all_data$OP_Area), sd(bal_all_data$StuntRate), sd(bal_all_data$Child_SupFeed), sd(bal_all_data$VitA),
         sd(bal_all_data$Compl_Imun), sd(bal_all_data$CleanWater), sd(bal_all_data$Sanitation), sd(bal_all_data$BKB),
         sd(bal_all_data$IntSerPost), sd(bal_all_data$Health_Insur), sd(bal_all_data$Poverty), sd(bal_all_data$FoodExp),
         sd(bal_all_data$NoElectricity), sd(bal_all_data$Wom_AvgSch), sd(bal_all_data$HealWork), sd(bal_all_data$deforest), sd(bal_all_data$emissions)),
  min = c(min(bal_all_data$OP_Area), min(bal_all_data$StuntRate), min(bal_all_data$Child_SupFeed), min(bal_all_data$VitA),
          min(bal_all_data$Compl_Imun), min(bal_all_data$CleanWater), min(bal_all_data$Sanitation), min(bal_all_data$BKB),
          min(bal_all_data$IntSerPost), min(bal_all_data$Health_Insur), min(bal_all_data$Poverty), min(bal_all_data$FoodExp),
          min(bal_all_data$NoElectricity), min(bal_all_data$Wom_AvgSch), min(bal_all_data$HealWork), min(bal_all_data$deforest), min(bal_all_data$emissions)),
  max = c(max(bal_all_data$OP_Area), max(bal_all_data$StuntRate), max(bal_all_data$Child_SupFeed), max(bal_all_data$VitA),
          max(bal_all_data$Compl_Imun), max(bal_all_data$CleanWater), max(bal_all_data$Sanitation), max(bal_all_data$BKB),
          max(bal_all_data$IntSerPost), max(bal_all_data$Health_Insur), max(bal_all_data$Poverty), max(bal_all_data$FoodExp),
          max(bal_all_data$NoElectricity), max(bal_all_data$Wom_AvgSch), max(bal_all_data$HealWork), max(bal_all_data$deforest), max(bal_all_data$emissions)),
  N = c(length(bal_all_data$OP_Area), length(bal_all_data$StuntRate), length(bal_all_data$Child_SupFeed), length(bal_all_data$VitA),
        length(bal_all_data$Compl_Imun), length(bal_all_data$CleanWater), length(bal_all_data$Sanitation), length(bal_all_data$BKB),
        length(bal_all_data$IntSerPost), length(bal_all_data$Health_Insur), length(bal_all_data$Poverty), length(bal_all_data$FoodExp),
        length(bal_all_data$NoElectricity), length(bal_all_data$Wom_AvgSch), length(bal_all_data$HealWork), length(bal_all_data$deforest), length(bal_all_data$emissions))
)
print(sum_stats)
