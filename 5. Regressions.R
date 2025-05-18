##Fixed Effects Regressions##

#1 Simple Reg.
#Full Panel
feols(StuntRate ~ OP_Area | Village_code + Year, data = full_stunt_po_village)

#Balanced Panel
feols(StuntRate ~ OP_Area | Village_code + Year, data = stunt_po_village)

#----------------------------------------------------------------
#2 Add Village level Controls
#Full Panel
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun + CleanWater + Sanitation + BKB + IntSerPost + Health_Insur | Village_code + Year, data = all_data_vil)
summary(stunting_model)

#Balanced Panel
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun + CleanWater + Sanitation + BKB + IntSerPost + Health_Insur | Village_code + Year, data = bal_all_data_vil)
summary(stunting_model)

#----------------------------------------------------------------
#3 Add Village & District level Controls
#Full Panel
stunting_model <- feols(StuntRate ~ OP_Area + Child_SupFeed + VitA + Compl_Imun +
                          CleanWater + Sanitation + BKB + IntSerPost + Health_Insur +
                          Poverty + Wom_AvgSch + FoodExp + NoElectricity + HealWork + deforest + emissions | Village_code + Year, data = all_data)
summary(stunting_model)
