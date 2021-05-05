#--------------------------------
# Compiling data for QAQC Reports
#--------------------------------

#----- Load libraries
library(tidyverse)
library(forestNETN)
library(htmltools)
library(knitr)
library(kableExtra)
source("QAQC_functions.R")
importData() #local instance

#----- Compile data
arglist = list(park = params$park, from = year, to = year, QAQC = TRUE, eventType = 'complete')

plotevs <- do.call(joinLocEvent, arglist) %>% filter_plot() %>% name_team()

qaqc_eid <- plotevs$EventID[plotevs$IsQAQC == TRUE]
crew_eid <- plotevs$EventID[plotevs$IsQAQC == FALSE]

#----- Stand data
stand <- do.call(joinStandData, c(arglist, list(output = 'verbose'))) %>% filter_plot() %>% name_team()

dist <- do.call(joinStandDisturbance, arglist) %>% filter_plot() %>% name_team()

crown_cov <- data.frame(txt = c("<10%", "10-25%", "25-50%", "50-75%", "75-100%", "PM"),
                        pct_class = c(1, 2, 3, 4, 5, 6))

stand_cov <- data.frame(txt = c("0%", "1-5%", "5-25%", "25-50%", "50-75%", "75-95%", "95-100%", "PM"),
                        pct_class = c(0, 1, 2, 3, 4, 5, 6, 7))

fol_cov <- data.frame(txt = c("0%", "1-10%", "10-50%", "50-90%", "90-100%"),
                      pct_class = c(0, 1, 2, 3, 4))

code_cols <- c("Txt_Understory_Low", "Txt_Understory_Mid", "Txt_Understory_High", 
               "Txt_Bare_Soil", "Txt_Bryophyte", "Txt_Lichen", "Txt_Rock", "Txt_Trampled", 
               "Txt_Water")

stand2 <- do.call(cbind, c(stand, 
                           add_covcode(stand, "Txt_Crown_Closure", crown_cov),
                           lapply(seq_along(code_cols), function(x){
                             add_covcode(stand, code_cols[x], stand_cov)}))) 

tree_hts <- get("COMN_StandTreeHeights", envir = VIEWS_NETN) %>% 
  select(ParkUnit, PlotCode, StartYear, IsQAQC, CrownClassLabel, TagCode, Height) %>% 
  mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
  filter_plot() %>% name_team() %>% select(-IsQAQC)

tree_hts_wide <- tree_hts %>% pivot_wider(names_from = Team,
                                          values_from = Height) %>% 
  mutate(ht_diff = abs(Crew - QAQC))

#----- Tree Data
live <- c("AB", "AF", "AL", "AS", "AM", "RB", "RF", "RL", "RS")

tree_data <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = 'all', canopyPosition = 'all', 
                                                   ouput = 'verbose'))) %>% 
                     filter_plot() %>% name_team() %>% 
                     select(Plot_Name, StartYear, Team, ScientificName, TagCode,
                            Fork, DBHcm, IsDBHVerified, TreeStatusCode,
                            CrownClassCode, DecayClassCode, Pct_Tot_Foliage_Cond,
                           HWACode, BBDCode, TreeEventNote) %>% 
                     mutate(TotFol_code = case_when((TreeStatusCode %in% live) & is.na(Pct_Tot_Foliage_Cond) ~ 0,
                                                     Pct_Tot_Foliage_Cond == 5.5 ~ 1,
                                                     Pct_Tot_Foliage_Cond == 30 ~ 2,
                                                     Pct_Tot_Foliage_Cond == 70 ~ 3,
                                                     Pct_Tot_Foliage_Cond == 95 ~ 4,
                                                     TRUE ~ NA_real_),
                            TotFol_txt = case_when(TotFol_code == 0 ~ "0%",
                                                   TotFol_code == 1 ~ "1-10%",
                                                   TotFol_code == 2 ~ "10-50%",
                                                   TotFol_code == 3 ~ "50-90%",
                                                   TotFol_code == 4 ~ "90-100%",
                                                   TRUE ~ NA_character_))

tree_crew <- tree_data %>% filter(Team == "Crew")
tree_qaqc <- tree_data %>% filter(Team == "QAQC")

tree_wide <- full_join(tree_crew, tree_qaqc, 
                       by = c("Plot_Name", "StartYear", "ScientificName", "TagCode", "Fork"),
                       suffix = c("_C", "_Q")) %>% 
             mutate(DBH_diff = DBHcm_C - DBHcm_Q, 
                    fol_diff = abs(TotFol_code_C - TotFol_code_Q),
                    dec_diff = abs(DecayClassCode_C - DecayClassCode_Q),
                    HWA_diff = abs(HWACode_C - HWACode_Q),
                    BBD_diff = abs(BBDCode_C - BBDCode_Q))

tree_wide2 <- make_sppcode(tree_wide) 

tree_wide2 <- tree_wide2 %>% 
              select(TagCode, sppcode, DBHcm_C, DBHcm_Q, DBH_diff, IsDBHVerified_C, IsDBHVerified_Q,
                     TreeStatusCode_C, TreeStatusCode_Q, CrownClassCode_C, CrownClassCode_Q,
                     TotFol_txt_C, TotFol_txt_Q, fol_diff, DecayClassCode_C, DecayClassCode_Q, dec_diff,
                     HWACode_C, HWACode_Q, HWA_diff, BBDCode_C, BBDCode_Q, BBD_diff)

#----- Tree Conditions
tr_cond <- do.call(joinTreeConditions, 
                   c(arglist[1:4], list(speciesType = 'all', status = 'all', canopyPosition = 'all'))) %>% 
           filter_plot() %>% name_team() %>% select(Plot_Name, TagCode, ScientificName, num_cond:Team)
tr_cond <- make_sppcode(tr_cond) %>% select(-ScientificName, -genus, -species)

trcond_c <- tr_cond %>% filter(Team == "Crew")
trcond_q <- tr_cond %>% filter(Team == "QAQC")

trcond_wide <- full_join(trcond_c, trcond_q, 
                         by = c("Plot_Name", "TagCode", "sppcode"),
                         suffix = c("_C", "_Q")) %>% 
               mutate(cond_diff = abs(num_cond_C - num_cond_Q)) %>% 
               select(TagCode, cond_diff, num_cond_C, num_cond_Q, H_C, H_Q, NO_C, NO_Q,
                      AD_C, AD_Q, BBD_C, BBD_Q, BC_C, BC_Q, BWA_C, BWA_Q, CAVL_C, CAVL_Q, CAVS_C, CAVS_Q,
                      CW_C, CW_Q, DBT_C, DBT_Q, DOG_C, DOG_Q, EAB_C, EAB_Q, EB_C, EB_Q, 
                      EHS_C, EHS_Q, G_C, G_Q, GM_C, GM_Q, HWA_C, HWA_Q, ID_C, ID_Q, 
                      OTH_C, OTH_Q, RPS_C, RPS_Q, SB_C, SB_Q, VIN_B_C, VIN_B_Q, VIN_C_C, VIN_C_Q)

#----- Tree Foliage Conditions
fol_cond <- do.call(joinTreeFoliageCond,
                   c(arglist[1:4], list(speciesType = 'all', canopyPosition = 'all', valueType = 'classes'))) %>%
  filter_plot() %>% name_team() %>% select(Plot_Name, TagCode, ScientificName, Team, Txt_Leaves_Aff_C:Txt_Leaf_Area_N) %>% 
  arrange(Team, TagCode)

fol_cond <- make_sppcode(fol_cond) %>% select(-ScientificName, -genus, -species)
fol_cols <- names(fol_cond[,4:13])
fol_cond[,fol_cols][is.na(fol_cond[,fol_cols])] <- "0%" # Only live trees, so okay to fill NAs

fol_cond2 <- do.call(cbind, c(fol_cond, 
                            lapply(seq_along(fol_cols), function(x){
                              add_folcode(fol_cond, fol_cols[x], fol_cov)}))) 

folcond_c <- fol_cond2 %>% filter(Team == "Crew")
folcond_q <- fol_cond2 %>% filter(Team == "QAQC")

folcond_wide <- full_join(folcond_c, folcond_q,
                          by = c("Plot_Name", "TagCode", "sppcode"),
                          suffix = c("_C", "_Q")) %>%
                select(TagCode, Txt_Leaves_Aff_C_C, Txt_Leaves_Aff_C_Q, Txt_Leaf_Area_C_C, Txt_Leaf_Area_C_Q,
                                Txt_Leaves_Aff_H_C, Txt_Leaves_Aff_H_Q, Txt_Leaf_Area_H_C, Txt_Leaf_Area_H_Q,
                                Txt_Leaves_Aff_L_C, Txt_Leaves_Aff_L_Q, 
                                Txt_Leaves_Aff_N_C, Txt_Leaves_Aff_N_Q, Txt_Leaf_Area_N_C, Txt_Leaf_Area_N_Q,
                                Txt_Leaves_Aff_O_C, Txt_Leaves_Aff_O_Q, 
                                Txt_Leaves_Aff_S_C, Txt_Leaves_Aff_S_Q, 
                                Txt_Leaves_Aff_W_C, Txt_Leaves_Aff_W_Q, 
                                contains("_code"))
names(folcond_wide)

check_covclass(folcond_wide, "Leaves_Aff_L_code_C", "Leaves_Aff_L_code_Q")
folcond_wide[, c("Leaves_Aff_L_code_C", "Leaves_Aff_L_code_Q")]

#----- Microplot Saplings
saps <- do.call(joinMicroSaplings, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>%  
  filter_plot() %>% name_team() %>% select(Team, SQSaplingCode, MicroplotCode, ScientificName, DBHcm) %>% 
  group_by(MicroplotCode, ScientificName, Team) %>% 
  mutate(rank = rank(DBHcm)) %>% 
  arrange(MicroplotCode, ScientificName, rank, Team) %>% select(-rank)

sap_sum <- saps %>% group_by(MicroplotCode, Team) %>% summarize(num_saps = sum(!is.na(DBHcm)),
                                                                avg_dbh = round(mean(DBHcm, na.rm = T),1),
                                                                .groups = "drop") %>% 
                    pivot_wider(names_from = Team,
                                values_from = c(num_saps, avg_dbh)) %>% 
                    mutate(dbh_diff = abs(avg_dbh_Crew - avg_dbh_QAQC)) %>% data.frame()

#----- Microplot Shrubs
shrubs <- do.call(joinMicroShrubData, c(arglist, list(speciesType = 'all', valueType = 'all'))) %>% 
  filter_plot() %>% name_team() %>% select(Team, ScientificName, Txt_Cov_UR:Txt_Cov_B, shrub_avg_cov,
                                           shrub_pct_freq) %>% 
  rename(UR = Txt_Cov_UR, UL = Txt_Cov_UL, B = Txt_Cov_B)

shrubs_long <- shrubs %>% select(Team, ScientificName, UR, UL, B) %>% 
                          pivot_longer(cols = c(UR, UL, B),
                                       names_to = 'Micro',
                                       values_to = 'Txt_Cov') %>% 
                          filter(!Txt_Cov %in% "0%")

shrub_cov <- data.frame(txt = c("0%", "<1%", "1-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-95%", "95-100%"),
                       pct_class = c(0, 1, 2, 3, 4, 5, 6, 7, 8))

shrubs_long2 <- left_join(shrubs_long, shrub_cov, by = c("Txt_Cov" = 'txt')) 

shrubs_comp <- full_join(shrubs_long2 %>% filter(Team == "Crew") %>% select(-Team), 
                         shrubs_long2 %>% filter(Team == "QAQC") %>% select(-Team),
                         by = c("Micro", "ScientificName"),
                         suffix = c("_C", "_Q")) %>% 
               select(Micro, everything()) %>% arrange(Micro, ScientificName)

# handle if microplot was dropped
micro_names <- data.frame(Micro = c("UR", "UL", "B")) 

shrubs_full <- full_join(shrubs_comp, micro_names, by = "Micro")
shrubs_full[, c("Txt_Cov_C", "Txt_Cov_Q")][is.na(shrubs_full[, c("Txt_Cov_C", "Txt_Cov_Q")])] <- "0%"
shrubs_full[, c("pct_class_C", "pct_class_Q")][is.na(shrubs_full[, c("pct_class_C", "pct_class_Q")])] <- 0
shrubs_full$ScientificName[is.na(shrubs_full$ScientificName)] <- "None present"

#----- Microplot seedlings
seeds <- do.call(joinMicroSeedlings, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>% 
         filter_plot() %>% name_team() %>% 
         select(Team, MicroplotCode, ScientificName, sd_15_30cm:sd_p150cm, tot_seeds)

seeds_comp <- full_join(seeds %>% filter(Team == "Crew") %>% select(-Team),
                        seeds %>% filter(Team == "QAQC") %>% select(-Team),
                        by = c("MicroplotCode", "ScientificName"),
                        suffix = c("_C", "_Q")) %>% 
              arrange(MicroplotCode, ScientificName) %>% 
              select(MicroplotCode, ScientificName, sd_15_30cm_C, sd_15_30cm_Q, sd_30_100cm_C, sd_30_100cm_Q,
                     sd_100_150cm_C, sd_100_150cm_Q, sd_p150cm_C, sd_p150cm_Q, tot_seeds_C, tot_seeds_Q)

seeds_comp[,3:12][is.na(seeds_comp[,3:12])] <- 0


