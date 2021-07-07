#--------------------------------
# Compiling data for QAQC Reports
#--------------------------------

#----- Load libraries
library(tidyverse)
library(forestNETN)
library(htmltools)
library(knitr)
library(kableExtra)
source("QAQC_report_functions.R")
#importData() #local instance

#----- Compile data
arglist = list(park = substr(params$plot, 1, 4), from = year, to = year, QAQC = TRUE, locType = loc_type, eventType = 'complete')

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
  filter_plot() %>% name_team() %>% select(-IsQAQC) %>% arrange(CrownClassLabel, TagCode, Team)

tree_hts_wide <- tree_hts %>% pivot_wider(names_from = Team,
                                          values_from = Height,
                                          values_fill = NA_real_)
tree_cols = c("ParkUnit", "PlotCode", "StartYear", "CrownClassLabel", "TagCode", "Plot_Name", "Crew", "QAQC")
missing_teams <- setdiff(tree_cols, names(tree_hts_wide))
tree_hts_wide[missing_teams] <- NA_real_
tree_hts_wide$ht_diff <- abs(tree_hts_wide$Crew - tree_hts_wide$QAQC)

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

# plot tree DBH differences
dbh_diff <- tree_wide2 %>% select(TagCode, DBH_diff) %>% na.omit() 
max_dbh <- max(abs(dbh_diff$DBH_diff))
diff_plot <- ggplot(data = dbh_diff, aes(x = DBH_diff)) + 
               geom_density(alpha = 0.5, fill = "#8CAF88", color = "#738C70") + theme_FHM() + 
               geom_vline(xintercept = 0, linetype = 'dashed', col = "#717171", size = 1) + 
               labs(y = "Density", x = "DBH difference (cm)") + 
               theme(legend.position = 'none', panel.border = element_blank(), panel.background = element_blank()) +
               annotate(geom = "text", x = -max_dbh, y = Inf, label = "Crew tighter", 
                        color = 'black', size = 5, hjust = 0, vjust = 1) + 
               annotate(geom = "text", x = max_dbh, y = Inf, label = "QAQC tighter", 
                        color = 'black', size = 5, hjust = 1, vjust = 1) +
               xlim(c(max_dbh * -1, max_dbh))

#----- Tree Conditions
tr_cond <- do.call(joinTreeConditions, 
                   c(arglist[1:4], list(speciesType = 'all', status = 'all', canopyPosition = 'all', locType = loc_type))) %>% 
           filter_plot() %>% name_team() %>% select(Plot_Name, TagCode, TreeStatusCode, ScientificName, num_cond:Team)
tr_cond <- make_sppcode(tr_cond) %>% select(-ScientificName, -genus, -species)

trcond_c <- tr_cond %>% filter(Team == "Crew")
trcond_q <- tr_cond %>% filter(Team == "QAQC")

live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")

trcond_wide <- full_join(trcond_c, trcond_q, 
                         by = c("Plot_Name", "TagCode", "sppcode", "TreeStatusCode"),
                         suffix = c("_C", "_Q")) %>% 
               mutate(cond_diff = abs(num_cond_C - num_cond_Q),
                      Status = ifelse(TreeStatusCode %in% live, "live", "dead")) %>% 
               select(TagCode, Status, cond_diff, num_cond_C, num_cond_Q, H_C, H_Q, NO_C, NO_Q,
                      AD_C, AD_Q, BBD_C, BBD_Q, BC_C, BC_Q, BWA_C, BWA_Q, CAVL_C, CAVL_Q, CAVS_C, CAVS_Q,
                      CW_C, CW_Q, DBT_C, DBT_Q, DOG_C, DOG_Q, EAB_C, EAB_Q, EB_C, EB_Q, 
                      EHS_C, EHS_Q, G_C, G_Q, GM_C, GM_Q, HWA_C, HWA_Q, ID_C, ID_Q, 
                      OTH_C, OTH_Q, RPS_C, RPS_Q, SB_C, SB_Q, VIN_B_C, VIN_B_Q, VIN_C_C, VIN_C_Q)

trcond_wide[,c(3:51)][is.na(trcond_wide[,c(3:51)])] <- 0
trcond_wide[,c(8:9)][trcond_wide$Status == 'live',] <- NA
trcond_wide[,c(6:7, 10:17, 22:51)][trcond_wide$Status == 'dead',] <- NA
trcond_wide[,c(3:4)][is.na(trcond_wide[,c(3:4)])] <- 0

#----- Tree Foliage Conditions
fol_cond <- do.call(joinTreeFoliageCond,
                   c(arglist[1:4], list(speciesType = 'all', canopyPosition = 'all', valueType = 'classes', locType = loc_type))) %>%
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

check_covclass(folcond_wide, "Leaves_Aff_L_code_C", "Leaves_Aff_L_code_Q")
folcond_wide[, c("Leaves_Aff_L_code_C", "Leaves_Aff_L_code_Q")]

#----- Microplot Saplings
saps <- do.call(joinMicroSaplings, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>%  
  filter_plot() %>% name_team() %>% select(Team, SQSaplingCode, MicroplotCode, ScientificName, DBHcm) %>% 
  group_by(MicroplotCode, ScientificName, Team) %>% 
  arrange(MicroplotCode, Team, ScientificName, DBHcm) 

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

seeds_comp1 <- full_join(seeds %>% filter(Team == "Crew") %>% select(-Team),
                        seeds %>% filter(Team == "QAQC") %>% select(-Team),
                        by = c("MicroplotCode", "ScientificName"),
                        suffix = c("_C", "_Q")) %>% 
              arrange(MicroplotCode, ScientificName) %>% 
              select(MicroplotCode, ScientificName, sd_15_30cm_C, sd_15_30cm_Q, sd_30_100cm_C, sd_30_100cm_Q,
                     sd_100_150cm_C, sd_100_150cm_Q, sd_p150cm_C, sd_p150cm_Q, tot_seeds_C, tot_seeds_Q) %>% 
              filter(!ScientificName %in% "None present")

micro_names <- data.frame(MicroplotCode = c("UR", "UL", "B")) 
seeds_comp <- full_join(seeds_comp1, micro_names, by = "MicroplotCode") %>% arrange(MicroplotCode, ScientificName)
seeds_comp[,3:12][is.na(seeds_comp[,3:12])] <- 0
seeds_comp$ScientificName[is.na(seeds_comp$ScientificName)] <- "None present"

regen_sum <- do.call(joinRegenData, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>%
             filter_plot() %>% name_team() %>%
             select(Team, ScientificName, seed_den, sap_den, stock) %>%
             filter(!ScientificName %in% "None present")

regen_comp1 <- full_join(regen_sum %>% filter(Team == "Crew") %>% select(-Team),
                        regen_sum %>% filter(Team == "QAQC") %>% select(-Team),
                        by = "ScientificName",
                        suffix = c("_C", "_Q"))

regen_comp1[,2:7][is.na(regen_comp1[,2:7])] <- 0

regen_comp <- regen_comp1 %>% mutate_if(is.numeric, round, 1) %>% 
                              mutate(seed_pct_diff = pct_diff(seed_den_C, seed_den_Q),
                                     sap_pct_diff = pct_diff(sap_den_C, sap_den_Q),
                                     stock_pct_diff = pct_diff(stock_C, stock_Q)) %>% 
              select(ScientificName, seed_den_C, seed_den_Q, sap_den_C, sap_den_Q,
                     stock_C, stock_Q, everything())

#----- Quadrat Character
quad_chr <- do.call(joinQuadData, c(arglist, list(valueType = 'classes'))) %>% filter_plot() %>% name_team() %>% 
            select(Team, CharacterLabel, starts_with("Txt")) 

newname <- substr(names(quad_chr[,3:10]), 9, 10)
quad_chr <- setNames(quad_chr, c(names(quad_chr[,1:2]), newname))

# quad_cov <- data.frame(txt = c("0%", "<1%", "1-2%", "2-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-95%", "95-100%"),
#                        pct_class = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

quad_chr_class <- quad_chr
quad_chr_class[,3:10][quad_chr_class[,3:10] == "0%"] <- 0
quad_chr_class[,3:10][quad_chr_class[,3:10] == "<1%"] <- 1
quad_chr_class[,3:10][quad_chr_class[,3:10] == "1-2%"] <- 2
quad_chr_class[,3:10][quad_chr_class[,3:10] == "2-5%"] <- 3
quad_chr_class[,3:10][quad_chr_class[,3:10] == "5-10%"] <- 4
quad_chr_class[,3:10][quad_chr_class[,3:10] == "10-25%"] <- 5
quad_chr_class[,3:10][quad_chr_class[,3:10] == "25-50%"] <- 6
quad_chr_class[,3:10][quad_chr_class[,3:10] == "50-75%"] <- 7
quad_chr_class[,3:10][quad_chr_class[,3:10] == "75-95%"] <- 8
quad_chr_class[,3:10][quad_chr_class[,3:10] == "95-100%"] <- 9

quad_chr_class <- quad_chr_class %>% mutate(across(UC:UL, ~as.numeric(.)))
quad_chr2 <- full_join(quad_chr, quad_chr_class, by = c("Team", "CharacterLabel"),
                       suffix = c("", "_class")) %>% 
                       mutate(order = case_when(CharacterLabel == "Soil" ~ 1,
                                                CharacterLabel == "Rock" ~ 2,
                                                CharacterLabel == "Stem" ~ 3,
                                                CharacterLabel == "Wood" ~ 4,
                                                CharacterLabel == "Sphagnum" ~ 5,
                                                CharacterLabel == "NonSphagnum" ~ 6,
                                                CharacterLabel == "Lichens" ~ 7
                                                ))

quad_chr_comp <- full_join(quad_chr2 %>% filter(Team == "Crew") %>% select(-Team), 
                       quad_chr2 %>% filter(Team == "QAQC") %>% select(-Team),
                       by = c("CharacterLabel", "order"),
                       suffix = c("_C", "_Q")) %>% 
                       mutate(UC_dif = abs(UC_class_C - UC_class_Q),
                              UR_dif = abs(UR_class_C - UR_class_Q),
                              MR_dif = abs(MR_class_C - MR_class_Q),
                              BR_dif = abs(BR_class_C - BR_class_Q),
                              BC_dif = abs(BC_class_C - BC_class_Q),
                              BL_dif = abs(BL_class_C - BL_class_Q),
                              ML_dif = abs(ML_class_C - ML_class_Q),
                              UL_dif = abs(UL_class_C - UL_class_Q)) %>%
                       arrange(order) %>% 
                       select(CharacterLabel, 
                              UC_C, UC_Q, UR_C, UR_Q,
                              MR_C, MR_Q, BR_C, BR_Q,
                              BC_C, BC_Q, BL_C, BL_Q,
                              ML_C, ML_Q, UL_C, UL_Q,
                              UC_dif, UR_dif, MR_dif, BR_dif,
                              BC_dif, BL_dif, ML_dif, UL_dif) %>% 
                       rename(Character = CharacterLabel)


#----- Quadrat species
quad_spp1 <- do.call(joinQuadSpecies, arglist) %>% filter_plot() %>% name_team() 

quad_spp <- quad_spp1 %>% 
  select(Team, ScientificName, IsGerminant, starts_with("Txt")) 

newname <- substr(names(quad_spp[,4:11]), 9, 10)
quad_spp <- setNames(quad_spp, c(names(quad_spp[,1:3]), newname))

# quad_cov <- data.frame(txt = c("0%", "<1%", "1-2%", "2-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-95%", "95-100%"),
#                        pct_class = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
quad_spp_class <- quad_spp
quad_spp_class[,4:11][quad_spp_class[,4:11] == "0%"] <- 0
quad_spp_class[,4:11][quad_spp_class[,4:11] == "<1%"] <- 1
quad_spp_class[,4:11][quad_spp_class[,4:11] == "1-2%"] <- 2
quad_spp_class[,4:11][quad_spp_class[,4:11] == "2-5%"] <- 3
quad_spp_class[,4:11][quad_spp_class[,4:11] == "5-10%"] <- 4
quad_spp_class[,4:11][quad_spp_class[,4:11] == "10-25%"] <- 5
quad_spp_class[,4:11][quad_spp_class[,4:11] == "25-50%"] <- 6
quad_spp_class[,4:11][quad_spp_class[,4:11] == "50-75%"] <- 7
quad_spp_class[,4:11][quad_spp_class[,4:11] == "75-95%"] <- 8
quad_spp_class[,4:11][quad_spp_class[,4:11] == "95-100%"] <- 9
quad_spp_class[,4:11][is.na(quad_spp_class[,4:11])] <- 0

quad_spp_class <- suppressWarnings(quad_spp_class %>% mutate(across(UC:UL, ~as.numeric(.))))

quad_spp2 <- full_join(quad_spp, quad_spp_class, by = c("Team", "ScientificName", "IsGerminant"),
                       suffix = c("", "_class"))
  

quad_spp_comp <- full_join(quad_spp2 %>% filter(Team == "Crew") %>% select(-Team), 
                           quad_spp2 %>% filter(Team == "QAQC") %>% select(-Team),
                           by = c("ScientificName", "IsGerminant"),
                           suffix = c("_C", "_Q"))  

quad_spp_comp[, c(3:10, 19:26)][is.na(quad_spp_comp[, c(3:10, 19:26)])] <- "0%"
quad_spp_comp[, c(11:18, 27:34)][is.na(quad_spp_comp[, c(11:18, 27:34)])] <- 0

  
quad_spp_comp <- quad_spp_comp %>% 
  mutate(UC_dif = abs(UC_class_C - UC_class_Q),
         UR_dif = abs(UR_class_C - UR_class_Q),
         MR_dif = abs(MR_class_C - MR_class_Q),
         BR_dif = abs(BR_class_C - BR_class_Q),
         BC_dif = abs(BC_class_C - BC_class_Q),
         BL_dif = abs(BL_class_C - BL_class_Q),
         ML_dif = abs(ML_class_C - ML_class_Q),
         UL_dif = abs(UL_class_C - UL_class_Q))# %>%

#quad_spp_comp[,35:42][is.na(quad_spp_comp[,35:42])] <- 0

quad_spp_comp$spp_miss_C <- ifelse(rowSums(
  quad_spp_comp[, c("UC_class_C", "UR_class_C", "MR_class_C", "BR_class_C", 
                     "BC_class_C", "BL_class_C", "ML_class_C", "UL_class_C")], na.rm = T) == 0, 1, 0)

quad_spp_comp$spp_miss_Q <- ifelse(rowSums(
  quad_spp_comp[, c("UC_class_Q", "UR_class_Q", "MR_class_Q", "BR_class_Q", 
                    "BC_class_Q", "BL_class_Q", "ML_class_Q", "UL_class_Q")], na.rm = T) == 0, 1, 0)

quad_spp_comp2 <- quad_spp_comp %>% select(ScientificName, IsGerminant,
                                           UC_C, UC_Q, UR_C, UR_Q,
                                           MR_C, MR_Q, BR_C, BR_Q,
                                           BC_C, BC_Q, BL_C, BL_Q,
                                           ML_C, ML_Q, UL_C, UL_Q,
                                           UC_dif, UR_dif, MR_dif, BR_dif,
                                           BC_dif, BL_dif, ML_dif, UL_dif,
                                           spp_miss_C, spp_miss_Q) %>% 
  arrange(ScientificName, IsGerminant) %>% rename(Germ = IsGerminant)


quad_spp_comp2[, 3:18][is.na(quad_spp_comp2[, 3:18])] <- "0%"
quad_spp_comp2[, 19:28][is.na(quad_spp_comp2[, 19:28])] <- 0

quad_sum <- quad_spp1 %>% select(Team, ScientificName, IsGerminant, quad_avg_cov, quad_pct_freq)

quad_sum_comp <- full_join(quad_sum %>% filter(Team == "Crew") %>% select(-Team), 
                           quad_sum %>% filter(Team == "QAQC") %>% select(-Team),
                           by = c("ScientificName", "IsGerminant"),
                           suffix = c("_C", "_Q")) 

quad_sum_comp[,2:6][is.na(quad_sum_comp[,2:6])] <- 0

quad_sum_comp2 <- quad_sum_comp %>%  
                    mutate(pct_diff_cov = pct_diff(quad_avg_cov_C, quad_avg_cov_Q),
                           pct_diff_freq = pct_diff(quad_pct_freq_C, quad_pct_freq_Q)) %>% 
                    select(ScientificName, IsGerminant, quad_avg_cov_C, quad_avg_cov_Q, 
                           quad_pct_freq_C, quad_pct_freq_Q, pct_diff_cov, pct_diff_freq) %>% 
                    mutate_if(is.numeric, round, 2) %>% arrange(ScientificName, IsGerminant)

#------ Additional species
spp_list <- do.call(sumSpeciesList, c(arglist, list(speciesType = 'all'))) %>% filter_plot() %>% name_team() 

spp_list2 <- spp_list %>% mutate(Trees = ifelse(DBH_mean > 0, 1, 0),
                                 Micros = ifelse(seed_den + sap_den + shrub_avg_cov > 0, 1, 0),
                                 Quads = ifelse(quad_avg_cov > 0, 1, 0),
                                 AddSpp = ifelse(addspp_present > 0 , 1, 0)) %>% 
                          select(Team, ScientificName, Trees, Micros, Quads, AddSpp)

spp_list_comp <- full_join(spp_list2 %>% filter(Team == "Crew") %>% select(-Team), 
                           spp_list2 %>% filter(Team == "QAQC") %>% select(-Team),
                           by = c("ScientificName"),
                           suffix = c("_C", "_Q")) %>% 
                 select(ScientificName, Trees_C, Trees_Q, Micros_C, Micros_Q, 
                        Quads_C, Quads_Q, AddSpp_C, AddSpp_Q)

spp_list_comp[,2:9][is.na(spp_list_comp[,2:9])] <- 0

spp_list_comp2 <- spp_list_comp %>% group_by(ScientificName) %>% summarize_if(is.numeric, sum)
spp_list_comp2[,2:9][spp_list_comp2[,2:9] > 1] <- 1

spp_list_comp2$missed_C <- ifelse(rowSums(spp_list_comp2[,c("Trees_C", "Micros_C", "Quads_C", "AddSpp_C")], na.rm = T) == 0, 1, 0)
spp_list_comp2$missed_Q <- ifelse(rowSums(spp_list_comp2[,c("Trees_Q", "Micros_Q", "Quads_Q", "AddSpp_Q")], na.rm = T) == 0, 1, 0)

#----- CWD
cwd_raw <- VIEWS_NETN$COMN_CWD %>% select(ParkUnit, PlotCode, StartYear, IsQAQC, SQTransectCode, TransectCode, 
                                          ScientificName, Distance, Diameter, Length, DecayClassCode,
                                          MultiCrossCode, IsHollow, CWDNote)
cwd_raw$Plot_Name <- paste(cwd_raw$ParkUnit, sprintf("%03d", cwd_raw$PlotCode), sep = "-") 
cwd_raw <- cwd_raw %>% filter_plot() %>% name_team() %>% 
  rename(Transect = TransectCode, SQ = SQTransectCode, Species = ScientificName, Decay = DecayClassCode, 
         MultiCross = MultiCrossCode, Hollow = IsHollow)

cwd_raw2 <- cwd_raw %>% select(Team, SQ, Transect, Distance, Species, Diameter, 
                               Length, Decay, MultiCross, Hollow, CWDNote) %>% 
                        arrange(Transect, Distance, Team, Diameter) %>% 
                        mutate(Species = ifelse(SQ == "NP", "None present", Species))

cwd_sum1 <- cwd_raw2 %>% group_by(Team, Species) %>% 
                        summarize(num_pieces = length(!is.na(Diameter)), .groups = 'drop') %>% 
                        filter(Species != "None present")

cwd_sum <- full_join(cwd_sum1 %>% filter(Team == "Crew") %>% select(-Team),
                     cwd_sum1 %>% filter(Team == "QAQC") %>% select(-Team),
                     by = "Species", suffix = c("_C", "_Q"))

cwd_vol <- do.call(joinCWDData, arglist) %>% filter_plot() %>% name_team() %>% 
  select(Team, ScientificName, DecayClassCode, CWD_Vol) %>% 
  rename(Species = ScientificName, Decay = DecayClassCode)

cwd_comp <- full_join(cwd_vol %>% filter(Team == "Crew") %>% select(-Team),
                      cwd_vol %>% filter(Team == "QAQC") %>% select(-Team),
                      by = c("Species"),#, "Decay"),
                      suffix = c("_C", "_Q")) %>% filter(Species != "None present") %>% 
            arrange(Species)#, Decay)


cwd_join <- full_join(cwd_comp, cwd_sum, by = "Species")

cwd_join$Decay_C <- as.integer(cwd_join$Decay_C)            
cwd_join$Decay_Q <- as.integer(cwd_join$Decay_Q)            

cwd_tot <- data.frame(Species = "CWD Total", 
                      Decay_C = mean(cwd_join$Decay_C, na.rm = T),
                      Decay_Q = mean(cwd_join$Decay_Q, na.rm = T),
                      CWD_Vol_C = sum(cwd_join$CWD_Vol_C, na.rm = T),
                      CWD_Vol_Q = sum(cwd_join$CWD_Vol_Q, na.rm = T),
                      num_pieces_C = sum(cwd_join$num_pieces_C, na.rm = T),
                      num_pieces_Q = sum(cwd_join$num_pieces_Q, na.rm = T))

cwd_comp2 <- rbind(cwd_join, cwd_tot) %>% 
             mutate_if(is.numeric, ~round(., 1)) %>% 
             mutate(vol_dif = pct_diff(CWD_Vol_C, CWD_Vol_Q)) %>% 
             select(Species, Decay_C, Decay_Q, CWD_Vol_C, CWD_Vol_Q, num_pieces_C, num_pieces_Q, vol_dif)
cwd_comp2

#----- Soil
soil_samp1 <- get("COMN_SoilSample", envir = VIEWS_NETN) %>% 
  select(ParkUnit, PlotCode, StartYear, IsQAQC, SampleSequenceCode, SQSoilCode, SQSoilEventNotes,
         SoilLayerLabel, Depth_cm, Note)

soil_samp1$Plot_Name <- paste(soil_samp1$ParkUnit, sprintf("%03d", soil_samp1$PlotCode), sep = "-") 

soil_samp <- soil_samp1 %>% filter_plot() %>% name_team() %>% 
                            select(Team, SampleSequenceCode, SQSoilCode, SoilLayerLabel, Depth_cm) 

SOILSAMPLE <- ifelse(any(soil_samp$SQSoilCode == "SS"), TRUE, FALSE)

if(SOILSAMPLE == TRUE){
soil_wide <- soil_samp %>% pivot_wider(names_from = SoilLayerLabel, 
                                       values_from = Depth_cm, 
                                       values_fill = NA_real_) %>% 
             rename(Sample = SampleSequenceCode,
                    Litter = `Unconsolidated Litter`) %>% select(-SQSoilCode) 

names(soil_wide) <- gsub(" ", "_", names(soil_wide))
names(soil_wide) <- gsub("Horizon", "hor", names(soil_wide))

all_soil_cols <- c("Team", "Sample", "Litter", "O_hor", "A_hor", "Total_Depth")
missing_soil_cols <- setdiff(all_soil_cols, names(soil_wide))
soil_wide[missing_soil_cols] <- 0

soil_comp <- full_join(soil_wide %>% filter(Team == "Crew") %>% select(-Team),
                       soil_wide %>% filter(Team == "QAQC") %>% select(-Team),
                       by = "Sample", suffix = c("_C", "_Q")) %>% 
             arrange(Sample) %>% select(Sample, Litter_C, Litter_Q, 
                                        O_hor_C, O_hor_Q, A_hor_C, A_hor_Q,
                                        Total_Depth_C, Total_Depth_Q) %>% 
             mutate_if(is.numeric, ~round(., 1))

soil_comp[,3:(ncol(soil_comp))][is.na(soil_comp[,3:(ncol(soil_comp))])] <- 0

}

