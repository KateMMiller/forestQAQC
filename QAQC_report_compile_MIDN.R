#--------------------------------
# Compiling data for QAQC Reports
#--------------------------------

#----- Load libraries
library(tidyverse)
library(forestMIDN)
library(htmltools)
library(knitr)
library(kableExtra)
library(vegan) # for betadiver()
source("QAQC_report_functions.R")

# importData() #local instance
# forestMIDN::importCSV(path = "D:/NETN/R_Dev/data/", zip_name = "MIDN_Forest_20220321.zip") # zip after new views validated

#----- Compile data
# year = 2024 #2023
# plot = "COLO-338" #VAFO-245
# loc_type = 'all'

arglist = list(park = substr(plot, 1, 4), 
               from = year, to = year, QAQC = TRUE, 
               locType = loc_type, eventType = 'all')

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

tree_hts <- get("StandTreeHeights_MIDN_NCBN", envir = VIEWS_MIDN_NCBN) %>%
  select(Plot_Name, ParkUnit, PlotCode, SampleYear, IsQAQC, CrownClassLabel, TagCode, Height) %>%
  filter_plot() %>% name_team() %>% select(-IsQAQC) %>% arrange(CrownClassLabel, TagCode, Team)

tree_hts_wide <- tree_hts %>% pivot_wider(names_from = Team,
                                          values_from = Height,
                                          values_fill = NA_real_)
tree_cols = c("ParkUnit", "PlotCode", "SampleYear", "CrownClassLabel", "TagCode", "Plot_Name", "Crew", "QAQC")
missing_teams <- setdiff(tree_cols, names(tree_hts_wide))
tree_hts_wide[missing_teams] <- NA_real_
tree_hts_wide$ht_diff <- abs(tree_hts_wide$Crew - tree_hts_wide$QAQC)
tree_hts_wide$ht_pct_diff <- pct_diff(tree_hts_wide$Crew, tree_hts_wide$QAQC)
check_20pct_diff(tree_hts_wide, "ht_pct_diff")

#----- Tree Data
live <- c("AB", "AF", "AL", "AS", "AM", "RB", "RF", "RL", "RS")

tree_data <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = 'all', canopyPosition = 'all'))) %>% 
                     filter_plot() %>% name_team() %>% 
                     select(Plot_Name, SampleYear, Team, ScientificName, TagCode,
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
                       by = c("Plot_Name", "SampleYear", "ScientificName", "TagCode", "Fork"),
                       suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
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
#if(nrow(tree_wide2) > 1){
dbh_diff <- tree_wide2 %>% select(TagCode, DBH_diff) %>% na.omit() 
max_dbh <- max(abs(dbh_diff$DBH_diff))
diff_plot <- ggplot(data = dbh_diff, aes(x = DBH_diff)) + 
               geom_density(alpha = 0.5, fill = "#8CAF88", color = "#738C70") + 
               theme_FVM() + 
               geom_vline(xintercept = 0, linetype = 'dashed', col = "#717171", linewidth = 1) + 
               labs(y = "Density", x = "DBH difference (cm)") + 
               theme(legend.position = 'none', panel.border = element_blank(), panel.background = element_blank()) +
               annotate(geom = "text", x = -max_dbh, y = Inf, label = "Crew tighter", 
                        color = 'black', size = 5, hjust = 0, vjust = 1) + 
               annotate(geom = "text", x = max_dbh, y = Inf, label = "QAQC tighter", 
                        color = 'black', size = 5, hjust = 1, vjust = 1) +
               xlim(c(max_dbh * -1, max_dbh))
#}
#----- Tree Conditions
tr_cond <- do.call(joinTreeConditions, 
                   c(arglist[1:4], list(speciesType = 'all', status = 'all', canopyPosition = 'all', 
                                        locType = loc_type))) %>% 
           filter_plot() %>% name_team() %>% select(Plot_Name, TagCode, TreeStatusCode, ScientificName, num_cond:Team)

tr_cond <- make_sppcode(tr_cond) %>% select(-ScientificName, -genus, -species)

trcond_c <- tr_cond %>% filter(Team == "Crew")
trcond_q <- tr_cond %>% filter(Team == "QAQC")

live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")

trcond_wide <- full_join(trcond_c, trcond_q, 
                         by = c("Plot_Name", "TagCode", "sppcode", "TreeStatusCode"),
                         suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
  mutate(cond_diff = abs(num_cond_C - num_cond_Q),
         Status = ifelse(TreeStatusCode %in% live, "live", "dead")) %>% 
  select(TagCode, Status, cond_diff, num_cond_C, num_cond_Q, H_C, H_Q, NO_C, NO_Q,
         AD_C, AD_Q, BBD_C, BBD_Q, BC_C, BC_Q, BLD_C, BLD_Q, BWA_C, BWA_Q, 
         CAVL_C, CAVL_Q, CAVS_C, CAVS_Q, CW_C, CW_Q, DBT_C, DBT_Q, DOG_C, DOG_Q, 
         EAB_C, EAB_Q, EB_C, EB_Q, EHS_C, EHS_Q, G_C, G_Q, GM_C, GM_Q, HWA_C, HWA_Q, 
         ID_C, ID_Q, OTH_C, OTH_Q, RPS_C, RPS_Q, SB_C, SB_Q, SLF_C, SLF_Q,
         VIN_B_C, VIN_B_Q, VIN_C_C, VIN_C_Q)

trcond_wide[,c(3:ncol(trcond_wide))][is.na(trcond_wide[,c(3:ncol(trcond_wide))])] <- 0
trcond_wide[,c(8:9)][trcond_wide$Status == 'live',] <- NA
trcond_wide[,c(6:7, 10:19, 24:55)][trcond_wide$Status == 'dead',] <- NA
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
                          suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>%
                select(TagCode, Txt_Leaves_Aff_C_C, Txt_Leaves_Aff_C_Q, Txt_Leaf_Area_C_C, Txt_Leaf_Area_C_Q,
                                Txt_Leaves_Aff_H_C, Txt_Leaves_Aff_H_Q, Txt_Leaf_Area_H_C, Txt_Leaf_Area_H_Q,
                                Txt_Leaves_Aff_L_C, Txt_Leaves_Aff_L_Q, 
                                Txt_Leaves_Aff_N_C, Txt_Leaves_Aff_N_Q, Txt_Leaf_Area_N_C, Txt_Leaf_Area_N_Q,
                                Txt_Leaves_Aff_O_C, Txt_Leaves_Aff_O_Q, 
                                Txt_Leaves_Aff_S_C, Txt_Leaves_Aff_S_Q, 
                                Txt_Leaves_Aff_W_C, Txt_Leaves_Aff_W_Q, 
                                contains("_code"))

# check_covclass(folcond_wide, "Leaves_Aff_L_code_C", "Leaves_Aff_L_code_Q")
# folcond_wide[, c("Leaves_Aff_L_code_C", "Leaves_Aff_L_code_Q")]

# head(folcond_wide)

#----- Microplot Saplings
live <- c("AB", "AF", "AL", "AS", "AM", "RB", "RF", "RL", "RS")

sap_data <- do.call(joinMicroSaplings, 
                    c(arglist, list(speciesType = 'all', status = 'all'))) %>% 
  filter_plot() %>% name_team() %>% 
  select(Plot_Name, SampleYear, Team, ScientificName, MicroplotCode, TagCode,
         Fork, DBHcm, IsDBHVerified, SaplingStatusCode) 

sap_crew <- sap_data %>% filter(Team == "Crew")
sap_qaqc <- sap_data %>% filter(Team == "QAQC")

sap_wide <- full_join(sap_crew, sap_qaqc, 
                       by = c("Plot_Name", "SampleYear", "ScientificName", "MicroplotCode", 
                              "TagCode", "Fork"),
                       suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
  mutate(DBH_diff = DBHcm_C - DBHcm_Q)

# For saplings not sampled, so doesn't fail
sap_wide$DBH_diff[is.na(sap_wide$DBH_diff)] <- 0

sap_wide2 <- make_sppcode(sap_wide) 

sap_wide2 <- sap_wide2 %>% 
  select(TagCode, MicroplotCode, sppcode, DBHcm_C, DBHcm_Q, DBH_diff, IsDBHVerified_C, IsDBHVerified_Q,
         SaplingStatusCode_C, SaplingStatusCode_Q) %>% arrange(TagCode)

sap_wide2

# plot sapling DBH differences
sap_dbh_diff <- sap_wide2 %>% select(TagCode, DBH_diff) %>% na.omit() 
sap_max_dbh <- max(abs(sap_dbh_diff$DBH_diff), na.rm = T)

sap_diff_plot <- ggplot(data = sap_dbh_diff, aes(x = DBH_diff)) + 
  geom_density(alpha = 0.5, fill = "#8CAF88", color = "#738C70") + 
  theme_FVM() + 
  geom_vline(xintercept = 0, linetype = 'dashed', col = "#717171", linewidth = 1) + 
  labs(y = "Density", x = "DBH difference (cm)") + 
  theme(legend.position = 'none', panel.border = element_blank(), panel.background = element_blank()) +
  annotate(geom = "text", x = -sap_max_dbh, y = Inf, label = "Crew tighter", 
           color = 'black', size = 5, hjust = 0, vjust = 1) + 
  annotate(geom = "text", x = sap_max_dbh, y = Inf, label = "QAQC tighter", 
           color = 'black', size = 5, hjust = 1, vjust = 1) +
  xlim(c(sap_max_dbh * -1, sap_max_dbh))

saps <- do.call(joinMicroSaplings, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>%
  filter_plot() %>% name_team() %>% select(Team, SQSaplingCode, MicroplotCode, ScientificName, DBHcm) %>%
  group_by(MicroplotCode, ScientificName, Team) %>%
  arrange(MicroplotCode, Team, ScientificName, DBHcm)

sap_sum <- saps %>% group_by(MicroplotCode, Team) %>% summarize(num_saps = sum(!is.na(DBHcm)),
                                                                avg_dbh = round(mean(DBHcm, na.rm = T),2),
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

shrubs_long2 <- left_join(shrubs_long, shrub_cov, by = c("Txt_Cov" = 'txt'), multiple = 'all') 

shrubs_comp <- full_join(shrubs_long2 %>% filter(Team == "Crew") %>% select(-Team), 
                         shrubs_long2 %>% filter(Team == "QAQC") %>% select(-Team),
                         by = c("Micro", "ScientificName"),
                         suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
               select(Micro, everything()) %>% arrange(Micro, ScientificName)

# handle if microplot was dropped
micro_names <- data.frame(Micro = c("UR", "UL", "B")) 

shrubs_full <- full_join(shrubs_comp, micro_names, by = "Micro", multiple = 'all', relationship = 'many-to-many')
shrubs_full[, c("Txt_Cov_C", "Txt_Cov_Q")][is.na(shrubs_full[, c("Txt_Cov_C", "Txt_Cov_Q")])] <- "0%"
shrubs_full[, c("pct_class_C", "pct_class_Q")][is.na(shrubs_full[, c("pct_class_C", "pct_class_Q")])] <- 0
shrubs_full$ScientificName[is.na(shrubs_full$ScientificName)] <- "None present"

# Shrub taxonomic accuracy +++ NEW 20220321 +++
shrub_ta <- full_join(shrubs %>% filter(Team == "Crew") %>% select(ScientificName, shrub_avg_cov),
                      shrubs %>% filter(Team == "QAQC") %>% select(ScientificName, shrub_avg_cov),
                      by = c("ScientificName"), suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many')

shrub_spp <- rbind(shrub_ta %>% filter(shrub_avg_cov_C >= 1) %>% #drop <1%
                     mutate(team = "crew", 
                            shrub_avg_cov = round(shrub_avg_cov_C, 2)) %>% 
                     select(team, ScientificName, shrub_avg_cov) %>% unique(),
                   shrub_ta %>% filter(shrub_avg_cov_Q >= 1) %>% #drop <1% 
                     mutate(team = 'qaqc',
                            shrub_avg_cov = round(shrub_avg_cov_Q, 2)) %>% 
                     select(team, ScientificName, shrub_avg_cov) %>% unique())

# Make report table and deal with empty dfs
if(nrow(shrub_spp)>0){
  
  # Will use shrub_ta for end to show avg. cover
  #shrub_ta[,2:3][is.na(shrub_ta[,2:3])] <- 0
  
  shrub_spp$shrub_avg_cov[is.na(shrub_spp$shrub_avg_cov)] <- 0
  
  shrub_spp_wide <- shrub_spp %>% pivot_wider(names_from = ScientificName, values_from = shrub_avg_cov, values_fill = 0)
  
  shrub_taxa_acc <- if(nrow(shrub_spp) > 1){
    round(betadiver(shrub_spp_wide[,-1], method = 'sor', order = F), 2)
    } else {0}
  
  shrub_spp_wide2 <- shrub_spp %>% pivot_wider(names_from = team, values_from = shrub_avg_cov, values_fill = 0)
  
  req_cols <- c("ScientificName", "crew", "qaqc")
  miss_cols <- setdiff(req_cols, names(shrub_spp_wide2))
  shrub_spp_wide2[miss_cols] <- 0

  shrub_spp_wide2 <- shrub_spp_wide2[req_cols]
  
  shrub_spp_wide2 <- shrub_spp_wide2 %>% mutate(missed_c = ifelse(crew == 0 & qaqc > 0, 1, 0),
                                                missed_q = ifelse(crew > 0 & qaqc == 1, 1, 0))

  shrub_spp_wide2 <- rbind(shrub_spp_wide2,
                           c("Sorensen Similarity", NA, shrub_taxa_acc, NA, NA))
}

regen_sum <- do.call(joinRegenData, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>%
  filter_plot() %>% name_team() %>%
  select(Team, ScientificName, seed_den, sap_den, stock) %>%
  filter(!ScientificName %in% "None present")

regen_comp1 <- full_join(regen_sum %>% filter(Team == "Crew") %>% select(-Team),
                         regen_sum %>% filter(Team == "QAQC") %>% select(-Team),
                         by = "ScientificName",
                         suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many')

regen_comp1[,2:7][is.na(regen_comp1[,2:7])] <- 0

regen_comp <- regen_comp1 %>% mutate_if(is.numeric, round, 2) %>% 
  mutate(seed_pct_diff = pct_diff(seed_den_C, seed_den_Q),
         sap_pct_diff = pct_diff(sap_den_C, sap_den_Q),
         stock_pct_diff = pct_diff(stock_C, stock_Q)) %>% 
  select(ScientificName, seed_den_C, seed_den_Q, sap_den_C, sap_den_Q,
         stock_C, stock_Q, everything())

#----- Quadrat Character for trampled
quad_tramp <- get("QuadNotes_MIDN_NCBN", envir = VIEWS_MIDN_NCBN)

quad_tramp <- quad_tramp %>% 
  #mutate(Plot_Name  = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', "0"), sep = "-")) %>% 
  filter_plot() %>% name_team() %>% select(Plot_Name, Team, SampleYear, QuadratCode, SQQuadCharCode, IsTrampled) %>% unique()

quad_tramp_wide <- quad_tramp %>% select(-SQQuadCharCode) %>% pivot_wider(names_from = c("QuadratCode"), 
                                                                          values_from = "IsTrampled")

quad_tramp_wide2 <- full_join(quad_tramp_wide %>% filter(Team == "Crew") %>% select(-Team), 
                              quad_tramp_wide %>% filter(Team == "QAQC") %>% select(-Team),
                              by = c("Plot_Name", "SampleYear"),
                              suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
  mutate(A2_dif = abs(A2_C - A2_Q),
         A5_dif = abs(A5_C - A5_Q),
         A8_dif = abs(A8_C - A8_Q),
         AA_dif = abs(AA_C - AA_Q),
         B2_dif = abs(B2_C - B2_Q),
         B5_dif = abs(B5_C - B5_Q),
         B8_dif = abs(B8_C - B8_Q),
         BB_dif = abs(BB_C - BB_Q),
         C2_dif = abs(C2_C - C2_Q),
         C5_dif = abs(C5_C - C5_Q),
         C8_dif = abs(C8_C - C8_Q),
         CC_dif = abs(CC_C - CC_Q),
         Character = "Trampled") %>% 
  select(Character, 
         A2_C, A2_Q, A5_C, A5_Q,
         A8_C, A8_Q, AA_C, AA_Q,
         B2_C, B2_Q, B5_C, B5_Q,
         B8_C, B8_Q, BB_C, BB_Q,
         C2_C, C2_Q, C5_C, C5_Q,
         C8_C, C8_Q, CC_C, CC_Q,
         A2_dif, A5_dif, A8_dif, AA_dif,
         B2_dif, B5_dif, B8_dif, BB_dif,
         C2_dif, C5_dif, C8_dif, CC_dif)

#----- Quadrat Character
quad_chr <- do.call(joinQuadData, c(arglist, list(valueType = 'classes'))) %>% filter_plot() %>% name_team() %>% 
            select(Team, CharacterLabel, starts_with("Txt")) 

newname <- substr(names(quad_chr[,3:14]), 9, 10)
quad_chr <- setNames(quad_chr, c(names(quad_chr[,1:2]), newname))

quad_chr_class <- quad_chr
quad_chr_class[,3:14][quad_chr_class[,3:14] == "0%"] <- 0
quad_chr_class[,3:14][quad_chr_class[,3:14] == "<1%"] <- 1
quad_chr_class[,3:14][quad_chr_class[,3:14] == "1-2%"] <- 2
quad_chr_class[,3:14][quad_chr_class[,3:14] == "2-5%"] <- 3
quad_chr_class[,3:14][quad_chr_class[,3:14] == "5-10%"] <- 4
quad_chr_class[,3:14][quad_chr_class[,3:14] == "10-25%"] <- 5
quad_chr_class[,3:14][quad_chr_class[,3:14] == "25-50%"] <- 6
quad_chr_class[,3:14][quad_chr_class[,3:14] == "50-75%"] <- 7
quad_chr_class[,3:14][quad_chr_class[,3:14] == "75-95%"] <- 8
quad_chr_class[,3:14][quad_chr_class[,3:14] == "95-100%"] <- 9

quad_chr_class <- quad_chr_class %>% mutate(across(A2:CC, ~as.numeric(.)))
quad_chr2 <- full_join(quad_chr, quad_chr_class, by = c("Team", "CharacterLabel"),
                       suffix = c("", "_class"), multiple = 'all', 
                       relationship = 'many-to-many') %>% 
                       mutate(order = case_when(CharacterLabel == "Soil" ~ 1,
                                                CharacterLabel == "Rock" ~ 2,
                                                CharacterLabel == "Stem" ~ 3,
                                                CharacterLabel == "Wood" ~ 4,
                                                CharacterLabel == "Sphagnum" ~ 5,
                                                CharacterLabel == "NonSphagnum" ~ 6,
                                                CharacterLabel == "Lichens" ~ 7,
                                                CharacterLabel == "Herbs" ~ 8
                                                ))

quad_chr_comp <- full_join(quad_chr2 %>% filter(Team == "Crew") %>% select(-Team), 
                       quad_chr2 %>% filter(Team == "QAQC") %>% select(-Team),
                       by = c("CharacterLabel", "order"),
                       suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
                       mutate(A2_dif = abs(A2_class_C - A2_class_Q),
                              A5_dif = abs(A5_class_C - A5_class_Q),
                              A8_dif = abs(A8_class_C - A8_class_Q),
                              AA_dif = abs(AA_class_C - AA_class_Q),
                              B2_dif = abs(B2_class_C - B2_class_Q),
                              B5_dif = abs(B5_class_C - B5_class_Q),
                              B8_dif = abs(B8_class_C - B8_class_Q),
                              BB_dif = abs(BB_class_C - BB_class_Q),
                              C2_dif = abs(C2_class_C - C2_class_Q),
                              C5_dif = abs(C5_class_C - C5_class_Q),
                              C8_dif = abs(C8_class_C - C8_class_Q),
                              CC_dif = abs(CC_class_C - CC_class_Q)) %>%
                       arrange(order) %>% 
                       select(CharacterLabel, 
                              A2_C, A2_Q, A5_C, A5_Q, A8_C, A8_Q, AA_C, AA_Q,
                              B2_C, B2_Q, B5_C, B5_Q, B8_C, B8_Q, BB_C, BB_Q,
                              C2_C, C2_Q, C5_C, C5_Q, C8_C, C8_Q, CC_C, CC_Q,
                              A2_dif, A5_dif, A8_dif, AA_dif, 
                              B2_dif, B5_dif, B8_dif, BB_dif,
                              C2_dif, C5_dif, C8_dif, CC_dif) %>% 
                       rename(Character = CharacterLabel)

quad_chr_comb <- rbind(quad_tramp_wide2, quad_chr_comp)
head(quad_chr_comb)

#----- Quadrat species
quad_spp1 <- do.call(joinQuadSpecies, arglist) %>% filter_plot() %>% name_team() 

quad_spp <- quad_spp1 %>% 
  select(Team, ScientificName, starts_with("Txt")) 

newname <- substr(names(quad_spp[,3:14]), 9, 10)
quad_spp <- setNames(quad_spp, c(names(quad_spp[,1:2]), newname))
names(quad_spp)
# 1 less column name in beginning b/c no IsGerminant compared with NETN
quad_spp_class <- quad_spp
quad_spp_class[,3:14][quad_spp_class[,3:14] == "0%"] <- 0
quad_spp_class[,3:14][quad_spp_class[,3:14] == "<1%"] <- 1
quad_spp_class[,3:14][quad_spp_class[,3:14] == "1-2%"] <- 2
quad_spp_class[,3:14][quad_spp_class[,3:14] == "2-5%"] <- 3
quad_spp_class[,3:14][quad_spp_class[,3:14] == "5-10%"] <- 4
quad_spp_class[,3:14][quad_spp_class[,3:14] == "10-25%"] <- 5
quad_spp_class[,3:14][quad_spp_class[,3:14] == "25-50%"] <- 6
quad_spp_class[,3:14][quad_spp_class[,3:14] == "50-75%"] <- 7
quad_spp_class[,3:14][quad_spp_class[,3:14] == "75-95%"] <- 8
quad_spp_class[,3:14][quad_spp_class[,3:14] == "95-100%"] <- 9
quad_spp_class[,3:14][is.na(quad_spp_class[,3:14])] <- 0

quad_spp_class <- suppressWarnings(quad_spp_class %>% mutate(across(A2:CC, ~as.numeric(.))))

quad_spp2 <- full_join(quad_spp, quad_spp_class, by = c("Team", "ScientificName"),
                       suffix = c("", "_class"), multiple = 'all', relationship = 'many-to-many')
  
quad_spp_comp <- full_join(quad_spp2 %>% filter(Team == "Crew") %>% select(-Team), 
                           quad_spp2 %>% filter(Team == "QAQC") %>% select(-Team),
                           by = c("ScientificName"),
                           suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many')  
names(quad_spp_comp)
#head(quad_spp_comp)
quad_spp_comp[, c(2:13, 26:37)][is.na(quad_spp_comp[, c(2:13, 26:37)])] <- "0%"
quad_spp_comp[, c(14:25, 38:49)][is.na(quad_spp_comp[, c(14:25, 38:49)])] <- 0

quad_spp_comp <- quad_spp_comp %>% 
  mutate(A2_dif = abs(A2_class_C - A2_class_Q),
         A5_dif = abs(A5_class_C - A5_class_Q),
         A8_dif = abs(A8_class_C - A8_class_Q),
         AA_dif = abs(AA_class_C - AA_class_Q),
         B2_dif = abs(B2_class_C - B2_class_Q),
         B5_dif = abs(B5_class_C - B5_class_Q),
         B8_dif = abs(B8_class_C - B8_class_Q),
         BB_dif = abs(BB_class_C - BB_class_Q),
         C2_dif = abs(C2_class_C - C2_class_Q),
         C5_dif = abs(C5_class_C - C5_class_Q),
         C8_dif = abs(C8_class_C - C8_class_Q),
         CC_dif = abs(CC_class_C - CC_class_Q))

#quad_spp_comp[,35:42][is.na(quad_spp_comp[,35:42])] <- 0

quad_spp_comp$spp_miss_C <- ifelse(rowSums(
  quad_spp_comp[, c("A2_class_C", "A5_class_C", "A8_class_C", "AA_class_C", 
                    "B2_class_C", "B5_class_C", "B8_class_C", "BB_class_C",
                    "C2_class_C", "C5_class_C", "C8_class_C", "CC_class_C")], 
                na.rm = T) == 0, 1, 0)

quad_spp_comp$spp_miss_Q <- ifelse(rowSums(
  quad_spp_comp[, c("A2_class_Q", "A5_class_Q", "A8_class_Q", "AA_class_Q", 
                    "B2_class_Q", "B5_class_Q", "B8_class_Q", "BB_class_Q",
                    "C2_class_Q", "C5_class_Q", "C8_class_Q", "CC_class_Q")], 
                na.rm = T) == 0, 1, 0)

quad_spp_comp2 <- quad_spp_comp %>% select(ScientificName, 
                                           A2_C, A2_Q, A5_C, A5_Q, A8_C, A8_Q, AA_C, AA_Q,
                                           B2_C, B2_Q, B5_C, B5_Q, B8_C, B8_Q, BB_C, BB_Q,
                                           C2_C, C2_Q, C5_C, C5_Q, C8_C, C8_Q, CC_C, CC_Q,
                                           A2_dif, A5_dif, A8_dif, AA_dif, 
                                           B2_dif, B5_dif, B8_dif, BB_dif,
                                           C2_dif, C5_dif, C8_dif, CC_dif,
                                           spp_miss_C, spp_miss_Q) %>% 
                                    arrange(ScientificName) 

quad_spp_comp2[, 2:25][is.na(quad_spp_comp2[, 2:25])] <- "0%"
quad_spp_comp2[, 26:39][is.na(quad_spp_comp2[, 26:39])] <- 0

quad_sum <- quad_spp1 %>% select(Team, ScientificName, quad_avg_cov, quad_pct_freq)

quad_sum_comp <- full_join(quad_sum %>% filter(Team == "Crew") %>% select(-Team), 
                           quad_sum %>% filter(Team == "QAQC") %>% select(-Team),
                           by = c("ScientificName"),
                           suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') 
names(quad_sum_comp)

quad_sum_comp[,2:5][is.na(quad_sum_comp[,2:5])] <- 0

quad_sum_comp2 <- quad_sum_comp %>%  
                    mutate(pct_diff_cov = pct_diff(quad_avg_cov_C, quad_avg_cov_Q),
                           pct_diff_freq = pct_diff(quad_pct_freq_C, quad_pct_freq_Q)) %>% 
                    select(ScientificName, quad_avg_cov_C, quad_avg_cov_Q, 
                           quad_pct_freq_C, quad_pct_freq_Q, pct_diff_cov, pct_diff_freq) %>% 
                    mutate_if(is.numeric, round, 2) %>% arrange(ScientificName)

# Quadrat taxonomic accuracy +++ NEW 20220321 +++
# Selecting the species that have >=1% cover in at least one of the visits for the comparison.
spp_to_include <- quad_sum_comp2 %>% mutate(cov1pct = ifelse(quad_avg_cov_C >= 1 | quad_avg_cov_Q >= 1, 1, 0)) %>% 
  filter(cov1pct == 1) %>% select(ScientificName)


quad_spp <- rbind(quad_sum_comp2 %>% filter(ScientificName %in% spp_to_include$ScientificName) %>% 
                    mutate(team = "crew") %>% 
                    rename(quad_avg_cov = quad_avg_cov_C) %>% 
                    select(team, ScientificName, quad_avg_cov) %>% unique(),
                  
                  quad_sum_comp2 %>% filter(ScientificName %in% spp_to_include$ScientificName) %>% 
                    mutate(team = 'qaqc') %>% 
                    rename(quad_avg_cov = quad_avg_cov_Q) %>% 
                    select(team, ScientificName, quad_avg_cov) %>% unique())

if(nrow(quad_spp)>0){
  
  quad_spp$quad_avg_cov[is.na(quad_spp$quad_avg_cov)] <- 0
  
  quad_spp_wide <- quad_spp %>% pivot_wider(names_from = ScientificName, values_from = quad_avg_cov, values_fill = 0)
  
  quad_taxa_acc <- round(betadiver(quad_spp_wide[,-1], method = 'sor', order = F), 2)
  
  quad_spp_wide2 <- quad_spp %>% pivot_wider(names_from = team, values_from = quad_avg_cov, values_fill = 0)
  quad_spp_wide2 <- quad_spp_wide2 %>% mutate(missed_c = ifelse(crew == 0 & qaqc > 0, 1, 0),
                                              missed_q = ifelse(crew > 0 & qaqc == 0, 1, 0))
  
  quad_spp_wide2 <- rbind(quad_spp_wide2,
                          c("Sorensen Similarity", NA, quad_taxa_acc, NA, NA))
  }
#----- Quadrat seedlings
seeds <- do.call(joinQuadSeedlings, c(arglist, list(speciesType = 'all', canopyForm = 'all'))) %>% 
  filter_plot() %>% name_team() %>% 
  select(Team, QuadratCode, ScientificName, Seedlings_15_30cm:Seedlings_Above_150cm, tot_seeds, Pct_Cov, Txt_Cov, BrowsedCount)

seeds_comp1 <- full_join(seeds %>% filter(Team == "Crew") %>% select(-Team),
                         seeds %>% filter(Team == "QAQC") %>% select(-Team),
                         by = c("QuadratCode", "ScientificName"),
                         suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
  arrange(QuadratCode, ScientificName) %>% 
  select(QuadratCode, ScientificName, 
         Seedlings_15_30cm_C, Seedlings_15_30cm_Q, Seedlings_30_100cm_C, Seedlings_30_100cm_Q,
         Seedlings_100_150cm_C, Seedlings_100_150cm_Q, Seedlings_Above_150cm_C, Seedlings_Above_150cm_Q, 
         tot_seeds_C, tot_seeds_Q, Pct_Cov_C, Pct_Cov_Q, Txt_Cov_C, Txt_Cov_Q, 
         BrowsedCount_C, BrowsedCount_Q) %>% 
  filter(!ScientificName %in% "None present")

quad_names <- data.frame(QuadratCode = c("A2", "A5", "A8", "AA", "B2", "B5", "B8", "BB", "C2", "C5", "C8", "CC")) 
seeds_comp <- full_join(seeds_comp1, quad_names, by = "QuadratCode", 
                        multiple = 'all', relationship = 'many-to-many') %>% 
  arrange(QuadratCode, ScientificName)
seeds_comp[,c(3:14, 17:18)][is.na(seeds_comp[,c(3:14, 17:18)])] <- 0
seeds_comp[,15:16][is.na(seeds_comp[,15:16])] <- "0%"
seeds_comp$ScientificName[is.na(seeds_comp$ScientificName)] <- "None present"

seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "0%"] <- 0
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "<1%"] <- 1
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "1-2%"] <- 2
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "2-5%"] <- 3
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "5-10%"] <- 4
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "10-25%"] <- 5
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "25-50%"] <- 6
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "50-75%"] <- 7
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "75-95%"] <- 8
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")] == "95-100%"] <- 9
seeds_comp[,c("Pct_Cov_C", "Pct_Cov_Q")][is.na(seeds_comp[,c("Txt_Cov_C", "Txt_Cov_Q")])] <- 0

seeds_comp$pct_dif <- abs(seeds_comp$Pct_Cov_C - seeds_comp$Pct_Cov_Q)

seeds_comp$spp_miss_C <- ifelse(rowSums(
  seeds_comp[, c("tot_seeds_C", "Pct_Cov_C")], 
  na.rm = T) == 0, 1, 0)

seeds_comp$spp_miss_Q <- ifelse(rowSums(
  seeds_comp[, c("tot_seeds_Q", "Pct_Cov_Q")], 
  na.rm = T) == 0, 1, 0)

# Seedling taxonomic accuracy +++ NEW 20220321 +++
# 100% of species ≥ 30cm tall are in agreement. 90% of species < 30 cm tall and >=3 stems are in agreement. as measured by  Sorensen

seeds_spp <- rbind(seeds_comp %>% group_by(ScientificName) %>% 
                     summarize(team = "crew",
                               sds_15 = sum(Seedlings_15_30cm_C), na.rm = T,
                               sds_30 = sum(Seedlings_30_100cm_C + Seedlings_100_150cm_C + 
                                              Seedlings_Above_150cm_C)) %>% 
                     select(team, ScientificName, sds_15, sds_30),
                   seeds_comp %>% group_by(ScientificName) %>% 
                     summarize(team = "qaqc",
                               sds_15 = sum(Seedlings_15_30cm_Q), na.rm = T,
                               sds_30 = sum(Seedlings_30_100cm_Q + Seedlings_100_150cm_Q + 
                                              Seedlings_Above_150cm_Q)) %>% 
                     select(team, ScientificName, sds_15, sds_30)) %>% 
  filter(!ScientificName %in% "None present") 

sd_spp_to_include_15 <- seeds_spp %>% filter(sds_15 >= 3) %>% select(ScientificName) %>% unique()
sd_spp_to_include_30 <- seeds_spp %>% filter(sds_30 > 0) %>% select(ScientificName) %>% unique()

# Make report table and deal with empty dfs

if(nrow(sd_spp_to_include_15) + nrow(sd_spp_to_include_30)>0){
  
  seeds_spp[, c("sds_15", "sds_30")][is.na(seeds_spp[, c("sds_15", "sds_30")])] <- 0
  seeds_spp <- seeds_spp %>% mutate(cnts = sds_15 + sds_30) %>% filter(cnts > 0) %>% select(-cnts)
  
  seeds_wide_15 <- seeds_spp %>% filter(ScientificName %in% sd_spp_to_include_15$ScientificName) %>% 
    select(team, ScientificName, sds_15) %>% 
    pivot_wider(names_from = ScientificName, values_from = sds_15)
  
  seeds_taxa_acc_15 <- ifelse(nrow(seeds_wide_15) > 0, 
                              round(betadiver(seeds_wide_15[,-1], method = 'sor', order = F), 2), 
                              NA_real_)
  seeds_taxa_acc_15 <- ifelse(is.nan(seeds_taxa_acc_15), NA, seeds_taxa_acc_15)
  
  seeds_wide_30 <- seeds_spp %>% filter(ScientificName %in% sd_spp_to_include_30$ScientificName) %>% 
    select(team, ScientificName, sds_30) %>% 
    pivot_wider(names_from = ScientificName, values_from = sds_30)
  
  seeds_taxa_acc_30 <- ifelse(nrow(seeds_wide_30) > 0, 
                              round(betadiver(seeds_wide_30[,-1], method = 'sor', order = F), 2), 
                              NA_real_)
  seeds_taxa_acc_30 <- ifelse(is.nan(seeds_taxa_acc_30), NA, seeds_taxa_acc_30)
  
  seeds_spp_wide2 <- seeds_spp %>% pivot_wider(names_from = team, values_from = c(sds_15, sds_30), values_fill = 0)
  
  
  seeds_spp_wide2 <- seeds_spp_wide2 %>% mutate(missed_c_15 = ifelse(sds_15_crew == 0 & sds_15_qaqc == 1, 1, 0),
                                                missed_q_15 = ifelse(sds_15_crew == 1 & sds_15_qaqc == 0, 1, 0),
                                                missed_c_30 = ifelse(sds_30_crew == 0 & sds_30_qaqc == 1, 1, 0),
                                                missed_q_30 = ifelse(sds_30_crew == 1 & sds_30_qaqc == 0, 1, 0),
  )
  
  seeds_spp_wide2 <- rbind(seeds_spp_wide2,
                           c("Sorensen Similarity", NA, seeds_taxa_acc_15, NA, seeds_taxa_acc_30, NA, NA, NA, NA))
}

#------ Additional species
spp_list <- do.call(sumSpeciesList, c(arglist, list(speciesType = 'all'))) %>% filter_plot() %>% name_team() 

spp_list2 <- spp_list %>% mutate(Trees = ifelse(DBH_mean > 0, 1, 0),
                                 Micros = ifelse(sap_den + shrub_avg_cov > 0, 1, 0),
                                 Quads = ifelse(quad_avg_cov > 0, 1, 0),
                                 AddSpp = ifelse(addspp_present + seed_den > 0 , 1, 0)) %>% 
                          select(Team, ScientificName, Trees, Micros, Quads, AddSpp)

spp_list_comp <- full_join(spp_list2 %>% filter(Team == "Crew") %>% select(-Team), 
                           spp_list2 %>% filter(Team == "QAQC") %>% select(-Team),
                           by = c("ScientificName"),
                           suffix = c("_C", "_Q"), multiple = 'all', relationship = 'many-to-many') %>% 
                 select(ScientificName, Trees_C, Trees_Q, Micros_C, Micros_Q, 
                        Quads_C, Quads_Q, AddSpp_C, AddSpp_Q)

spp_list_comp[,2:9][is.na(spp_list_comp[,2:9])] <- 0

spp_list_comp2 <- spp_list_comp %>% group_by(ScientificName) %>% summarize_if(is.numeric, sum)

#if(ncol(spp_list_comp2) > 1){
spp_list_comp2[,2:ncol(spp_list_comp2)][spp_list_comp2[,2:ncol(spp_list_comp2)] > 1] <- 1

spp_list_comp2$missed_C <- ifelse(rowSums(spp_list_comp2[,c("Trees_C", "Micros_C", "Quads_C", "AddSpp_C")], na.rm = T) == 0, 1, 0)
spp_list_comp2$missed_Q <- ifelse(rowSums(spp_list_comp2[,c("Trees_Q", "Micros_Q", "Quads_Q", "AddSpp_Q")], na.rm = T) == 0, 1, 0)

# Plot Species taxonomic accuracy +++ NEW 20220321 +++
spp_list_comp3 <- spp_list_comp2

spp_list_comp3$crew <- ifelse(rowSums(spp_list_comp3[,c("Trees_C", "Micros_C", "Quads_C", "AddSpp_C")], na.rm = T) > 0, 1, 0)
spp_list_comp3$qaqc <- ifelse(rowSums(spp_list_comp3[,c("Trees_Q", "Micros_Q", "Quads_Q", "AddSpp_Q")], na.rm = T) > 0, 1, 0)

plot_spp <- spp_list_comp3 %>% select(ScientificName, crew, qaqc) %>% 
  pivot_longer(-ScientificName, names_to = "team", values_to = "present")

#if(nrow(plot_spp)>0){
  
  plot_spp$present[is.na(plot_spp$present)] <- 0
  
  plot_spp_wide <- plot_spp %>% pivot_wider(names_from = ScientificName, 
                                            values_from = present, values_fill = 0)
  
  plot_spp_taxa_acc <- round(betadiver(plot_spp_wide[,-1], method = 'sor', order = F), 2)
  
  plot_spp_wide2 <- plot_spp %>% pivot_wider(names_from = team, values_from = present, values_fill = 0)
  
  plot_spp_wide2 <- plot_spp_wide2 %>% mutate(missed_c = ifelse(crew == 0 & qaqc == 1, 1, 0),
                                              missed_q = ifelse(crew == 1 & qaqc == 0, 1, 0))
  
  plot_spp_wide2 <- rbind(plot_spp_wide2,
                          c("Sorensen Similarity", NA, plot_spp_taxa_acc, NA, NA))
#}
#}

#----- CWD
cwd_raw <- VIEWS_MIDN_NCBN$CWD_MIDN_NCBN %>% select(Plot_Name, ParkUnit, PlotCode, SampleYear, IsQAQC, SQTransectCode, TransectCode, 
                                          ScientificName, Distance, Diameter, Length, DecayClassCode,
                                          MultiCrossCode, IsHollow, CWDNote)

cwd_raw <- cwd_raw %>% filter_plot() %>% name_team() %>% 
  rename(Transect = TransectCode, SQ = SQTransectCode, Species = ScientificName, Decay = DecayClassCode, 
         MultiCross = MultiCrossCode, Hollow = IsHollow)

cwd_raw2 <- cwd_raw %>% select(Team, SQ, Transect, Distance, Species, Diameter, 
                               Length, Decay, MultiCross, Hollow, CWDNote) %>% 
                        arrange(Transect, Distance, Team, Diameter) %>% 
                        mutate(Species = ifelse(SQ == "NP", "None present", Species))

cwd_sum1 <- cwd_raw2 %>% 
  group_by(Team, Species) %>% 
  summarize(num_pieces = length(!is.na(Diameter)), .groups = 'drop') %>% 
  filter(Species != "None present") %>% 
  mutate(Species = as.character(Species),
         Team = as.character(Team))

cwd_sum <- full_join(cwd_sum1 %>% filter(Team == "Crew") %>% select(-Team),
                     cwd_sum1 %>% filter(Team == "QAQC") %>% select(-Team),
                     by = "Species", suffix = c("_C", "_Q"), 
                     multiple = 'all', relationship = 'many-to-many')

cwd_vol <- do.call(joinCWDData, arglist[1:5]) %>% filter_plot() %>% name_team() %>% 
  select(Team, ScientificName, DecayClassCode, CWD_Vol) %>% 
  rename(Species = ScientificName, Decay = DecayClassCode)

cwd_comp <- full_join(cwd_vol %>% filter(Team == "Crew") %>% select(-Team),
                      cwd_vol %>% filter(Team == "QAQC") %>% select(-Team),
                      by = c("Species"),#, "Decay"),
                      suffix = c("_C", "_Q"), multiple = 'all') %>% filter(Species != "None present") %>% 
            arrange(Species)#, Decay)

cwd_join <- full_join(cwd_comp, cwd_sum, by = "Species", multiple = 'all', relationship = 'many-to-many')

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
             mutate_if(is.numeric, ~round(., 2)) %>% 
             mutate(vol_dif = pct_diff(CWD_Vol_C, CWD_Vol_Q)) %>% 
             select(Species, Decay_C, Decay_Q, CWD_Vol_C, CWD_Vol_Q, num_pieces_C, num_pieces_Q, vol_dif)

