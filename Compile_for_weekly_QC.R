#--------------------------------
# Compiling data for QAQC Reports
#--------------------------------
#----- Load libraries
library(forestNETN)
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)

source("Weekly_QC_functions.R")

#----- Compile data -----
arglist1 = list(to = curr_year, QAQC = TRUE, eventType = 'complete', locType = 'all')

plotevs <- do.call(joinLocEvent, arglist1) 

old_evs <- plotevs %>% filter(StartDate < params$week_start)
new_evs <- plotevs %>% filter_week() %>% name_plot()
new_evs_list <- unique(new_evs$Plot_Name)

park_ev_list <- sort(unique(new_evs$ParkUnit))
arglist = list(park = park_ev_list, to = curr_year, QAQC = TRUE, eventType = 'complete', locType = 'all')

#----- Check for complete visit info -----
comp_visits <- new_evs %>% select(Plot_Name, PanelCode, xCoordinate, yCoordinate, ZoneCode, PhysiographyCode,
                                  Aspect, Orientation, IsOrientationChanged, IsStuntedWoodland) %>% 
                           filter(!complete.cases(.))

QC_table <- QC_check(comp_visits, "Plot & Visit Data", "Missing plot and event data")

plot_na_table <- make_kable(comp_visits, "Plot visits missing at least one record.")

missing_directions <- new_evs %>% filter(Directions %in% "NO DIRECTIONS ENTERED YET") %>% 
                                  select(Plot_Name, Directions)
QC_table <- rbind(QC_table, 
                  QC_check(missing_directions, "Plot & Visit Data", "Plots with missing directions"))

miss_dir_table <- make_kable(missing_directions, "Plots with missing directions.")

#----- + Summarize plot and visit checks + -----
plot_check <- QC_table %>% filter(Data %in% "Plot & Visit Data" & Num_Records > 0) 
include_plot_tab <- tab_include(plot_check)

#----- Visit Notes -----
visit_notes <- joinVisitNotes(from = curr_year, to = curr_year) %>% 
               filter_week() %>% name_plot() %>% 
               select(Plot_Name, Note_Type, Sample_Info, Notes) %>% 
               arrange(Plot_Name, Note_Type, Sample_Info, Notes)

RC_visit_notes <- sapply(1:nrow(visit_notes), function(x){
  check = ifelse(visit_notes$Plot_Name[x] != visit_notes$Plot_Name[x + 1], x, NA)
}) %>% data.frame() %>% na.omit(.)

RC_visit_note_type <- sapply(1:nrow(visit_notes), function(x){
  check = ifelse(visit_notes$Note_Type[x] != visit_notes$Note_Type[x + 1], x, NA)
}) %>% data.frame() %>% na.omit(.) 
                    
visit_table <- kable(visit_notes, format = 'html', align = c('c', 'c', 'c', 'l'),
                     col.names = c("Plot", "Note Type", "Info", "Note")) %>% 
               kable_styling(fixed_thead = TRUE, bootstrap_options = 'condensed', full_width = TRUE,
                 position = 'left', font_size = 11) %>% 
               column_spec(1:3, width = "15%") %>% 
               collapse_rows(1, valign = 'top') %>% 
               row_spec(RC_visit_notes[,1], extra_css = "border-bottom: 1px solid #000000;") %>% 
               row_spec(RC_visit_note_type[,1], extra_css = 'border-bottom: 1px solid #000000') %>% 
               row_spec(c(0, nrow(visit_notes)), extra_css = 'border-bottom: 1px solid #000000')

include_visit_notes <- tab_include(visit_notes) #++++++ UPDATE AFTER FIGURE OUT TAB CONTROL APPROACH ++++++

#----- Stand Data -----
stand <- do.call(joinStandData, arglist) %>% name_plot() %>% select(-c(Network:PlotTypeCode))
stand_old <- filter_old(stand) 
stand_new <- filter_week(stand)

# Check for PMs in stand data
stand_pm <- PM_check(stand_new)# %>% select_if(is.character) %>% select(-StartDate)

PM_stand_col <- sapply(names(stand_pm), function(x) any(stand_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_stand_col[c(1,8)] <- TRUE # For Plot_Name

stand_pm2 <- data.frame(stand_pm[, PM_stand_col])

QC_table <- rbind(QC_table, 
                  QC_check(stand_pm2, "Stand Data", "Permanently Missing records in stand data"))

stand_pm_table <- make_kable(stand_pm2, "Permanently Missing records in stand data")

# Check for DBI = 1 not in deer exclosure
dbi1 <- stand_new %>% filter(Deer_Browse_Index == 1)
QC_table <- rbind(QC_table, QC_check(dbi1, "Stand Data", "DBI = 1 outside of exclosure"))

DBI_1_table <- make_kable(dbi1, "DBI = 1 outside exclosure") 

# Check plots with fluctuating stand structure
stand_str <- stand %>% filter(IsQAQC == 0) %>% 
                       filter(Plot_Name %in% new_evs_list) %>% 
                       select(Plot_Name, cycle, Stand_Structure) %>% 
                       pivot_wider(names_from = cycle,
                                   values_from = Stand_Structure,
                                   names_prefix = "cycle_") %>% 
                       filter(cycle_3 != cycle_4) # Update after start cycle 5

QC_table <- rbind(QC_table, QC_check(stand_str, "Stand Data", "Stand structure in cycle 4 != cycle 3"))

stand_str_table <- make_kable(stand_str, "Fluctuating stand structures")

# Check plots with fluctuating microtopography
microtop <- stand %>% filter(IsQAQC == 0) %>% 
                      filter(Plot_Name %in% new_evs_list) %>% 
                      select(Plot_Name, cycle, Microtopography) %>% 
                      pivot_wider(names_from = cycle,
                                  values_from = Microtopography,
                                  names_prefix = "cycle_") %>% 
                      filter(cycle_3 != cycle_4) # Update after start cycle 5

QC_table <- rbind(QC_table, QC_check(microtop, "Stand Data", "Microtopography in cycle 4 != cycle 3"))

microtop_table <- make_kable(microtop, "Fluctuating microtopography")

# Disturbances
stand_dist <- do.call(joinStandDisturbance, c(arglist, list(from = curr_year))) %>% 
              filter_week() %>% filter(DisturbanceCode > 0)

QC_table <- rbind(QC_table, QC_check(stand_dist, "Stand Data", "Reported stand disturbances"))

stand_dist_table <- make_kable(stand_dist, "Recorded disturbances")

#----- + Summarize stand checks + -----
stand_check <- QC_table %>% filter(Data %in% "Stand Data" & Num_Records > 0) 
include_stand_tab <- tab_include(stand_check)

#----- Tree checks -----
tree_data <- do.call(joinTreeData, c(arglist, list(speciesType = 'all'))) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(cycle %in% c(params$cycle_latest, params$cycle_latest-1))

tree_data_old <- tree_data %>% filter(StartYear < curr_year)
tree_data_new <- tree_data %>% filter(StartDate >= week_start)

# Check for PMs in tree data
tree_data_pm <- PM_check(tree_data_new)# %>% select_if(is.character) %>% select(-StartDate)

PM_tree_data_col <- sapply(names(tree_data_pm), function(x) any(tree_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_tree_data_col[c(1)] <- TRUE # For Plot_Name

tree_data_pm2 <- data.frame(tree_data_pm[, PM_tree_data_col])

QC_table <- rbind(QC_table, 
                  QC_check(tree_data_pm2, "Tree Data", "Permanently Missing records in tree data"))

tree_data_pm_table <- make_kable(tree_data_pm2, "Permanently Missing records in tree data")

# Check for zombie trees
alive <- c("1","AB","AF","AL","AS")
recr <- c("RB","RF","RL","RS") 
dead <- c("2","DB","DC","DL","DS")
exc <- c("0","ES","EX","XO","XP")
missed <- c("AM", "DM")

status_check1 <- tree_data %>% filter(IsQAQC == 0) %>% 
                               select(Plot_Name, TagCode, cycle, TreeStatusCode) %>% 
                               mutate(status_simp = case_when(TreeStatusCode %in% alive ~ "alive",
                                                              TreeStatusCode %in% dead ~ "dead", 
                                                              TreeStatusCode %in% recr ~ "alive_recruit",
                                                              TreeStatusCode %in% missed ~ "missed",
                                                              TreeStatusCode %in% exc ~ "exclude",
                                                              TreeStatusCode %in% "NL" ~ "not_located",
                                                              TreeStatusCode %in% "XS" ~ "exclude_shrank",
                                                              TreeStatusCode %in% "DF" ~ "dead fallen",
                                                              TRUE ~ NA_character_)) %>%
                              select(-TreeStatusCode) %>% 
                              pivot_wider(names_from = cycle,
                                          values_from = status_simp,
                                          names_prefix = "cycle_")

status_check <- status_check1 %>% mutate(check = case_when(cycle_latest == "alive" & cycle_prev == "dead" ~ "zombie",
                                                           cycle_latest == "missed" ~ "missed",
                                                           cycle_latest == "exclude" ~ "excluded",
                                                           cycle_latest == "not_located" ~ "not_located",
                                                           TRUE ~ "correct")) %>% 
                                  filter(!check %in% "correct")

QC_table <- rbind(QC_table, 
                  QC_check(status_check, "Tree Data", "Zombie and excluded trees"))

status_table <- make_kable(status_check, "Zombie and excluded trees")

# Trees with major crown class changes
tree_data_live <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = "live"))) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(cycle %in% c(params$cycle_latest, params$cycle_latest-1))

crown_check1 <- tree_data_live %>% filter(IsQAQC == 0) %>% 
                                   select(Plot_Name, TagCode, cycle, CrownClassCode) %>% 
                                   pivot_wider(names_from = cycle,
                                   values_from = CrownClassCode,
                                   names_prefix = "cycle_") %>% 
                                   filter(!is.na(noquote(cycle_prev)))  
                                   
crown_check1$crown_change <- abs(crown_check1[,cycle_latest] - crown_check1[,cycle_prev])
crown_check <- crown_check1 %>% filter(crown_change > 1) %>% select(-crown_change)

QC_table <- rbind(QC_table, 
                  QC_check(crown_check, "Tree Data", "Major crown class changes"))

crown_table <- make_kable(crown_check, "Major crown class changes")

# Check that trees with > 3cm growth or <-0.1 cm growth have DBH verified selected
tree_dbh <- tree_data_live %>% select(Plot_Name, TagCode, cycle, ScientificName, DBHcm, IsDBHVerified) %>% 
                               pivot_wider(names_from = cycle,
                                           values_from = c(DBHcm, IsDBHVerified),
                                           names_prefix = "cycle_") 

old_dbh <- paste0("DBHcm_cycle_", params$cycle_latest-1)
new_dbh <- paste0("DBHcm_cycle_", params$cycle_latest)

tree_dbh$DBH_diff <- tree_dbh[,new_dbh] - tree_dbh[,old_dbh]

tree_dbh$Missing_DBHVer = ifelse((tree_dbh$DBH_diff >= 3 | tree_dbh$DBH_diff < -0.1) & 
                                 tree_dbh[, paste0("IsDBHVerified_cycle_", params$cycle_latest)] == 0, 1, 0)

tree_dbh_check <- tree_dbh %>% filter(Missing_DBHVer == 1) 
tree_dbh_check <- tree_dbh_check[, c("Plot_Name", "TagCode", "ScientificName", 
                                     old_dbh, new_dbh, "DBH_diff", 
                                     paste0("IsDBHVerified_cycle_", params$cycle_latest))]

QC_table <- rbind(QC_table, 
                  QC_check(tree_dbh_check, "Tree Data", "DBH zoinks missing DBH Verified check"))

tree_dbh_table <- make_kable(tree_dbh_check, "DBH zoinks missing DBH Verified check")

# Check for major foliage outbreak
fol_new <- joinTreeFoliageCond(from = curr_year, to = curr_year, QAQC = FALSE, locType = 'all') %>% 
           name_plot() %>% 
           filter(Plot_Name %in% new_evs_list) 

fol_major <- fol_new %>% filter(Pct_Tot_Foliage_Cond >= 70) %>% 
             select(Plot_Name, TagCode) %>% 
             unique() %>% 
             group_by(Plot_Name) %>%
             summarize(num_trees_50pct = n()) %>% 
             filter(num_trees_50pct > 1)

QC_table <- rbind(QC_table, 
                  QC_check(fol_major, "Tree Data", "Plots with potential foliage outbreak (2+ trees with TotFol > 50%)"))

fol_maj_table <- make_kable(fol_major, "Plots with potential foliage outbreak (2+ trees with TotFol > 50%)")

# Check tree conditions 
tree_cond <- joinTreeConditions(to = curr_year, QAQC = FALSE, locType = 'all') %>% 
             name_plot() %>% 
             filter(Plot_Name %in% new_evs_list) 

# Check for conditions applied to wrong tree species
trcond_spp_check <- tree_cond %>% filter((!str_detect(ScientificName, "Fraxinus") & EAB == 1)|
                                        (!str_detect(ScientificName, "Picea") & SB == 1)|
                                        (!str_detect(ScientificName, "Quercus") & SOD == 1)|
                                        (!(ScientificName %in% "Abies balsamea") & BWA == 1)|      
                                        (!(ScientificName %in% "Cornus florida") & DOG == 1)|   
                                        (!(ScientificName %in% "Juglans cinerea") & BC == 1)| 
                                        (!(ScientificName %in% "Fagus grandifolia") & BBD == 1)|
                                        (!(ScientificName %in% "Pinus resinosa") & RPS == 1)|   
                                        (!(ScientificName %in% "Tsuga canadensis") & HWA == 1)|
                                        (!(ScientificName %in% "Tsuga canadensis") & EHS == 1))

QC_table <- rbind(QC_table, 
                  QC_check(trcond_spp_check, "Tree Data", "Trees with conditions applied to wrong species."))

trcond_spp_table <- make_kable(trcond_spp_check, "Trees with conditions applied to wrong species.")

# Check for priority pest detections
pest_check <- tree_cond %>% filter(across(c(ALB, BC, BBD, BWA, DOG, EAB, EHS, GM, HWA, RPS, SB, SOD, SPB, SW), ~.x == 1))

QC_table <- rbind(QC_table,
                  QC_check(pest_check, "Tree Data", "Priority forest pest/pathogen detections."))

pest_table <- make_kable(pest_check, "Priority forest pest/pathogen detections.")

#----- + Summarize tree data checks + -----
tree_check <- QC_table %>% filter(Data %in% "Tree Data" & Num_Records > 0) 
include_tree_tab <- tab_include(tree_check)

#----- Microplot Saplings -----
# Check sample qualifiers
shrubs_sq <- get("COMN_MicroplotShrubs", env = VIEWS_NETN) %>% 
             mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
             name_plot() %>% 
             filter(Plot_Name %in% new_evs_list) %>% 
             filter(StartYear %in% curr_year) %>% 
             select(Plot_Name, MicroplotCode, SQShrubCode, SQShrubNotes) %>% 
             rename(SQ = SQShrubCode, SQNotes = SQShrubNotes) %>% 
             mutate(type = "shrub")

saps_sq <- get("NETN_MicroplotSaplings", env = VIEWS_NETN) %>% 
           mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
           name_plot() %>% 
           filter(Plot_Name %in% new_evs_list) %>% 
           filter(StartYear %in% curr_year) %>% 
           select(Plot_Name, MicroplotCode, SQSaplingCode, SQSaplingNotes) %>% 
           rename(SQ = SQSaplingCode, SQNotes = SQSaplingNotes) %>% 
           mutate(type = "sapling")

seeds_sq <- get("NETN_MicroplotSeedlings", env = VIEWS_NETN) %>% 
            mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
            name_plot() %>% 
            filter(Plot_Name %in% new_evs_list) %>% 
            filter(StartYear %in% curr_year) %>% 
            select(Plot_Name, MicroplotCode, SQSeedlingCode, SQSeedlingNotes) %>% 
            rename(SQ = SQSeedlingCode, SQNotes = SQSeedlingNotes) %>% 
            mutate(type = "seedling")

micro_sq <- rbind(shrubs_sq, saps_sq, seeds_sq) 

# Check that each microplot and data type has 3 sample qualifiers
micro_check3 <- micro_sq %>% select(-SQNotes) %>% unique() %>% 
                             group_by(Plot_Name, type) %>% 
                             summarize(num_SQ = length(MicroplotCode),
                             .groups = "drop") %>% 
                             filter(num_SQ < 3)

QC_table <- rbind(QC_table, 
                  QC_check(micro_check3, "Microplot", "Microplots missing a sample qualifier"))

micro_check3_table <- make_kable(micro_check3, "Microplots missing a sample qualifier")

# Check for NS SQ
micro_ns <- micro_sq %>% filter(SQ %in% "NS")

QC_table <- rbind(QC_table,
                  QC_check(micro_ns, "Microplot", "Microplots with NS sample qualifier"))

micro_ns_table <- make_kable(micro_ns, "Microplots with NS sample qualifier")

# Check for PMs in sapling data
saps <- do.call(joinMicroSaplings, arglist) %>% 
        name_plot() %>% 
        filter(Plot_Name %in% new_evs_list) %>% 
        filter(cycle %in% params$cycle_latest)

sap_data_pm <- PM_check(saps)

PM_sap_col <- sapply(names(sap_data_pm), function(x) any(sap_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_sap_col[c(1)] <- TRUE # For Plot_Name

sap_data_pm2 <- data.frame(sap_data_pm[, PM_sap_col])

QC_table <- rbind(QC_table, 
                  QC_check(sap_data_pm2, "Microplot", "Saplings: Permanently Missing records"))

sap_pm_table <- make_kable(sap_data_pm2, "Saplings: Permanently Missing records")

# Check for PMs in shrub data
shrubs <- do.call(joinMicroShrubData, arglist) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(cycle %in% params$cycle_latest)

shrub_data_pm <- PM_check(shrubs)# %>% select_if(is.character) %>% select(-StartDate)

PM_shrub_col <- sapply(names(shrub_data_pm), function(x) any(shrub_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_shrub_col[c(1)] <- TRUE # For Plot_Name

shrub_data_pm2 <- data.frame(shrub_data_pm[, PM_shrub_col])

QC_table <- rbind(QC_table, 
                  QC_check(shrub_data_pm2, "Microplot", "Shrubs: Permanently Missing records"))

shrub_pm_table <- make_kable(shrub_data_pm2, "Shrubs: Permanently Missing records")

# Check for shrub species entered with no cover
shrubs_0cov <- shrubs %>% filter((!ScientificName %in% c("Not Sampled", "None present")) & 
                                  (Pct_Cov_UR == 0 & Pct_Cov_UL == 0 & Pct_Cov_B == 0) |
                                   is.na(Pct_Cov_UR) | is.na(Pct_Cov_UL) | is.na(Pct_Cov_B)) %>% 
               select(Plot_Name, ScientificName, Pct_Cov_UR:Pct_Cov_B,
                      shrub_avg_cov)
  
QC_table <- rbind(QC_table,
                  QC_check(shrubs_0cov, "Microplot", "Shrubs: species recorded with 0 or missing cover"))

shrub_0cov_table <- make_kable(shrubs_0cov, "Shrubs: species recorded with 0 or missing cover")

# Check for PMs in seedling data
seeds <- do.call(joinMicroSeedlings, arglist) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(cycle %in% params$cycle_latest)

seed_data_pm <- PM_check(seeds)# %>% select_if(is.character) %>% select(-StartDate)

PM_seed_col <- sapply(names(seed_data_pm), function(x) any(seed_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_seed_col[c(1)] <- TRUE # For Plot_Name

seed_data_pm2 <- data.frame(seed_data_pm[, PM_seed_col])

QC_table <- rbind(QC_table, 
                  QC_check(seed_data_pm2, "Microplot", "Seedlings: Permanently Missing records"))

seed_pm_table <- make_kable(seed_data_pm2, "Seedlings: Permanently Missing records")

# Check for plots with species recorded but 0 tallies
seeds_0tally <- seeds %>% filter((!ScientificName %in% c("Not Sampled", "None present")) & 
                                    (tot_seeds == 0 | 
                                    is.na(sd_15_30cm)|
                                    is.na(sd_30_100cm)|
                                    is.na(sd_100_150cm)|
                                    is.na(sd_p150cm))) %>% 
                select(Plot_Name, MicroplotCode, ScientificName, sd_15_30cm:tot_seeds)

QC_table <- rbind(QC_table,
                  QC_check(seeds_0tally, "Microplot", "Seedlings: species recorded with 0 missing tally"))

seed_0tally_table <- make_kable(seeds_0tally, "Seedlings: species recorded with 0 or missing tally")

#----- + Summarize microplot checks + -----
micro_check <- QC_table %>% filter(Data %in% "Microplot" & Num_Records > 0) 
include_micro_tab <- tab_include(micro_check)

#----- Quadrats ------
# Check for NS SQs and make sure there are notes for each NS
quad_notes <- do.call(joinQuadNotes, arglist) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(cycle %in% params$cycle_latest)

#check for NS SQs
quad_ns <- quad_notes %>% filter(Sample_Info == "NS") %>% select(Plot_Name, Note_Type, Sample_Info, Note_Info, Notes)

QC_table <- rbind(QC_table, 
                  QC_check(quad_ns, "Quadrat", "Quadrats with Not Sampled sample qualifier"))

quad_ns_table <- make_kable(quad_ns, "Quadrats with Not Sampled sample qualifier")

#check for NS SQs missing notes
quad_ns_missing_notes <- quad_ns %>% filter(is.na(Notes)) 

QC_table <- rbind(QC_table, 
                  QC_check(quad_ns_missing_notes, "Quadrat", "Quadrats with Not Sampled sample qualifier missing notes"))

quad_ns_missing_notes_table <- make_kable(quad_ns_missing_notes, "Quadrats with Not Sampled sample qualifier missing notes")

# Quadrat tab-specific checks
quad_data <- do.call(joinQuadData, arglist) %>% 
             name_plot() %>% 
             filter(Plot_Name %in% new_evs_list) %>% 
             filter(cycle %in% params$cycle_latest)

quad_spp <- do.call(joinQuadSpecies, arglist) %>% 
            name_plot() %>% 
            filter(Plot_Name %in% new_evs_list) %>% 
            filter(cycle %in% params$cycle_latest)

# Check for PMs in quadrat data
quad_data_pm <- PM_check(quad_data)

PM_quad_data_col <- sapply(names(quad_data_pm), function(x) any(quad_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_quad_data_col[c(1)] <- TRUE # For Plot_Name

quad_data_pm2 <- data.frame(quad_data_pm[, PM_quad_data_col])

QC_table <- rbind(QC_table, 
                  QC_check(quad_data_pm2, "Quadrat", "Quadrat Data: Permanently Missing records"))

quad_data_pm_table <- make_kable(quad_data_pm2, "Quadrat Data: Permanently Missing records")

# Check for PMs in quadrat species
quad_spp_pm <- PM_check(quad_spp)

PM_quad_spp_col <- sapply(names(quad_spp_pm), function(x) any(quad_spp_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_quad_spp_col[c(1)] <- TRUE # For Plot_Name

quad_spp_pm2 <- data.frame(quad_spp_pm[, PM_quad_spp_col])

QC_table <- rbind(QC_table, 
                  QC_check(quad_spp_pm2, "Quadrat", "Quadrat Species: Permanently Missing records"))

quad_spp_pm_table <- make_kable(quad_spp_pm2, "Quadrat Species: Permanently Missing records")

# Check for quadrat species recorded with 0 cover
quad_0cov <- quad_spp %>% filter((!ScientificName %in% c("Not Sampled", "None present")) & 
                                   quad_avg_cov == 0 | is.na(quad_avg_cov)) %>% 
             select(Plot_Name, ScientificName, IsGerminant, Txt_Cov_UC:Txt_Cov_UL, quad_avg_cov)

QC_table <- rbind(QC_table,
                  QC_check(quad_0cov, "Quadrat", "Quadrat Species: species recorded with 0 or missing cover"))

quad_0cov_table <- make_kable(quad_0cov, "Quadrat Species: species recorded with 0 or missing cover")

#----- + Summarize quadrat checks + -----
quad_check <- QC_table %>% filter(Data %in% "Quadrat" & Num_Records > 0) 
include_quad_tab <- tab_include(quad_check)

#----- Additional Species -----
# Check for NS SQ
addspp <- do.call(joinAdditionalSpecies, arglist) %>% 
          name_plot() %>% 
          filter(Plot_Name %in% new_evs_list) %>% 
          filter(cycle %in% params$cycle_latest)

# check for NS
addspp_ns <- NS_check(addspp) %>% select(Plot_Name, SQAddSppCode, SQAddSppNotes)

QC_table <- rbind(QC_table, 
                  QC_check(addspp_ns, "Additional Species", "Additional Species tab with Not Sampled sample qualifier"))

addspp_ns_table <- make_kable(addspp_ns, "Additional Species tab with Not Sampled sample qualifier")

# check for NS SQs missing notes
addspp_ns_missing_notes <- addspp_ns %>% filter(is.na(SQAddSppNotes)) 

QC_table <- rbind(QC_table, 
                  QC_check(addspp_ns_missing_notes, "Additional Species", "Additional Species tab with Not Sampled sample qualifier missing notes"))

addspp_ns_missing_notes_table <- make_kable(addspp_ns_missing_notes, "Additional Species tab with Not Sampled sample qualifier missing notes")

# check for null SQ
addspp_na_sq <- addspp %>% filter(is.na(SQAddSppCode)) %>% select(Plot_Name, SQAddSppCode, SQAddSppNotes)

QC_table <- rbind(QC_table, 
                  QC_check(addspp_na_sq, "Additional Species", "Additional Species tab missing a sample qualifier"))

addspp_na_sq_table <- make_kable(addspp_na_sq, "Additional Species tab missing a sample qualifier")

#----- + Summarize Additional Species checks + -----
addspp_check <- QC_table %>% filter(Data %in% "Additional Species" & Num_Records > 0) 
include_addspp_tab <- tab_include(addspp_check)

#----- CWD -----
# Check for NS SQ
cwd <- do.call(joinCWDData, arglist) %>% 
       name_plot() %>% 
       filter(Plot_Name %in% new_evs_list) %>% 
       filter(cycle %in% params$cycle_latest)

cwd_vw <- get("COMN_CWD", env = VIEWS_NETN) %>% 
          mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
          name_plot() %>% 
          filter(Plot_Name %in% new_evs_list) %>% 
          filter(StartYear %in% curr_year) 

cwd_sq <- cwd_vw %>% select(Plot_Name, TransectCode, SQTransectCode, SQTransectNotes) %>% 
                     rename(SQ = SQTransectCode, SQNotes = SQTransectNotes) 

# check for NS
cwd_ns <- NS_check(cwd_sq) 

QC_table <- rbind(QC_table, 
                  QC_check(cwd_ns, "CWD", "CWD transect with Not Sampled sample qualifier"))

cwd_ns_table <- make_kable(cwd_ns, "CWD transect with Not Sampled sample qualifier")

# check for NS SQs missing notes
cwd_ns_missing_notes <- cwd_ns %>% filter(is.na(SQNotes)) 

QC_table <- rbind(QC_table, 
                  QC_check(cwd_ns_missing_notes, "CWD", "CWD transect with Not Sampled sample qualifier missing notes"))

cwd_ns_missing_notes_table <- make_kable(cwd_ns_missing_notes, "CWD transect with Not Sampled sample qualifier missing notes")

# Check for PM or NA data
cwd_pm <- PM_check(cwd_vw)

PM_cwd_col <- sapply(names(cwd_pm), function(x) any(cwd_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_cwd_col[c(1)] <- TRUE # For Plot_Name

cwd_pm2 <- data.frame(cwd_pm[, PM_cwd_col])

QC_table <- rbind(QC_table, 
                  QC_check(cwd_pm2, "CWD", "CWD with Permanently Missing records"))

cwd_pm_table <- make_kable(cwd_pm2, "CWD with Permanently Missing records")

# check for < 3 SQs per plot
cwd_check3 <- cwd_sq %>% filter(!is.na(SQ)) %>%
                         select(-SQNotes) %>% 
                         group_by(Plot_Name) %>% 
                         unique() %>% 
                         summarize(num_transects = length(TransectCode),
                                   .groups = "drop") %>% 
                         filter(num_transects < 3)

QC_table <- rbind(QC_table, 
                  QC_check(cwd_check3, "CWD", "Plots missing at least one transect sample qualifier"))

cwd_check3_table <- make_kable(cwd_check3, "Plots missing at least one transect sample qualifier")

#----- + Summarize CWD checks + -----
cwd_check <- QC_table %>% filter(Data %in% "CWD" & Num_Records > 0) 
include_cwd_tab <- tab_include(cwd_check)


#----- Soil-----
# Check for NS SQ
soil_vw <- get("COMN_SoilSample", env = VIEWS_NETN) %>% 
  mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(StartYear %in% curr_year) 


soil_sq <- soil_vw %>% select(Plot_Name, SQSoilCode, SampleSequenceCode, SoilLayerLabel, Depth_cm, Note) %>% 
                       rename(SQ = SQSoilCode, Sample = SampleSequenceCode) 

# check for NS
soil_ns <- NS_check(soil_sq) 

QC_table <- rbind(QC_table, 
                  QC_check(soil_ns, "Soil", "Soil sample with Not Sampled sample qualifier"))

soil_ns_table <- make_kable(soil_ns, "Soil sample with Not Sampled sample qualifier")

# check for NS SQs missing notes
soil_ns_missing_notes <- soil_ns %>% filter(is.na(Note)) 

QC_table <- rbind(QC_table, 
                  QC_check(soil_ns_missing_notes, "Soil", "Soil sample with Not Sampled sample qualifier missing notes"))

soil_ns_missing_notes_table <- make_kable(soil_ns_missing_notes, "Soil sample with Not Sampled sample qualifier missing notes")

# Check for PM or NA data
soil_pm <- PM_check(soil_vw)

PM_soil_col <- sapply(names(soil_pm), function(x) any(soil_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()

PM_soil_col[c(1)] <- TRUE # For Plot_Name

soil_pm2 <- data.frame(soil_pm[, PM_soil_col])

QC_table <- rbind(QC_table, 
                  QC_check(soil_pm2, "Soil", "Soil with Permanently Missing records"))

soil_pm_table <- make_kable(soil_pm2, "Soil with Permanently Missing records")

# check for < 3 SQs per plot
soil_check3 <- soil_sq %>% filter(!is.na(SQ)) %>%
                           select(-Note) %>% 
                           group_by(Plot_Name) %>% 
                           unique() %>% 
                           summarize(num_samples = length(Sample),
                           .groups = "drop") %>% 
                           filter(num_samples < 3)
soil_check3

QC_table <- rbind(QC_table, 
                  QC_check(soil_check3, "Soil", "Plots missing at least one soil sample qualifier"))

soil_check3_table <- make_kable(soil_check3, "Plots missing at least one soil sample qualifier")

#----- + Summarize CWD checks + -----
soil_check <- QC_table %>% filter(Data %in% "Soil" & Num_Records > 0) 
include_soil_tab <- tab_include(soil_check)


#----- Plant ID checks-----
unknowns <- c(-9999999901:-9999999944, -9999999950:-9999999960)

spplist <- do.call(sumSpeciesList, arglist) %>% 
           filter(IsQAQC == 0) %>% 
           name_plot() %>% 
           filter(!TSN %in% unknowns) %>% 
           select(Plot_Name, ParkUnit, StartYear, cycle, TSN, ScientificName, BA_cm2:addspp_present)

# check species new to a plot
spplist_new <- spplist %>% filter(cycle %in% params$cycle_latest) %>% 
                           filter(Plot_Name %in% new_evs_list) %>% mutate(pres = 1)

spplist_old <- spplist %>% filter(cycle < params$cycle_latest) %>%  
                           filter(Plot_Name %in% new_evs_list) %>% mutate(pres = 1) %>% 
                           select(Plot_Name, TSN, ScientificName,
                                  pres)

spp_plotcheck <- full_join(spplist_new, spplist_old, 
                           by = c("Plot_Name", "TSN", "ScientificName"),
                           suffix = c("_new", "_old")) 

spp_plotcheck[, 7:19][is.na(spp_plotcheck[, 7:19])] <- 0

spp_newplot <- spp_plotcheck %>% filter(pres_new == 1 & pres_old == 0) %>% 
               select(-StartYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
                      -quad_pct_freq, -pres_new, -pres_old) %>% arrange(Plot_Name, ScientificName)

QC_table <- rbind(QC_table, 
                  QC_check(spp_newplot, "Plant ID", "Species new to a plot"))

spp_newplot_table <- make_kable(spp_newplot, "Species new to a plot")

# check species new to a park
park_spplist <- spplist %>% filter(cycle < params$cycle_latest) %>% 
                            select(ParkUnit, TSN, ScientificName) %>% unique() %>% 
                            arrange(ParkUnit, ScientificName) %>% 
                            mutate(pres = 1)

spp_parkcheck <- full_join(spplist_new, park_spplist, 
                           by = c("ParkUnit", "TSN", "ScientificName"),
                           suffix = c("_new", "_old")) 

spp_parkcheck[, 7:19][is.na(spp_parkcheck[, 7:19])] <- 0

spp_newpark <- spp_parkcheck %>% filter(pres_new == 1 & pres_old == 0) %>% 
                                 select(-StartYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
                                 -quad_pct_freq, -pres_new, -pres_old) %>% arrange(Plot_Name, ScientificName)

QC_table <- rbind(QC_table, 
                  QC_check(spp_newpark, "Plant ID", "Species new to a park"))

spp_newpark_table <- make_kable(spp_newpark, "Species new to a park")

# check possible miss-IDed species
spp_checks <- c('Acer saccharinum', 'Acer nigrum', 'Persicaria posumbu', 'Lonicera', 'Euonymus americanus',
                'Kalmia latifolia', 'Picea abies')

sppID_check <- spplist_new %>% filter(ScientificName %in% spp_checks)

QC_table <- rbind(QC_table, 
                  QC_check(sppID_check, "Plant ID", "Potentially incorrect species entries"))

sppID_table <- make_kable(sppID_check, "Potentially incorrect species entries")

#----- + Summarize Plant ID checks + -----
plantID_check <- QC_table %>% filter(Data %in% "Plant ID" & Num_Records > 0) 
include_plantID_tab <- tab_include(plantID_check)


#----- ISED checks-----
taxa <- prepTaxa()

# Need to import ParkTaxonProtectedStatus table from local database until it's added to the taxon view
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)
xref_taxon <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[xrefCOMN].[ParkTaxonProtectedStatus]"))
tlu_park <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[tluCOMN].[Park]"))
RODBC::odbcClose(con)

tlu_park2 <- tlu_park %>% select(ID, Unit) %>% 
                          unique() %>% 
                          rename(ParkID = ID) %>% 
                          filter(Unit %in% park_ev_list)


ised_taxon1 <- xref_taxon %>% select(ParkID, TaxonID, IsEarlyDetection) %>% 
                              filter(IsEarlyDetection == 1) %>% 
                              unique()

ised_taxon2 <- inner_join(ised_taxon1, tlu_park2, by = "ParkID") %>% select(-ParkID) %>% unique()

ised_taxon <- left_join(ised_taxon2, taxa %>% select(TaxonID, TSN, ScientificName), 
                        by = "TaxonID")

ised_join <- left_join(spplist_new, ised_taxon, by = c("TSN", "ScientificName")) %>% 
             filter(IsEarlyDetection == 1) %>% 
             select(-StartYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
             -quad_pct_freq, -IsEarlyDetection) %>% 
             arrange(Plot_Name, ScientificName)

QC_table <- rbind(QC_table, 
                  QC_check(ised_join, "Early Detection", "Invasive species early detections"))

ised_table <- make_kable(ised_join, "Invasive species early detections")

#----- + Summarize ISED checks + -----
ISED_check <- QC_table %>% filter(Data %in% "Early Detection" & Num_Records > 0) 
include_ISED_tab <- tab_include(ISED_check)


#+++++ Compile final QC Table
QC_check_table <-  kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                         col.names = c("Data Tab", "Check Description", "Number of Records")) %>% 
                   kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'), 
                                 full_width = TRUE, position = 'left', font_size = 12) %>%
                   row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") %>% 
                   column_spec(2:ncol(QC_table), background = ifelse(QC_table$Num_Records > 0, "#F2F2A0", "#ffffff")) %>% 
                   collapse_rows(1, valign = 'top') %>% 
                   row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;') 

