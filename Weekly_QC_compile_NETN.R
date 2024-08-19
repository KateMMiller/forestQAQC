#--------------------------------
# Compiling data for QAQC Reports
#--------------------------------

#profvis::profvis({
#----- Load libraries
library(forestNETN)
#library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(knitr)
library(kableExtra)
library(stringr)
library(purrr)

source("Weekly_QC_functions.R")

#----- Compile data -----
# week_start = "2024-06-14"
# curr_year <- year(week_start)
# week_start <- as_date(week_start)
# loc_type <- 'all'
# importData()

arglist1 = list(to = curr_year, QAQC = TRUE, eventType = 'complete', locType = loc_type)

plotevs <- do.call(joinLocEvent, arglist1) 

old_evs <- plotevs %>% filter(SampleDate < week_start)
new_evs <- plotevs %>% filter_week() %>% name_plot()
new_evs_list <- unique(new_evs$Plot_Name)

park_ev_list <- sort(unique(new_evs$ParkUnit))
park_orig_list <- unique(substr(new_evs_list, 1, 4)) 
#park_orig_list <- unique(substr(new_evs_list[!grepl("ASIS", new_evs_list)], 1, 4)) 
#Example if new park is ever added for first cycle

arglist = list(park = park_ev_list, to = curr_year, QAQC = TRUE, eventType = 'complete', locType = loc_type)

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
visit_notes <- joinVisitNotes(from = curr_year, to = curr_year, noteType = 'visit', 
                              QAQC = TRUE, locType = loc_type) %>% 
               filter_week() %>% name_plot() %>% 
               select(Plot_Name, Note_Type, Sample_Info, Notes) %>% 
               arrange(Plot_Name, Note_Type, Sample_Info, Notes)

RC_visit_notes <- sapply(1:nrow(visit_notes), function(x){
  check = ifelse(visit_notes$Plot_Name[x] != visit_notes$Plot_Name[x + 1], x, NA)
}) %>% data.frame() %>% na.omit(.)

colnames(RC_visit_notes) <- c("col")

if(nrow(RC_visit_notes) == 0){
  RC_visit_notes <- data.frame(col = 0)
}

RC_visit_note_type <- sapply(1:nrow(visit_notes), function(x){
  check = ifelse(visit_notes$Note_Type[x] != visit_notes$Note_Type[x + 1], x, NA)
}) %>% data.frame() %>% na.omit(.) 
         
colnames(RC_visit_note_type) <- c("col")

if(nrow(RC_visit_note_type) == 0){
  RC_visit_note_type <- data.frame(col = 0)
}

visit_table <- kable(visit_notes, format = 'html', align = c('c', 'c', 'c', 'l'),
                     col.names = c("Plot", "Note Type", "Info", "Note")) %>% 
               kable_styling(fixed_thead = TRUE, bootstrap_options = 'condensed', full_width = TRUE,
                 position = 'left', font_size = 11) %>% 
               column_spec(1:3, width = "15%") %>% 
               collapse_rows(1, valign = 'top') %>% 
               row_spec(RC_visit_notes[,1], extra_css = "border-bottom: 1px solid #000000;") %>%  
               # row_spec(RC_visit_note_type[,1], extra_css = 'border-bottom: 1px solid #000000') %>%  
               row_spec(c(0, nrow(visit_notes)), extra_css = 'border-bottom: 1px solid #000000')

include_visit_notes <- tab_include(visit_notes) #

#----- Stand Data -----
stand <- do.call(joinStandData, arglist) %>% name_plot() %>% select(-c(Network, ParkSubUnit:PlotTypeCode)) %>% 
           filter(Plot_Name %in% new_evs_list) 
stand_old <- filter_old(stand) 
stand_new <- filter_week(stand)

# Check for PMs in stand data
stand_pm <- PM_check(stand_new)# %>% select_if(is.character) %>% select(-SampleDate)

PM_stand_col <- sapply(names(stand_pm), function(x) any(stand_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_stand_col[c(1,8)] <- TRUE # For Plot_Name

stand_pm2 <- data.frame(stand_pm[, PM_stand_col])

QC_table <- rbind(QC_table, 
                  QC_check(stand_pm2, "Stand Data", "Permanently Missing records in stand data"))

stand_pm_table <- make_kable(stand_pm2, "Permanently Missing records in stand data")

# Check plots with fluctuating stand structure
stand_str_latest <- stand %>% filter(IsQAQC == 0) %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  select(Plot_Name, ParkUnit, cycle, Stand_Structure) %>% 
  group_by(Plot_Name) %>% slice(which.max(cycle)) %>% ungroup()

stand_str_prev <- stand %>% filter(IsQAQC == 0) %>% 
  filter(!ParkUnit %in% "ASIS") %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  select(Plot_Name, ParkUnit, cycle, Stand_Structure)%>% 
  group_by(Plot_Name) %>% slice(which.max(cycle)-1) %>% ungroup()

stand_str1 <- full_join(stand_str_latest, stand_str_prev, by = c("Plot_Name", "ParkUnit"),
                        suffix = c("_latest", "_prev"), multiple = 'all', relationship = 'many-to-many')

stand_str1$stand_diff <- 
  ifelse(stand_str1$Stand_Structure_latest == stand_str1$Stand_Structure_prev, 0, 1)

stand_str <- stand_str1 %>% filter(stand_diff > 0)

QC_table <- rbind(QC_table, QC_check(stand_str, "Stand Data", "Stand structure differs from previous cycles"))

stand_str_table <- make_kable(stand_str, "Stand structure differs from previous cycles")

# Check plots with fluctuating microtopography
microtop_latest <- stand %>% filter(IsQAQC == 0) %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  select(Plot_Name, ParkUnit, cycle, Microtopography) %>% 
  group_by(Plot_Name) %>% slice(which.max(cycle)) %>% ungroup()

microtop_prev <- stand %>% filter(IsQAQC == 0) %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  select(Plot_Name, ParkUnit, cycle, Microtopography) %>% 
  group_by(Plot_Name) %>% slice(which.max(cycle)-1) %>% ungroup()

microtop1 <- full_join(microtop_latest, microtop_prev, by = c("Plot_Name", "ParkUnit"),
                       suffix = c("_latest", "_prev"), multiple = 'all', relationship = 'many-to-many')

microtop1$micro_diff <- ifelse(microtop1$Microtopography_latest == microtop1$Microtopography_prev, 0, 1)

microtop <- microtop1 %>% filter(micro_diff > 0)

QC_table <- rbind(QC_table, QC_check(microtop, "Stand Data", "Microtopography differs from previous cycles"))

microtop_table <- make_kable(microtop, "Microtopography differs from previous cycles")

# Check for DBI = 1 not in deer exclosure
dbi1 <- stand_new %>% filter(Deer_Browse_Index == 1)
QC_table <- rbind(QC_table, QC_check(dbi1, "Stand Data", "DBI = 1 outside of exclosure"))

DBI_1_table <- make_kable(dbi1, "DBI = 1 outside exclosure") 

# Check for big DBI shift between cycles
dbi_latest <- stand %>% filter(IsQAQC == 0) %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  select(Plot_Name, ParkUnit, cycle, Deer_Browse_Index) %>% 
  group_by(Plot_Name) %>% slice(which.max(cycle)) %>% ungroup()

dbi_prev <- stand %>% filter(IsQAQC == 0) %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  select(Plot_Name, ParkUnit, cycle, Deer_Browse_Index) %>% 
  group_by(Plot_Name) %>% slice(which.max(cycle)-1) %>% ungroup()

dbi_diff <- full_join(dbi_latest, dbi_prev,
                      by = c("Plot_Name", "ParkUnit"), 
                      suffix = c("_latest", "_prev"), multiple = 'all', relationship = 'many-to-many') %>% 
  mutate(DBI_diff = abs(Deer_Browse_Index_latest - Deer_Browse_Index_prev)) %>% 
  filter(DBI_diff > 1) %>% select(-DBI_diff)

QC_table <- rbind(QC_table, QC_check(dbi_diff, "Stand Data", "DBI change > 1 point from previous cycle"))

DBI_diff_table <- make_kable(dbi_diff, "DBI change > 1 point from previous cycle")

# Disturbances
stand_dist <- do.call(joinStandDisturbance, c(arglist, list(from = curr_year))) %>% 
  filter_week() %>% filter(DisturbanceCode > 0) %>% 
  select(Plot_Name, SampleYear, IsQAQC, DisturbanceCode, DisturbanceSummary, ThresholdCode,
         ThresholdLabel, DisturbanceCoverClassLabel, DisturbanceNote) %>% 
  rename(Summary = DisturbanceSummary, 
         Label = ThresholdLabel,
         CoverClass = DisturbanceCoverClassLabel,
         Note = DisturbanceNote)

QC_table <- rbind(QC_table, QC_check(stand_dist, "Stand Data", "Reported stand disturbances"))

stand_dist_table <- make_kable(stand_dist, "Recorded disturbances")

# Check for potential stand height outliers
stand_ht <- get("StandTreeHeights_NETN", env = VIEWS_NETN) %>% 
            select(Plot_Name, ParkUnit, SampleYear, SampleDate, IsQAQC, CrownClassLabel, Height) %>% 
            filter(!is.na(CrownClassLabel)) 

stand_ht_new <- stand_ht %>% filter_week() %>% filter(IsQAQC == 0)

stand_ht_sum <- stand_ht %>% filter(SampleYear < curr_year) %>% group_by(ParkUnit) %>% 
                summarize(stand_ht_99 = quantile(Height, probs = 0.99))

stand_ht_99_check <- left_join(stand_ht_new, stand_ht_sum, 
                               by = "ParkUnit", multiple = 'all', relationship = 'many-to-many') %>% 
                     filter(Height > stand_ht_99)

QC_table <- rbind(QC_table, QC_check(stand_ht_99_check, "Stand Data", "Stand heights > 99% percentile for a given park"))

stand_ht_99_table <- make_kable(stand_ht_99_check, "Stand heights > 99% percentile for a given park")

# Compare to previous stand height
stand_ht_prev <- do.call(joinStandData, arglist) %>% name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>%
  group_by(Plot_Name) %>% slice(which.max(cycle) - 1) %>% ungroup() %>% 
  select(Plot_Name, Avg_Height_Codom, Avg_Height_Inter) %>% 
  mutate(Codom_prev_up50 = Avg_Height_Codom + Avg_Height_Codom*0.5,
         Codom_prev_low50 = Avg_Height_Codom - Avg_Height_Codom*0.5,
         Inter_prev_up50 = Avg_Height_Inter + Avg_Height_Inter*0.5,
         Inter_prev_low50 = Avg_Height_Inter - Avg_Height_Inter*0.5) 

stand_ht_new_wide <- full_join(stand_ht_new %>% filter(CrownClassLabel == "Co-dominant") %>% 
                                 select(Plot_Name, Height),
                               stand_ht_new %>% filter(CrownClassLabel == "Intermediate") %>% 
                                 select(Plot_Name, Height),
                               by = c("Plot_Name"),
                               suffix = c("_codom", "_inter"),
                               multiple = 'all', relationship = 'many-to-many')

stand_ht_comp <- left_join(stand_ht_new_wide, stand_ht_prev, by = "Plot_Name",
                           multiple = 'all', relationship = 'many-to-many') %>% 
  filter((Height_codom > Codom_prev_up50 & !is.na(Codom_prev_up50))| 
           (Height_codom < Codom_prev_low50 & !is.na(Codom_prev_low50))| 
           (Height_inter > Inter_prev_up50 & !is.na(Inter_prev_up50))| 
           (Height_inter < Inter_prev_low50 & !is.na(Inter_prev_low50))) %>% 
  select(Plot_Name, Height_codom, Height_inter, Avg_Height_Codom, Avg_Height_Inter) %>% 
  rename(Prev_Codom_Avg = Avg_Height_Codom, Prev_Inter_Avg = Avg_Height_Inter) %>% 
  unique()

QC_table <- rbind(QC_table, QC_check(stand_ht_comp, "Stand Data", "Stand heights 50% higher or lower than previous visit"))

stand_ht_comp_table <- make_kable(stand_ht_comp, "Stand heights 50% higher or lower than previous visit")

#----- + Summarize stand checks + -----
stand_check <- QC_table %>% filter(Data %in% "Stand Data" & Num_Records > 0) 
include_stand_tab <- tab_include(stand_check)

#----- Tree checks -----
tree_data <- do.call(joinTreeData, c(arglist, list(speciesType = 'all'))) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) 

tree_data_live <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = "live"))) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) 

tree_data_old <- tree_data %>% filter(SampleYear >= (curr_year - 5) & SampleYear < curr_year) # Covers COVID years
tree_data_new <- tree_data %>% filter(SampleDate >= week_start)

# Check for PMs in tree data
tree_data_pm <- PM_check(tree_data_new)# %>% select_if(is.character) %>% select(-SampleDate)

PM_tree_data_col <- sapply(names(tree_data_pm), 
                           function(x) any(tree_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
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

status_check1 <- tree_data %>% filter(IsQAQC == 0) %>% filter(!ParkUnit %in% "ASIS") %>% 
  select(Plot_Name, ParkUnit, TagCode, SampleYear, cycle, TreeStatusCode) %>% 
  mutate(status = case_when(TreeStatusCode %in% alive ~ "alive",
                            TreeStatusCode %in% dead ~ "dead", 
                            TreeStatusCode %in% recr ~ "alive_recruit",
                            TreeStatusCode %in% missed ~ "missed",
                            TreeStatusCode %in% exc ~ "exclude",
                            TreeStatusCode %in% "NL" ~ "not_located",
                            TreeStatusCode %in% "XS" ~ "exclude_shrank",
                            TreeStatusCode %in% "DF" ~ "dead fallen",
                            TRUE ~ NA_character_)) %>% 
  select(-TreeStatusCode) 


status_latest <- map_dfr(seq_along(park_orig_list), function(x){
  filter(status_check1, 
         SampleYear == curr_year & ParkUnit == park_orig_list[x]
  )
})

status_prev <- map_dfr(seq_along(park_orig_list), function(x){
  filter(status_check1, 
         between(SampleYear, curr_year - 6, curr_year - 1) & # covers SAHI too
           ParkUnit == park_orig_list[x]
  )
})

status_check <- left_join(status_latest, status_prev, by = c("Plot_Name", "TagCode"),
                          suffix = c("_latest", "_prev"),
                          multiple = 'all', relationship = 'many-to-many') %>% 
  mutate(check = case_when(status_latest == "alive" & status_prev == "dead" ~ "zombie",
                           status_latest == "missed" ~ "missed",
                           status_latest == "exclude" ~ "excluded",
                           status_latest == "not_located" ~ "not_located",
                           TRUE ~ "correct")) %>% 
  filter(!check %in% "correct")

QC_table <- rbind(QC_table, 
                  QC_check(status_check, "Tree Data", "Zombie and excluded trees"))

status_table <- make_kable(status_check, "Zombie and excluded trees")

# Check for potential elevated mortality events
live_stems_prev <- tree_data_live %>% filter(SampleYear < curr_year & IsQAQC == 0) %>% 
  group_by(Plot_Name, SampleYear) %>% 
  summarize(num_live = sum(num_stems), .groups = 'drop')

live_stems_new <- tree_data_live %>% filter(SampleYear == curr_year & IsQAQC == 0) %>% 
  group_by(Plot_Name, SampleYear) %>% 
  summarize(num_live = sum(num_stems), .groups = 'drop')

elev_mort <- full_join(live_stems_new, live_stems_prev, 
                       by = "Plot_Name", suffix = c("_new", "_old"), 
                       multiple = 'all', relationship = 'many-to-many') %>% 
  mutate(mort = 100*(num_live_new - num_live_old)/(num_live_old * (SampleYear_new - SampleYear_old))) %>% 
  filter(mort < -1.6)

QC_table <- rbind(QC_table, 
                  QC_check(elev_mort, "Tree Data", "Plots with elevated mortality (> 1.6% mort/ year)"))

elev_mort_table <- make_kable(elev_mort, "Plots with elevated mortality (> 1.6% mort/ year)")

# Report more detail on species composition in elevated mortality plots
em_live_old <- tree_data_old %>% filter(Plot_Name %in% elev_mort$Plot_Name) %>% 
                                 filter(TreeStatusCode %in% c(alive, recr, "AM")) %>% 
                                 select(Plot_Name, TagCode, ScientificName)

em_dead_new <- tree_data_new %>% filter(Plot_Name %in% elev_mort$Plot_Name) %>% 
                                 filter(TreeStatusCode %in% dead) %>% 
                                 select(Plot_Name, TagCode, ScientificName, TreeStatusCode)

em_spp <- left_join(em_live_old, em_dead_new, by = c("Plot_Name", "TagCode", "ScientificName"),
                    multiple = 'all', relationship = 'many-to-many') %>% 
           filter(!is.na(TreeStatusCode))

QC_table <- rbind(QC_table, 
                  QC_check(em_spp, "Tree Data", "Elevated mortality by tree species"))

em_spp_table <- make_kable(em_spp, "Elevated mortality by tree species")

# Trees with major crown class changes
crown_check_prev <- tree_data_old %>% filter(TreeStatusCode %in% alive) %>% 
  select(Plot_Name, ParkUnit, TagCode, cycle, CrownClassCode)

crown_check_latest <- tree_data_new %>% filter(TreeStatusCode %in% alive) %>% 
  filter(!ParkUnit %in% "ASIS") %>% 
  select(Plot_Name, ParkUnit, TagCode, cycle, CrownClassCode)

crown_check1 <- left_join(crown_check_latest, crown_check_prev, 
                          by = c("Plot_Name", "ParkUnit", "TagCode"),
                          suffix = c("_latest", "_prev"),
                          multiple = 'all', relationship = 'many-to-many') %>% 
  filter(!is.na(CrownClassCode_prev)) %>% 
  filter(!is.na(CrownClassCode_latest)) %>% 
  mutate(crown_change = abs(CrownClassCode_latest - CrownClassCode_prev))

crown_check <- crown_check1 %>% filter(crown_change > 1) %>% select(-crown_change)

QC_table <- rbind(QC_table, 
                  QC_check(crown_check, "Tree Data", "Major crown class changes"))

crown_table <- make_kable(crown_check, "Major crown class changes")

# Check that trees with > 3cm growth or <-0.1 cm growth have DBH verified selected
tree_live_prev <- tree_data_old %>% filter(TreeStatusCode %in% alive) %>% 
  filter(!ParkUnit %in% "ASIS") %>% 
  select(Plot_Name, ParkUnit, TagCode, cycle, DBHcm)

tree_live_latest <- tree_data_new %>% filter(TreeStatusCode %in% alive) %>% 
  filter(!ParkUnit %in% "ASIS") %>% 
  select(Plot_Name, ParkUnit, TagCode, cycle, DBHcm, IsDBHVerified)

tree_dbh <- left_join(tree_live_latest, tree_live_prev, 
                      by = c("Plot_Name", "ParkUnit", "TagCode"),
                      suffix = c("_latest", "_prev"),
                      multiple = 'all', relationship = 'many-to-many') %>% 
  filter(!is.na(DBHcm_latest)) %>% 
  filter(!is.na(DBHcm_prev)) %>% 
  mutate(DBH_diff = DBHcm_latest - DBHcm_prev)

# Identify Zoinks trees 
zoinks_tree <- tree_dbh %>% filter(DBH_diff >= 3 | DBH_diff < -0.1)

QC_table <- rbind(QC_table, 
                  QC_check(zoinks_tree, "Tree Data", "Zoinks trees with > 3cm growth or < -0.1cm growth"))

zoinks_table <- make_kable(zoinks_tree, "Zoinks trees with > 3cm growth or < -0.1cm growth")

# Identify zoinks trees missing DBH Verified check box
tree_dbh$Missing_DBHVer = ifelse((tree_dbh$DBH_diff >= 3 | tree_dbh$DBH_diff < -0.1) & 
                                   tree_dbh$IsDBHVerified == 0, 1, 0)

tree_dbh_check <- tree_dbh %>% filter(Missing_DBHVer == 1) %>% 
  select(Plot_Name, TagCode, DBHcm_prev, DBHcm_latest, DBH_diff, IsDBHVerified) %>% 
  data.frame()

QC_table <- rbind(QC_table, 
                  QC_check(tree_dbh_check, "Tree Data", "DBH zoinks missing DBH Verified check"))

tree_dbh_table <- make_kable(tree_dbh_check, "DBH zoinks missing DBH Verified check")

# Identify non-zoinks that has DBH Verified Checkbox
tree_nz <- tree_dbh %>% 
  mutate(notZoinks = ifelse(is.na(DBH_diff) | (DBH_diff > -0.01 & DBH_diff < 3), TRUE, FALSE))

tree_nz$wrongZoinks <- ifelse(tree_nz$notZoinks == TRUE & tree_nz$IsDBHVerified == TRUE, TRUE, FALSE)

tree_nz <- tree_nz %>% filter(wrongZoinks == TRUE)

tree_nz <- tree_nz[, c("Plot_Name", "TagCode", 'DBHcm_prev', 'DBHcm_latest',
                       "DBH_diff", "IsDBHVerified")]

QC_table <- rbind(QC_table,
                  QC_check(tree_nz, "Tree Data", "DBH non-zoinks with DBH Verified check"))

tree_dbhnz_table <- make_kable(tree_nz, "DBH non-zoinks with DBH Verified check")

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
                                        (!str_detect(ScientificName, "Picea|Abies balsamea") & SB == 1)|
                                        (!str_detect(ScientificName, "Quercus") & SOD == 1)|
                                        (!(ScientificName %in% "Abies balsamea") & BWA == 1)|      
                                        (!(ScientificName %in% "Cornus florida") & DOG == 1)|   
                                        (!(ScientificName %in% "Juglans cinerea") & BC == 1)| 
                                        (!(ScientificName %in% "Fagus grandifolia") & BBD == 1)|
                                        (!(ScientificName %in% "Fagus grandifolia") & BLD == 1)|  
                                        (!(ScientificName %in% "Pinus resinosa") & RPS == 1)|   
                                        (!(ScientificName %in% "Tsuga canadensis") & HWA == 1)|
                                        (!(ScientificName %in% "Tsuga canadensis") & EHS == 1))

QC_table <- rbind(QC_table, 
                  QC_check(trcond_spp_check, "Tree Data", "Trees with conditions applied to wrong species."))

trcond_spp_table <- make_kable(trcond_spp_check, "Trees with conditions applied to wrong species.")

# Check for priority pest detections
pest_list <- c("ALB", "BC", "BBD", "BLD", "BWA", "DOG", "EAB", "EHS", "GM", 
               "HWA", "RPS", "SB", "SLF", "SOD", "SPB", "SW")

pest_check <- tree_cond %>% filter(SampleYear == curr_year) %>% 
                            mutate(pest_det = rowSums(across(all_of(pest_list)), na.rm = T)) %>% 
                            filter(pest_det > 0) %>% select(Plot_Name, TagCode, all_of(pest_list)) %>% 
                            pivot_longer(cols = all_of(pest_list), 
                                         names_to = "Pest",
                                         values_to = "Detection") %>% 
                            filter(Detection == 1) %>% select(-Detection)


QC_table <- rbind(QC_table,
                  QC_check(pest_check, "Tree Data", "Priority forest pest/pathogen detections."))

pest_table <- make_kable(pest_check, "Priority forest pest/pathogen detections.")

#----- + Summarize tree data checks + -----
tree_check <- QC_table %>% filter(Data %in% "Tree Data" & Num_Records > 0) 
include_tree_tab <- tab_include(tree_check)

#----- Microplot Saplings -----
# Check sample qualifiers
shrubs_sq <- get("MicroplotShrubs_NETN", env = VIEWS_NETN) %>% 
             mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
             name_plot() %>% 
             filter(Plot_Name %in% new_evs_list) %>% 
             filter(SampleYear %in% curr_year) %>% 
             select(Plot_Name, MicroplotCode, SQShrubCode, SQShrubNotes) %>% 
             rename(SQ = SQShrubCode, SQNotes = SQShrubNotes) %>% 
             mutate(type = "shrub")

saps_sq <- get("MicroplotSaplings_NETN", env = VIEWS_NETN) %>% 
           mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
           name_plot() %>% 
           filter(Plot_Name %in% new_evs_list) %>% 
           filter(SampleYear %in% curr_year) %>% 
           select(Plot_Name, MicroplotCode, SQSaplingCode, SQSaplingNotes) %>% 
           rename(SQ = SQSaplingCode, SQNotes = SQSaplingNotes) %>% 
           mutate(type = "sapling")

seeds_sq <- get("MicroplotSeedlings_NETN", env = VIEWS_NETN) %>% 
            name_plot() %>% 
            filter(Plot_Name %in% new_evs_list) %>% 
            filter(SampleYear %in% curr_year) %>% 
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
  filter(SampleYear %in% curr_year)

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
  filter(SampleYear %in% curr_year)

shrub_data_pm <- PM_check(shrubs)# %>% select_if(is.character) %>% select(-SampleDate)

PM_shrub_col <- sapply(names(shrub_data_pm), function(x) any(shrub_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_shrub_col[c(1)] <- TRUE # For Plot_Name

shrub_data_pm2 <- data.frame(shrub_data_pm[, PM_shrub_col])

QC_table <- rbind(QC_table, 
                  QC_check(shrub_data_pm2, "Microplot", "Shrubs: Permanently Missing records"))

shrub_pm_table <- make_kable(shrub_data_pm2, "Shrubs: Permanently Missing records")

# Check for plots with SS shrubsample qualifier and no species data
shrub_ss_sq <- get("MicroplotShrubs_NETN", envir = VIEWS_NETN) %>% 
               name_plot() %>% 
               filter(Plot_Name %in% new_evs_list) %>% filter(SampleYear %in% curr_year) %>%
               select(Plot_Name, SampleYear, IsQAQC, SQShrubCode, MicroplotCode, ScientificName, CoverClassCode) %>%
               filter(SQShrubCode %in% "SS" & is.na(ScientificName))

QC_table <- rbind(QC_table, 
                  QC_check(shrub_ss_sq, "Microplot", "Shrubs: SS sample qualifier without cover data"))

shrub_ss_sq_table <- make_kable(shrub_ss_sq, "Shrubs: SS sample qualifier without cover data")

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
  filter(SampleYear %in% curr_year)

seed_data_pm <- PM_check(seeds)

PM_seed_col <- sapply(names(seed_data_pm), function(x) any(seed_data_pm[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_seed_col[c(1)] <- TRUE # For Plot_Name

seed_data_pm2 <- data.frame(seed_data_pm[, PM_seed_col])

QC_table <- rbind(QC_table, 
                  QC_check(seed_data_pm2, "Microplot", "Seedlings: Permanently Missing records"))

seed_pm_table <- make_kable(seed_data_pm2, "Seedlings: Permanently Missing records")

# Check for plots with SS sample qualifier and no species data
seed_ss_sq <- get("MicroplotSeedlings_NETN", envir = VIEWS_NETN) %>% 
              name_plot() %>% 
              filter(Plot_Name %in% new_evs_list) %>%  filter(SampleYear %in% curr_year) %>%
              select(Plot_Name, SampleYear, IsQAQC, SQSeedlingCode, MicroplotCode, ScientificName) %>%
              filter(SQSeedlingCode %in% "SS" & is.na(ScientificName))

QC_table <- rbind(QC_table, 
                  QC_check(seed_ss_sq, "Microplot", "Seedlings: SS sample qualifier without seedling tallies"))

seed_ss_sq_table <- make_kable(seed_ss_sq, "Seedlings: SS sample qualifier without seedling tallies")

# Check for plots with species recorded but 0 tallies
seeds_0tally <- seeds %>% filter((!ScientificName %in% c("Not Sampled", "None present")) & 
                                    (tot_seeds == 0 | 
                                    is.na(Seedlings_15_30cm)|
                                    is.na(Seedlings_30_100cm)|
                                    is.na(Seedlings_100_150cm)|
                                    is.na(Seedlings_Above_150cm))) %>% 
                select(Plot_Name, MicroplotCode, ScientificName, Seedlings_15_30cm:tot_seeds)

QC_table <- rbind(QC_table,
                  QC_check(seeds_0tally, "Microplot", "Seedlings: species recorded with 0 missing tally"))

seed_0tally_table <- make_kable(seeds_0tally, "Seedlings: species recorded with 0 or missing tally")

# check for seedling tallies > 99 percentile of non-zero counts in a microplot
seeds <- do.call(joinMicroSeedlings, arglist) %>% name_plot()
seeds_new <- seeds %>% filter_week()
seeds_old <- seeds %>% filter(SampleYear < curr_year) %>% filter(!ScientificName %in% c("Not Sampled", "None present"))
na_cols <- c("Seedlings_15_30cm", "Seedlings_30_100cm", "Seedlings_100_150cm",  
             "Seedlings_Above_150cm", "tot_seeds")

seeds_old[,na_cols][seeds_old[,na_cols]==0] <- NA_real_

seeds_sum <- seeds_old %>% filter(SampleYear > 2006) %>% 
                           group_by(ParkUnit) %>%
                           summarize(Seedlings_15_30cm_99 = quantile(Seedlings_15_30cm, probs = 0.99, na.rm = T),
                           Seedlings_30_100cm_99 = quantile(Seedlings_30_100cm, probs = 0.99, na.rm = T),
                           Seedlings_100_150cm_99 = quantile(Seedlings_100_150cm, probs = 0.99, na.rm = T),
                           Seedlings_Above_150cm_99 = quantile(Seedlings_Above_150cm, probs = 0.99, na.rm = T)) %>%
             as.data.frame()

# Handle error if 99 comes in as NA b/c there were non recorded in that size class
seeds_sum$Seedlings_15_30cm_99 <- ifelse(is.na(seeds_sum$Seedlings_15_30cm_99), 
                                         max(seeds_sum$Seedlings_15_30cm_99, na.rm = T),
                                         seeds_sum$Seedlings_15_30cm_99)
seeds_sum$Seedlings_30_100cm_99 <- ifelse(is.na(seeds_sum$Seedlings_30_100cm_99), 
                                          max(seeds_sum$Seedlings_30_100cm_99, na.rm = T),
                                          seeds_sum$Seedlings_30_100cm_99)
seeds_sum$Seedlings_100_150cm_99 <- ifelse(is.na(seeds_sum$Seedlings_100_150cm_99), 
                                           max(seeds_sum$Seedlings_100_150cm_99, na.rm = T),
                                           seeds_sum$Seedlings_100_150cm_99)
seeds_sum$Seedlings_Above_150cm_99 <- ifelse(is.na(seeds_sum$Seedlings_Above_150cm_99), 
                                             max(seeds_sum$Seedlings_Above_150cm_99, na.rm = T),
                                             seeds_sum$Seedlings_Above_150cm_99)

seeds_99_check <- left_join(seeds_new %>% select(Plot_Name, ParkUnit, cycle, Seedlings_15_30cm:Seedlings_Above_150cm), 
                            seeds_sum, by = "ParkUnit", multiple = 'all', relationship = 'many-to-many') %>% 
                  filter(Seedlings_15_30cm > Seedlings_15_30cm_99 |
                         Seedlings_30_100cm > Seedlings_30_100cm_99 |
                         Seedlings_100_150cm > Seedlings_100_150cm_99 |
                         Seedlings_Above_150cm > Seedlings_Above_150cm_99) %>% as.data.frame()
                  
seeds_99_check2 <- if(nrow(seeds_99_check) > 0){
  seed_cols <- c(ifelse(seeds_99_check$Seedlings_15_30cm > seeds_99_check$Seedlings_15_30cm_99, 
                        "Seedlings_15_30cm", "ParkUnit"),
                 ifelse(seeds_99_check$Seedlings_30_100cm > seeds_99_check$Seedlings_30_100cm_99 , 
                        "Seedlings_30_100cm", "ParkUnit"),
                 ifelse(seeds_99_check$Seedlings_100_150cm > seeds_99_check$Seedlings_100_150cm_99, 
                        "Seedlings_100_150cm", "ParkUnit"),
                 ifelse(seeds_99_check$Seedlings_Above_150cm > seeds_99_check$Seedlings_Above_150cm_99, 
                        "Seedlings_Above_150cm", "ParkUnit"))#) 
  
  seeds_99_check_final <- seeds_99_check[, c("Plot_Name", 'cycle', unique(seed_cols))]
  seeds_99_check_final
} else {seeds_99_check}

seeds_99_check_final <- seeds_99_check2 %>% filter(cycle > 1)

QC_table <- rbind(QC_table, QC_check(seeds_99_check_final, "Quadrat", "Seedling records with a size class tally > 99% percentile for a given park"))

seeds_99_table <- make_kable(seeds_99_check_final, "Seedling records with a size class tally > 99% percentile for a given park")

# check for sapling counts > 99 percentile of non-zero counts
saps <- do.call(joinMicroSaplings, arglist) %>% name_plot()
saps_new <- saps %>% filter_week() %>% group_by(Plot_Name, ParkUnit) %>% 
            summarize(sap_count = sum(Count, na.rm = T), .groups = 'drop')
  
saps_old <- saps %>% filter(SampleYear < curr_year & IsQAQC == 0) %>% 
                     filter(!ScientificName %in% c("Not Sampled", "None present"))
                       
saps_sum <- saps_old %>% filter(IsQAQC == 0) %>% filter(SampleYear > 2006) %>% 
            group_by(Plot_Name, ParkUnit, SampleYear)  %>% 
            summarize(sap_count = sum(Count), .groups = 'drop') %>% 
            filter(sap_count > 0) %>% 
            group_by(ParkUnit) %>% 
            summarize(sap_count_99 = quantile(sap_count, probs = 0.99, na.rm = T))


saps_99_check <- left_join(saps_new, saps_sum, by = "ParkUnit", 
                           multiple = 'all', relationship = 'many-to-many') %>% 
                 filter(sap_count > sap_count_99)

QC_table <- rbind(QC_table, QC_check(saps_99_check, "Microplot", "Sapling tallies > 99% percentile for a given park"))

saps_99_table <- make_kable(saps_99_check, "Sapling tallies > 99% percentile for a given park")

#----- + Summarize microplot checks + -----
micro_check <- QC_table %>% filter(Data %in% "Microplot" & Num_Records > 0) 
include_micro_tab <- tab_include(micro_check)

#----- Quadrats ------
# Check for NS SQs and make sure there are notes for each NS
quad_notes <- do.call(joinQuadNotes, arglist) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(SampleYear %in% curr_year)

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
             filter(SampleYear %in% curr_year)
quad_spp <- do.call(joinQuadSpecies, arglist) %>% 
            name_plot() %>% 
            filter(Plot_Name %in% new_evs_list) %>% 
            filter(SampleYear %in% curr_year)

#check for trampled quadrats
quad_tramp <- get("QuadNotes_NETN", envir = VIEWS_NETN) 

quad_tramp2 <- quad_tramp %>% #mutate(Plot_Name = 
                              #         paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', "0"), sep = "-")) %>% 
                              filter(IsQAQC == 0) %>% 
                              filter(Plot_Name %in% new_evs_list) %>% 
                              select(Plot_Name, SampleYear, QuadratCode, SQQuadCharCode, IsTrampled, IsQAQC) %>% 
                              unique()

quad_tramp3 <- left_join(quad_tramp2, plotevs %>% select(Plot_Name, SampleYear, cycle, IsQAQC), 
                         by = c("Plot_Name", "SampleYear", "IsQAQC"),
                         multiple = 'all', relationship = 'many-to-many') 

quad_tramp4 <- rbind(quad_tramp3 %>% filter(SampleYear == curr_year) %>% mutate(cycle_txt = "prev"),
                     quad_tramp3 %>% filter(between(SampleYear, curr_year - 5, curr_year - 1)) %>% # covers SAHI
                       mutate(cycle_txt = 'curr')
)

quad_tramp_wide <- quad_tramp4 %>% select(Plot_Name, SampleYear, cycle_txt, QuadratCode, IsTrampled) %>% 
  arrange(Plot_Name, -SampleYear) %>% select(-SampleYear) %>%
  pivot_wider(names_from = c("QuadratCode", "cycle_txt"), values_from = "IsTrampled",
              names_glue = ("{QuadratCode}_{cycle_txt}")) 

tramp_check <- data.frame(table(quad_tramp_wide$Plot_Name)) %>% filter(Freq > 1)

if(nrow(tramp_check) > 0){
  warning("Year logic in quad_tramp checks not working properly")
}

quad_tramp_wide2 <- quad_tramp_wide %>% mutate(UC_dif = abs(UC_prev - UC_curr),
                                               UR_dif = abs(UR_prev - UR_curr),
                                               MR_dif = abs(MR_prev - MR_curr),
                                               BR_dif = abs(BR_prev - BR_curr),
                                               BC_dif = abs(BC_prev - BC_curr),
                                               BL_dif = abs(BL_prev - BL_curr),
                                               ML_dif = abs(ML_prev - ML_curr),
                                               UL_dif = abs(UL_prev - UL_curr)) %>% 
  select(Plot_Name, 
         UC_prev, UC_curr, UR_prev, UR_curr, MR_prev, MR_curr, BR_prev, BR_curr,
         BC_prev, BC_curr, BL_prev, BL_curr, ML_prev, ML_curr, UL_prev, UL_curr,
         UC_dif, UR_dif, MR_dif, BR_dif, 
         BC_dif, BL_dif, ML_dif, UL_dif)

quad_tramp_diff <- quad_tramp_wide2 %>% filter(UC_dif > 0 | UR_dif > 0 | MR_dif > 0 | BR_dif > 0 | 
                                               BC_dif > 0 | BL_dif > 0 | ML_dif > 0 | UL_dif > 0)

#quad_tramp_diff <- quad_tramp_wide3 %>% filter(colSums(A2_dif:CC_dif)>0) 

QC_table <- rbind(QC_table, QC_check(quad_tramp_diff, "Quadrat", "Trampled in current cycle differs from previous"))

tramp_plots2 <- quad_tramp_wide %>% filter(Plot_Name %in% quad_tramp_diff$Plot_Name) %>% 
  mutate(SampleYear = curr_year) %>%
  select(Plot_Name, 
         UC_c = UC_curr, UC_p = UC_prev, 
         UR_c = UR_curr, UR_p = UR_prev, 
         MR_c = MR_curr, MR_p = MR_prev, 
         BR_c = BR_curr, BR_p = BR_prev, 
         BC_c = BC_curr, BC_p = BC_prev, 
         BL_c = BL_curr, BL_p = BL_prev, 
         ML_c = ML_curr, ML_p = ML_prev, 
         UL_c = UL_curr, UL_p = UL_prev)

if(nrow(tramp_plots2 > 0)){
  quad_tramp_table <- make_kable(tramp_plots2, "Trampled in current cycle differs from previous") %>%
      purrr::reduce(2:ncol(tramp_plots2), function(x, y){
        col <- tramp_plots2[, y]
        column_spec(x, y, background = ifelse(col == TRUE, "#F2F2A0", "#FFFFFF"))
      }, .init = .) # fills TRUE as yellow, so easier to see
} else {
  quad_tramp_table <- make_kable(tramp_plots2, "Trampled in current cycle differs from previous")}


# Check for all quads trampled
tramp_all <- tramp_plots2 %>% 
  mutate(tramp_quads = UC_c + UR_c + MR_c + BR_c + 
                       BC_c + BL_c + ML_c + UL_c) %>% 
  filter(tramp_quads == 8)

QC_table <- rbind(QC_table, QC_check(tramp_all, "Quadrat", "All quadrats trampled"))

quad_tramp_all_table <- make_kable(tramp_all, "All quadrats trampled")

# Check for PMs in quadrat data
pms <- apply(quad_data, 1, function(x) sum(grepl("Permanently Missing|PM", x))) > 0
pm_check <- quad_data[pms,]
PM_quad_data_col <- sapply(names(quad_data), function(x) any(quad_data[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PM_cols <- names(pm_check[which(PM_quad_data_col==TRUE)])
quad_data_pm2 <- data.frame(pm_check[, c("Plot_Name", "CharacterLabel", PM_cols)])

QC_table <- rbind(QC_table, 
                  QC_check(quad_data_pm2, "Quadrat", "Quadrat Data: Permanently Missing records"))

quad_data_pm_table <- make_kable(quad_data_pm2, "Quadrat Data: Permanently Missing records")

# Check for PMs in quadrat species
pmsp <- apply(quad_spp, 1, function(x) sum(grepl("Permanently Missing|PM", x))) > 0
pmsp_check <- quad_spp[pmsp,]
PM_quad_spp_col <- sapply(names(quad_spp), function(x) any(quad_spp[,x] %in% c("Permanently Missing", "PM"))) %>% 
  as.logical()
PMsp_cols <- names(pmsp_check[which(PM_quad_spp_col == TRUE)])
quad_spp_pm2 <- data.frame(pmsp_check[, c("Plot_Name", "ScientificName", PMsp_cols)])

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

# Check for quadrats with SS sample qualifier, but no quad character data (eg potentially lost)
quad_sq_data1 <- get("QuadCharacter_NETN", envir = VIEWS_NETN) %>% 
                 name_plot() %>% filter(Plot_Name %in% new_evs_list) %>% filter(SampleYear %in% curr_year)

quad_sq_data1$num_missing <- rowSums(is.na(quad_sq_data1[, c("UC_txt", "UR_txt", "MR_txt", "BR_txt", 
                                                             "BC_txt", "BL_txt", "ML_txt", "UL_txt")]))

quad_sq_data <- quad_sq_data1 %>% filter(num_missing > 0)

QC_table <- rbind(QC_table, 
                  QC_check(quad_sq_data, "Quadrat", "Quadrat Data: SS sample qualifier without % cover data"))

quad_sq_data_table <- make_kable(quad_sq_data, "Quadrat Data: SS sample qualifier without % cover data")


# Check for quadrats with SS sample qualifier, but no quad species data (eg potentially lost)
quad_sq_spp1 <- get("QuadSpecies_NETN", envir = VIEWS_NETN) %>% 
  name_plot() %>% filter(Plot_Name %in% new_evs_list) %>%
  filter(SampleYear == curr_year)

quad_sq_spp1$num_missing <- rowSums(is.na(quad_sq_spp1[, c("UC_txt", "UR_txt", "MR_txt", "BR_txt", 
                                                           "BC_txt", "BL_txt", "ML_txt", "UL_txt")]))

quad_sq_spp <- quad_sq_spp1 %>% filter(num_missing > 0)

QC_table <- rbind(QC_table, 
                  QC_check(quad_sq_spp, "Quadrat", "Quadrat Species: SS sample qualifier without % cover data"))

quad_sq_spp_table <- make_kable(quad_sq_spp, "Quadrat Species: SS sample qualifier without % cover data")

# Check for species marked as Germinant - should only be tree species
quad_germ <- quad_spp %>% filter(IsGerminant == TRUE & Tree != 1) %>% 
                          select(Plot_Name, ParkUnit, IsQAQC, ScientificName, IsGerminant)

QC_table <- rbind(QC_table, 
                  QC_check(quad_germ, "Quadrat", "Non-tree species marked as germinants"))

quad_germ_table <- make_kable(quad_germ, "Non-tree species marked as germinants")

#----- + Summarize quadrat checks + -----
quad_check <- QC_table %>% filter(Data %in% "Quadrat" & Num_Records > 0) 
include_quad_tab <- tab_include(quad_check)

#----- Additional Species -----
# Check for NS SQ
addspp <- do.call(joinAdditionalSpecies, arglist) %>% 
          name_plot() %>% 
          filter(Plot_Name %in% new_evs_list) %>% 
          filter(SampleYear %in% curr_year)

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
cwd_vw <- get("CWD_NETN", env = VIEWS_NETN) %>% 
          #mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
          name_plot() %>% 
          filter(Plot_Name %in% new_evs_list) %>% 
          filter(SampleYear %in% curr_year) 

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

# Check for plots with >99% CWD volume 
cwd <- do.call(joinCWDData, list(park = park_ev_list, to = curr_year, 
                                 QAQC = TRUE, locType = loc_type)) %>% name_plot() 

cwd_new <- cwd %>% filter(Plot_Name %in% new_evs_list) %>% 
                   filter(SampleYear %in% curr_year)

cwd_old <- cwd %>% filter(SampleYear < curr_year)

cwd_sum <- cwd_old %>% group_by(ParkUnit) %>% 
  summarize(CWD_Vol_99 = quantile(CWD_Vol, probs = 0.99))

cwd_99_check <- left_join(cwd_new, cwd_sum, by = "ParkUnit", 
                          multiple = 'all', relationship = 'many-to-many') %>% 
  filter(CWD_Vol > CWD_Vol_99)

QC_table <- rbind(QC_table, QC_check(cwd_99_check, "CWD", "CWD Volume > 99% percentile for a given park"))

cwd_99_table <- make_kable(cwd_99_check, "CWD Volume > 99% percentile for a given park")

# Check for plots with >99% CWD lenght or diameter

cwdvw_new <- cwd_vw %>% filter(Plot_Name %in% new_evs_list) %>% 
                        filter_week() %>% name_plot() %>% 
                        select(Plot_Name, ParkUnit, SampleYear, SampleDate, IsQAQC, TransectCode, ScientificName,
                               Distance, Diameter, Length, DecayClassCode)
                        
cwdvw_old <- cwd_vw %>% filter(SampleYear < curr_year) %>% 
                        name_plot() %>% 
                        select(Plot_Name, ParkUnit, SampleYear, SampleDate, IsQAQC, TransectCode, ScientificName,
                               Distance, Diameter, Length, DecayClassCode)

cwdvw_sum <- cwdvw_old %>% group_by(ParkUnit) %>% 
                           summarize(cwd_diam_99 = quantile(Diameter, probs = 0.99, na.rm = T),
                                     cwd_length_99 = quantile(Length, probs = 0.99, na.rm = T))


cwd_data_99_check <- left_join(cwdvw_new, cwdvw_sum, by = "ParkUnit", 
                               multiple = 'all', relationship = 'many-to-many') %>% 
                     filter(Diameter > cwd_diam_99 |
                            Length > cwd_length_99) %>% 
                     select(Plot_Name, TransectCode, ScientificName, Distance, Diameter, Length, DecayClassCode)

QC_table <- rbind(QC_table, QC_check(cwd_data_99_check, "CWD", "CWD Length and/or Diameter > 99% percentile for a given park"))

cwd_data_99_table <- make_kable(cwd_data_99_check, "CWD Length and/or Diameter > 99% percentile for a given park")

#----- + Summarize CWD checks + -----
cwd_check <- QC_table %>% filter(Data %in% "CWD" & Num_Records > 0) 
include_cwd_tab <- tab_include(cwd_check)

#----- Soil-----
# Check for NS SQ
soil_vw <- get("SoilSample_NETN", env = VIEWS_NETN) %>% 
  #mutate(Plot_Name = paste(ParkUnit, stringr::str_pad(PlotCode, 3, side = 'left', '0'), sep = "-")) %>% 
  name_plot() %>% 
  filter(Plot_Name %in% new_evs_list) %>% 
  filter(SampleYear %in% curr_year) 


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

QC_table <- rbind(QC_table, 
                  QC_check(soil_check3, "Soil", "Plots missing at least one soil sample qualifier"))

soil_check3_table <- make_kable(soil_check3, "Plots missing at least one soil sample qualifier")

# Check for soil layers > 99 percentile for a given park
# soil_samp <- do.call(joinSoilSampleData, list(park = park_ev_list, to = curr_year, 
#                                               QAQC = TRUE, locType = loc_type,
#                                               last_lab_year = 2006)) %>% name_plot()
# the above code was really slow b/c QCs horizons, so doing manually below

soil_samp1 <- get("SoilSample_NETN", envir = VIEWS_NETN) %>% 
  select(Plot_Name, ParkUnit, ParkSubUnit, SampleYear, SampleDate, IsQAQC, SQSoilCode, 
         Sample = SampleSequenceCode, 
         Horizon = SoilLayerCode, Depth_cm, Note) %>% name_plot() %>%
  filter(Horizon %in% c("L", "O", "A", "T")) %>%
  pivot_wider(names_from = Horizon, values_from = Depth_cm) 

soil_samp2 <- soil_samp1 %>% group_by(Plot_Name, ParkUnit, ParkSubUnit, SampleYear, 
                                      SampleDate, IsQAQC) %>%
  summarize(num_samps = sum(SQSoilCode == "SS"),
            Litter_cm = sum(L, na.rm = T)/num_samps,
            O_Horizon_cm = sum(O, na.rm = T)/num_samps,
            A_Horizon_cm = sum(A, na.rm = T)/num_samps,
            Total_Depth_cm = sum(`T`, na.rm = T)/num_samps,
            .groups = 'drop')


soil_new <- soil_samp2 %>% filter_week() |> filter(SampleYear %in% curr_year)
soil_old <- soil_samp2 %>% filter(SampleYear < curr_year)

soil_sum <- soil_old %>% group_by(ParkUnit) %>% 
            summarize(litter_99 = quantile(Litter_cm, probs = 0.99),
                      O_hor_99 = quantile(O_Horizon_cm, probs = 0.99),
                      A_hor_99 = quantile(A_Horizon_cm, probs = 0.99),
                      Depth_99 = quantile(Total_Depth_cm, probs = 0.99))

soil_99_check <- left_join(soil_new, soil_sum, by = "ParkUnit", 
                           multiple = 'all', relationship = 'many-to-many') %>% 
                 filter(Litter_cm > litter_99 |
                        O_Horizon_cm > O_hor_99|
                        A_Horizon_cm > A_hor_99|
                        Total_Depth_cm > Depth_99) %>% 
                 select(Plot_Name, SampleYear, num_samps, Litter_cm:A_Horizon_cm, 
                        litter_99:Depth_99)

QC_table <- rbind(QC_table, QC_check(soil_99_check, "Soil", "Soil Depth > 99% percentile for at least one horizon in a given park"))

soil_99_table <- make_kable(soil_99_check, "Soil Depth > 99% percentile for at least one horizon in a given park")

#----- + Summarize soil checks + -----
soil_check <- QC_table %>% filter(Data %in% "Soil" & Num_Records > 0) 
include_soil_tab <- tab_include(soil_check)

#----- Plant ID checks-----
unknowns <- c(-9999999901:-9999999944, -9999999950:-9999999960)

spplist <- do.call(sumSpeciesList, arglist) %>% 
           #filter(IsQAQC == 0) %>% 
           #name_plot() %>% 
           filter(!TSN %in% unknowns) %>% 
           select(Plot_Name, ParkUnit, SampleYear, cycle, IsQAQC, TSN, ScientificName, BA_cm2:addspp_present)

# check species new to a plot
spplist_new <- spplist %>% filter(SampleYear %in% curr_year) %>% 
                           filter(Plot_Name %in% new_evs_list) %>% mutate(pres = 1)

spplist_old <- spplist %>% filter(SampleYear < curr_year) %>%  
                           filter(Plot_Name %in% new_evs_list) %>% mutate(pres = 1) %>% 
                           select(Plot_Name, TSN, ScientificName,
                                  pres)

spp_plotcheck <- full_join(spplist_new, spplist_old, 
                           by = c("Plot_Name", "TSN", "ScientificName"),
                           suffix = c("_new", "_old"), 
                           multiple = 'all', relationship = 'many-to-many') 

nacols <- c("BA_cm2", "DBH_mean", "tree_stems", "seed_den", "sap_den", "stock", 
            "shrub_avg_cov", "shrub_pct_freq", "quad_avg_cov", "quad_pct_freq", 
            "addspp_present", "pres_new", "pres_old")

spp_plotcheck[, nacols][is.na(spp_plotcheck[, nacols])] <- 0

spp_newplot <- spp_plotcheck %>% filter(pres_new == 1 & pres_old == 0) %>% 
               select(-SampleYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
                      -quad_pct_freq, -pres_new, -pres_old) %>% arrange(Plot_Name, IsQAQC, ScientificName)

QC_table <- rbind(QC_table, 
                  QC_check(spp_newplot, "Plant ID", "Species new to a plot"))

spp_newplot_table <- make_kable(spp_newplot, "Species new to a plot")

# check species new to a park
park_spplist <- spplist %>% filter(SampleYear < curr_year) %>% 
                            select(ParkUnit, TSN, ScientificName) %>% unique() %>% 
                            arrange(ParkUnit, ScientificName) %>% 
                            mutate(pres = 1)

spp_parkcheck <- full_join(spplist_new, park_spplist, 
                           by = c("ParkUnit", "TSN", "ScientificName"),
                           suffix = c("_new", "_old"), 
                           multiple = 'all', relationship = 'many-to-many') 

spp_parkcheck[, nacols][is.na(spp_parkcheck[, nacols])] <- 0

spp_newpark <- spp_parkcheck %>% filter(pres_new == 1 & pres_old == 0) %>% 
                                 select(-SampleYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
                                 -quad_pct_freq, -pres_new, -pres_old) %>% arrange(Plot_Name, ScientificName)

QC_table <- rbind(QC_table, 
                  QC_check(spp_newpark, "Plant ID", "Species new to a park"))

spp_newpark_table <- make_kable(spp_newpark, "Species new to a park")

# check possible miss-IDed species
spp_checks <- c('Acer saccharinum', 'Acer nigrum', 'Euonymus americanus',
                'Ilex montana', 'Kalmia latifolia', 
                'Larix decidua', 'Lonicera', 'Lysimachia quadriflora',
                'Maianthemum', 'Picea abies', 'Persicaria posumbu', 'Ulmus alata')

sppID_check <- spplist_new %>% filter(ScientificName %in% spp_checks)

QC_table <- rbind(QC_table, 
                  QC_check(sppID_check, "Plant ID", "Potentially incorrect species entries"))

sppID_table <- make_kable(sppID_check, "Potentially incorrect species entries")

# check for plant species collected i.e. Collected check box = True (only available in Quads and Add sp)
addsppColl <- addspp %>% filter(IsCollected == TRUE) %>% 
  select(Plot_Name, ParkUnit, IsQAQC, ScientificName, IsCollected)

#no collected checkbox included in joinQuadSpecies
quad_spp2 <- quad_spp %>% filter(IsGerminant == FALSE) %>%  select(Plot_Name, ParkUnit, IsQAQC, ScientificName)
quad_raw <- VIEWS_NETN[["QuadSpecies_NETN"]]
quad_raw2 <- quad_raw %>% filter(SampleYear == curr_year) %>% filter(IsGerminant == FALSE) %>% 
                          select(Plot_Name, ParkUnit, IsQAQC, ScientificName, IsCollected)
quad_sppColl <- left_join(quad_spp2, quad_raw2, by = c("Plot_Name", "ParkUnit", "IsQAQC", "ScientificName"))
quad_sppColl2 <- quad_sppColl %>% filter(IsCollected == TRUE)

spp_coll <- rbind(addsppColl, quad_sppColl2)

QC_table <- rbind(QC_table, 
                  QC_check(spp_coll, "Plant ID", "Species collected"))

spp_coll_table <- make_kable(spp_coll, "Species collected")

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

ised_taxon2 <- inner_join(ised_taxon1, tlu_park2, by = "ParkID", 
                          multiple = 'all', relationship = 'many-to-many') %>% select(-ParkID) %>% unique()

ised_taxon <- left_join(ised_taxon2, taxa %>% select(TaxonID, TSN, ScientificName), 
                        by = "TaxonID", multiple = 'all', relationship = 'many-to-many')

ised_join <- left_join(spplist_new, ised_taxon, by = c("TSN", "ScientificName", "ParkUnit" = "Unit"),
                       multiple = 'all', relationship = 'many-to-many') %>% 
             filter(IsEarlyDetection == 1) %>% 
             select(-SampleYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
             -quad_pct_freq, -IsEarlyDetection) %>% 
             arrange(Plot_Name, ScientificName)

QC_table <- rbind(QC_table, 
                  QC_check(ised_join, "Early Detection", "Invasive species early detections"))

ised_table <- make_kable(ised_join, "Invasive species early detections")

#----- + Summarize ISED checks + -----
ISED_check <- QC_table %>% filter(Data %in% "Early Detection" & Num_Records > 0) 
include_ISED_tab <- tab_include(ISED_check)

#+++++ Compile final QC Table
QC_table$Notes <- as.character(NA)
QC_check_table <-  kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                         col.names = c("Data Tab", "Check Description", "Number of Records", "Notes")) %>% 
                   kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'), 
                                 full_width = TRUE, position = 'left', font_size = 12) %>%
                   row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") %>% 
                   column_spec(2:ncol(QC_table), background = ifelse(QC_table$Num_Records > 0, "#F2F2A0", "#ffffff")) %>% 
                   collapse_rows(1, valign = 'top') %>% 
                   row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;') %>% 
                   column_spec(3, width = "150px")

