#--------------------------------
# Compiling data for Previous Visit Field Forms
#--------------------------------

#----- Load libraries
library(tidyverse)
library(forestNETN)
library(htmltools)
library(knitr)
library(kableExtra)

importData() #local instance
#importData(name = "NETN_Forest_MABI_training")

# Params for troubleshooting inside script
year = as.numeric(2018)
park = "MABI"
loc_type = "all"

# #----- Functions -----
# make_kable <- function(df, cap){
#   QC_table <- if(nrow(df) > 0){
#     kable(df, format = 'html', align = 'c', caption = cap) |> 
#       kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'), 
#                     full_width = TRUE, position = 'left', font_size = 12) |> 
#       row_spec(0, extra_css = 
#                  "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |> 
#       collapse_rows(1, valign = 'top') |> 
#       row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;') 
#   } else NULL 
# }

#----- Compile data -----
arglist = list(park = park, from = year, to = year, QAQC = FALSE, 
               locType = loc_type, eventType = 'complete')

plotevs <- do.call(joinLocEvent, arglist)
ev_list <- plotevs |> select(EventID) 

#----- Visit notes -----
visit_notes <- do.call(joinVisitNotes, c(arglist, noteType = 'all')) |> 
  filter(!Note_Type %in% ("Quad_Species")) |> 
  arrange(Plot_Name, Note_Type) |> 
  select(Plot_Name, SampleYear, Note_Type, Sample_Info, Notes)

head(visit_notes)

#----- Stand data -----
stand <- do.call(joinStandData, arglist) |> 
  select(Plot_Name, SampleYear, cycle, PlotSlope, 
         IsStuntedWoodland, Stand_Structure, 
         Txt_Crown_Closure, Deer_Browse_Index, Microtopography, 
         Earthworms, Water_on_Plot_Code, 
         Txt_Understory_Low, Txt_Understory_Mid, Txt_Understory_High,
         Txt_Bare_Soil, Txt_Bryophyte, Txt_Lichen, Txt_Rock, Txt_Trampled, Txt_Water,
         Avg_Height_Codom, Avg_Height_Inter, Note = StandNotes)
head(stand)

#----- Stand heights -----
treeht <- get("StandTreeHeights_NETN", envir = VIEWS_NETN) |> 
  filter(EventID %in% ev_list$EventID) |> 
  select(Plot_Name, CrownClassCode, CrownClassLabel, TagCode, Height) |> 
  arrange(Plot_Name, CrownClassLabel, TagCode)

head(treeht)

#----- Stand disturbances -----
dist <- do.call(joinStandDisturbance, arglist) |> 
  select(Plot_Name, SampleYear, cycle, Disturbance = DisturbanceLabel, ThresholdCode, 
         ThresholdLabel, CoverClass = DisturbanceCoverClassLabel, Note = DisturbanceNote) 
head(dist)

#---- Tree data -----
tree <- do.call(joinTreeData, arglist) |> 
  select(Plot_Name, SampleYear, cycle, ScientificName, Tag = TagCode, Fork, Azimuth, Distance,
         DBH = DBHcm, DBHVer = IsDBHVerified, Status = TreeStatusCode, 
         CrownClass = CrownClassCode,
         Decay = DecayClassCode, HWACode, BBDCode, Note = TreeEventNote)

tree_sum <- tree |> group_by(Plot_Name, Status, ScientificName) |> 
  summarize(num_stems = n()) |> ungroup()


# Color code statuses in table so dead are grey, live are white, and NL or EX are bold (or something like that)

#----- Tree conditions -----
treecond <- do.call(joinTreeConditions, arglist[1:5]) |> 
  select(Plot_Name, SampleYear, ScientificName, Tag = TagCode, Status = TreeStatusCode,
         num_cond, AD:VIN_C) 

treecondl <- treecond |> pivot_longer(cols = c(AD:VIN_C), 
                                      values_to = "Present", 
                                      names_to = "Condition") |> 
  filter(Present > 0) |> 
  arrange(Plot_Name, Tag, Condition) |> select(-Present)

head(treecondl)

#----- Tree foliage conditions -----
treefol <- do.call(joinTreeFoliageCond, 
                   list(park = park, from = year, to = year, QAQC = FALSE, locType = "all",
                        valueType = "classes")) |> 
  select(Plot_Name, SampleYear, ScientificName, Tag = TagCode, 
         Txt_Tot_Foliage_Cond:Txt_Leaf_Area_N)

treefoll <- treefol |> pivot_longer(cols = Txt_Leaves_Aff_C:Txt_Leaf_Area_N,
                                    values_to = "Pct",
                                    names_to = "Condition") |> 
  filter(!Pct %in% "0%") |> 
  mutate(type = ifelse(grepl("Txt_Leaves_Aff", Condition), "Pct_Leaves", "Pct_Leaf_Area"),
         cond = substr(Condition, nchar(Condition), nchar(Condition))) |> 
  select(-Condition) |> 
  pivot_wider(names_from = type, values_from = Pct)

#----- Microplots -----
saps <- do.call(joinMicroSaplings, arglist) |> 
  select(Plot_Name, SampleYear, Micro = MicroplotCode, ScientificName, DBHcm, Count) |> 
  arrange(Plot_Name, Micro, Count, ScientificName, DBHcm)

head(saps)

seeds <- do.call(joinMicroSeedlings, arglist) |> 
  select(Plot_Name, SampleYear, Micro = MicroplotCode, ScientificName, 
         Seedlings_15_30cm, Seedlings_30_100cm, Seedlings_100_150cm, Seedlings_Above_150cm) |> 
  arrange(Plot_Name, Micro, ScientificName)

head(seeds)

shrubs <- do.call(joinMicroShrubData, arglist) |> 
  select(Plot_Name, SampleYear, ScientificName, Txt_Cov_B, Txt_Cov_UL, Txt_Cov_UR) |> 
  pivot_longer(cols = Txt_Cov_B:Txt_Cov_UR, 
               values_to = "Pct_Cover",
               names_to = "Micro") |> 
  mutate(Micro = substr(Micro, 9, nchar(Micro)))

#----- Quadrats -----
quaddata <- do.call(joinQuadData, arglist) |> 
  select(Plot_Name, SampleYear, CharacterLabel, Txt_Cov_UC:Txt_Cov_UL)

quad_tramp <- get("QuadNotes_NETN", envir = VIEWS_NETN) 

quad_tramp2 <- quad_tramp %>% 
  filter(EventID %in% ev_list$EventID) %>% 
  select(Plot_Name, SampleYear, QuadratCode, SQQuadCharCode, IsTrampled) %>% 
  unique() |> 
  mutate(Trampled = ifelse(IsTrampled == TRUE, "X", NA_character_)) |> 
  select(-IsTrampled, -SQQuadCharCode)

head(quad_tramp2)

quad_tramp_w <- quad_tramp2 |> pivot_wider(names_from = "QuadratCode", 
                                           values_from = "Trampled") |> 
  arrange(Plot_Name)

quadspp <- do.call(joinQuadSpecies, arglist) |> 
  select(Plot_Name, SampleYear, ScientificName, Txt_Cov_UC:Txt_Cov_UL, Germ = IsGerminant) |> 
  arrange(Plot_Name, ScientificName, desc(Germ))

#----- Additional Species -----
addspp <- do.call(joinAdditionalSpecies, arglist) |> 
  select(Plot_Name, SampleYear, ScientificName, Note)

head(addspp)

#----- CWD -----
cwd <- get("CWD_NETN", env = VIEWS_NETN) %>% 
  filter(EventID %in% ev_list$EventID) |> 
  select(Plot_Name, SampleYear, TransectCode,  Distance, ScientificName, Diameter, 
         MultiCrossCode, Length, IsHollow, Decay = DecayClassCode, CWDNote) |> 
  arrange(Plot_Name, TransectCode, ScientificName)

head(cwd)

#----- Soils -----
soil <- get("SoilSample_NETN", env = VIEWS_NETN) |> 
  filter(EventID %in% ev_list$EventID) |> 
  select(Plot_Name, SampleYear, Sample = SampleSequenceCode, Horizon = SoilLayerCode, Depth_cm, Note) |> 
  mutate(hor_order = case_when(Horizon == "L" ~ 1,
                               Horizon == "O" ~ 2,
                               Horizon == "A" ~ 3,
                               Horizon == "T" ~ 4, 
                               TRUE ~ 5)) |> 
  arrange(Plot_Name, Sample, hor_order)

