#--------------------------------
# Compiling data for Previous Visit Field Forms
#--------------------------------

#----- Load libraries
library(tidyverse)
library(forestMIDN)
library(htmltools)
library(knitr)
library(kableExtra)
#importData()
#----- Compile data -----
# params <- data.frame(parkcode = "APCO", plot_name = "APCO-262", yearpv = 2018)
# park <- params$parkcode
# year <- as.numeric(params$yearpv)

arglist = list(park = park, from = year, to = year, QAQC = FALSE)

plotevs <- do.call(joinLocEvent, arglist) |> 
  mutate(Unit = ifelse(nchar(ParkSubUnit) > 4,                                                                           
                       substr(ParkSubUnit, 6, nchar(ParkSubUnit)),
                       paste0("None"))) 

plotevs$Unit <- gsub("_", " ", plotevs$Unit)

ev_list <- plotevs |> select(EventID) 
head(plotevs)

#----- Visit notes -----
visit_notes <- do.call(joinVisitNotes, c(arglist, noteType = 'all')) |> 
  filter(!Note_Type %in% ("Quad_Species")) |> 
  arrange(Plot_Name, Note_Type) |> 
  select(Plot_Name, SampleYear, Note_Type, Sample_Info, Notes)

head(visit_notes)

#----- Stand data -----
stand <- do.call(joinStandData, arglist) |> 
  select(Plot_Name, SampleYear, cycle, PlotSlope, 
         Stand_Structure, Txt_Crown_Closure, Deer_Browse_Index, Microtopography, 
         Water_on_Plot_Code, 
         Txt_Understory_Low, Txt_Understory_Mid, Txt_Understory_High,
         Txt_Bare_Soil, Txt_Bryophyte, Txt_Lichen, Txt_Rock, Txt_Trampled, Txt_Water,
         Avg_Height_Codom, Avg_Height_Inter, Note = StandNotes) 

head(stand)

#---- Stand disturbances -----
sdist <- do.call(joinStandDisturbance, arglist) |> 
  select(Plot_Name, Code = DisturbanceCode)
head(sdist)

#----- Stand heights -----
treeht <- get("StandTreeHeights_MIDN", envir = VIEWS_MIDN) |> 
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

head(tree)

#----- Tree conditions -----
treecond <- do.call(joinTreeConditions, arglist) |> 
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

head(treefoll)

#----- Microplots -----
saps <- do.call(joinMicroSaplings, c(arglist, status = 'live')) |> 
  select(Plot_Name, SampleYear, Micro = MicroplotCode, Tag = TagCode, 
         Fork, ScientificName, DBHcm, Status = SaplingStatusCode) |> 
  arrange(Plot_Name, Micro, ScientificName, DBHcm)

head(saps)

seeds <- do.call(joinQuadSeedlings, arglist) |> 
  select(Plot_Name, SampleYear, Quad = QuadratCode, ScientificName, 
         Seedlings_15_30cm, Seedlings_30_100cm, Seedlings_100_150cm, 
         Seedlings_Above_150cm, BrowsedCount, Txt_Cov) |> 
  arrange(Plot_Name, Quad, ScientificName)

head(seeds)

shrubs <- do.call(joinMicroShrubData, arglist) |> 
  select(Plot_Name, SampleYear, ScientificName, Txt_Cov_B, Txt_Cov_UL, Txt_Cov_UR) |> 
  pivot_longer(cols = Txt_Cov_B:Txt_Cov_UR, 
               values_to = "Pct_Cover",
               names_to = "Micro") |> 
  mutate(Micro = substr(Micro, 9, nchar(Micro)))

head(shrubs)

#----- Quadrats -----
quaddata <- do.call(joinQuadData, arglist) |> 
  select(Plot_Name, SampleYear, Species = CharacterLabel, Txt_Cov_A2:Txt_Cov_CC) |> 
  rename_with(stringr::str_replace, pattern = "Txt_Cov_", replacement = "") |> 
  mutate(Note = NA_character_)

head(quaddata)

quad_tramp <- get("QuadNotes_MIDN", envir = VIEWS_MIDN) 

quad_tramp_note <- quad_tramp |> group_by(Plot_Name, SampleYear) |> 
  filter(!is.na(SQQuadCharNotes)) |> 
  summarize(Note = toString(unique(SQQuadCharNotes)))

quad_tramp2 <- quad_tramp %>% 
  filter(EventID %in% ev_list$EventID) %>% 
  select(Plot_Name, SampleYear, QuadratCode, SQQuadCharCode, IsTrampled) %>% # Note = SQQuadCharNotes) %>% 
  unique() |> 
  mutate(Trampled = ifelse(IsTrampled == TRUE, "X", NA_character_)) |> 
  select(-IsTrampled, -SQQuadCharCode)

quad_tramp_w <- quad_tramp2 |> pivot_wider(names_from = "QuadratCode", 
                                           values_from = "Trampled") |> 
  arrange(Plot_Name) |> 
  mutate(Species = "Trampled") |> 
  select(Plot_Name, SampleYear, Species, 
         A2, A5, A8, AA, B2, B5, B8, BB, C2, C5, C8, CC) 

quad_tramp_w2 <- left_join(quad_tramp_w, quad_tramp_note, by = c("Plot_Name", "SampleYear"))

quadspp <- do.call(joinQuadSpecies, arglist) |> 
  select(Plot_Name, SampleYear, Species = ScientificName, 
         Txt_Cov_A2:Txt_Cov_CC, Note = QuadSppNote) |> 
  arrange(Plot_Name, Species)|> 
  rename_with(stringr::str_replace, pattern = "Txt_Cov_", replacement = "") 

quad_all <- rbind(quad_tramp_w2, quaddata, quadspp)
head(quad_all)

#----- Additional Species -----
addspp <- do.call(joinAdditionalSpecies, arglist) |> 
  select(Plot_Name, SampleYear, ScientificName, Note)

head(addspp)

#----- CWD -----
cwd <- get("CWD_MIDN", env = VIEWS_MIDN) %>% 
  filter(EventID %in% ev_list$EventID) |> 
  select(Plot_Name, SampleYear, TransectCode, Distance, ScientificName, Diameter, 
         MultiCrossCode, Length, IsHollow, Decay = DecayClassCode, CWDNote) |> 
  arrange(Plot_Name, TransectCode, ScientificName)

head(cwd)

