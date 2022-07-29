# NETN
# parks <- joinLocEvent(from = 2019, to = 2019) %>% select(ParkUnit) %>% 
#   arrange(ParkUnit) %>% unique()
parks = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
path = "D:/NETN/Monitoring_Projects/Forest_Health/Photopoints/2022/"
lapply(seq_along(parks), function(x){dir.create(paste0(path, parks[x]))})


library(forestNETN)
library(tidyverse)
importData()

plot_list1 <- joinLocEvent(from = 2016, to = 2017) %>% filter(ParkUnit != "ACAD") %>% 
  select(Plot_Name, ParkUnit, PlotCode) %>% arrange(Plot_Name)

plot_list1

acad_list <- joinLocEvent(park = "ACAD", from = 2018, to = 2018) %>% 
  select(Plot_Name, ParkUnit, PlotCode) %>% arrange(Plot_Name)

plot_list <- rbind(plot_list1, acad_list)

# MIDN
library(forestMIDN)
library(tidyverse)
importData()

# parks <- joinLocEvent(from = 2019, to = 2019) %>% select(ParkUnit) %>% 
#   arrange(ParkUnit) %>% unique()
parks = c("APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", 
          "GEWA", "HOFU", "PETE", "RICH", "THST", "VAFO")
path = 'D:/MIDN/ForestVegetation/2022/03_Data/Photos/PhotoPoints'
lapply(seq_along(parks), function(x){dir.create(paste0(path, parks[x]))})


plot_list1 <- joinLocEvent(from = 2016, to = 2019, panel = 2:3) %>% 
  filter(!ParkUnit %in% c("FRSP", "PETE", "RICH", "GEWA", "THST")) %>% 
  select(Plot_Name, ParkUnit, PlotCode) %>% arrange(Plot_Name)

plot_list2 <- joinLocEvent(park = c("FRSP", "PETE", "RICH", "GEWA", "THST"),
                           from = 2016, to = 2019, panel = 4) %>% 
  select(Plot_Name, ParkUnit, PlotCode) %>% arrange(Plot_Name)

plot_list2

plot_list <- rbind(plot_list1, plot_list2)
plot_list

asis <- joinLocEvent(park = "ASIS") %>% select(Plot_Name) %>% arrange(Plot_Name)
asis
