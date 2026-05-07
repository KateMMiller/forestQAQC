#-----------------------------------------
# Generate list of plots for NETN and MIDN
#-----------------------------------------
#update years and parks annually
#path = "C:/NETN/Forest_Health/2025/Plot_info/"
path = getwd()

library(tidyverse)
library(forestNETN)
importData()

curr_year = 2026
prevyr_ACAD = 2022  
prevyr_NHP = 2022  
parks_NHP = c('MORR', 'ROVA', 'WEFA') 
panel_ACAD = 1
panel_NHP = 4
#treemap_from = 2019 
#treemap_to = 2021

NETN <- rbind(joinLocEvent(park = "ACAD", from = prevyr_ACAD, panel = panel_ACAD) |> select(Plot_Name, ParkSubUnit),
              joinLocEvent(park = parks_NHP, from = prevyr_NHP, panel = panel_NHP) |> select(Plot_Name, ParkSubUnit)) |> 
        mutate(Park = substr(Plot_Name, 1, 4), 
               Plot = as.numeric(substr(Plot_Name, 6, 8)),
               Unit = substr(ParkSubUnit, 6, nchar(ParkSubUnit))) |> select(Park, Plot, Unit) 

write.csv(NETN, file = paste0(path, "NETN_plot_list_", curr_year, ".csv"), row.names = F)

# Restart R Session
library(tidyverse)
library(forestMIDN)
importData()
path = getwd()
curr_year = 2026

MIDN1 = c("FRSP", "RICH", "PETE","GEWA", "THST")
prevyr_MIDN1 = 2022
panel_MIDN1 = 4
MIDN2 = c("VAFO", "HOFU", "GETT", "APCO", "BOWA","COLO")
prevyr_MIDN2 = 2022
panel_MIDN2 = 3
prevyr_ASIS = 2022
panel_ASIS = 2

#treemap_from = 2019 
#treemap_to = 2021

MIDN <- rbind(joinLocEvent(park = MIDN1, from = prevyr_MIDN1, panel = panel_MIDN1) |> select(Plot_Name, ParkSubUnit), 
              joinLocEvent(park = MIDN2, from = prevyr_MIDN2, panel = panel_MIDN2) |> select(Plot_Name, ParkSubUnit),
              joinLocEvent(park = 'ASIS', from = prevyr_ASIS, panel = panel_ASIS) |> select(Plot_Name, ParkSubUnit)) |> 
        mutate(Park = substr(Plot_Name, 1, 4), 
               Plot = as.numeric(substr(Plot_Name, 6, 8)),
               Unit = substr(ParkSubUnit, 6, nchar(ParkSubUnit))
               ) |> select(Park, Plot, Unit) |> 
  arrange(Park, Plot)


write.csv(MIDN, file = paste0(path, "/MIDN_plot_list_", curr_year, ".csv"), row.names = F)
path

