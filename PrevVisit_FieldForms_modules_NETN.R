#------------------------------------------------
# Generate Previous Visit Field Forms by Module
#------------------------------------------------

# The scripts below will save paged html files of each plot per park and year
# to a FieldForms folder on your desktop. If The Field_Forms folder doesn't
# exist on your desktop, it will be added. Same goes for NETN_Tree_Maps folder

path <- paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 
               "\\Field_Forms\\")

# Park-level tree and quad data go into "Field_Forms"
if(!dir.exists(path)){dir.create(path)}

# Individual tree and quad plot files go into "Field_Forms/indiv" folder.
if(!dir.exists(paste0(path, "indiv"))){
  dir.create(paste0(path, "\\indiv\\"))}

# Individual full visit of plot files go into "Field_Forms/indiv_all" folder.
if(!dir.exists(paste0(path, "indiv_all"))){
  dir.create(paste0(path, "indiv_all\\"))}

# Tree maps go into "NETN_Tree_Maps" folder on Desktop
path_trmaps <- paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 
                      "\\NETN_Tree_Maps\\")

if(!dir.exists(path_trmaps)){dir.create(path_trmaps)}

#----- Imports/libraries -----
library(tidyverse)
library(knitr)
library(rmarkdown)
library(forestNETN)
library(pagedown)
library(pdftools)

source("PrevVisit_functions.R")
importData()

#----- Tree Maps -----
parks = c("ACAD", "MABI", "MIMA", "SAGA", "SARA")
plotTreeMap(park = parks, from = 2018, to = 2019, 
            path = path_trmaps, output_to = 'file')

#----- Park-level Tree and Quadrat reports -----
##----- Render Functions to iterate on -----
rmdtr <- "PrevVisit_3A_Tree_Measurements.Rmd"

render_trees <- function(plot, pv_year){
  park = substr(plot, 1, 4)
  render(input = rmdtr,
         params = list(plot_name = plot, 
                       year = as.numeric(pv_year), 
                       print = TRUE),
         output_file = paste0(path, "indiv\\", 
                              plot, "_", pv_year, "_Trees.html"))
}

rmdqd <- "PrevVisit_4A_Quadrat_Data.Rmd"

render_quads <- function(plot, pv_year){
  park = substr(plot, 1, 4)
  render(input = rmdqd,
         params = list(plot_name = plot, 
                       year = as.numeric(pv_year), 
                       print = TRUE),
         output_file = paste0(path, "indiv\\",
                              plot, "_", pv_year, "_Quads.html"))
}

# Since NHPs and ACAD are off by a year, you have to run for all NHPs, then reset
# params and rerun remaining code for all ACAD plots 
# Note: Sourcing specific lines in compile script to speed process
# Have to update line numbers if code changes (or just source entire script)

##----- params for NHPs -----
year <- 2018
park <- c("MABI", "MIMA", "SAGA", "SARA")

##----- Source from compile script -----
source("PrevVisit_FieldForms_NETN_compile.R") 

plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports- NHP -----
map(plots, ~render_trees(., pv_year = 2018)) # trees
map(plots, ~render_quads(., pv_year = 2018)) # quads

##----- params for ACAD -----
year <- 2019
park <- "ACAD"

##----- Source from compile script -----
source("PrevVisit_FieldForms_NETN_compile.R") 

plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports -----
map(plots, ~render_trees(., pv_year = 2019)) # trees
map(plots, ~render_quads(., pv_year = 2019)) # quads

#----- Convert Tree and Quad htmls to pdf -----
##----- Convert individual html to pdf (ignore warnings unless it doesn't work) -----
html_list <- list.files(paste0(path, "indiv\\"), pattern = '.html', full.names = T)
pdf_list <- paste0(substr(html_list, 1, nchar(html_list) - 4), "pdf")
walk2(html_list, pdf_list, ~pagedown::chrome_print(.x, .y))

##----- Combine park-level pdfs into 1 pdf per module -----
nhps <- c("MABI", "MIMA", "SAGA", "SARA")

combine_tree_pdfs <- function(park, path, year){
  pdf_list <- list.files(path, pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Trees"), pdf_list)]
  park_trees <- trees[grep((park), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, park, "_", year, "_Trees.pdf"))
}

purrr::map(nhps, ~combine_tree_pdfs(., path, year = 2018))
combine_tree_pdfs("ACAD", path, year = 2019)


combine_quad_pdfs <- function(park, path, year){
  pdf_list <- list.files(path, pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Quads"), pdf_list)]
  park_trees <- trees[grep((park), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, park, "_", year, "_Quads.pdf"))
}

purrr::map(nhps, ~combine_quad_pdfs(., path, year = 2018))
combine_quad_pdfs("ACAD", path, year = 2019)

#---- Render plot viewers for each park -----
parks = c("ACAD", "MABI", "MIMA", "SAGA", "SARA")
years = c(2019, rep(2018, 4))

render_viewer <- function(park, year){
  render(input = "PrevVisit_Plot_Viewer_All_NETN.Rmd",
         params = list(parkcode = park, 
                       yearpv = as.numeric(year), 
                       print = FALSE),
         output_file = paste0(path, park, "_", year, "_Plot_Viewer.html"))
}

#render_poss <- possibly(.f = render_viewer, otherwise = NULL)

# running through a few at a time b/c bogs down laptop
purrr::map2(parks, years, ~render_viewer(.x, .y))



#---- OPTIONAL: Render report of all visit data on a plot -----
# all_plots <- joinLocEvent(park = 'all', from = 2018, to = 2019) |> 
#   select(ParkUnit, Plot_Name, SampleYear, PanelCode) |> 
#   filter(ParkUnit == "ACAD" & PanelCode == 2 |
#            ParkUnit != "ACAD" & PanelCode == 1) |> 
#   arrange(Plot_Name)
# 
# plot_list <- all_plots$Plot_Name
# year_list <- all_plots$SampleYear
# plot_list
# year_list
# 
# render_all <- function(plot, pv_year){
#   park = substr(plot, 1, 4)
#   render(input = "PrevVisit_FieldForms_NETN.Rmd",
#          params = list(plot_name = plot, 
#                        year = as.numeric(pv_year), 
#                        print = FALSE),
#          output_file = paste0(path, "indiv_all\\",
#                               plot, "_", pv_year, "_Visits.html"))
# }
# render_poss <- possibly(.f = render_all, otherwise = NULL)
# 
# # running through a few at a time b/c bogs down laptop
# purrr::map2(plot_list[51:92], year_list[51:92], ~render_poss(.x, .y))
# 
# ##----- Convert individual html to pdf (ignore warnings unless it doesn't work) -----
# html_list <- list.files(paste0(path, "indiv_all\\"), pattern = '.html', full.names = T)
# pdf_list <- paste0(substr(html_list, 1, nchar(html_list) - 4), "pdf")
# walk2(html_list, pdf_list, ~pagedown::chrome_print(.x, .y))
# 
# ##----- Combine park-level pdfs into 1 pdf -----
# nhps <- c("MABI", "MIMA", "SAGA", "SARA")
# 
# combine_visit_pdfs <- function(park, path, year){
#   pdf_list <- list.files(paste0(path, "indiv_all\\"), pattern = ".pdf", full.names = TRUE)
#   park_list <- pdf_list[grep((park), pdf_list)]
#   pdftools::pdf_combine(input = park_list,
#                         output = paste0(path, park, "_", year, "_All_Visits.pdf"))
# }
# 
# purrr::map(nhps, ~combine_visit_pdfs(., path, year = 2018))
# combine_visit_pdfs("ACAD", path, year = 2019)
