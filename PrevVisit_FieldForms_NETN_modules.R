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

# Individual tree and quad plot files go into "Field_Forms/indiv" folder.
if(!dir.exists(paste0(path, "indiv\\", "NHPs"))){
  dir.create(paste0(path, "\\indiv\\NHPs\\"))}

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

render_trees <- function(plot, pv_year, type = "NHPs"){
  park = substr(plot, 1, 4)
  plotname = plot
  outfile = ifelse(type == 'NHPs', 
                   paste0(path, "indiv\\NHPs\\", plot, "_", pv_year, "_Trees.html"),
                   paste0(path, "indiv\\", plot, "_", pv_year, "_Trees.html"))
  render(input = rmdtr,
         params = list(plot_name = plotname, 
                       year = as.numeric(pv_year), 
                       print = TRUE),
         output_file = outfile)
}

rmdqd <- "PrevVisit_4A_Quadrat_Data_NETN.Rmd"

render_quads <- function(plot, pv_year, type = "NHPs"){
  park = substr(plot, 1, 4)
  plotname = plot
  outfile = ifelse(type == 'NHPs', 
                   paste0(path, "indiv\\NHPs\\", plot, "_", pv_year, "_Quads.html"),
                   paste0(path, "indiv\\", plot, "_", pv_year, "_Quads.html"))
  render(input = rmdqd,
         params = list(plot_name = plotname, 
                       year = as.numeric(pv_year), 
                       print = TRUE),
         output_file = outfile)
}

# Since NHPs and ACAD are off by a year, you have to run for all NHPs, then reset
# params and rerun remaining code for all ACAD plots 
# Note: Sourcing specific lines in compile script to speed process
# Have to update line numbers if code changes (or just source entire script)

#--------------------------
#       NHP Workflow
#--------------------------
##----- params for NHPs -----
year <- 2018
park <- c("MABI", "MIMA", "SAGA", "SARA")

##----- Source from compile script -----
source("PrevVisit_FieldForms_NETN_compile.R") 

plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports- NHP -----
map(plots, ~render_trees(., pv_year = 2018)) # trees
map(plots, ~render_quads(., pv_year = 2018)) # quads

#----- Convert Tree and Quad htmls to pdf -----
##----- Convert individual html to pdf (ignore warnings unless it doesn't work) -----
html_list <- list.files(paste0(path, "indiv\\NHPs\\"), pattern = '.html', full.names = T)
pdf_list <- paste0(substr(html_list, 1, nchar(html_list) - 4), "pdf")
walk2(html_list, pdf_list, ~pagedown::chrome_print(.x, .y))

##----- Combine park-level pdfs into 1 pdf per module -----
combine_tree_pdfs <- function(park, path, year){
  pdf_list <- list.files(paste0(path, "indiv\\NHPs\\"), pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Trees"), pdf_list)]
  park_trees <- trees[grep((park), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, park, "_", year, "_Trees.pdf"))
}

combine_quad_pdfs <- function(park, path, year){
  pdf_list <- list.files(paste0(path, "indiv\\NHPs\\"), pattern = ".pdf", full.names = TRUE)
  quads <- pdf_list[grep(("Quads"), pdf_list)]
  park_quads <- quads[grep((park), quads)]
  pdftools::pdf_combine(input = park_quads,
                        output = paste0(path, park, "_", year, "_Quads.pdf"))
}

nhps <- c("MABI", "MIMA", "SAGA", "SARA")
purrr::map(nhps, ~combine_tree_pdfs(., path, year = 2018))
purrr::map(nhps, ~combine_quad_pdfs(., path, year = 2018))

#------------------------------
#        ACAD Workflow 
#------------------------------
##----- params for ACAD -----
year <- 2019
park <- "ACAD"

##----- Source from compile script -----
source("PrevVisit_FieldForms_NETN_compile.R") 

plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports- ACAD -----
map(plots, ~render_trees(., pv_year = 2019, type = "ACAD")) # trees
map(plots, ~render_quads(., pv_year = 2019, type = "ACAD")) # quads

#----- Convert Tree and Quad htmls to pdf -----
##----- Convert individual html to pdf (ignore warnings unless it doesn't work) -----
html_list <- list.files(paste0(path, "indiv\\"), pattern = '.html', full.names = T)
pdf_list <- paste0(substr(html_list, 1, nchar(html_list) - 4), "pdf")
walk2(html_list, pdf_list, ~pagedown::chrome_print(.x, .y))

# Combine ACAD forms by subunit
plot_subs <- plotevs[, c("Plot_Name", "ParkSubUnit")] # plot list to iterate on below
ACAD_MDIE = plot_subs$Plot_Name[plot_subs$ParkSubUnit == "ACAD_MDI_East"]
ACAD_MDIW = plot_subs$Plot_Name[plot_subs$ParkSubUnit == "ACAD_MDI_West"]
ACAD_IAH = plot_subs$Plot_Name[plot_subs$ParkSubUnit == "ACAD_Isle_au_Haut"]
ACAD_SCH = plot_subs$Plot_Name[plot_subs$ParkSubUnit == "ACAD_Schoodic"]

combine_tree_pdfs_ACAD <- function(plot_list, subunit, path, year){
  pdf_list <- list.files(paste0(path, "indiv\\"), pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Trees"), pdf_list)]
  grep_list <- paste(plot_list, collapse = "|")
  park_trees <- trees[grep((grep_list), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, "ACAD_", subunit, "_", year, "_Trees.pdf"))
}

combine_tree_pdfs_ACAD(ACAD_MDIE, "MDI-E", path, 2019)
combine_tree_pdfs_ACAD(ACAD_MDIW, "MDI-W", path, 2019)
combine_tree_pdfs_ACAD(ACAD_IAH, "IAH", path, 2019)
combine_tree_pdfs_ACAD(ACAD_SCH, "SCH", path, 2019)

combine_quad_pdfs_ACAD <- function(plot_list, subunit, path, year){
  pdf_list <- list.files(paste0(path, "indiv\\"), pattern = ".pdf", full.names = TRUE)
  quads <- pdf_list[grep(("Quads"), pdf_list)]
  grep_list <- paste(plot_list, collapse = "|")
  park_quads <- quads[grep((grep_list), quads)]
  pdftools::pdf_combine(input = park_quads,
                        output = paste0(path, "ACAD_", subunit, "_", year, "_Quads.pdf"))
}

combine_quad_pdfs_ACAD(ACAD_MDIE, "MDI-E", path, 2019)
combine_quad_pdfs_ACAD(ACAD_MDIW, "MDI-W", path, 2019)
combine_quad_pdfs_ACAD(ACAD_IAH, "IAH", path, 2019)
combine_quad_pdfs_ACAD(ACAD_SCH, "SCH", path, 2019)


#---- Render plot viewers for each park -----
#source("PrevVisit_FieldForms_NETN_compile.R") 
parks = c("ACAD", "MABI", "MIMA", "SAGA", "SARA")
years = c(2019, rep(2018, 4))

render_viewer <- function(park, year){
  park_code = park
  yr = as.numeric(year)
  render(input = "PrevVisit_Plot_Viewer_NETN_All.Rmd",
         params = list(parkcode = park_code, 
                       yearpv = yr, 
                       print = FALSE),
         output_file = paste0(path, park, "_", yr, "_Plot_Viewer.html"))
}

#render_poss <- possibly(.f = render_viewer, otherwise = NULL)

# running through a few at a time b/c bogs down laptop
purrr::map2(parks, years, ~render_viewer(.x, .y))

render_viewer("MABI", 2018)

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
#   render(input = "PrevVisit_FieldForms_NETN_paged_plot.Rmd",
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
