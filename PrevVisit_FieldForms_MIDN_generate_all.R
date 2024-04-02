#------------------------------------------------
# Generate Printed Previous Visit Field Forms
#------------------------------------------------

# The scripts below will save paged html files of each plot per park and year
# to a FieldForms folder on your desktop. These are the pdfs that are printed and 
# referred to in the field for plot info, trees, saplings, and quadrat species

#----- Set up params ----- #++++ UPDATE ANNUALLY ++++
MIDN1 <- c("FRSP", "PETE", "RICH")
MIDN2 <- c("APCO", "BOWA", "GETT", "HOFU", "VAFO")
NCBN <- c("GEWA", "THST")

prevyr_MIDN1 = 2021
prevyr_MIDN2 = 2019
prevyr_NCBN = 2021
prevyr_COLO = 2019
  
panel_MIDN1 = 2
panel_MIDN2 = 1
panel_NCBN = 2
panel_COLO = 1

treemap_from = 2019 
treemap_to = 2021

parks <- c(MIDN1, MIDN2, NCBN, "COLO")
years = c(rep(prevyr_MIDN1, length(MIDN1)), rep(prevyr_MIDN2, length(MIDN2)), 
          rep(prevyr_NCBN, length(NCBN)), prevyr_COLO)
panels <- c(rep(panel_MIDN1, length(MIDN1)), rep(panel_MIDN2, length(MIDN2)), 
            rep(panel_NCBN, length(NCBN)), panel_COLO)

#----- Imports/libraries -----
library(tidyverse)
library(knitr)
library(rmarkdown)
library(forestNETN)
library(pagedown)
library(pdftools)

source("PrevVisit_modules\\PrevVisit_functions.R")
importData()

#----- Set up Directory for output to save to -----
# If The Field_Forms or MIDN_Tree_Maps folders don't exist on your desktop, they 
# will be added. Note that the commented out code is for local desktop address, 
# wherease the uncommented code is for your OneDrive desktop location. Use the 
# one that works for you.

# Field form directory for OneDrive - potentially slower if slow internet connection
path <- paste0(file.path(Sys.getenv("USERPROFILE"),"OneDrive - DOI\\Desktop",fsep="\\"),
               "\\Field_Forms\\MIDN\\")

## Field form directory for local desktop
# path <- paste0(file.path(Sys.getenv("USERPROFILE"), "Desktop", fsep = "\\"), "\\Field_Forms\\MIDN\\")

# Tree map directory for OneDrive - potentially slower because requires internet connection
path_trmaps <- paste0(file.path(Sys.getenv("USERPROFILE"),"OneDrive - DOI\\Desktop",fsep="\\"),
                      "\\MIDN_Tree_Maps\\")

# Tree map directory for local desktop 
# path_trmaps <- paste0(file.path(Sys.getenv("USERPROFILE"), "Desktop", fsep = "\\"), 
#                       "\\MIDN_Tree_Maps\\")

# Check if directories for each output type exist. If not, the files are created
# Park-level tree and quad data go into "Field_Forms"
if(!dir.exists(path)){dir.create(path)}

# Individual tree, sapling and quad plot files go into "Field_Forms/MIDN/indiv" folder.
if(!dir.exists(paste0(path, "indiv"))){
  dir.create(paste0(path, "indiv"))}

# Individual full visit of plot files go into "Field_Forms/MIDN/indiv_all" folder.
if(!dir.exists(paste0(path, "indiv_all"))){
  dir.create(paste0(path, "indiv_all\\"))}

# Tree maps go into "Desktop\\MIDN_Tree_Maps" folder
if(!dir.exists(path_trmaps)){dir.create(path_trmaps)}

#----- Tree Maps -----
# Uncomment to clear out the folder from the previous year
# do.call(file.remove, list(list.files(path_trmaps, full.names = TRUE)))

plotTreeMap(park = MIDN1, from = prevyr_MIDN1, to = treemap_to, 
            panel = panel_MIDN1, path = path_trmaps, output_to = 'file')

plotTreeMap(park = MIDN2, from = prevyr_MIDN2, to = treemap_to, 
            panel = panel_MIDN2, path = path_trmaps, output_to = 'file')

plotTreeMap(park = NCBN, from = prevyr_NCBN, to = treemap_to, 
            panel = panel_MIDN1, path = path_trmaps, output_to = 'file')

plotTreeMap(park = "COLO", from = prevyr_COLO, to = treemap_to, 
            panel = panel_COLO, path = path_trmaps, output_to = 'file')

# plotTreeMap(park = "SAHI", from = 2023, to = 2023,
#             path = path_trmaps, output_to = 'file')

# plotTreeMap(park = "ASIS", from = 2019, to = 2019,
#             path = path_trmaps, output_to = 'file')

#----- Park-level Tree and Quadrat reports -----
##----- Render Functions to iterate on -----
# Uncomment to clear out the folder from the previous year
# do.call(file.remove, list(list.files(paste0(path, "\\MIDN\\indiv\\"), full.names = TRUE)))
# do.call(file.remove, list(list.files(paste0(path), full.names = TRUE)))

render_reports <- function(plot, pv_year, panel, rmd, name){
  park = substr(plot, 1, 4)
  plotname = plot
  pnl = panel
  render(input = rmd,
         params = list(plot_name = plotname, 
                       year = as.numeric(pv_year), 
                       panel = pnl,
                       print = TRUE),
         output_file = paste0(path, "indiv\\", 
                              plot, "_", pv_year, "_", name, ".html"))
  }

rmdtr <- "PrevVisit_modules\\PrevVisit_3A_Tree_Measurements.Rmd"
rmdqd <- "PrevVisit_modules\\PrevVisit_4A_Quadrat_Data_MIDN.Rmd"

# Since revisits are different years, you have to run for each set, then reset params 
# and run for next set.
##----- params for MIDN1 parks -----
park <- MIDN1
year <- prevyr_MIDN1
panel <- panel_MIDN1 

##----- Source from compile script -----
source("PrevVisit_modules\\PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = prevyr_MIDN1, panel = panel_MIDN1, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = prevyr_MIDN1, panel = panel_MIDN1, rmdqd, "Quads")) # quads

##----- params for MIDN2 parks -----
park <- MIDN2
year <- prevyr_MIDN2
panel = panel_MIDN2

##----- Source from compile script -----
source("PrevVisit_modules\\PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below
#plots <- plots[grepl("VAFO", plots)] # not all VAFO tree reports rendered the first time
# 39 panel 4 plots

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = prevyr_MIDN2, panel = panel_MIDN2, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = prevyr_MIDN2, panel = panel_MIDN2, rmdqd, "Quads")) # quads

##----- params for NCBN parks -----
park <- NCBN
year <- prevyr_NCBN
panel = panel_NCBN

##----- Source from compile script -----
source("PrevVisit_modules\\PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = prevyr_NCBN, panel = panel_NCBN, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = prevyr_NCBN, panel = panel_NCBN, rmdqd, "Quads")) # quads

##----- params for COLO parks -----
park <- "COLO"
year <- prevyr_COLO
panel = panel_COLO

##----- Source from compile script -----
source("PrevVisit_modules\\PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = prevyr_COLO, panel = panel_COLO, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = prevyr_COLO, panel = panel_COLO, rmdqd, "Quads")) # quads

#----- Sapling reports by parks -----
##----- Generates park-level reports, so don't have a bunch of blank space b/t page breaks. 

parks <- c(MIDN1, MIDN2, NCBN, "COLO")
years = c(rep(prevyr_MIDN1, length(MIDN1)), rep(prevyr_MIDN2, length(MIDN2)), 
          rep(prevyr_NCBN, length(NCBN)), prevyr_COLO)
panels <- c(rep(panel_MIDN1, length(MIDN1)), rep(panel_MIDN2, length(MIDN2)), 
            rep(panel_NCBN, length(NCBN)), panel_COLO)

render_saps <- function(parkcode, pv_year, panel){
  pnl = panel
  render(input = "PrevVisit_modules\\PrevVisit_FieldForms_MIDN_Saplings_by_Park.Rmd",
         params = list(year = as.numeric(pv_year), 
                       park = parkcode,
                       panel = pnl,
                       print = TRUE),
         output_file = paste0(path, parkcode, "_", pv_year, "_Saplings.html"))
}

purrr::pmap(list(parks, years, panels), ~render_saps(..1, ..2, ..3)) 

#----- Convert Tree, Sapling, & Quad htmls to pdf -----
##----- Convert individual html to pdf (ignore warnings unless it doesn't work) -----
html_list <- list.files(paste0(path, "indiv\\"), pattern = '.html', full.names = T)
pdf_list <- paste0(substr(html_list, 1, nchar(html_list) - 4), "pdf")
walk2(html_list, pdf_list, ~pagedown::chrome_print(.x, .y))

sap_list <- list.files(path, pattern = 'Saplings.html', full.names = T)
sap_pdf <- paste0(substr(sap_list, 1, nchar(sap_list) - 4), "pdf")
walk2(sap_list, sap_pdf, ~pagedown::chrome_print(.x, .y))

##----- Combine park-level pdfs into 1 pdf per module -----
parks
years

combine_tree_pdfs <- function(park, year, path){
  pdf_list <- list.files(paste0(path, "\\indiv\\"), pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Trees"), pdf_list)]
  park_trees <- trees[grep((park), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, park, "_", year, "_Trees.pdf"))
}

combine_quad_pdfs <- function(park, year, path){
  pdf_list <- list.files(paste0(path, "\\indiv\\"), pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Quads"), pdf_list)]
  park_trees <- trees[grep((park), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, park, "_", year, "_Quads.pdf"))
}

purrr::map2(parks, years, ~combine_tree_pdfs(.x, .y, path))
purrr::map2(parks, years, ~combine_quad_pdfs(.x, .y, path))

#---- Render plot viewers for each park -----
render_viewer <- function(parkcode, yearpv, panel){
  year = as.numeric(yearpv)
  pnl = panel
  render(input = "PrevVisit_Plot_Viewer_MIDN_All.Rmd",
         params = list(parkcode = parkcode, 
                       yearpv = year, 
                       panel = pnl,
                       print = FALSE),
         output_file = paste0(path, parkcode, "_", year, "_Plot_Viewer.html"))
}

#render_poss <- possibly(.f = render_viewer, otherwise = NULL)

# running through a few at a time b/c bogs down laptop
#render_viewer("APCO", 2019, 1)
purrr::pmap(list(parks, years, panels), ~render_viewer(..1, ..2, ..3))

render_viewer("FRSP", 2021, 2)
