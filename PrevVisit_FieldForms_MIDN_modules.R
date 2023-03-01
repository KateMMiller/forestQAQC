#------------------------------------------------
# Generate Previous Visit Field Forms by Module
#------------------------------------------------

# The scripts below will save paged html files of each plot per park and year
# to a FieldForms folder on your desktop. If The Field_Forms folder doesn't
# exist on your desktop, it will be added. Same goes for NETN_Tree_Maps folder

path <- paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 
               "\\Field_Forms\\MIDN\\")

# Park-level tree and quad data go into "Field_Forms"
if(!dir.exists(path)){dir.create(path)}

# Individual tree and quad plot files go into "Field_Forms/indiv" folder.
if(!dir.exists(paste0(path, "indiv"))){
  dir.create(paste0(path, "\\indiv\\"))}

# Tree maps go into "NETN_Tree_Maps" folder on Desktop
path_trmaps <- paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 
                      "\\MIDN_Tree_Maps\\")

if(!dir.exists(path_trmaps)){dir.create(path_trmaps)}

#----- Imports/libraries -----
library(tidyverse)
library(knitr)
library(rmarkdown)
library(forestMIDN)
library(pagedown)
library(pdftools)

source("PrevVisit_functions.R")
importData()

#----- Tree Maps -----
park_4 <- c("APCO", "BOWA", "COLO", "GETT", "HOFU", "VAFO") # Panel 4, sampled 2018
park_1 <- c("FRSP", "GEWA", "PETE", "RICH", "THST") # Panel 1, sampled 2019
SAHI <- "SAHI" # Sampled 2017

plotTreeMap(park = park_4, from = 2018, to = 2018, 
            path = path_trmaps, output_to = 'file')

plotTreeMap(park = park_1, from = 2019, to = 2019, 
            path = path_trmaps, output_to = 'file')

plotTreeMap(park = "SAHI", from = 2017, to = 2017,
            path = path_trmaps, output_to = 'file')

#params <- data.frame(parkcode = "APCO", plot_name = "APCO-262", yearpv = 2018)
#params <- data.frame(parkcode = "FRSP", plot_name = "FRSP-002", yearpv = 2019)

#----- Park-level Tree and Quadrat reports -----
##----- Render Functions to iterate on -----
render_reports <- function(plot, pv_year, rmd, name){
  park = substr(plot, 1, 4)
  plotname = plot
  render(input = rmd,
         params = list(plot_name = plotname, 
                       year = as.numeric(pv_year), 
                       print = TRUE),
         output_file = paste0(path, "indiv\\", 
                              plot, "_", pv_year, "_", name, ".html"))
  }

rmdtr <- "PrevVisit_3A_Tree_Measurements.Rmd"
rmdqd <- "PrevVisit_4A_Quadrat_Data_MIDN.Rmd"

# Since revisits are different years, you have to run for each set, then reset params 
# and run for next set.
##----- params for panel 4 -----
park <- c("APCO", "BOWA", "COLO", "GETT", "HOFU", "VAFO") # Panel 4, sampled 2018
year = 2018

##----- Source from compile script -----
source("PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below
#plots <- plots[grepl("VAFO", plots)] # not all VAFO tree reports rendered the first time
# 39 panel 4 plots

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = year, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = year, rmdqd, "Quads")) # quads

##----- params for panel 1 -----
park <- c("FRSP", "GEWA", "PETE", "RICH", "THST") # Panel 1, sampled 2019
year = 2019

##----- Source from compile script -----
source("PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below
length(plots) #51 panel 1 plots

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = year, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = year, rmdqd, "Quads")) # quads

##----- params for SAHI -----
park <- "SAHI" # Sampled 2017
year = 2017

##----- Source from compile script -----
source("PrevVisit_FieldForms_MIDN_compile.R") 
plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below

length(plots) # 4 SAHI plots

##----- Render Reports -----
map(plots, ~render_reports(., pv_year = year, rmdtr, "Trees")) # trees
map(plots, ~ render_reports(., pv_year = year, rmdqd, "Quads")) # quads

#----- Sapling reports by parks -----
##----- Generates park-level reports, so don't have a bunch of blank space b/t page breaks. 
park_4 <- c("APCO", "BOWA", "COLO", "GETT", "HOFU", "VAFO") # Panel 4, sampled 2018
park_1 <- c("FRSP", "GEWA", "PETE", "RICH", "THST") # Panel 1, sampled 2019
SAHI <- "SAHI" # Sampled 2017

parks <- c(park_4, park_1, SAHI)
years = c(rep(2018, length(park_4)), rep(2019, length(park_1)), 2017)

render_saps <- function(parkcode, pv_year){
  render(input = "PrevVisit_FieldForms_MIDN_Saplings_by_Park.Rmd",
         params = list(year = as.numeric(pv_year), 
                       park = parkcode,
                       print = TRUE),
         output_file = paste0(path, parkcode, "_", pv_year, "_Saplings.html"))
}

purrr::map2(parks, years, ~render_saps(.x, .y)) # quads

#----- Convert Tree, Sapling, & Quad htmls to pdf -----
##----- Convert individual html to pdf (ignore warnings unless it doesn't work) -----
html_list <- list.files(paste0(path, "indiv\\"), pattern = '.html', full.names = T)
pdf_list <- paste0(substr(html_list, 1, nchar(html_list) - 4), "pdf")
walk2(html_list, pdf_list, ~pagedown::chrome_print(.x, .y))

##----- Combine park-level pdfs into 1 pdf per module -----
park_4 <- c("APCO", "BOWA", "COLO", "GETT", "HOFU", "VAFO") # Panel 4, sampled 2018
park_1 <- c("FRSP", "GEWA", "PETE", "RICH", "THST") # Panel 1, sampled 2019
SAHI <- "SAHI" # Sampled 2017

parks <- c(park_4, park_1, "SAHI")
years = c(rep(2018, length(park_4)), rep(2019, length(park_1)), 2017)

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
park_4 <- c("APCO", "BOWA", "COLO", "GETT", "HOFU", "VAFO") # Panel 4, sampled 2018
park_1 <- c("FRSP", "GEWA", "PETE", "RICH", "THST") # Panel 1, sampled 2019
SAHI <- "SAHI" # Sampled 2017

parks <- c(park_4, park_1, SAHI)
years <- c(rep(2018, length(park_4)), rep(2019, length(park_1)), 2017)

#source("PrevVisit_FieldForms_MIDN_compile.R") 

render_viewer <- function(parkcode, yearpv){
  year = as.numeric(yearpv)
  render(input = "PrevVisit_Plot_Viewer_MIDN_All.Rmd",
         params = list(parkcode = parkcode, 
                       yearpv = year, 
                       print = FALSE),
         output_file = paste0(path, parkcode, "_", year, "_Plot_Viewer.html"))
}

#render_poss <- possibly(.f = render_viewer, otherwise = NULL)

# running through a few at a time b/c bogs down laptop
purrr::map2(parks[c(1)], years[c(1)], ~render_viewer(.x, .y))
purrr::map2(parks[c(2:6)], years[c(2:6)], ~render_viewer(.x, .y))
purrr::map2(parks[c(7:11)], years[c(7:11)], ~render_viewer(.x, .y))
render_viewer("SAHI", 2017)

