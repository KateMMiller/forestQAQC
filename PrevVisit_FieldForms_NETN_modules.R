#------------------------------------------------
# Generate Previous Visit Field Forms by Module
#------------------------------------------------

# The scripts below will save paged html files of each plot per park and year
# to a FieldForms folder on your desktop. If The Field_Forms folder doesn't
# exist on your desktop, it will be added. Same goes for NETN_Tree_Maps folder

path <- paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 
               "\\Field_Forms\\")

if(!dir.exists(path)){dir.create(path)}

path_trmaps <- paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 
                      "\\NETN_Tree_Maps\\")

if(!dir.exists(path_trmaps)){dir.create(path_trmaps)}

#----- Imports/libraries -----
library(tidyverse)
library(knitr)
library(rmarkdown)
library(forestNETN)
importData()
source("PrevVisit_functions.R")
library(psycModel) #for converting htmls to PDF
library(pdftools)
#----- params -----
# Since NHPs and ACAD are off by a year, you have to run for all NHPs, then reset
# params and rerun remaining code for all ACAD plots

year <- 2018
park <- c("MABI", "MIMA", "SAGA", "SARA")

year <- 2019
park <- "ACAD"
#----- Source from compile script -----
# Sourcing specific lines in compile script to speed process
source_lines("PrevVisit_FieldForms_NETN_compile.R", 1, 42) # arglist & plotevs
source_lines("PrevVisit_FieldForms_NETN_compile.R", 85, 94) # tree data
source_lines("PrevVisit_FieldForms_NETN_compile.R", 154, 191) # quads & addspp

plots <- sort(unique(plotevs$Plot_Name)) # plot list to iterate on below


#----- Tree Measurements -----
rmdtr <- "PrevVisit_3A_Tree_Measurements.Rmd"

render_trees <- function(plot_name, pv_year){
  park = substr(plot_name, 1, 4)
  render(input = rmdtr,
         params = list(plot = plot_name, 
                       year = as.numeric(pv_year), 
                       print = TRUE),
         output_file = paste0(path, plot_name, "_", pv_year, "_Trees.html"))
  }

# NHPs
map(plots, ~render_trees(., pv_year = 2018))

# ACAD
map(plots, ~render_trees(., pv_year = 2019))

# Convert htmls into  1 pdf per park

# First convert html to pdf (ignore warnings unless it doesn't work)
html_to_pdf(dir = path, render_exist = TRUE) 


# Combine park-level pdfs into 1 pdf
nhps <- c("ACAD", "MABI", "MIMA", "SAGA", "SARA")

combine_tree_pdfs <- function(park, path, year){
  pdf_list <- list.files(path, pattern = ".pdf", full.names = TRUE)
  trees <- pdf_list[grep(("Trees"), pdf_list)]
  park_trees <- trees[grep((park), trees)]
  pdftools::pdf_combine(input = park_trees,
                        output = paste0(path, park, "_", year, "_Trees.pdf"))
  }

purrr::map(nhps, ~combine_tree_pdfs(., path, year = 2018))
combine_tree_pdfs("ACAD", path, year = 2019)

#----- Tree Maps -----
parks = c("ACAD", "MABI", "MIMA", "SAGA", "SARA")
plotTreeMap(park = parks, from = 2018, to = 2019, 
            path = path_trmaps, output_to = 'file')

#----- Quadrat Species -----