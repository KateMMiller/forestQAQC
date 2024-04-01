
path = c("C:/NETN/Monitoring_Projects/Forest_Health/2023_data/Maps&Data/")


# Knit weekly reports
# For ASIS visit 1
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_ASIS.html"),
                  params = list(week_start = '2023-09-10',
                                locType = 'VS')) 
# For MIDN
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_PETE.html"),
                  params = list(week_start = '2023-08-13',
                                locType = 'VS')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_VAFO-244-2023.html"),
                  params = list(year = 2023, plot = 'VAFO-244', locType = 'all'))

rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_VAFO-245-2023.html"),
                  params = list(year = 2023, plot = 'VAFO-245', locType = 'all'))

rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_COLO-381-2023.html"),
                  params = list(year = 2023, plot = 'COLO-381', locType = 'all'))

rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_COLO-376-2023.html"),
                  params = list(year = 2023, plot = 'COLO-376', locType = 'all'))

rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_FRSP-044-2023.html"),
                  params = list(year = 2023, plot = 'FRSP-044', locType = 'all'))

rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_FRSP-047-2023.html"),
                  params = list(year = 2023, plot = 'FRSP-047', locType = 'all'))
