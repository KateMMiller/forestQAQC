
path = c("C:/NETN/Monitoring_Projects/Forest_Health/2023_data/Maps&Data/")


# Knit weekly reports
# For ASIS visit 1
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_ASIS.html"),
                  params = list(week_start = '2023-09-10',
                                cycle_latest_num = 1,
                                locType = 'VS')) 
# For MIDN
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_VAFO_HOFU_GETT.html"),
                  params = list(week_start = '2023-07-16',
                                cycle_latest_num = 4,
                                locType = 'VS')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_VAFO-244-2023.html"),
                  params = list(year = 2023, plot = 'VAFO-244', locType = 'all'))

rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_VAFO-245-2023.html"),
                  params = list(year = 2023, plot = 'VAFO-245', locType = 'all'))

