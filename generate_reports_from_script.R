# Knit QAQC Report
path = c("D:/NETN/Monitoring_Projects/Forest_Health/2022_data/Maps_and_Data/Weekly_Checks/")

rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_MIMA-015-2022.html"),
                  params = list(year = 2022, plot = 'MIMA-015', locType = 'all'))

rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_MIMA-013-2022.html"),
                  params = list(year = 2022, plot = 'MIMA-013', locType = 'all'))

# Knit weekly reports
rmarkdown::render('Weekly_QC_check_NETN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_NETN_Week4_WEFA_MIMA.html"),
                  params = list(week_start = '2022-06-12',
                               cycle_latest_num = 4,
                               locType = 'all'))

