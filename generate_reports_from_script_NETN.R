
path = c("C:/NETN/Monitoring_Projects/Forest_Health/2023_data/Maps&Data/")


# Knit weekly reports
rmarkdown::render('Weekly_QC_check_NETN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_NETN_allweeks.html"),
                  params = list(week_start = '2023-05-01',
                                locType = 'all')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_ACAD-172-2022.html"),
                  params = list(year = 2022, plot = 'ACAD-172', locType = 'all'))


# Knit weekly reports
rmarkdown::render('Weekly_QC_check_NETN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_NETN_Week8_ACAD.html"),
                  params = list(week_start = '2022-07-17',
                               cycle_latest_num = 5,
                               locType = 'all')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_ACAD-172-2022.html"),
                  params = list(year = 2022, plot = 'ACAD-172', locType = 'all'))

rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_ACAD-014-2022.html"),
                  params = list(year = 2022, plot = 'ACAD-014', locType = 'all'))

rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_ACAD-015-2022.html"),
                  params = list(year = 2022, plot = 'ACAD-015', locType = 'all'))

rmarkdown::render('QAQC_report_NETN.Rmd', 
                  output_file = paste0(path, "QAQC_report_NETN", "_MIMA-013-2022.html"),
                  params = list(year = 2022, plot = 'MIMA-013', locType = 'all'))

