
path = c("D:/NETN/Monitoring_Projects/Forest_Health/2022_data/Maps_and_Data/Weekly_Checks/")


# Knit weekly reports
# For ASIS visit 1
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_Week9.html"),
                  params = list(week_start = '2022-09-18',
                                cycle_latest_num = 1,
                                locType = 'VS')) 
# For MIDN
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_Week8.html"),
                  params = list(week_start = '2022-09-11',
                                cycle_latest_num = 4,
                                locType = 'VS')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_FRSP-281-2022.html"),
                  params = list(year = 2022, plot = 'FRSP-281', locType = 'all'))


