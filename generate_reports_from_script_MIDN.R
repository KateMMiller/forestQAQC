
path = c("D:/NETN/Monitoring_Projects/Forest_Health/2022_data/Maps_and_Data/Weekly_Checks/")


# Knit weekly reports
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_Week1.html"),
                  params = list(week_start = '2022-07-17',
                                cycle_latest_num = 4,
                                locType = 'all')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_VAFO-999-2022.html"),
                  params = list(year = 2022, plot = 'VAFO-999', locType = 'all'))


