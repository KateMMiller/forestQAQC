
path = c("D:/NETN/Monitoring_Projects/Forest_Health/2022_data/Maps_and_Data/Weekly_Checks/")


# Knit weekly reports
rmarkdown::render('Weekly_QC_check_MIDN.Rmd',
                  output_file = paste0(path, "Weekly_QC_check_MIDN_Week1_T3.html"),
                  params = list(week_start = '2022-07-24',
                                cycle_latest_num = 4,
                                locType = 'all')) 

# Knit QAQC Report
rmarkdown::render('QAQC_report_MIDN.Rmd', 
                  output_file = paste0(path, "QAQC_report_MIDN", "_VAFO-9999-2022.html"),
                  params = list(year = 2022, plot = 'VAFO-9999', locType = 'all'))


