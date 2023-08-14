#----- Functions for weekly QC Checks
# prep data
filter_week <- function(df){filter(df, SampleDate >= week_start)}
filter_old <- function(df){filter(df, SampleYear < curr_year)}

name_plot <- function(df){df %>% mutate(Plot_Name = ifelse(IsQAQC == 1, paste0(Plot_Name, "-Q"), Plot_Name))}

#------
# Determine whether to include/drop tab in output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Compile output
QC_check <- function(df, tab, check){
  result <- data.frame("Data" = tab, "Description" = check, "Num_Records" = nrow(df))
}

PM_check <- function(df){
  pms <- apply(df, 1, function(x) sum(grepl("Permanently Missing|PM", x))) > 0
  pm_check <- df[pms,] 
  return(pm_check)
}

NS_check <- function(df){
  ns <- apply(df, 1, function(x) sum(grepl("Not Sampled|NS", x))) > 0
  ns_check <- df[ns,] 
  return(ns_check)
}

make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    kable(df, format = 'html', align = 'c', caption = cap) %>% 
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'), 
                    full_width = TRUE, position = 'left', font_size = 12) %>%
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") %>% 
      collapse_rows(1, valign = 'top') %>% 
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;') 
  } else NULL 
}

check_null <- function(table){
  if(!is.null(table)){table}  
}