#----- Functions for weekly QC Checks
# prep data
filter_week <- function(df){filter(df, StartDate >= week_start)}
filter_old <- function(df){filter(df, StartYear < curr_year)}

name_plot <- function(df){df %>% mutate(Plot_Name = ifelse(IsQAQC == 1, paste0(Plot_Name, "-Q"), Plot_Name))}

# Determine whether to include/drop tab in output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Compile output
QC_check <- function(df, tab, check){
  result <- data.frame("Data" = tab, "Description" = check, "Num_Records" = nrow(df))
}

PM_check <- function(df){
  pms <- apply(df, 1, function(x) sum(grepl("Permanently Missing", x))) > 0
  pms2 <- apply(df, 1, function(x) sum(grepl("PM", x))) > 0
  pm_tot <- rbind(pms, pms2)
  pm_check <- df[pm_tot,] %>% filter(!is.na(Plot_Name))
  return(pm_check)
}

NS_check <- function(df){
  ns <- apply(df, 1, function(x) sum(grepl("Not Sampled", x))) > 0
  ns2 <- apply(df, 1, function(x) sum(grepl("NS", x))) > 0
  ns_tot <- rbind(ns, ns2)
  ns_check <- df[ns_tot,] %>% filter(!is.na(Plot_Name))
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