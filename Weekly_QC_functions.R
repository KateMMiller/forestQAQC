#----- Functions for weekly QC Checks
# prep data
filter_week <- function(df){filter(df, SampleDate >= week_start)}
filter_old <- function(df){filter(df, SampleYear < curr_year)}

name_plot <- function(df){df %>% mutate(Plot_Name = ifelse(IsQAQC == 1, paste0(Plot_Name, "-Q"), Plot_Name))}

# Add cycle for views that I load directly into checks
add_cycle <- function(df){
  df$cycle[df$SampleYear %in% c(2006:2009)] <- 1
  df$cycle[df$SampleYear %in% c(2010:2013)] <- 2
  df$cycle[df$SampleYear %in% c(2014:2017)] <- 3
  df$cycle[df$SampleYear %in% c(2018:2021)] <- 4
  return(df)
}

add_cycle_MIDN <- function(df){
  midn <- c("APCO", "BOWA", "FRSP", "GETT", "HOFU", "PETE", "RICH", "VAFO")
  ncbn <- c("GEWA", "SAHI", "THST")
  
  #+++++ UPDATE AFTER 2021 SEASON +++++#
  
  df$cycle[df$SampleYear %in% c(2007:2010) & df$ParkUnit %in% midn] <- 1
  df$cycle[df$SampleYear %in% c(2011:2014) & df$ParkUnit %in% midn] <- 2
  df$cycle[df$SampleYear %in% c(2015:2018) & df$ParkUnit %in% midn] <- 3
  df$cycle[df$SampleYear %in% c(2019:2022) & df$ParkUnit %in% midn] <- 4
  
  df$cycle[df$SampleYear %in% c(2008:2011) & df$ParkUnit %in% ncbn] <- 1
  df$cycle[df$SampleYear %in% c(2012:2015) & df$ParkUnit %in% ncbn] <- 2
  df$cycle[df$SampleYear %in% c(2016:2019) & df$ParkUnit %in% ncbn] <- 3
  df$cycle[df$SampleYear %in% c(2020:2023) & df$ParkUnit %in% ncbn] <- 4
  
  df$cycle[df$SampleYear %in% c(2011:2014) & df$ParkUnit %in% "COLO"] <- 1
  df$cycle[df$SampleYear %in% c(2015:2018) & df$ParkUnit %in% "COLO"] <- 2
  df$cycle[df$SampleYear %in% c(2019:2022) & df$ParkUnit %in% "COLO"] <- 3
  df$cycle[df$SampleYear %in% c(2023:2026) & df$ParkUnit %in% "COLO"] <- 4
  
  df$cycle[df$SampleYear %in% c(2019:2022) & df$ParkUnit %in% "ASIS"] <- 1
  df$cycle[df$SampleYear %in% c(2023:2026) & df$ParkUnit %in% "ASIS"] <- 2
  
  return(df)
}

# Find latest and previous cycles for MIDN, since parks aren't on same cycle
cycle_df <- data.frame(park = c("APCO", "BOWA", "FRSP", "GETT", "HOFU", "PETE", "RICH", "VAFO", 
                                "GEWA", "SAHI", "THST", 
                                "COLO", "ASIS"),
                       group = c("MIDN", "MIDN", "MIDN", "MIDN", "MIDN", "MIDN", "MIDN", "MIDN",
                                 "NCBN", "NCBN", "NCBN",
                                 "COLO", "ASIS"),
                       cycle_latest_num = c(rep(4, 8), rep(4, 3), 3, 1))
cycle_df$cycle_latest <- paste0("cycle_", cycle_df$cycle_latest_num)
cycle_df$cycle_prev_num <- cycle_df$cycle_latest_num - 1
cycle_df$cycle_prev <- paste0("cycle_", cycle_df$cycle_latest_num - 1)

get_latest <- function(parkcode){
  cycle_df %>% filter(park == parkcode) %>% select(cycle_latest_num) %>% as.numeric()
}

get_prev <- function(parkcode){
  cycle_df %>% filter(park == parkcode) %>% select(cycle_prev_num) %>% as.numeric()
}
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