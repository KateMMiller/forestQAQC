#---------------------------------
#     QAQC Summary Functions 
#---------------------------------
filter_plot <- function(df){filter(df, Plot_Name %in% params$plot & StartYear %in% params$year)}
name_team <- function(df){mutate(df, Team = ifelse(IsQAQC == 0, "Crew", "QAQC"))}

diff_ok <- "#F2F2A0" #yellow
diff_bad <- "#F5BA59" #orange

check_dif <- function(df, col){
  dif <- ifelse(is.numeric(df[, col]), 
                abs(diff(df[, col])), 
                df[1,col] == df[2,col])
  
  color <- case_when(is.logical(dif) & dif == TRUE ~ "#ffffff", #white
                     is.logical(dif) & dif == FALSE ~ diff_ok,
                     is.numeric(dif) & dif == 0 ~ "#ffffff", 
                     is.numeric(dif) & dif == 1 ~ diff_ok,
                     is.numeric(dif) & dif > 1 ~ diff_bad,
                     is.na(dif) ~ "#ffffff") 
  return(color)
}

check_dif_col <- function(df, col){
  df[,col][is.na(df[,col])] <- 0
  color <- case_when(df[,col] == 0 ~ "#ffffff",
                     df[,col] == 1 ~ diff_ok,
                     df[,col] > 1 ~ diff_bad)
  return(color)
}

check_dbh <- function(df, col){
  df$diff <- abs(df[,col])
  #df$diff[is.na(df$diff)]
  color = case_when(df$diff > 0.2 ~ diff_bad,
                    between(df$diff, 0.01, 0.2) ~ diff_ok,
                    df$diff == 0 ~ "#ffffff", 
                    is.na(df$diff) ~ "#ffffff")
  return(color)
}

add_covcode <- function(df, col, jointbl){
  var_name <- paste0(substr(col, 5, nchar(col)), "_code")
  df2 <- merge(df, jointbl, by.x = col, by.y = 'txt', all.x = T, all.y = F) 
  names(df2)[names(df2) == "pct_class"] <- var_name
  return(df2 %>% arrange(Team) %>% select(var_name))
}


make_sppcode <- function(df){
  df2 <- df %>% mutate(genus = word(ScientificName, 1),
                       species = ifelse(is.na(word(ScientificName, 2)), "spp",
                                        word(ScientificName,2)),
                       sppcode = toupper(paste0(substr(genus, 1, 3),
                                                substr(species, 1, 3))))
  return(df2)
}

check_cond <- function(df, col1, col2){
  color = ifelse(df[,col1] != df[,col2], diff_ok, "#ffffff")
  return(color)
}

add_folcode <- function(df, col, jointbl){
  var_name <- paste0(substr(col, 5, nchar(col)), "_code")
  df2 <- merge(df, jointbl, by.x = col, by.y = 'txt', all.x = T, all.y = F) 
  names(df2)[names(df2) == "pct_class"] <- var_name
  return(df2 %>% arrange(Team, TagCode) %>% select(var_name))
}

check_covclass <- function(df, col1, col2){
  color = case_when(df[,col1] == df[,col2] ~ "#ffffff",
                    abs(df[,col1] - df[,col2]) > 1 ~ diff_bad,
                    abs(df[,col1] - df[,col2]) == 1 ~ diff_ok,
                    TRUE ~ "#ffffff")
  return(color)
}


check_stems <- function(df, col1, col2){
  color = case_when(df[,col1] == df[,col2] ~ "#ffffff",
                    abs(df[,col1] - df[,col2]) > 0 ~ diff_bad,
                    TRUE ~ "#ffffff")
  return(color)
}

check_stems_small <- function(df, col1, col2){
  color = case_when(df[,col1] == df[,col2] ~ "#ffffff",
                    abs(df[,col1] - df[,col2]) == 1 ~ diff_ok,
                    abs(df[,col1] - df[,col2]) > 1 ~ diff_bad,
                    TRUE ~ "#ffffff")
  return(color)
}

check_pct_diff <- function(df, col1, col2, pct_diff){
  color = case_when(between(df[,pct_diff], 0, 1) ~ "#ffffff",
                    between(df[,pct_diff], 1, 5) ~ diff_ok,
                    df[,col1] + df[,col2] <= 5 & df[,pct_diff] > 10 ~ diff_ok, #b/c  small tallies can have large % diff
                    df[,col1] + df[,col2] > 5 & df[,pct_diff] > 10 ~ diff_bad,
                    TRUE ~ "#ffffff")
  return(color)
}


pct_diff <- function(col1, col2){
  pct_diff =  (abs(col1 - col2)/((col1 + col2)/2))*100
  return(pct_diff)
}
