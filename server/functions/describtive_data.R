descrebtive_stat <- function(){
  returns_weekly <- returns_weekly %>% 
    melt(id = "week") %>% 
    rename("log returns" = "value",
           "stock" = "variable") 
  
  data_full <- returns_weekly %>% 
    dplyr::left_join(state_var_weekly, by = "week") 
  return(data_full)
}


#Statistics Summary of individual companies within a range of dates
summary_fxn<- function(df,name,start_date,end_date){
  
  
  data <- df
  
  selection <- data %>% dplyr::filter(stock == name & week >= start_date & week <= end_date)

  sum_data <-  selection%>% select(-stock, -week)
  
  
  return(as.data.frame(do.call(cbind, lapply(sum_data, summary, digits=7))))
}


balance_sheet_data_cleaned <- balance_sheet_data %>% 
  dplyr::mutate(date = as.yearqtr(stringr::str_replace(qtr, "q", " Q"))) %>% #edit date column so that it matches with our data layout 
  select(date,ticker ,LEVm2, MMM_alt2, boom90os, logSIZEcs) %>% rename("stock" = "ticker") %>% 
  group_by(stock) %>% mutate_at("date", as.character) %>% rename("quarter" = "date")

