log_returns_plot <- function(company, returns_df){
  
  returns <- returns_df %>% select(Date, company)
  
  dygraph(xts(x=returns[,2], order.by = as.Date(returns$Date, format = "%d.%m.%y")))  %>% dyRangeSelector()
}


var_reg_plot <- function(company, df){
  
  VaR <- df %>% 
    mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) %>% 
    select(date, company) 
  
  data_var = xts(x= format(VaR[,2], scientific=F), order.by = as.Date(VaR$date,format = "%d.%m.%y"))
  
  dygraph(data_var) %>%  dySeries("V1", label = "VAR") %>% dyRangeSelector()
  
}

covar_reg_plot <- function(company, df){
  
  VaR <- df %>% 
    mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) %>% 
    select(date, company) 
  
  data_var = xts(x= format(VaR[,2], scientific=F), order.by = as.Date(VaR$date,format = "%d.%m.%y"))
  
  dygraph(data_var) %>%  dySeries("V1", label = "CoVaR") %>% dyRangeSelector()
  
}



delta_covar_reg_plot <- function(covar_df, company){
  
  delta_covar <- covar_df %>% dplyr::select_("date", company)
  
  data_cvar <- xts(x= format(delta_covar[,2], scientific=F), order.by = as.Date(delta_covar$date,format = "%d.%m.%y"))
  
  dygraph(data_cvar) %>% dySeries("V1", label = "delta covar") %>% dyRangeSelector()
}


var_delta_cvar <- function(VaR_df, delta_CoVar_df){
  VaR_qrt<- VaR_df %>%  
    #filter(date == "2006-12-31") %>%
    melt(id = "date") %>% 
    rename("VaR" = "value",
           "Stock" = "variable") %>% 
    mutate(date = zoo::as.yearqtr(date)) %>%
    group_by(date,Stock) %>% summarise(VaR = mean(VaR)) %>% 
    filter(date == "2006 Q4")
  
  CoVaR_qrt <- delta_CoVar_df %>% 
    melt(id = "date") %>% 
    rename("delta_covar" = "value",
           "Stock" = "variable")%>% 
    mutate(date = zoo::as.yearqtr(date)) %>%
    group_by(date,Stock) %>% summarise(delta_covar = mean(delta_covar)) %>% 
    filter(date == "2006 Q4")
  
  var_covar <- VaR_qrt %>% 
    dplyr::left_join(CoVaR_qrt, by= c("Stock","date")) %>% 
    na.omit() 
  
  
  var_covar <- var_covar %>% mutate(institution_type = case_when(Stock %in% c("AIG", 
                                                                              "ALL", 
                                                                              "BRK",
                                                                              "MET",
                                                                              "PRU") ~ "Insurance Company",
                                                                 Stock %in% c("BAC",
                                                                              "C",
                                                                              "GS",
                                                                              "JPM",
                                                                              "LEH",
                                                                              "MS")  ~ "Investment Banks",
                                                                 Stock %in% c("FMCC", 
                                                                              "FNMA") ~ " Government-sponsored Enterprises", 
                                                                 TRUE ~ "Comercial Banks"))
  plotly::ggplotly(
    ggplot(var_covar, aes(x=VaR, y=delta_covar, shape= institution_type, color = institution_type)) +
      geom_point(size=2)  + 
      ggtitle("Cross-Section Relationship between Delta CoVaR and VaR for 2006 Q4")
  )
}
