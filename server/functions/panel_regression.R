#create a function that estimates covar for all banks on one reference: 
#Network-CoVaR Quantile Regression 
CoVaR_pannel <- function(reference, tau, df, window_length){
  # read the macro state variables 
  data_state_variables = as.matrix(state_var_weekly[, 2:8])
  # read the log returns 
  data_returns = as.matrix(returns_weekly[, 2:21])
  
  nncol <- ncol(data_returns) 
  nnrow <- nrow(data_returns)
  lengthfull <- nnrow
  
  VaR <- df
  #Reference Company
  VaR_ref <- subset(VaR, select = reference)
  #VaR Without Reference
  VaR_withoutref <- VaR[,-c(which(colnames(VaR)==reference))]
  
  state_var_standarised <- data_state_variables[1:(nnrow - window_length),]
  # standardize only the macro variables
  for (k in 1:ncol(data_state_variables)) {
    state_var_standarised[, k] = (state_var_standarised[, k] - min(state_var_standarised[, k]))/(max(state_var_standarised[, k]) - min(state_var_standarised[, k]))
  }
  CV_q    = matrix(0, ncol = ncol(VaR_withoutref), nrow = (lengthfull - window_length))
  # Calculating at the tau quantile
  for(l in 1:ncol(VaR_withoutref)){
    
    #Combine reference with state variables dataset
    Combined_data <-cbind(state_var_standarised, VaR_withoutref[,l])
    
    CoVaR_qth  <- rq(VaR_ref ~ Combined_data, tau)
    CV_q[,l]   = predict.rq(CoVaR_qth, level = tau)
    
    Combined_data <- Combined_data[,-8]
  }
  colnames(CV_q) <- colnames(VaR_withoutref)
  return(CV_q)
}

#get delta ($)covar normalised by average cross-sectional mkt cap
delta_size_covar  <- function(delta_cvar_df, market_cap){
  #aggregate delta_covar quarterly
  delta_cvar <- delta_cvar_df %>% melt(id = "date") %>% 
    rename("delta_covar" = "value",
           "stock" = "variable") %>%
    mutate(date = zoo::as.yearqtr(date)) %>%
    group_by(date,stock) %>% 
    summarise(delta_covar = mean(delta_covar)) %>% 
    ungroup()
  #aggregate market cap quarterly
  mkt_cap <- market_cap %>% 
    melt(id = "date") %>% 
    rename("mkt_cap" = "value",
           "stock" = "variable") %>% 
    mutate(date = zoo::as.yearqtr(date)) %>%
    group_by(date,stock) %>% 
    summarise(mkt_cap = mean(mkt_cap)) %>%
    ungroup()
  #1-join delta covar with mkt cap
  #2- multiply delta covar by mkt cap
  #3- achieve stationarity by dividing by cross sectional mkt cap
  delta_dollar_cvar <- delta_cvar %>% 
    left_join(mkt_cap, by = c("date","stock")) %>% 
    mutate(dollar_delta_covar = delta_covar * mkt_cap) %>% 
    group_by(date) %>% 
    mutate(cs_avg_mkt_cap = mean(mkt_cap),
           delta_covar_normalised = (dollar_delta_covar/cs_avg_mkt_cap)) %>% ungroup() 
  
  delta_dollar_cvar <- delta_dollar_cvar %>% select(date,stock,delta_covar_normalised)
  
  #  delta_dollar_cvar <- delta_dollar_cvar %>%
  #    tidyr::spread(key = "stock", value = "delta_covar_normalised")
  
  return(delta_dollar_cvar)
}

#regress delta ($) covar on lagged balance sheet data using fixed effects panel regression
panel_reg <- function(bs_df, state_var_df, delta_cvar_size_df, forcast_lag){
  
  #first get, aggregate quarterly, and lag and scale balance sheet data
  balance_sheet_data <- bs_df %>% 
    dplyr::mutate(date = as.yearqtr(stringr::str_replace(qtr, "q", " Q"))) %>% #edit date column so that it matches with our data layout 
    select(date,ticker ,LEVm2, MMM_alt2, boom90os, logSIZEcs) %>% rename("stock" = "ticker") %>% 
    group_by(stock) %>% 
    mutate_at(c("LEVm2", "MMM_alt2", "boom90os", "logSIZEcs"), as.numeric) %>%
    dplyr::mutate(LEVm2_8 = dplyr::lag(LEVm2,8),
                  MMM_alt2_8 = dplyr::lag(MMM_alt2,8), 
                  boom90os_8 = dplyr::lag(boom90os,8), 
                  logSIZEcs_8 = dplyr::lag(logSIZEcs,8),
                  LEVm2_4 = dplyr::lag(LEVm2, 4),
                  MMM_alt2_4 = dplyr::lag(MMM_alt2, 4),
                  boom90os_4 = dplyr::lag(boom90os, 4),
                  logSIZEcs_4 = dplyr::lag(logSIZEcs, 4),
                  LEVm2_1 = dplyr::lag(LEVm2, 1),
                  MMM_alt2_1 = dplyr::lag(MMM_alt2, 1),
                  boom90os_1 = dplyr::lag(boom90os, 1),
                  logSIZEcs_1 = dplyr::lag(logSIZEcs, 1)) %>%
    ungroup() #%>% filter(stock == reference) %>% 
#    mutate_at(c("LEVm2_8", "MMM_alt2_8", "logSIZEcs_8", 
#                "LEVm2_4", "MMM_alt2_4", "logSIZEcs_4",
#                "LEVm2_1", "MMM_alt2_1", "logSIZEcs_1"), scale)
#  
  #second get, aggregate quarterly lag and scale state variables
  state_var_qrt <- state_var_df %>% 
    mutate(week = zoo::as.yearqtr(week)) %>%
    group_by(week) %>% 
    dplyr::summarise_all(mean) %>% 
    ungroup() %>% 
    rename("date" = "week") %>% 
    dplyr::mutate(
      RESI_8 = dplyr::lag(RESI,8),
      VIX_8 = dplyr::lag(VIX,8),
      TBR3M_8 = dplyr::lag(TBR3M,8),
      CRESPR_8 = dplyr::lag(CRESPR,8),
      LIQSPR_8 = dplyr::lag(LIQSPR,8),
      YIESPR_8 = dplyr::lag(YIESPR,8),
      SP500_8 = dplyr::lag(SP500,8),
      
      RESI_4 = dplyr::lag(RESI,4),
      VIX_4 = dplyr::lag(VIX,4),
      TBR3M_4 = dplyr::lag(TBR3M,4),
      CRESPR_4 = dplyr::lag(CRESPR,4),
      LIQSPR_4 = dplyr::lag(LIQSPR,4),
      YIESPR_4 = dplyr::lag(YIESPR,4),
      SP500_4 = dplyr::lag(  SP500,4),
      
      RESI_1 = dplyr::lag(RESI,1),
      VIX_1 = dplyr::lag(VIX,1),
      TBR3M_1 = dplyr::lag(TBR3M,1),
      CRESPR_1 = dplyr::lag(CRESPR,1),
      LIQSPR_1 = dplyr::lag(LIQSPR,1),
      YIESPR_1 = dplyr::lag(YIESPR,1),
      SP500_1 = dplyr::lag(  SP500,1)
    ) #%>% 
  #  mutate_at(c("RESI_8", "VIX_8", "TBR3M_8", "CRESPR_8", "LIQSPR_8", "YIESPR_8", "SP500_8",
  #              "RESI_4", "VIX_4", "TBR3M_4", "CRESPR_4", "LIQSPR_4", "YIESPR_4", "SP500_4",
  #              "RESI_1", "VIX_1", "TBR3M_1", "CRESPR_1", "LIQSPR_1", "YIESPR_1", "SP500_1"), 
  #            scale)
  
  #merge balance sheet, and state var data with delta $covar
  delta_sizee <- delta_cvar_size_df %>% 
    tidyr::spread(key = "stock", value = "delta_covar_normalised") %>%
    melt(id = "date") %>% 
    rename("delta_covar_normalised" = "value",
           "stock" = "variable") %>%
    left_join(balance_sheet_data, by = c("date", "stock")) %>% 
    left_join(state_var_qrt, by = "date") %>% 
    drop_na() %>%mutate_at(c("RESI_8", "VIX_8", "TBR3M_8", "CRESPR_8", "LIQSPR_8", "YIESPR_8", "SP500_8",
                             "RESI_4", "VIX_4", "TBR3M_4", "CRESPR_4", "LIQSPR_4", "YIESPR_4", "SP500_4",
                             "RESI_1", "VIX_1", "TBR3M_1", "CRESPR_1", "LIQSPR_1", "YIESPR_1", "SP500_1",
                             
                             "LEVm2_8", "MMM_alt2_8","boom90os_8" ,"logSIZEcs_8", 
                             "LEVm2_4", "MMM_alt2_4","boom90os_4" ,"logSIZEcs_4",
                             "LEVm2_1", "MMM_alt2_1","boom90os_1" ,"logSIZEcs_1"), function(x) (x- min(x))/(max(x)- min(x)))
    
  
  # Set data as panel data
  pdata <- pdata.frame(delta_sizee, index=c("stock","date"))
  # Fixed effects pannel regression
  if(forcast_lag == "1 Year"){
    fixed <- plm(delta_covar_normalised ~ LEVm2_4 + MMM_alt2_4 + boom90os_4 + logSIZEcs_4 +
                   RESI_4+ VIX_4+ TBR3M_4+ CRESPR_4+ LIQSPR_4+ YIESPR_4+ SP500_4
                 ,  data=pdata, model= "within")
  } else if(forcast_lag == "2 Year"){
    fixed <- plm(delta_covar_normalised ~ LEVm2_8 + MMM_alt2_8 + boom90os_8 + logSIZEcs_8 +
                   RESI_8+ VIX_8+ TBR3M_8+ CRESPR_8+ LIQSPR_8+ YIESPR_8+ SP500_8
                 ,  data=pdata, model= "within")
  } else {
    fixed <- plm(delta_covar_normalised ~ LEVm2_1 + MMM_alt2_1 + boom90os_1 + logSIZEcs_1 +
                   RESI_1+ VIX_1+ TBR3M_1+ CRESPR_1+ LIQSPR_1+ YIESPR_1+ SP500_1
                 ,  data=pdata, model= "within")
  }
  return(summary(fixed))
}