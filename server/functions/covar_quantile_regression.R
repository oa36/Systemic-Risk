#Network-CoVaR Quantile Regression 
CoVaR <- function(reference, tau, df, window_length){
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
  #Combine reference with state variables dataset
  Combined_data <-cbind(data_state_variables[1:(nnrow - window_length),], VaR_ref)
  # standardize only the macro variables
  for (k in 1:(ncol(data_state_variables) + 1)) {
    Combined_data[, k] = (Combined_data[, k] - min(Combined_data[, k]))/(max(Combined_data[, k]) - min(Combined_data[, k]))
  }
  CV_coef <- list()
  CV_q    = matrix(0, ncol = ncol(VaR_withoutref), nrow = (lengthfull - window_length))
  # Calculating at the tau quantile
  for(l in 1:ncol(VaR_withoutref)){
    Var_temp   = VaR_withoutref[,l]
    CoVaR_qth  <- rq(Var_temp ~ Combined_data, tau)
    CV_q[,l]   = predict.rq(CoVaR_qth, level = tau)
    
    CV_coef[[l]] <- CoVaR_qth
   # print(CV_coef)
  }
  colnames(CV_q) <- colnames(VaR_withoutref)
  names(CV_coef) <- colnames(VaR_withoutref)
 # print(CV_coef)
  return(list(CV_q = CV_q, CV_coef=CV_coef))
}