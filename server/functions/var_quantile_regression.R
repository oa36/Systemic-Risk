#calculate VAR- quantile regression-rolling window
#--> mkt.equ.returns_i = a + b*state_variables
VaR <- function(tau, window_length){
  # read the macro state variables 
  data_state_variables = as.matrix(state_var_weekly[, 2:8])
  # read the log returns 
  data_returns = as.matrix(returns_weekly[, 2:21])
  
  nncol <- ncol(data_returns) 
  nnrow <- nrow(data_returns)
  lengthfull <- nnrow
  
  VaR <- matrix(0, ncol = nncol, nrow = (lengthfull - window_length))
  
  for(j in 1:nncol){
    for(i in 1:(lengthfull - window_length)){
      mkt_returns <- data_returns[1:(i + window_length),j]
      state_var <- data_state_variables[1:(i + window_length),]
      
      state_var_standardized  = matrix(0, nrow(state_var), ncol(state_var))
      
      for (k in 1:ncol(state_var)) {
        state_var_standardized[, k] <- (state_var[, k] - min(state_var[, k]))/(max(state_var[, k]) - min(state_var[, k]))
      }
      
      #this step is to handle cases like LEH where they got bankrupt in 2008, in the data returns are filled with zeros 
      #after 2008, keeping these zeros affects the calculation. So we change the 0s to NA then drop them when regresising
      mkt_returns[mkt_returns ==0] <- NA 
      
      fit <- rq(mkt_returns ~ state_var_standardized, tau,na.action = na.omit)
      pre <- predict(fit, quantiles = tau)
      VaR[i, j] <- pre[length(pre)] 
    }
  }
  VaR <- round(VaR, digits = 9)
  colnames(VaR) <- colnames(returns_weekly)[2:21]
  return(VaR)
} 