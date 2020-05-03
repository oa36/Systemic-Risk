#try lm 

library(lmtest)

linear_model <- stats::lm(delta_covar_normalised ~ LEVm2_8 + MMM_alt2_8 + boom90os_8 + logSIZEcs_8 +
                            RESI_8+ VIX_8+TBR3M_8+CRESPR_8+LIQSPR_8+YIESPR_8+SP500_8, data=delta_size_ss)


plot(x = delta_size_ss$logSIZEcs_8, 
     y = delta_size_ss$delta_covar_normalised,
     pch = 20,
     col = "steelblue")

abline(linear_model, lwd = 1)


plot(x = delta_size_ss$boom90os, 
     y = delta_size_ss$delta_covar_normalised,
     pch = 20,
     col = "steelblue")

abline(linear_model, lwd = 1)
