#Intro to VAR - Generate a plot of historical value at risk for different stocks 
var_plot <- function(df, company, start_date, end_date, quant){
  
  comp <- rlang::sym(company)
  
  df <- df %>% select_("Date", company) %>% 
    dplyr::filter(as.Date(Date,format = "%d.%m.%y") >= as.Date(start_date) & as.Date(Date,format = "%d.%m.%y") <= as.Date(end_date))
  
  var <- round(PerformanceAnalytics::VaR(as.double(unlist(df[company])) , p = quant, method = "historical"),2)
  breaks <- round(c(seq(min(as.numeric(unlist(df[company]))), max(as.numeric(unlist(df[company]))), 0.05),0), 2)
  plotly::ggplotly(
    ggplot(df, aes(!!comp)) +
      geom_histogram(bins = 200, fill = "#6B8E23", alpha = 0.8)+ 
      geom_vline(xintercept = var, color = "red",linetype="dotted") + 
      xlim(min(as.numeric(unlist(df[company]))), max(as.numeric(unlist(df[company])))) +
      scale_x_continuous(breaks=breaks) + 
      theme_economist() +
      ggtitle(paste("VaR at", percent(quant), "Confidence Level"))
    
  )
}