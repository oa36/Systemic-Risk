server <- function(input, output, session) {
  
  plot1 <-  reactive({
    
    
    var_plot(df = daily_returns, company = input$name, start_date = input$dateRange[1], 
             end_date=input$dateRange[2],quant = input$confidence_var)
  }
  )
  
  data <- reactive({
    full_data <- descrebtive_stat()
  })
  
  log_returns <- reactive({
    log_returns_plot(company = input$name, daily_returns)
  })
  
  getSummary <- reactive({
    full_data <- data()
    
    summary <- summary_fxn(df = full_data, name = input$name_summary, start_date = input$dateRange_summary[1], end_date = input$dateRange_summary[2])
    return(summary)
  })
  
  values <- reactiveValues(summ = NULL,
                           var_estimated= NULL,
                           covar_reg = NULL)
  
  observe({
    values$summ <- getSummary()
    
  })
  
  #view data
  getViewedData <- function(df,type, length) {
    if(type == "head")
      df <- data.frame(head(df, n = length))
    else if(type == "tail")
      df <- data.frame(tail(df, n = length))
    return(df)
  }
  
  estimate_var <- eventReactive(input$cal_var, {
    withBusyIndicatorServer("cal_var", {
      estimate_var <- VaR(tau = as.numeric(input$tau), window_length = 28) 
      values$var_estimated <- estimate_var 
      return(as.data.frame(estimate_var) %>% 
               mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) %>% 
               select(date, everything())
      )
    })
  })
  
  estimated_var_plot <- reactive({
    if(!is.null(values$var_estimated)){
      var_reg_plot(df = estimate_var(), company = input$name)
    }
  })
  
  estimate_covar <- eventReactive(input$cal_covar, {
    withBusyIndicatorServer("cal_covar", {
      
      validate(
        need(!is.null(values$var_estimated), "VaR needs to be estimated first as it is an input for the estimation of CoVaR")
      )
      estimate_covar <- CoVaR(reference = input$reference_company, df = values$var_estimated, tau = as.numeric(input$tau), window_length = 28) 
      
      values$covar_estimated <- estimate_covar[["CV_q"]]
      values$covar_reg <- estimate_covar[["CV_coef"]]
     # print(values$covar_reg)
      return(as.data.frame(values$covar_estimated) %>% 
               mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) %>% 
               select(date, everything())
      )
    })
  })
  
  covar_coef <- reactive({
    if(!is.null(values$covar_reg)){
      #print(values$covar_reg)
      
      summary(values$covar_reg[[input$name]], se = "boot")
    }
  })
  
  estimated_covar_plot <- reactive({
    if(!is.null(values$covar_estimated)){
      
      validate(need(input$reference_company != input$name, "Choose a company from the sidebar controls that is different than the chosen reference"))
      
      covar_reg_plot(df = estimate_covar(), company = input$name)
    }
  })
  
  
  delta_covar_plot <- reactive({
    if(!is.null(values$delta_covar_df))
      delta_covar_reg_plot(covar_df = values$delta_covar_df, company = input$name)
  })
  
  
  estimate_delta_covar <- eventReactive(input$cal_delta_covar, {
    withBusyIndicatorServer("cal_delta_covar", {
      
      validate(
        need(!is.null(values$covar_estimated), "CoVaR at 99% or 95% confidence level needs to be estimated first as it is an input for the estimation of Delta CoVaR")
      )
      
      covar_q <- values$covar_estimated
     
      var_50 <- VaR(tau = 0.5, window_length = 28)
      
      values$var_50 <- var_50
      
      covar_50 <- CoVaR(reference = input$reference_company, df = var_50, tau = as.numeric(input$tau), window_length = 28) 

      delta_covar <- covar_q - covar_50[["CV_q"]]
      
      delta_covar <- as.data.frame(delta_covar) %>% 
        mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) %>% 
        select(date, everything())
      values$delta_covar_df <- delta_covar
      
      return(delta_covar)
    })
  })
  
  var_covar_plot <- reactive({
    if(!is.null(values$var_estimated) & !is.null(values$delta_covar_df)){
      var_df <- as.data.frame(values$var_estimated) %>% 
        mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) 
      
      var_delta_cvar(VaR_df = var_df, delta_CoVar_df = values$delta_covar_df)
    }
  })
  
  pannel_forward_cvar <- eventReactive(input$cal_forward_covar, {
    withBusyIndicatorServer("cal_forward_covar", {
      #if(!is.null(values$delta_covar_df)){
    #  var_50 <- VaR(tau = 0.5, window_length = 28)
      validate(
        need(!is.null(values$var_estimated), "VaR and delta CoVaR must be estimated first")
      )
      
      validate(
        need(!is.null(values$var_50), "VaR and delta CoVaR must be estimated first")
      )
      
      CoVaR_pan <- CoVaR_pannel(reference = input$affected_company, df = values$var_estimated, tau = as.numeric(input$tau), window_length = 28)
      CoVaR_pan_50 <- CoVaR_pannel(reference = input$affected_company, df = values$var_50, tau = as.numeric(input$tau), window_length = 28)
      
      delta_pannel <- as.data.frame(CoVaR_pan - CoVaR_pan_50) %>% mutate(date = returns_weekly$week[1:(nrow(returns_weekly)-28)]) %>% 
        select(date, everything())
      
      delta_cvar_nor <- delta_size_covar(delta_pannel, market_cap_weekly)
      
      reg_result <- panel_reg(bs_df= balance_sheet_data, 
                              state_var_df = state_var_weekly, 
                              delta_cvar_size_df = delta_cvar_nor,
                              forcast_lag = "1 Year")
                             # forcast_lag = input$forcast)
      
      values$pannel_result <- reg_result
      #}
    })
  })
  
  
  #observe and update column names
  observe({
    updateSelectInput(session, "name_summary",
                      choices = unique(data()$stock))
  })  
  
  
  #outputs   
  output$data_structure <- renderPrint({
    str(data())
  })
  
  
  output$table1 <- renderPlotly({
    plot1()
  })
  
  output$log_returns_plot <- renderDygraph({
    log_returns()
  })
  
  output$summary_out <- DT::renderDT({
    DT::datatable(
      values$summ,
      options = list(
        dom = "lBfrtip",
        search = list(regex = TRUE, caseInsensitive = FALSE),
        buttons =
          list(list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )),
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0),
        pageLength = 10,
        lengthChange = T
      )
    )
  } 
  )
  
  output$data_viewer_rs <- DT::renderDT({
    DT::datatable(
      getViewedData(data(),input$type, input$length),
      options = list(
        dom = "lBfrtip",
        search = list(regex = TRUE, caseInsensitive = FALSE),
        buttons =
          list(list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )),
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0),
        pageLength = 10,
        lengthChange = T
      )
    )
  } 
  )
  
  output$data_viewer_bs <- DT::renderDT({
    DT::datatable(
      getViewedData(balance_sheet_data_cleaned,input$type, input$length),
      options = list(
        dom = "lBfrtip",
        search = list(regex = TRUE, caseInsensitive = FALSE),
        buttons =
          list(list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )),
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0),
        pageLength = 10,
        lengthChange = T
      )
    )
  } 
  )
  
  output$var_estimated <- DT::renderDT({
    DT::datatable(
      estimate_var(),
      options = list(
        dom = "lBfrtip",
        search = list(regex = TRUE, caseInsensitive = FALSE),
        buttons =
          list(list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )),
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0),
        pageLength = 10,
        lengthChange = T
      )
    )
  } 
  )
  
  output$var_plot <- renderDygraph({
    estimated_var_plot()
  })
  
  output$covar_plot <- renderDygraph({
    estimated_covar_plot()
  })
  
  output$covar_estimated <- DT::renderDT({
    DT::datatable(
      estimate_covar(),
      options = list(
        dom = "lBfrtip",
        search = list(regex = TRUE, caseInsensitive = FALSE),
        buttons =
          list(list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )),
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0),
        pageLength = 10,
        lengthChange = T
      )
    )
  } 
  )
  
  output$delta_covar_plot <- renderDygraph({
    delta_covar_plot()
  })
  
  output$delta_covar_estimated <- DT::renderDT({
    DT::datatable(
      estimate_delta_covar(),
      options = list(
        dom = "lBfrtip",
        search = list(regex = TRUE, caseInsensitive = FALSE),
        buttons =
          list(list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )),
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0),
        pageLength = 10,
        lengthChange = T
      )
    )
  } 
  )
  output$cvar_reg_title <- renderUI({
    
   string <-  paste("$$CoVaR_{q,t}^{", input$name, "|", input$reference_company, "}= \\hat{\\alpha}_q ^{",
                    input$name, "|", input$reference_company,
                    "} + \\hat{\\gamma}_q ^{", 
                    input$name, "|", input$reference_company,
                    "} * M_{t-1} + \\hat{\\beta}_q ^{",
                    input$name, "|", input$reference_company,
                    "} * VaR_{q,t}^{"
                    ,input$reference_company,
                    "}$$", sep = "")
    
    box(title = withMathJax(div(string, style = "color:green; font-size:15px")),
        status = "primary", verbatimTextOutput("cvar_regg"), width = 6, collapsible = T)
  
    
  })
  
  output$covar_var_plot <- renderPlotly({
    var_covar_plot()
  })
  
  output$forward_covar_summary <- renderPrint({
  #  if(!is.null(values$pannel_result)){
    pannel_forward_cvar()
    #}
  })
  
  output$cvar_regg <- renderPrint({
    
    covar_coef()
  })
}