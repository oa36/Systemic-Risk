ui <- dashboardPage(
  dashboardHeader(title = "Systemic Risk"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("VaR Intro", tabName = "var_intro"),
      menuItem("Systemic Risk - CoVaR",
               menuItem("Data", tabName = "data_summary"),
               menuItem("Log Returns", tabName = "returns"),
               menuItem("VaR", tabName = "var"),
               menuItem("CoVaR", tabName = "covar"),
               menuItem("Delta-CoVaR", tabName = "delta_covar"),
               menuItem("VaR-CoVar Relationship", tabName = "var_covar"),
               menuItem("Forward-CoVaR", tabName = "forward_covar")
      ),
      
      menuItem("Controls",
               selectInput("name", "Company:",choices = company_key
                           , selected = "BAC")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(
        HTML(".skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
        
        
        
        .sidebar {
                      height: 90vh; overflow-y: auto;
                    }
        
        
        .tab-content { overflow-y:scroll;height:100vh !important; }
      ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "var_intro", 
        fluidRow(
          box(status = "primary", plotlyOutput("table1"), width = 12, collapsible = T, 
              column(2, 
                     sliderInput("confidence_var",
                                 label = "Confidence Level:",
                                 value = 0.99,
                                 min = 0,
                                 max = 1.0), style = "margin-top: 24px; margin-bottom: 16px"
              ),
              column(4, 
                     dateRangeInput("dateRange", "Date Range:",
                                    start  = "2005-01-01",
                                    end    = "2010-01-01",
                                    min    = "1999-12-30",
                                    max    = "2014-12-31",
                                    format = "yyyy-mm-dd",
                                    separator = " - "), style = "margin-top: 24px; margin-bottom: 16px")
          )
        )
      ),
      tabItem(tabName = "data_summary",
              fluidRow(
                box(status="danger", width = 12, title = "Data Structure",verbatimTextOutput("data_structure")),
                box(
                  status = "warning", 
                  title = "Controls",
                  height = 400,
                  width = 4,
                  selectInput("name_summary", "Company:", choices = NULL), 
                  dateRangeInput("dateRange_summary", "Date Range:",
                                 start  = "2000-01-02",
                                 end    = "2014-12-28",
                                 min    = "2000-01-02",
                                 max    = "2014-12-28",
                                 format = "yyyy-mm-dd",
                                 separator = " - ")#,
#                  actionButton("search_summary", "Get Statistic Summary")
                ),
                box(status = "primary", title = "Descriptive Statistics", width = 8, DT::DTOutput("summary_out"), height = 400),
                box(
                  title = "View Data",
                  status = "warning",
                  width = 12,
                  selectInput("type",
                              "Select: ",
                              c("Head" = "head", "Tail" = "tail"),
                              selected = "head"),
                  numericInput("length", "Length: ", 10, 1, 1000)
                ),
                box(title = "Log Returns and State Variables Data",status = "primary", width = 6, DT::DTOutput("data_viewer_rs")),
                box(title ="Balance Sheet Data",status = "primary", width = 6, DT::DTOutput("data_viewer_bs"))
              )
      ),
      tabItem(
        tabName = "returns",
        fluidRow(
          box(status = "primary", dygraphOutput("log_returns_plot"), width = 12, collapsible = T)
        )
      ),
      tabItem(
        tabName = "var",
        fluidRow(
          box(title = withMathJax("VaR Estimation: $$VaR_{q,t}^i = \\hat{\\alpha}_q ^i + \\hat{\\gamma}_q ^i * M_{t-1}$$"),
              status = "primary", dygraphOutput("var_plot"), width = 12, collapsible = T,
              column(
                width = 3,  radioButtons(
                  inputId = "tau", label = "Choose Confidence Level",
                  choices = c("99%" = 0.01, "95%" = 0.05),
                  selected = 0.01
                ),  style = "margin-top: 24px; margin-bottom: 16px"
              ),
              column(withBusyIndicatorUI((actionButton("cal_var", "Estimate VaR"))), width = 3, style = "margin-top: 24px; margin-bottom: 16px")
          ),
          box(title = "Estimated Dataset",
              status = "primary", DT::DTOutput("var_estimated"), width = 12, collapsible = T)
        )
      ),
      tabItem(
        tabName = "covar",
        fluidRow(
          box(title = withMathJax("CoVaR Estimation: $$CoVaR_{q,t}^{A|i} = \\hat{\\alpha}_q ^{A|i} + \\hat{\\gamma}_q ^{A|i} * M_{t-1} + \\hat{\\beta}_q ^{A|i} * VaR_{q,t}^i$$"),
              status = "primary", dygraphOutput("covar_plot"), width = 12, collapsible = T,
              column(width = 3, selectInput("reference_company", "Reference(i):",choices = company_key
                                            , selected = "JPM"),style = "margin-top: 24px; margin-bottom: 16px"),
              column(withBusyIndicatorUI((actionButton("cal_covar", "Estimate CoVaR"))), width = 3, style = "margin-top: 48px; margin-bottom: 16px")
          ),
          box(title = "Estimated Dataset",
              status = "primary", DT::DTOutput("covar_estimated"), width = 6, collapsible = T), 
          uiOutput("cvar_reg_title")
        )
      ),
      tabItem(
        tabName = "delta_covar",
        fluidRow(
          box(title = withMathJax("CoVaR Estimation: $$\\Delta CoVaR_{q,t}^{A|i} = CoVaR_{q,t}^{A|VaR_{q,t}^i} - CoVaR_{q,t}^{A|VaR_{50,t}^i}$$"),
              status = "primary", dygraphOutput("delta_covar_plot"), width = 12, collapsible = T,
              column(withBusyIndicatorUI((actionButton("cal_delta_covar", "Estimate Delta-CoVaR"))), width = 3, style = "margin-top: 24px; margin-bottom: 16px")
          ),
          box(title = "Estimated Dataset",
              status = "primary", DT::DTOutput("delta_covar_estimated"), width = 12, collapsible = T)
        )
      ),
      tabItem(
        tabName = "var_covar",
        fluidRow(
          box(title = h4(HTML("The plot below shows no cross sectional relationship between Delta-CoVaR and VaR, hence focusing on </br> 
                             VaR alone is not sufficient"), style = "margin-top: 2px; padding-top:0px"),
              status = "primary", plotlyOutput("covar_var_plot"), width = 12, height = 700,collapsible = T)
        )
        ),
      tabItem(
        tabName = "forward_covar",
        fluidRow(
          box(title = withMathJax("Forward CoVaR - Fixed Effects Panel Regression (1 Year Forecast): $$\\Delta ^$ CoVaR_{q,t}^{A|i} = \\hat\\alpha + \\hat c * M_{t-h}+ \\hat b * Z_{t-h}$$"),
              status = "primary", verbatimTextOutput("forward_covar_summary"), 
              column(width = 3, selectInput("affected_company", "Company Affected(A):",choices = company_key
                                            , selected = "JPM"),style = "margin-top: 24px; margin-bottom: 16px"),
#              column(width = 3, selectInput("forcast", "Forcast:",choices = c(
#                "2 Years",
#                "1 Year",
#                "1 Quarter"
#              )
#                                            , selected = "1 Year"),style = "margin-top: 24px; margin-bottom: 16px"),
              column(width=3, withBusyIndicatorUI(actionButton("cal_forward_covar", "run regression")), style = "margin-top: 48px; margin-bottom: 16px"),
              width = 12,collapsible = T)
        )
      )
    )
  )
)
