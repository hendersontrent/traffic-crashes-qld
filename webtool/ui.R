# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "orbisant_logo.png", height = '50px', hspace = '30'),
                               ""),
                   position = c("static-top"), windowTitle = "QLD Crash Statistical Modeller",
                   id = "page_tab",
                   
                   
                   #----------------------Analysis pages---------------------            
                   tabPanel(navtab0,
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "corp-styles.css")
                            ),
                            
                            fluidRow(
                              h1("QLD Traffic Crash Statistical Modelling Tool")
                            ),
                            tabsetPanel(id = "analysis_tabs",
                                        tabPanel("Time Series",
                                          sidebarLayout(
                                            sidebarPanel(
                                              h2("Page Details"),
                                              selectInput("ts_severity", "Select a crash severity",
                                                          choices = severities, selected = severities[1])
                                            ),
                                            mainPanel(
                                              fluidRow(column(9,
                                                              h3("Raw Time Series Data"),
                                                              shinycssloaders::withSpinner(plotlyOutput("raw_ts", height = "450px")),
                                                              p("Click on and hover over the graph with your mouse to interact with it.")
                                              )
                                             ),
                                             hr(),
                                             fluidRow(column(9,
                                                             h3("Generalised Additive Model Outputs"),
                                                             p("Model is a Poisson-distributed Generalised Additive Model (GAM) with a smooth term on overall trend and a smooth term on monthly seasonality. Smooth term on seasonality uses cyclic cubic spline to ensure continuity."),
                                                             br(),
                                                             htmlOutput(ts_gam_dev)
                                              ),
                                                      column(4,
                                                             shinycssloaders::withSpinner(plotOutput("ts_gam_trend", height = "450px"))
                                              ),
                                                      column(1),
                                                      column(4,
                                                             shinycssloaders::withSpinner(plotOutput("ts_gam_seas", height = "450px"))
                                              )
                                             ),
                                             fluidRow(column(9,
                                                             h3("Forecast Modelling"),
                                                             p("Forecast uses the GAM model defined above."),
                                                             br(),
                                                             shinycssloaders::withSpinner(plotOutput("forecast_mod", height = "450px"))
                                             )
                                            )
                                           )
                                          )
                                         ),
                                        
                                        tabPanel("Cross-Sectional",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     h2("Page Details"),
                                                     p("Data is at the postcode level."),
                                                     selectInput("cs_year", "Select a year",
                                                                 choices = years, selected = years[1])
                                                   ),
                                                   mainPanel()
                                          )
                                         )
                                        )
                                       ),
                   
                   #----------------------Help page header-------------------
                   
                   tabPanel(navtab1,
                            fluidRow(h1("About")
                            ),
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #05445E; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px;")
 )
)