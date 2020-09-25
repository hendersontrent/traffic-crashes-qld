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
                                        tabPanel("Cross-Sectional",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     h2("Page Details"),
                                                     p("Data is at the postcode level."),
                                                     selectInput("cs_year", "Select a year",
                                                                 choices = years, selected = years[8]),
                                                     selectInput("cs_severity", "Select a crash severity",
                                                                 choices = severities, selected = severities[1]),
                                                     br(),
                                                     p("Click on and hover over the graphs with your mouse to interact with them.")
                                                   ),
                                                   mainPanel(
                                                     fluidRow(
                                                       column(11,
                                                              h3("Statistical Model Components"),
                                                              p("Model is a Poisson-distributed Generalised Additive Model (GAM) with smooth terms on population size and socioeconomic advantage index."),
                                                              br(),
                                                              htmlOutput("cs_gam_dev"),
                                                              br(),
                                                              column(5,
                                                                     h4("Usual Resident Population"),
                                                                     shinycssloaders::withSpinner(plotlyOutput("pop_plot", height = "450px"))
                                                                     ),
                                                              column(1),
                                                              column(5,
                                                                     h4("SEIFA Index of Occupation Education"),
                                                                     shinycssloaders::withSpinner(plotlyOutput("ses_plot", height = "450px"))
                                                       )
                                                      )
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(10,
                                                              shinycssloaders::withSpinner(plotlyOutput("ra_plot", height = "450px"))
                                                       )
                                                     ),
                                                     hr(),
                                                     fluidRow(
                                                       column(11,
                                                              h3("Statistical Model Outputs"),
                                                              br(),
                                                              column(5,
                                                                     h4("Summary of Coefficients"),
                                                                     shinycssloaders::withSpinner(tableOutput("tidy_table"))
                                                                     ),
                                                              column(1),
                                                              column(5,
                                                                     h4("Summary of Model Fit"),
                                                                     shinycssloaders::withSpinner(tableOutput("glance_table"))
                                                     )
                                                    )
                                                   )
                                                  )
                                                 )
                                                ),
                                        tabPanel("Time Series",
                                          sidebarLayout(
                                            sidebarPanel(
                                              h2("Page Details"),
                                              selectInput("ts_severity", "Select a crash severity",
                                                          choices = severities, selected = severities[3]),
                                              br(),
                                              p("Click on and hover over the graphs with your mouse to interact with them.")
                                            ),
                                            mainPanel(
                                              fluidRow(column(11,
                                                              h3("Raw Time Series Data"),
                                                              br(),
                                                              shinycssloaders::withSpinner(plotlyOutput("raw_ts", height = "450px")),
                                                              br()
                                              )
                                             ),
                                             hr(),
                                             fluidRow(column(11,
                                                             h3("Generalised Additive Model Outputs"),
                                                             p("Model is a Poisson-distributed Generalised Additive Model (GAM) with a smooth term on overall trend and a smooth term on monthly seasonality. Smooth term on seasonality uses cyclic cubic spline to ensure continuity."),
                                                             br(),
                                                             htmlOutput("ts_gam_dev"),
                                                             br(),
                                              ),
                                              br(),
                                                      column(5,
                                                             h4("Trend Component"),
                                                             shinycssloaders::withSpinner(plotlyOutput("ts_gam_trend", height = "450px"))
                                              ),
                                                      column(1),
                                                      column(5,
                                                             h4("Seasonal Component"),
                                                             shinycssloaders::withSpinner(plotlyOutput("ts_gam_seas", height = "450px"))
                                              )
                                             ),
                                             br(),
                                             hr(),
                                             fluidRow(column(11,
                                                             h3("Forecast Modelling"),
                                                             p("Forecast uses the GAM model defined above."),
                                                             br(),
                                                             #shinycssloaders::withSpinner(plotOutput("forecast_mod", height = "450px"))
                                                             p("COMING SOON")
                                             )
                                            )
                                           )
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