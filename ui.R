#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Multiplex Analysis Plots"),
  
  # Divide into panels
  tabsetPanel(
    
    ########################
    ### FUNCTIONAL PANEL ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ########################
    
    tabPanel("Functional Panel",
             
             tabsetPanel(
               
               #################
               ### VIEW DATA ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               #################
               
               tabPanel("View Data",
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            ## Load file
                            fileInput(inputId = "fxnlFile", label = h4("Select Input Data File"), placeholder = "/path/to/fxnl.xlsx"),
                            ## Select View
                            radioButtons("dataSelect", label = h4("Select Which Data to View"),
                                         choices = list("Raw" = "raw", "Summarized" = "sum", "Calculated" = "calc"),
                                         selected = "raw"),
                            #br(),
                            h4("Select Optional Subsets to View"),
                            h6("Population and Gate are options for the 'Raw' and 'Summarized' views, while Group and Calculation are for the 'Calculated' view.
                               Samples can be used for all views."),
                            ## Select Populations
                            selectInput(inputId = "fxnlPop",
                                        label = "Population/Group",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Select Gates
                            selectInput(inputId = "fxnlGate",
                                        label = "Gate/Calculation",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Select columns
                            selectInput(inputId = "fxnlSamples",
                                        label = "Samples",
                                        choices = "all", 
                                        selected = "all",
                                        multiple = T),
                            ## Output file name
                            textInput(inputId = "fxnlOutName",
                                      label = "Output File Name",
                                      value = "fxnl.xlsx"),
                            ## Output directory
                            textInput(inputId = "fxnlOutDir",
                                      label = "Output Directory",
                                      value = "~/Downloads"),
                            ## Download
                            actionButton(inputId = "saveFxnl", label = "Download Current Data View"),
                            ## Update user
                            textOutput(outputId = "fxnlUpdate")
                          ), # sidebarPanel - FunctionalPanel - ViewData
                          
                          mainPanel(DT::dataTableOutput("fxnl")
                                    
                          ) # mainPanel - FunctionalPanel - ViewData
                        ) # sidebarLayout - FunctionalPanel - ViewData
                ), # tabPanel - FunctionalPanel - ViewData
               
               ##################
               ### PIE CHARTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               ##################

               tabPanel("Pie Charts",

                        sidebarLayout(
                          sidebarPanel(
                            h3("Pie Chart for Different Immune Cell Distributions"),
                            h6("You must upload data in the 'View Data' tab first."),
                            br(),
                            h4("Can select:"),
                            tags$ol(tags$li("A single sample and a single group"),
                                    tags$li("Multiple samples and a single group"),
                                    tags$li("a single sample and multiple groups.")),
                            br(),
                            selectInput(inputId = "pieGrp",
                                        label = h4("Select Which Group(s) to Plot"),
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            selectInput(inputId = "pieSample",
                                        label = h4("Select Which Sample(s) to Plot"),
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            h4("Select How to Display Percentages"),
                            tags$ol(tags$li("Character vector of calculations that will be left blank"),
                                    tags$li("Numeric vector of indices of calculations that will be left blank"),
                                    tags$li("NA - will print all percentages greater than 5%")),
                            textInput(inputId = "piePct",
                                      label = NULL,
                                      value = NA),
                            br(),
                            h4("Plot"),
                            actionButton(inputId = "doPlotPie", label = "Plot"),
                            actionButton(inputId = "stopPlotPie", label = "Clear"),
                            br(),
                            h4("Save"),
                            ## Output file name
                            textInput(inputId = "fxnlPieOutName",
                                      label = "Output File Name",
                                      value = "fxnlPie.pdf"),
                            ## Output directory
                            textInput(inputId = "fxnlPieOutDir",
                                      label = "Output Directory",
                                      value = "~/Downloads"),
                            ## Download
                            actionButton(inputId = "saveFxnlPie", label = "Download Current Data View"),
                            ## Update user
                            textOutput(outputId = "fxnlPieUpdate")
                          ), # sidebarPanel - FunctionalPanel - PieCharts
                          
                          mainPanel(plotOutput("fxnlPie", width = 1000, height = 1000)
                          ) # mainPanel - FunctionalPanel - PieCharts
                        ) # sidebarLayout - FunctionalPanel - PieCharts
                ), # tabPanel - FunctionalPanel - PieCharts
               
               #############################
               ### HORIZONTAL BAR CHARTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               #############################
               
               tabPanel("Horizontal Bar Chart",

                        sidebarLayout(
                          sidebarPanel(
                            h3("Stacked Bar for Pct CD8 - Check this"),
                            h6("You must upload data in the 'View Data' tab first."),
                            br(),
                            selectInput(inputId = "hBarGrp",
                                        label = h4("Select Which Group(s) to Plot"),
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            selectInput(inputId = "hBarSample",
                                        label = h4("Select Which Sample(s) to Plot"),
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            br(),
                            h4("Plot"),
                            actionButton(inputId = "doPlotHBar", label = "Plot"),
                            actionButton(inputId = "stopPlotHBar", label = "Clear"),
                            br(),
                            h4("Save"),
                            ## Output file name
                            textInput(inputId = "fxnlHBarOutName",
                                      label = "Output File Name",
                                      value = "fxnlHBar.pdf"),
                            ## Output directory
                            textInput(inputId = "fxnlHBarOutDir",
                                      label = "Output Directory",
                                      value = "~/Downloads"),
                            ## Download
                            actionButton(inputId = "saveFxnlHBar", label = "Download Current Data View"),
                            ## Update user
                            textOutput(outputId = "fxnlHBarUpdate")
                          ), # sidebarPanel - FunctionalPanel - horizontalBar
                          
                          mainPanel(plotOutput("fxnlHBar", width = 1000, height = 1000)
                          ) # mainPanel - FunctionalPanel - horizontalBar
                        ) # sidebarLayout - FunctionalPanel - horizontalBar


             ), # tabPanel - FunctionalPanel - horizontalBar
             
             #######################
             ### SUNBURST CHARTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             #######################
             
             tabPanel("Sunburst Chart",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h3("Sunburst Chart for CD8 T Cell Groups"),
                          h6("You must upload data in the 'View Data' tab first."),
                          br(),
                          selectInput(inputId = "fSunGrp", # Pct Ki67, etc.
                                      label = h4("Select Which Sub-Group(s) to Plot"),
                                      choices = c("all", "PctKi67", "PctGRZB+"),
                                      selected = "all",
                                      multiple = T),
                          selectInput(inputId = "fSunSample",
                                      label = h4("Select Which Sample(s) to Plot"),
                                      choices = "all",
                                      selected = "all",
                                      multiple = T),
                          checkboxInput(inputId = "fSunPct",
                                        label = h4("Output Percentages"),
                                        value = TRUE),
                          br(),
                          h4("Plot"),
                          actionButton(inputId = "doPlotFSun", label = "Plot"),
                          actionButton(inputId = "stopPlotFSun", label = "Clear"),
                          br(),
                          h4("Save"),
                          ## Output file name
                          textInput(inputId = "fxnlSunOutName",
                                    label = "Output File Name",
                                    value = "fxnlSun.pdf"),
                          ## Output directory
                          textInput(inputId = "fxnlSunOutDir",
                                    label = "Output Directory",
                                    value = "~/Downloads"),
                          ## Download
                          actionButton(inputId = "saveFxnlSun", label = "Download Current Data View"),
                          ## Update user
                          textOutput(outputId = "fxnlSunUpdate")
                        ), # sidebarPanel - FunctionalPanel - Sunburst
                        
                        mainPanel(plotOutput("fxnlSun", width = 1000, height = 1000)
                        ) # mainPanel - FunctionalPanel - Sunburst
                      ) # sidebarLayout - FunctionalPanel - Sunburst
                      
                      
             ) # tabPanel - FunctionalPanel - sunburst
             ) # tabsetPanel - FunctionalPanel
    ),
             
    ##############################
    ### MYELOID/LYMPHOID PANEL ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##############################
    
    tabPanel("Myeloid/Lymphoid Panel",

             tabsetPanel(

               #################
               ### VIEW DATA ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               #################

               tabPanel("View Data",

                        sidebarLayout(

                          sidebarPanel(
                            ## Load file
                            fileInput(inputId = "mlFile", label = h4("Select Input Data File"), placeholder = "/path/to/myeloid_lymphoid.xlsx"),
                            ## Select View
                            radioButtons("mlDataSelect", label = h4("Select Which Data to View"),
                                         choices = list("Raw" = "raw", "Summarized" = "sum", "Normalized" = "norm", "Ratio" = "ratio"),
                                         selected = "raw"),
                            h4("Select Optional Subsets to View"),
                            ## Select Populations
                            selectInput(inputId = "mlPanel",
                                        label = "Panel/Cell",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Select Gates
                            selectInput(inputId = "mlGate",
                                        label = "Gate/Subtype",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Select columns
                            selectInput(inputId = "mlSamples",
                                        label = "Samples",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Output file name
                            textInput(inputId = "mlOutName",
                                      label = "Output File Name",
                                      value = "ml.xlsx"),
                            ## Output directory
                            textInput(inputId = "mlOutDir",
                                      label = "Output Directory",
                                      value = "~/Downloads"),
                            ## Download
                            actionButton(inputId = "saveML", label = "Download Current Data View"),
                            ## Update user
                            textOutput(outputId = "mlUpdate")
                            ## Select
                            ), # sidebarPanel

                          mainPanel(DT::dataTableOutput("ml")) # mainPanel
                          #mainPanel(plotOutput("tester", width = 1000, height = 1000))

                        )), # sidebarLayout, tabPanel

               ##########################
               ### STACKED BAR CHARTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               ##########################

               tabPanel("Stacked Bar Chart",

                        sidebarLayout(
                          sidebarPanel(
                            h3("Stacked Bar Chart For Immune Cell Composition"),
                            br(),
                            h4("Can select:"),
                            tags$ol(tags$li("A single sample and a single group"),
                                    tags$li("Multiple samples and a single group"),
                                    tags$li("a single sample and multiple groups.")),
                            br(),
                            selectInput(inputId = "mlSbarSample",
                                        label = h4("Select Which Sample(s) to Plot"),
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            br(),
                            h4("Plot"),
                            actionButton(inputId = "doPlotSbar", label = "Plot"),
                            actionButton(inputId = "stopPlotSbar", label = "Clear"),
                            br(),
                            h4("Save"),
                            ## Output file name
                            textInput(inputId = "mlSbarOutName",
                                      label = "Output File Name",
                                      value = "mlSbar.pdf"),
                            ## Output directory
                            textInput(inputId = "mlSbarOutDir",
                                      label = "Output Directory",
                                      value = "~/Downloads"),
                            ## Download
                            actionButton(inputId = "saveMLSbar", label = "Download Current Data View"),
                            ## Update user
                            textOutput(outputId = "mlSbarUpdate")
                          ), #sidebarPanel
                          mainPanel(plotOutput("mlSbar", width = 1000, height = 1000))
                          #mainPanel(DT::dataTableOutput("calc"))
                        )), # sidebarLayout, tabPanel
               
               #######################
               ### SUNBURST CHARTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               #######################
               
               tabPanel("Sunburst Chart",
                        
                        sidebarLayout(
                          sidebarPanel(
                            h3("Sunburst Chart for Immune Cell Composition"),
                            h6("You must upload data in the 'View Data' tab first."),
                            br(),
                            selectInput(inputId = "mlSunSample",
                                        label = h4("Select Which Sample(s) to Plot"),
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            checkboxInput(inputId = "mlSunPct",
                                          label = h4("Output Percentages"),
                                          value = TRUE),
                            br(),
                            h4("Plot"),
                            actionButton(inputId = "doPlotMLSun", label = "Plot"),
                            actionButton(inputId = "stopPlotMLSun", label = "Clear"),
                            br(),
                            h4("Save"),
                            ## Output file name
                            textInput(inputId = "mlSunOutName",
                                      label = "Output File Name",
                                      value = "mlSun.pdf"),
                            ## Output directory
                            textInput(inputId = "mlSunOutDir",
                                      label = "Output Directory",
                                      value = "~/Downloads"),
                            ## Download
                            actionButton(inputId = "saveMLSun", label = "Download Current Data View"),
                            ## Update user
                            textOutput(outputId = "mlSunUpdate")
                          ), # sidebarPanel - FunctionalPanel - horizontalBar
                          
                          mainPanel(plotOutput("mlSun", width = 1000, height = 1000)
                          ) # mainPanel - myeloid/lymphoid Panel - sunburst
                        ) # sidebarLayout - myeloid/lymphoid Panel - sunburst
                        
                        
               ) # tabPanel - myeloid/lymphoid Panel - sunburst
             ) # tabsetPanel
    ) # tabPanel
  ) # tabset panel
)) # fluidPage
