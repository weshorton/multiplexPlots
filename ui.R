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
    
    #################
    ### Data Page ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #################
    
    tabPanel("Functional Panel",
             
             tabsetPanel(
               tabPanel("View Data",
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            ## Load file
                            fileInput(inputId = "fxnlFile", label = h4("Select Input Data File"), placeholder = "/path/to/fxnl.xlsx"),
                            ## Select View
                            radioButtons("dataSelect", label = h4("Select Which Data to View"),
                                         choices = list("Raw" = "raw", "Summarized" = "sum"),
                                         selected = "raw"),
                            #br(),
                            h4("Select Optional Subsets to View"),
                            ## Select Populations
                            selectInput(inputId = "Population",
                                        label = "Populations",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Select Gates
                            selectInput(inputId = "Gate",
                                        label = "Gates",
                                        choices = "all",
                                        selected = "all",
                                        multiple = T),
                            ## Select columns
                            selectInput(inputId = "Samples",
                                        label = "Samples",
                                        choices = "all", 
                                        selected = "all",
                                        multiple = T)
                          ), # sidebarPanel
                          
                          mainPanel(DT::dataTableOutput("fxnl")) # mainPanel
                          #mainPanel(plotOutput("tester", width = 1000, height = 1000))
                          
                        )), # sidebarLayout, tabPanel
               tabPanel("Venn Diagrams",

                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "Test",
                                        label = "Test",
                                        choices = "all",
                                        selected = "all")
                          ), #sidebarPanel
                          mainPanel(plotOutput("tester", width = 1000, height = 1000))
                        )) # sidebarLayout, tabPanel

             ) # tabsetPanel
    ),
             
    ######################
    ### Pie Chart Page ###
    ######################
    tabPanel("Top Clone Pie Charts",
             
             # Sidebar with all input options and plot space
             sidebarLayout(
               
               sidebarPanel(
                 # File import
                 fileInput(inputId = "pieFile", label = h3("File input"), placeholder = "/path/to/aggregate_clone_files.txt"),
      
                 # Select Grouping Variable
                 selectInput(inputId = "pieGroup",
                             label = "Grouping Variable:",
                             choices = ""),
      
                 # Select Members of Grouping Variable
                 selectInput(inputId = "whichPieGroup",
                             label = "Group Subset to Plot:",
                             choices = "", 
                             multiple = T),
      
                 # Select Divisions
                 textInput(inputId = "pieDivisions", 
                           label = h3("Comma-separated list of top clones or ranges to subset by. (e.g. 1, 2, 3 or 1:10, 11:30, 31:50)"),
                           value = "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"),
      
                 # Select operation
                 selectInput(inputId = "pieOperation",
                             label = h3("Summary function to use:"),
                             choices = list("Mean" = "mean", "Median" = "median")),
      
                 # Provide plotting action button and wait button
                 actionButton(inputId = "doPlotPie", label = "Create Plot"),
                 actionButton(inputId = "stopPlotPie", label = "Clear Plot") # New
                 ) # sidebarPanel
               ,
               
               mainPanel(
                 plotOutput("pieChart", width = 1000, height = 1000)
                 ) # mainPanel
               ) # sidebarLayout
             ), # Pie Chart Tab Panel
    
    ########################
    ### Stacked Bar Page ###
    ########################
    tabPanel("Top Clone Stacked Bar",
             
             # Sidebar with input options
             sidebarLayout(
               
               sidebarPanel(
                 # File import
                 fileInput(inputId = "sBarFile", label = h3("File input"), placeholder = "/path/to/aggregate_clone_files.txt"),
                 
                 # Select Grouping Variable
                 selectInput(inputId = "sBarGroup",
                             label = "Grouping Variable:",
                             choices = ""),
                 
                 # Select Members of Grouping Variable
                 selectInput(inputId = "whichsBarGroup",
                             label = "Group Subset to Plot:",
                             choices = "", 
                             multiple = T),
                 
                 # Select Divisions
                 textInput(inputId = "sBarDivisions", 
                           label = h3("Comma-separated list of top clones or ranges to subset by. (e.g. 1, 2, 3 or 1:10, 11:30, 31:50)"),
                           value = "1:10, 11:30, 31:50"),
                 
                 # Select operation
                 selectInput(inputId = "sBarOperation",
                             label = h3("Summary function to use:"),
                             choices = list("Mean" = "mean", "Median" = "median")),
                 
                 # Provide plotting action button and wait button
                 actionButton(inputId = "doPlotsBar", label = "Create Plot"),
                 actionButton(inputId = "stopPlotsBar", label = "Clear Plot") # New
               ) # sidebarPanel
               
               ,
               
               mainPanel(
                 plotOutput("stackedBarPlot", width = 1000, height = 1000)
                 ) # mainPanel
               ) # sidebarLayout
             ) # Stacked Bar Tab Panel
    ) # tabsetPanel
)) # fluidPage
