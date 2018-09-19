#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

####################
### DEPENDENCIES ###
####################

### Libraries
library(shiny)
library(data.table)
library(DT)

### Code
files_v <- list.files("./scripts/", full.names = T)
invisible(lapply(files_v, function(x) source(x)))
source("./ui.R")

### Options
options(shiny.maxRequestSize=1000*1024^2)

### Reference
cellTypeColors_dt <- fread("./data/cellTypeColors.txt")
functionalColors_dt <- fread("./data/functionalColors.txt")
subTypeColors_dt <- fread("./data/subTypeColors.txt")
tCellColors_dt <- fread("./data/tCellColors.txt")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  ############################
  ### FUNCTIONAL PANEL TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ############################
  
  #####################
  ### VIEW DATA TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~
  #####################
  
  ## Obtain File
  fxnlInFile <- reactive({
    if (is.null(input$fxnlFile)) {
      print("fxnlInFile is NULL")
      return(NULL)
    } else {
      return(input$fxnlFile)
    }})
    
  ## Obtain Data
  fxnlInData <- reactive({
    if (is.null(input$fxnlFile)) {
      print("fxnlInData is NULL in reactive.")
      return(NULL)
    } else {
      data_ls <- readRaw(fxnlInFile()$datapath)
      print(data_ls$raw[1:5,])
      return(data_ls)
      #fread(fxnlInFile()$datapath)
    }})
  
  ## Select which data to view
  whichData <- reactiveVal()
  whichData("raw")
  observeEvent(input$dataSelect, {whichData(input$dataSelect)})
  
  ## Options to subset
  observe({
    if (is.null(fxnlInData())) {
      return(NULL)
    } else {
      currData <- fxnlInData()[[whichData()]]
      updateSelectInput(session, "Population", choices = c("all", unique(currData[,get("Population")])), selected = "all")
      updateSelectInput(session, "Samples", choices = c("all", setdiff(names(currData), c("Population", "Gate"))), selected = "all")
    }})
  
  # Subset Gate
  observe({
    if (is.null(fxnlInData())) {
      return(NULL)
    } else {
      currData <- fxnlInData()[[whichData()]]
      if (is.null(input$Population)) {
        return(NULL)
      } else if (input$Population == "all") {
        currGate_v <- unique(currData[,get("Gate")])
      } else {
        currGate_v <- unique(currData[Population %in% input$Population, get("Gate")])
      }
      updateSelectInput(session, "Gate", choices = c("all", currGate_v), selected = "all")
    }})
  
  ## Output
  output$fxnl <- DT::renderDataTable({
    if (is.null(fxnlInData())){
      print("fxnlIndata is null in renderDataTable")
      return()
    } else {
      DT::datatable({
      data <- fxnlInData()[[whichData()]]
      if (is.null(input$Population)) {
        data <- data
        } else if (input$Population != "all") { data <- data[Population %in% input$Population,]}
      if (is.null(input$Gate)) {
        data <- data
      }  else if (input$Gate != "all") { data <- data[Gate %in% input$Gate,]}
      if (is.null(input$Samples)) {
        data <- data
      } else if (input$Samples != "all") {data <- data[,mget(c("Population", "Gate", input$Samples))]}
      data
    })}})
  
  ########################
  ### Venn Diagram Tab ###~~~~~~~~~~~~~~~~~~~~~~~
  ########################
  
  ### Perform standard calculations
  fxnlCalcData <- reactive({
    if (is.null(fxnlInData())) {
      print("fxnlInData is null in calc reactive")
      return(NULL)
    } else {
      standardCalcs(sum_dt = fxnlInData()[["raw"]])
    }
  })
  
  output$calc <- DT::renderDataTable({
    if (is.null(fxnlCalcData())) {
      print("fxnlCalcData() is null")
      return()
    } else {
      print("This is supposed to be output of standardCalcs():")
      DT::datatable({
        foo <- fxnlCalcData()
        return(foo)
      })
    }
  })
  
  ######################
  ### Pie Chart Page ###
  ######################
  
  # Obtain file
  pieInFile <- reactive({
    if (is.null(input$pieFile)){
      #print("input$file is null")
      return(NULL)
    } else {
      return(input$pieFile)
      #print(input$pieFile)
    } # fi
  }) # pieInFile
  
  # Obtain data
  pieInData <- reactive({
    if (is.null(pieInFile())){
      return(NULL)
    } else {
      fread(pieInFile()$datapath)
    } # fi
  }) # pieInData
  
  # Update grouping variable options
  observe({
    if (is.null(pieInData())){
      return(NULL)
    } else {
      updateSelectInput(session, "pieGroup", choices = names(pieInData()))
    }
  }) # update pieGroup
  
  # Update pieGroup subset options
  observe({
    pieGroup_v <- input$pieGroup
    # print(c("pieGroup_v: ", pieGroup_v))
    if (pieGroup_v == ''){
      pieOptions_v <- NULL
    } else {
      pieOptions_v <- pieInData()[,get(pieGroup_v)]
    }
    #print(c("pieOptions_v:", pieOptions_v))
    updateSelectInput(session, "whichPieGroup", choices = c("all", unique(pieOptions_v)))
  }) # update group subset
  
  # Pie chart toggle - default is FALSE (no plot output)
  pie_plot_logical <- reactiveValues(result = FALSE)
  
  # Set logical to true, if "Create Plot" is pressed.
  observeEvent(input$doPlotPie, {
    pie_plot_logical$result <- TRUE
  })
  
  # Reset to false if "Clear Plot" is pressed
  observeEvent(input$stopPlotPie, {
    pie_plot_logical$result <- FALSE
  })
  
  # Pie chart output based on plot_logical status
  output$pieChart <- renderPlot({
    if (pie_plot_logical$result){
      tcrPieChart(pieInData(), 
                  grouping_column_v = input$pieGroup, 
                  which_groups_v = input$whichPieGroup,
                  divisions_v = unlist(strsplit(input$pieDivisions, split = ',')), 
                  operation_v = input$pieOperation)
      } else {
        return()
        } # fi
    }) # renderPlot
  
  output$tester <- renderPlot(plot(1:10))
  
  ########################
  ### Stacked Bar Page ###
  ########################
  # Obtain file
  sBarInFile <- reactive({
    if (is.null(input$sBarFile)){
      #print("input$file is null")
      return(NULL)
    } else {
      return(input$sBarFile)
      #print(input$pieFile)
    } # fi
  }) # pieInFile
  
  # Obtain data
  sBarInData <- reactive({
    if (is.null(sBarInFile())){
      return(NULL)
    } else {
      fread(sBarInFile()$datapath)
    } # fi
  }) # pieInData
  
  # Update grouping variable options
  observe({
    if (is.null(sBarInData())){
      return(NULL)
    } else {
      updateSelectInput(session, "sBarGroup", choices = names(sBarInData()))
    }
  }) # update pieGroup
  
  # Update pieGroup subset options
  observe({
    sBarGroup_v <- input$sBarGroup
    # print(c("pieGroup_v: ", pieGroup_v))
    if (sBarGroup_v == ''){
      sBarOptions_v <- NULL
    } else {
      sBarOptions_v <- sBarInData()[,get(sBarGroup_v)]
    }
    #print(c("sBarOptions_v:", pieOptions_v))
    updateSelectInput(session, "whichsBarGroup", choices = c("all", unique(sBarOptions_v)))
  }) # update group subset
  
  # Pie chart toggle - default is FALSE (no plot output)
  sBar_plot_logical <- reactiveValues(result = FALSE)
  
  # Set logical to true, if "Create Plot" is pressed.
  observeEvent(input$doPlotsBar, {
    sBar_plot_logical$result <- TRUE
  })
  
  # Reset to false if "Clear Plot" is pressed
  observeEvent(input$stopPlotsBar, {
    sBar_plot_logical$result <- FALSE
  })
  
  # Pie chart output based on plot_logical status
  output$stackedBarPlot <- renderPlot({
    if (sBar_plot_logical$result){
      tcrStackedBar(sBarInData(), 
                    grouping_column_v = input$sBarGroup, 
                    which_groups_v = input$whichsBarGroup,
                    divisions_v = unlist(strsplit(input$sBarDivisions, split = ',')), 
                    operation_v = input$sBarOperation)
    } else {
      return()
    } # fi
  }) # renderPlot
  
}) # shinyServer

shinyApp(ui = ui, server = server)
