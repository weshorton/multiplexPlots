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
library(gridExtra)

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
      return(data_ls)
    }})
  
  ### Perform standard calculations
  fxnlCalcData <- reactive({
    if (is.null(fxnlInData())) {
      print("fxnlInData is null in calc reactive")
      return(NULL)
    } else {
      standardCalcs(sum_dt = fxnlInData()[["sum"]])
    }
  })
  
  ## Select which data to view
  whichData <- reactiveVal()
  whichData("raw")
  observeEvent(input$dataSelect, {whichData(input$dataSelect)})
  
  ## Options to subset
  observe({
    if (is.null(fxnlInData())) {
      return(NULL)
    } else {
      if (whichData() %in% c("raw", "sum")) {
        currData <- fxnlInData()[[whichData()]]
        updateSelectInput(session, "Population", choices = c("all", unique(currData[,get("Population")])), selected = "all")
        updateSelectInput(session, "Samples", choices = c("all", setdiff(names(currData), c("Population", "Gate"))), selected = "all")
      } else if (whichData() == "calc") {
        currData <- fxnlCalcData()
        updateSelectInput(session, "Population", choices = c("all", unique(currData[,get("Group")])), selected = "all")
        updateSelectInput(session, "Samples", choices = c("all", setdiff(names(currData), c("Group", "Calc"))), selected = "all")
      }
    }})
  
  # Subset Gate
  observe({
    if (is.null(fxnlInData())) {
      return(NULL)
    } else {
      ## Need to set data and column depending on which View is selected
      if (whichData() %in% c("raw", "sum")){
        currData <- fxnlInData()[[whichData()]]; currCol <- "Gate"; currSubCol <- "Population"
      } else if (whichData() %in% c("calc")) {
        currData <- fxnlCalcData(); currCol <- "Calc"; currSubCol <- "Group"
      }
      
      ## Now need to dynamically update the gate/calc column based on the population/group selection
      if (is.null(input$Population)) {
        return(NULL)
      } else if (input$Population == "all") {
        currGate_v <- unique(currData[,get(currCol)])
      } else {
        currGate_v <- unique(currData[get(currSubCol) %in% input$Population, get(currCol)])
      }
        
      ## Update selection
      updateSelectInput(session, "Gate", choices = c("all", currGate_v), selected = "all")
    }})
  
  ## Output
  output$fxnl <- DT::renderDataTable({
    if (is.null(fxnlInData())){
      print("fxnlInData is null in renderDataTable")
      return()
    } else {
      DT::datatable({
        ## Set data and columns based on which View
        if (whichData() %in% c("raw", "sum")) {
          currData <- fxnlInData()[[whichData()]]; currCol <- "Gate"; currSubCol <- "Population"
        } else if (whichData() %in% c("calc")) {
          currData <- fxnlCalcData(); currCol <- "Calc"; currSubCol <- "Group"
        }
        
        ## Subset Population/Group column based on selection
        if (is.null(input$Population)) {
          currData <- currData
        } else if (input$Population != "all") { 
          currData <- currData[get(currSubCol) %in% input$Population,]
        }
        
        ## Subset gate/calc column based on selection
        if (is.null(input$Gate)) {
          currData <- currData
        } else if (input$Gate != "all") { 
          currData <- currData[get(currCol) %in% input$Gate,]
        }
        
        ## Subset sample column based on selection
        if (is.null(input$Samples)) {
          currData <- currData
        } else if (input$Samples != "all") {
          currData <- currData[,mget(c(currSubCol, currCol, input$Samples))]}
      currData
    })}})
  
  ########################
  ### Venn Diagram Tab ###~~~~~~~~~~~~~~~~~~~~~~~
  ########################

  ## Update Group and Variable options
  observe({
    if (is.null(fxnlCalcData())){
      return(NULL)
    } else {
      data <- fxnlCalcData()
      updateSelectInput(session, "pieGrp", choices = unique(data[, Group]))
      updateSelectInput(session, "pieSample", choices = c("all", setdiff(names(data), c("Group", "Calc"))), selected = "all")
    }
  }) # update pieGrp and pieSample
  
  ## Plotting logic - set to TRUE if "plot" button is pressed, FALSE if 'clear' is pressed. Default is FALSE
  piePlot_logical <- reactiveVal(value = FALSE)
  observeEvent(input$doPlotPie, {piePlot_logical(TRUE)})
  observeEvent(input$stopPlotPie, {piePlot_logical(FALSE)})
  
  ## Plot
  output$fxnlPie <- renderPlot({
    if (piePlot_logical()) {
      print(fxnlCalcData()[1:5,1:5])
      print(input$pieGrp)
      print(input$pieSample)
      print(functionalColors_dt[1:5,1:5])
      print(input$piePct)
      pies <- plotPie(fxnlCalcData(),
                      group_v = input$pieGrp,
                      sample_v = input$pieSample,
                      color_dt = functionalColors_dt,
                      pct_v = input$piePct)
      grid.arrange(grobs = pies)
    } else {
      return()
    }
  })
  
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
