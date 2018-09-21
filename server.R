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
library(writexl)

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
  
  ## Update available Population and Sample based on which input is selected
  observe({
    if (is.null(fxnlInData())) {
      return(NULL)
    } else {
      if (whichData() %in% c("raw", "sum")) {
        currData <- fxnlInData()[[whichData()]]
        updateSelectInput(session, "fxnlPop", choices = c("all", unique(currData[,get("Population")])), selected = "all")
        updateSelectInput(session, "fxnlSamples", choices = c("all", setdiff(names(currData), c("Population", "Gate"))), selected = "all")
      } else if (whichData() == "calc") {
        currData <- fxnlCalcData()
        updateSelectInput(session, "fxnlPop", choices = c("all", unique(currData[,get("Group")])), selected = "all")
        updateSelectInput(session, "fxnlSamples", choices = c("all", setdiff(names(currData), c("Group", "Calc"))), selected = "all")
      }
    }})
  
  ## Update Gates available based on which input is selected
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
      if (is.null(input$fxnlPop)) {
        return(NULL)
      } else if (input$fxnlPop == "all") {
        currGate_v <- unique(currData[,get(currCol)])
      } else {
        currGate_v <- unique(currData[get(currSubCol) %in% input$fxnlPop, get(currCol)])
      }
      
      ## Update selection
      updateSelectInput(session, "fxnlGate", choices = c("all", currGate_v), selected = "all")
      }})
  
  ## Update output filename based on the selected arguments
  observe({
    if (is.null(fxnlInData())) {
      return(NULL)
    } else {
      ## Just "fxnl" and either "raw/sum/calc"
      baseName_v <- paste("fxnl", whichData(), sep = "_-_")
      
      ## Add population/group, if specified
      if (!is.null(input$fxnlPop)){ 
        if (input$fxnlPop[1] != "all") {
          temp <- paste(input$fxnlPop, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        }
      } # fi fxnlPop
      
      ## Add gate/calc, if specified
      if (!is.null(input$fxnlGate)){
        if (input$fxnlGate[1] != "all"){
          temp <- paste(input$fxnlGate, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } 
      } # fi fxnlGate
      
      ## Add samples, if specified
      if (!is.null(input$fxnlSamples)) {
        if (input$fxnlSamples[1] != "all"){
          temp <- paste(input$fxnlSamples, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        }
      } # fi fxnlSamples
      ## Remove spaces
      baseName_v <- gsub(" ", "_", baseName_v)
      ## Add extension
      baseName_v <- paste0(baseName_v, ".xlsx")
      ## Update
      cat(sprintf("fxnlOutName should be set as %s\n", baseName_v))
      updateTextInput(session, "fxnlOutName", value = baseName_v)
    }
  }) # update filename
  
  ## Get current view
  outputData <- reactive({
    if (is.null(fxnlInData())){
      return(NULL)
    } else {
      ## Set data and columns based on which View
      if (whichData() %in% c("raw", "sum")) {
        currData <- fxnlInData()[[whichData()]]; currCol <- "Gate"; currSubCol <- "Population"
      } else if (whichData() %in% c("calc")) {
        currData <- fxnlCalcData(); currCol <- "Calc"; currSubCol <- "Group"
      }
      
      ## Subset Population/Group column based on selection
      if (is.null(input$fxnlPop)) {
        currData <- currData
      } else if (input$fxnlPop[1] != "all") {
        currData <- currData[get(currSubCol) %in% input$fxnlPop,]
      }
      
      ## Subset gate/calc column based on selection
      if (is.null(input$fxnlGate)) {
        currData <- currData
      } else if (input$fxnlGate[1] != "all") {
        currData <- currData[get(currCol) %in% input$fxnlGate,]
      }
      
      ## Subset sample column based on selection
      if (is.null(input$fxnlSamples)) {
        currData <- currData
      } else if (input$fxnlSamples[1] != "all") {
        currData <- currData[,mget(c(currSubCol, currCol, input$fxnlSamples))]}
      currData
    }
  })
  
  ## Output the table
  output$fxnl <- DT::renderDataTable({
    if (is.null(fxnlInData())) {
      print("fxnlInData is null in renderDataTable")
      return()
    } else {
      DT::datatable(outputData())
    }
  })

  ## Reactive value
  message_v <- reactiveVal(); message_v("blank")
  
  ## Write table
  observeEvent(input$saveFxnl, {
    ## Make name
    out_v <- file.path(input$fxnlOutDir, input$fxnlOutName)
    ## Make message
    message_v(paste0("Saved current view to: ", out_v))
    ## Write table
    write_xlsx(x = outputData(), path = out_v)
  })
  
  ## Update user
  output$fxnlUpdate <- renderText({ if (message_v() == "blank") { return(NULL) } else { message_v() } })
  
  #####################
  ### PIE CHART TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~
  #####################

  ## Update grouping variable options
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
  
  ## Update output filename based on select arguments
  observe({
    if (is.null(fxnlCalcData())) {
      return(NULL)
    } else {
      
      baseName_v <- "fxnlPie"
      
      ## Add group, if specified
      if (!is.null(input$pieGrp)){ 
        if (input$pieGrp[1] != "all") {
          temp <- paste(input$pieGrp, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        }
      } # fi fxnlPop
      
      ## Add gate/calc, if specified
      if (!is.null(input$pieSample)){
        if (input$pieSample[1] != "all"){
          temp <- paste(input$pieSample, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } 
      } # fi fxnlGate

      ## Remove spaces
      baseName_v <- gsub(" ", "_", baseName_v)
      ## Add extension
      baseName_v <- paste0(baseName_v, ".pdf")
      ## Update
      cat(sprintf("fxnlPieOutName should be set as %s\n", baseName_v))
      updateTextInput(session, "fxnlPieOutName", value = baseName_v)
    }
  }) # update filename

  ## Get plot
  outputFxnlPie <- reactive({
    if (piePlot_logical()) {
      pies <- plotPie(fxnlCalcData(),
                      group_v = input$pieGrp,
                      sample_v = input$pieSample,
                      color_dt = functionalColors_dt,
                      pct_v = input$piePct)
      return(pies)
    } else {
      return(NULL)
    }
  })
  
  ## Plot
  output$fxnlPie <- renderPlot({
    if (piePlot_logical()) {
      grid.arrange(grobs = outputFxnlPie())
    }
  })
  
  ## Reactive value
  pieMessage_v <- reactiveVal(); pieMessage_v("blank")
  
  ## Write pie
  observeEvent(input$saveFxnlPie, {
    ## Make name
    out_v <- file.path(input$fxnlPieOutDir, input$fxnlPieOutName)
    ## Make message
    pieMessage_v(paste0("Saved current view to: ", out_v))
    ## Plot
    pdf(file = out_v)
    grid.arrange(grobs = outputFxnlPie())
    dev.off()
  })
  
  ## Update user
  output$fxnlPieUpdate <- renderText({ if (pieMessage_v() == "blank") { return(NULL) } else { pieMessage_v() } })
  
  # output$fxnlPie <- renderPlot({
  #   if (piePlot_logical()) {
  #     pies <- plotPie(fxnlCalcData(),
  #                     group_v = input$pieGrp,
  #                     sample_v = input$pieSample,
  #                     color_dt = functionalColors_dt,
  #                     pct_v = input$piePct)
  #     grid.arrange(grobs = pies)
  #   } else {
  #     return()
  #   }})

  output$tester <- renderPlot(plot(1:10))

  ##################################
  ### MYELOID/LYMPHOID PANEL TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##################################

  #####################
  ### VIEW DATA TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~
  #####################



  # ########################
  # ### Stacked Bar Page ###
  # ########################
  # # Obtain file
  # sBarInFile <- reactive({
  #   if (is.null(input$sBarFile)){
  #     #print("input$file is null")
  #     return(NULL)
  #   } else {
  #     return(input$sBarFile)
  #     #print(input$pieFile)
  #   } # fi
  # }) # pieInFile
  # 
  # # Obtain data
  # sBarInData <- reactive({
  #   if (is.null(sBarInFile())){
  #     return(NULL)
  #   } else {
  #     fread(sBarInFile()$datapath)
  #   } # fi
  # }) # pieInData
  # 
  # # Update grouping variable options
  # observe({
  #   if (is.null(sBarInData())){
  #     return(NULL)
  #   } else {
  #     updateSelectInput(session, "sBarGroup", choices = names(sBarInData()))
  #   }
  # }) # update pieGroup
  # 
  # # Update pieGroup subset options
  # observe({
  #   sBarGroup_v <- input$sBarGroup
  #   # print(c("pieGroup_v: ", pieGroup_v))
  #   if (sBarGroup_v == ''){
  #     sBarOptions_v <- NULL
  #   } else {
  #     sBarOptions_v <- sBarInData()[,get(sBarGroup_v)]
  #   }
  #   #print(c("sBarOptions_v:", pieOptions_v))
  #   updateSelectInput(session, "whichsBarGroup", choices = c("all", unique(sBarOptions_v)))
  # }) # update group subset
  # 
  # # Pie chart toggle - default is FALSE (no plot output)
  # sBar_plot_logical <- reactiveValues(result = FALSE)
  # 
  # # Set logical to true, if "Create Plot" is pressed.
  # observeEvent(input$doPlotsBar, {
  #   sBar_plot_logical$result <- TRUE
  # })
  # 
  # # Reset to false if "Clear Plot" is pressed
  # observeEvent(input$stopPlotsBar, {
  #   sBar_plot_logical$result <- FALSE
  # })
  # 
  # # Pie chart output based on plot_logical status
  # output$stackedBarPlot <- renderPlot({
  #   if (sBar_plot_logical$result){
  #     tcrStackedBar(sBarInData(),
  #                   grouping_column_v = input$sBarGroup,
  #                   which_groups_v = input$whichsBarGroup,
  #                   divisions_v = unlist(strsplit(input$sBarDivisions, split = ',')),
  #                   operation_v = input$sBarOperation)
  #   } else {
  #     return()
  #   } # fi
  # }) # renderPlot
  
}) # shinyServer

shinyApp(ui = ui, server = server)
