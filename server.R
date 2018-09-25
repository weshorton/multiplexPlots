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

### GENERAL FUNCTIONS ###

getFile <- function(input, file) {
  #' Get file name
  #' @description Shiny reactive to grab an input file name
  #' @param input The input argument to the shinyServer() fxn, which is a list of all input variables
  #' @param file character vector - should be the name of the input file that is used as the 'inputId' in fileInput() in ui.R
  #' @value return a character vector of a file name
  #' @export
  
    if (is.null(input[[file]])) {
      cat(sprintf("%s is NULL in getFile\n", file))
      return(NULL)
    } else {
      cat(sprintf("%s is %s in getFile\n", file, input[[file]]))
      return(input[[file]])
    } # fi
  
} # getFile

getData <- function(input, file, fileFxn, fxn, args_ls = NA) {
  #' Get data
  #' @description Shiny reactive to grab data from an input file, using the specified read function
  #' @param input The input argument to the shinyServer() fxn, which is a list of all input variables
  #' @param file character name of input file from ui.R
  #' @param fileFxn should be output of getFile(). Character vector that is a path to an input file
  #' @param fxn some sort of function to read in 'file'
  #' @param args_ls named list of arguments to pass to fxn
  #' @value Some sort of table
  #' @export

  if (is.null(input[[file]])) {
    cat(sprintf("%s is NULL in getData\n", file))
    return(NULL)
  } else {
    ## Make args list
    if (is.na(args_ls)){
      args_ls <- list(fileFxn)
    } else {
      args_ls <- c("input_xlsx" = fileFxn, args_ls)
    } # fi
    #data <- do.call(fxn, args = list(fileFxn))
    data <- do.call(fxn, args = args_ls)
    return(data)
  }
}

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  ############################
  ### FUNCTIONAL PANEL TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ############################
  
  #####################
  ### VIEW DATA TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~
  #####################
  
  ## Obtain File
  fxnlInFile <- reactive({getFile(input, "fxnlFile")})
  
  ## Obtain Data
  fxnlInData <- reactive({getData(input, "fxnlFile", fxnlInFile()$datapath, readRaw)})
  
  ### Perform standard calculations
  fxnlCalcData <- reactive({
    if (is.null(fxnlInData())) {
      print("fxnlInData is null in calc reactive")
      print("Class of fxnlInData")
      print(class(fxnlInData()))
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
  
  ##########################
  ### HORIZONTAL BAR TAB ###~~~~~~~~~~~~~~~~~~~~~
  ##########################
  
  ## Update grouping variable options
  observe({
    if (is.null(fxnlCalcData())){
      return(NULL)
    } else {
      data <- fxnlCalcData()
      updateSelectInput(session, "hBarGrp", choices = unique(data[, Group]))
      updateSelectInput(session, "hBarSample", choices = c("all", setdiff(names(data), c("Group", "Calc"))), selected = "all")
    }
  }) # update pieGrp and pieSample
  
  ## Plotting logic - set to TRUE if "plot" button is pressed, FALSE if 'clear' is pressed. Default is FALSE
  hBarPlot_logical <- reactiveVal(value = FALSE)
  observeEvent(input$doPlotHBar, {hBarPlot_logical(TRUE)})
  observeEvent(input$stopPlotHBar, {hBarPlot_logical(FALSE)})
  
  ## Update output filename based on select arguments
  observe({
    if (is.null(fxnlCalcData())) {
      return(NULL)
    } else {
      
      baseName_v <- "fxnlHBar"
      
      ## Add group, if specified
      if (!is.null(input$hBarGrp)){ 
        if (input$hBarGrp[1] != "all") {
          temp <- paste(input$hBarGrp, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        }
      } # fi fxnlPop
      
      ## Add gate/calc, if specified
      if (!is.null(input$hBarSample)){
        if (input$hBarSample[1] != "all"){
          temp <- paste(input$hBarSample, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } 
      } # fi fxnlGate
      
      ## Remove spaces
      baseName_v <- gsub(" ", "_", baseName_v)
      ## Add extension
      baseName_v <- paste0(baseName_v, ".pdf")
      ## Update
      cat(sprintf("fxnlHBarOutName should be set as %s\n", baseName_v))
      updateTextInput(session, "fxnlHBarOutName", value = baseName_v)
    }
  }) # update filename
  
  ## Get plot
  outputFxnlHBar <- reactive({
    if (piePlot_logical()) {
      hBar <- horizBar(data_dt = fxnlCalcData(),
                            group_v = input$hBarGrp,
                            sample_v = input$hBarSample,
                            color_dt = functionalColors_dt)
      return(hBar)
    } else {
      return(NULL)
    }
  })
  
  ## Plot
  output$fxnlHBar <- renderPlot({
    if (hBarPlot_logical()) {
      grid.arrange(outputFxnlHBar())
    }
  })
  
  ## Reactive value
  hBarMessage_v <- reactiveVal(); hBarMessage_v("blank")
  
  ## Write pie
  observeEvent(input$saveFxnlHBar, {
    ## Make name
    out_v <- file.path(input$fxnlHBarOutDir, input$fxnlHBarOutName)
    ## Make message
    hBarMessage_v(paste0("Saved current view to: ", out_v))
    ## Plot
    pdf(file = out_v)
    grid.arrange(outputFxnlHBar())
    dev.off()
  })
  
  ## Update user
  output$fxnlHBarUpdate <- renderText({ if (hBarMessage_v() == "blank") { return(NULL) } else { hBarMessage_v() } })

  output$tester <- renderPlot(plot(1:10))

  ##################################
  ### MYELOID/LYMPHOID PANEL TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##################################
  
  #####################
  ### VIEW DATA TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~
  #####################

  ## Obtain File
  mlInFile <- reactive({getFile(input, "mlFile")})
  
  ## Obtain Data
  # mlInData <- reactive({getData(input, "mlFile", mlInFile()$datapath, readRaw, 
  #                               args_ls = list(sheetName_v = "Cohort", popCol_v = "Panel", 
  #                                              gateCol_v = "Gate", infoCol_v = "Info"))})
  mlInData <- reactive({
    if (is.null(mlInFile())){
      return(NULL)
    } else {
      foo <- readRaw(input_xlsx = mlInFile()$datapath, sheetName_v = "Cohort", popCol_v = "Panel",
              gateCol_v = "Gate", infoCol_v = "Info")
      print("Read in ML data")
      return(foo)
    }
  })
  
  ### Perform standard calculations
  mlCalcData <- reactive({
    if (is.null(mlInData())) {
      return(NULL)
    } else {
      print("mhead mlInData()[['sum']]")
      print(mlInData()[["sum"]][1:5,1:5])
      foo <- mlCalculations(sum_dt = mlInData()[["sum"]])
      print("mhead mlCalculations(mlInData())$norm")
      boo <- foo[["norm"]]
      print(boo[1:5,1:5])
      return(foo)
    }
  })
  
  ## Select which data to view
  mlWhichData <- reactiveVal()
  mlWhichData("raw")
  observeEvent(input$mlDataSelect, {mlWhichData(input$mlDataSelect)})
  
  ## Update available Population and Sample based on which input is selected
  observe({
    if (is.null(mlInData())) {
      return(NULL)
    } else {
      if (mlWhichData() %in% c("raw", "sum", "norm")) {
        ## Get raw or calculated data
        if (mlWhichData() == "norm") {
          currData <- mlCalcData()[[ mlWhichData() ]]
        } else {
          currData <- mlInData()[[ mlWhichData() ]]
        } # fi
        ## Update selections
        updateSelectInput(session, "mlPanel", choices = c("all", unique(currData[,get("Panel")])), selected = "all")
        updateSelectInput(session, "mlSamples", choices = c("all", setdiff(names(currData), c("Panel", "Gate", "Info"))), selected = "all")
      } else if (mlWhichData() == "ratio") {
        currData <- mlCalcData()[[ mlWhichData() ]]
        updateSelectInput(session, "mlPanel", choices = c("all", unique(currData[,get("Cell")])), selected = "all")
        updateSelectInput(session, "mlSamples", choices = c("all", setdiff(names(currData), c("Cell", "Subtype"))), selected = "all")
      } # fi
    } # fi
  })
  
  ## Update Gates/Subtypes available based on which input is selected
  observe({
    if (is.null(mlInData())) {
      return(NULL)
    } else {
      ## Set columns
      if (mlWhichData() %in% c("raw", "sum", "norm")){ currPanelCell <- "Panel"; currGateSubtype <- "Gate" } else { currPanelCel <- "Cell"; currGateSubtype <- "Subtype" } # fi
      ## Set data
      if (mlWhichData() %in% c("raw", "sum")) { currData <- mlInData()[[ mlWhichData() ]] } else { currData <- mlCalcData()[[ mlWhichData() ]]}
      ## Dynamically update columns based on previous selections
      if (is.null(input$mlPanel)) {
        return(NULL)
      } else if (input$mlPanel == "all") {
        currMLGate_v <- unique(currData[, get(currGateSubtype)])
      } else {
        currMLGate_v <- unique(currData[get(currPanelCell) %in% input$mlPanel, get(currGateSubtype)])
      } # fi
      ## Update selection
      updateSelectInput(session, "mlGate", choices = c("all", currMLGate_v), selected = "all")
    }
  })
  
  ## Update output filename based on the selected arguments
  observe({
    if (is.null(mlInData())) {
      return(NULL)
    } else {
      ## Just "ml" and either "raw/sum/calc/ratio"
      baseName_v <- paste("ml", whichData(), sep = "_-_")
      
      ## Add panel/cell
      if (!is.null(input$mlPanel)) {
        if (input$mlPanel[1] != "all") {
          temp <- paste(input$mlPanel, collapse = '-')
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } # fi
      } # fi mlPanel
      
      ## Add gate/subtype
      if (!is.null(input$mlGate)) {
        if (input$mlGate[1] != "all"){
          temp <- paste(input$mlGate, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } # fi
      } # fi mlGate
      
      ## Add samples
      if (!is.null(input$mlSamples)) {
        if (input$mlSamples[1] != "all"){
          temp <- paste(input$mlSamples, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } # fi
      } # fi mlSamples
      
      ## Remove spaces
      baseName_v <- gsub(" ", "_", baseName_v)
      
      ## Add extension
      baseName_v <- paste0(baseName_v, ".xlsx")
      
      ## Update
      cat(sprintf("mlOutName should be set as %s\n", baseName_v))
      updateTextInput(session, "mlOutName", value = baseName_v)
    }
  })
  
  ## Get current view
  mlOutputData <- reactive({
    if (is.null(mlInData())) {
      return(NULL)
    } else {
      ## Set data and columns based on which View
      ## Set columns
      if (mlWhichData() %in% c("raw", "sum", "norm")){ currPanelCell <- "Panel"; currGateSubtype <- "Gate" } else { currPanelCel <- "Cell"; currGateSubtype <- "Subtype" } # fi
      ## Set data
      if (mlWhichData() %in% c("raw", "sum")) { currData <- mlInData()[[ mlWhichData() ]] } else { currData <- mlCalcData()[[ mlWhichData() ]]}
      
      ## Subset Panel/Cell based on selection
      if (is.null(input$mlPanel)) {
        currData <- currData
      } else if (input$mlPanel[1] != "all") {
        currData <- currData[get(currPanelCell) %in% input$mlPanel,]
      } # fi
      
      ## Subset Gate/Subtype based on selection
      if (is.null(input$mlGate)) {
        currData <- currData
      } else if (input$mlGate[1] != "all") {
        currData <- currData[get(currGateSubtype) %in% input$mlGate,]
      } # fi
      
      ## Subset sample
      if (is.null(input$mlSamples)) {
        currData <- currData
      } else if (input$mlSamples[1] != "all") {
        currData <- currData[,mget(c(currPanelCell, currGateSubtype, input$mlSamples))]
      } # fi
      currData
    } # fi
  })
  
  ## Output the table
  output$ml <- DT::renderDataTable({
    if (is.null(mlInData())) {
      print("mlInData is null in renderDataTable")
      return()
    } else {
      #print(mlOutputData()[1:5,1:5])
      DT::datatable(mlOutputData())
    }
  })
  
  ## Reactive value
  mlMessage_v <- reactiveVal(); mlMessage_v("blank")
  
  ## Write table
  observeEvent(input$saveML, {
    ## Make name
    out_v <- file.path(input$mlOutDir, input$mlOutName)
    ## Make message
    mlMessage_v(paste0("Saved current view to: ", out_v))
    ## Write table
    write_xlsx(x = mlOutputData(), path = out_v)
  })
  
  ## Update user
  output$mlUpdate <- renderText({ if (mlMessage_v() == "blank") { return(NULL)} else { mlMessage_v() } })
  
  #######################
  ### STACKED BAR TAB ###~~~~~~~~~~~~~~~~~~~~~~~~
  #######################
  
  ## Update samples
  observe({
    if (is.null(mlCalcData())) {
      return(NULL)
    } else {
      data <- mlCalcData()[["ratio"]]
      updateSelectInput(session, "mlSbarSample", choices = c("all", setdiff(names(data), c("Subtype", "Cell"))), selected = "all")
    }
  })
  
  ## Plotting logic - set to TRUE if "plot" button is pressed, FALSE if 'clear' is pressed. Default is FALSE
  sBarPlot_logical <- reactiveVal(value = FALSE)
  observeEvent(input$doPlotSbar, {sBarPlot_logical(TRUE)})
  observeEvent(input$stopPlotSbar, {sBarPlot_logical(FALSE)})
  
  ## Update filename
  observe({
    if (is.null(mlCalcData())){
      return(NULL)
    } else {
      baseName_v <- "mlSbar"
      
      ## Add samples
      if (!is.null(input$mlSbarSample)){
        if (input$mlSbarOutDir[1] != "all"){
          temp <- paste(input$mlSbarSample, collapse = "-")
          baseName_v <- paste0(baseName_v, "_-_", temp)
        } # fi
      } # fi mlSbarSample
      
      ## Remove spaces
      baseName_v <- gsub(" ", "_", baseName_v)
      ## Add extension
      baseName_v <- paste0(baseName_v, ".xlsx")
      ## Update
      cat(sprintf("mlSbarOutName should be set as %s\n", baseName_v))
      updateTextInput(session, "mlSbarOutName")
    } # fi
  })
  
  ## Get plot
  outputMLSBar <- reactive({
    if (sBarPlot_logical()) {
      sBar <- stackedBar(mlCalcData()[["ratio"]],
                         color_dt = subTypeColors_dt,
                         sample_v = input$mlSbarSample)
      return(sBar)
    } else {
      return(NULL)
    } # fi
  })

  ## Plot
  output$mlSbar <- renderPlot({
    if (sBarPlot_logical()) {
      grid.arrange(outputMLSBar())
    }
  })
  
  ## Reactive value
  sBarMessage_v <- reactiveVal(); sBarMessage_v("blank")
  
  ## Write sbar
  observeEvent(input$saveMLSbar, {
    ## Make Name
    out_v <- file.path(input$mlSbarOutDir, input$mlSbarOutName)
    ## Make message
    sBarMessage_v(paste0("Saved current view to: ", out_v))
    ## Plot
    pdf(file = out_v)
    grid.arrange(outputMLSBar())
    dev.off()
  })
  
  ## Update user
  output$mlSbarUpdate <- renderText({ if (sBarMessage_v() == "blank") {return(NULL)} else { sBarMessage_v() } })
  
}) # shinyServer

shinyApp(ui = ui, server = server)
