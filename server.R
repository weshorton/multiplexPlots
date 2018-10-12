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
library(gtable)
library(grid)
library(ggpubr)
library(writexl)
library(dplyr)

### Code
files_v <- list.files("./scripts/", full.names = T)
invisible(lapply(files_v, function(x) source(x)))
source("./ui.R")
source("./fxns.R")
source("./temp.R")

### Options
options(shiny.maxRequestSize=1000*1024^2)

### Reference
cellTypeColors_dt <- fread("./data/cellTypeColors.txt")
functionalColors_dt <- fread("./data/functionalColors.txt")
subTypeColors_dt <- fread("./data/subTypeColors.txt")
tCellColors_dt <- fread("./data/tCellColors.txt")

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
  fxnlCalcData <- reactive({calcData(fxnlInData, standardCalcs)})
  
  ### Combine data
  fxnlAllData <- reactive({combineData(dataFxn1 = fxnlInData, dataFxn2 = fxnlCalcData, name = "calc")})
  
  ## Select which data to view and get associated info
  whichData <- reactiveVal(); whichData("raw")
  dataInfo <- reactiveValues()
  
  #observeEvent(input$dataSelect, {
  observe({
    ## Assign data
    whichData(input$dataSelect)
    dataInfo$data <- fxnlAllData()[[ whichData() ]]
    ## Get related info
    if (input$dataSelect %in% c("raw", "sum")) {
      dataInfo$c1 <- "Population"; dataInfo$c2 <- "Gate"
    } else {
      dataInfo$c1 <- "Group"; dataInfo$c2 <- "Calc"
    }})
  
  ## Test again
  #observe({if (!is.null(fxnlAllData())) {
  observe({if (!is.null(dataInfo$data)) {
    print("dataInfo$data: "); print(dataInfo$data[1:5,1:5])
    print("class(dataInfo$data): "); print(class(dataInfo$data))
    print("dataInfo$c1: "); print(dataInfo$c1)
    updateSelectInput(session, "fxnlPop", choices = c("all", unique(dataInfo$data[,get(dataInfo$c1)])), selected = "all")
    updateSelectInput(session, "fxnlSamples", choices = c("all", setdiff(names(dataInfo$data), c(dataInfo$c1, dataInfo$c2))), selected = "all")
  }})
  
  ## Update gates
  #observe({if (!is.null(fxnlAllData())) {
  observe({if (!is.null(dataInfo$data)) {
    if (!is.null(input$fxnlPop)) {
      if (input$fxnlPop == "all") {
        currGate_v <- unique(dataInfo$data[,get(dataInfo$c2)])
      } else {
        currGate_v <- unique(dataInfo$data[get(dataInfo$c1) %in% input$fxnlPop, get(dataInfo$c2)])
      } # fi
      updateSelectInput(session, "fxnlGate", choices = c("all", currGate_v), selected = "all")
    } # fi
  }})
    
  
  ## Update output filename based on the selected arguments
  observe({if (!is.null(fxnlInData())) {
      ## Just "fxnl" and either "raw/sum/calc"
      baseName_v <- paste("fxnl", whichData(), sep = "_-_")
      
      ## Add other sections
      for (id in c("fxnlPop", "fxnlGate", "fxnlSamples")) {
        baseName_v <- addName(input, id, baseName_v)
      }

      ## Remove spaces and add extension
      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".xlsx")
      
      ## Update
      updateTextInput(session, "fxnlOutName", value = baseName_v)
    }}) # update filename
  
  ## Get current view
  outputData <- reactive({if (!is.null(fxnlInData())){
    currData <- dataInfo$data
    currData <- subOutput(input, "fxnlPop", currData, dataInfo$c1)
    currData <- subOutput(input, "fxnlGate", currData, dataInfo$c2)
    if (!is.null(input$fxnlSamples)) {
      if (input$fxnlSamples[1] != "all") {
        currData <- currData[,mget(c(dataInfo$c1, dataInfo$c2, input$fxnlSamples))]
      } #fi
    } # fi
    return(currData)
    }})
  
  
  ## Output the table
  output$fxnl <- DT::renderDataTable({
    if (!is.null(fxnlInData())) {
      DT::datatable(outputData())
    }})

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
      pieGrp_v <- unique(data[, Group])
      pieGrp_v <- setdiff(pieGrp_v, c("pctCD45", "PctKi67", "PctGRZB+", "CD4", "PctIl10_CD3negCD68pos"))
      updateSelectInput(session, "pieGrp", choices = pieGrp_v)
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
      
      for (id in c("pieGrp", "pieSample")){
        baseName_v <- addName(input, id, baseName_v)
      }

      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".pdf")

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
    ## Determine width and height
    height_v <- width_v <- 7
    if (length(outputFxnlPie()) > 6) {
      height_v <- height_v * (ceiling(sqrt(length(outputFxnlPie()))) / 2)
      width_v <- height_v
    } 
    ## Plot
    pdf(file = out_v, width = width_v, height = height_v)
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
  }) # update hBarGrp and hBarSample
  
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
      
      for (id in c("hBarGrp", "hBarSample")){
        baseName_v <- addName(input, id, baseName_v)
      }
      
      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".pdf")
      
      updateTextInput(session, "fxnlHBarOutName", value = baseName_v)
    }
  }) # update filename
  
  ## Get plot
  outputFxnlHBar <- reactive({
    if (hBarPlot_logical()) {
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
  
  ## Write horizontal bar
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
  
  ####################
  ### SUNBURST TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####################
  
  ## Update grouping variable options
  observe({
    if (is.null(fxnlCalcData())){
      return(NULL)
    } else {
      data <- fxnlCalcData()
      #updateSelectInput(session, "fSunGrp", choices = c("all", "PctKi67", "PctGRZB+"), selected = "all")
      updateSelectInput(session, "fSunSample", choices = c("all", setdiff(names(data), c("Group", "Calc"))), selected = "all")
    }
  }) # update fSunSample
  
  ## Plotting logic - set to TRUE if "plot" button is pressed, FALSE if 'clear' is pressed. Default is FALSE
  fSunPlot_logical <- reactiveVal(value = FALSE)
  observeEvent(input$doPlotFSun, {fSunPlot_logical(TRUE)})
  observeEvent(input$stopPlotFSun, {fSunPlot_logical(FALSE)})
  
  ## Update output filename based on select arguments
  observe({
    if (is.null(fxnlCalcData())) {
      return(NULL)
    } else {
      
      baseName_v <- "fxnlSun"
      
      for (id in c("fSunGrp", "fSunSample")){
        baseName_v <- addName(input, id, baseName_v)
      }
      
      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".pdf")
      
      updateTextInput(session, "fxnlSunOutName", value = baseName_v)
    }
  }) # update filename
  
  ## Get plot
  outputFxnlSun <- reactive({
    if (fSunPlot_logical()) {
      cat("Functional Calc Data:\n")
      print(fxnlCalcData()[1:5,1:5])
      cat("Sample_v:\n")
      print(input$fSunSample)
      cat("Groups_v:\n")
      print(input$fSunGrp)
      fSun <- fxnlSunburstChart(data_dt = fxnlCalcData(),
                                color_dt = functionalColors_dt,
                                sample_v = input$fSunSample,
                                groups_v = input$fSunGrp)
      return(fSun)
    } else {
      return(NULL)
    }
  })
  
  ## Plot
  output$fxnlSun <- renderPlot({
    if (fSunPlot_logical()) {
      sunburstPlot(sunburst_lslsgg = outputFxnlSun(), type_v = "fxnl", pct_v = input$fSunPct)
    }
  })
  
  ## Reactive value
  fSunMessage_v <- reactiveVal(); fSunMessage_v("blank")
  
  ## Write sunburst
  observeEvent(input$saveFxnlSun, {
    ## Make name
    out_v <- file.path(input$fxnlSunOutDir, input$fxnlSunOutName)
    ## Make message
    fSunMessage_v(paste0("Saved current view to: ", out_v))
    ## Determine width and height
    height_v <- width_v <- 7
    if (length(outputFxnlSun()$plot) > 6) {
      # height_v <- height_v * (ceiling(sqrt(length(outputFxnlSun()$plot))) / 2)
      # width_v <- height_v
      height_v <- width_v <- 20
    }
    cat(sprintf("Width: %s\n", width_v))
    cat(sprintf("Height: %s\n", height_v))
    ## Plot
    pdf(file = out_v, width = width_v, height = height_v)
    sunburstPlot(sunburst_lslsgg = outputFxnlSun(), type_v = "fxnl", pct_v = input$fSunPct)
    dev.off()
  })
  
  ## Update user
  output$fxnlSunUpdate <- renderText({ if (fSunMessage_v() == "blank") { return(NULL) } else { fSunMessage_v() } })

  ##################################
  ### MYELOID/LYMPHOID PANEL TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##################################
  
  #####################
  ### VIEW DATA TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~
  #####################

  ## Obtain File
  mlInFile <- reactive({getFile(input, "mlFile")})
  
  ## Obtain Data
  mlInData <- reactive({getData(input, "mlFile", mlInFile()$datapath, readRaw,
                                args_ls = list(sheetName_v = "Cohort", popCol_v = "Panel",
                                               gateCol_v = "Gate", infoCol_v = "Info"))})

  ### Perform standard calculations
  mlCalcData <- reactive({calcData(mlInData, mlCalculations)})
  
  ### Combine data (appears to work)
  mlAllData <- reactive({combineData(dataFxn1 = mlInData, dataFxn2 = mlCalcData, name = c("norm", "ratio"))})
  
  ### To delete - just checking that mlAllData is working
  observe({
    if (!is.null(mlAllData())){
      print("mlAllData names:")
      print(names(mlAllData()))
      print("head of mlAllData()$ratio")
      print(mlAllData()[["ratio"]])
    }
  })
  
  ## Select which data to view
  mlWhichData <- reactiveVal(); mlWhichData("raw")
  mlData <- reactiveValues()
  
  observe({
    ## Assign data
    mlWhichData(input$mlDataSelect)
    mlData$data <- mlAllData()[[ mlWhichData() ]]
    ## Get other info
    if (input$mlDataSelect %in% c("raw", "sum", "norm")) {
      mlData$c1 <- "Panel"; mlData$c2 <- "Gate"; mlData$c3 <- "Info"
    } else {
      mlData$c1 <- "Cell"; mlData$c2 <- "Subtype"; mlData$c3 <- NULL
    }})
  
  ## Update available Population and Sample based on which input is selected
  observe({if (!is.null(mlAllData())) {
    updateSelectInput(session, "mlPanel", choices = c("all", unique(mlData$data[,get(mlData$c1)])), selected = "all")
    updateSelectInput(session, "mlSamples", choices = c("all", setdiff(names(mlData$data), c(mlData$c1, mlData$c2, mlData$c3))), selected = "all")
  }})
  
  ## Update gates
  observe({if (!is.null(mlAllData())) {
    if (!is.null(input$mlPanel)) {
      if (input$mlPanel == "all") {
        currMLGate_v <- unique(mlData$data[, get(mlData$c2)])
      } else {
        currMLGate_v <- unique(mlData$data[get(mlData$c1) %in% input$mlPanel, get(mlData$c2)])
      } # fi
      updateSelectInput(session, "mlGate", choices = c("all", currMLGate_v), selected = "all")
    } # fi
  }})
  
  ## Update output filename based on the selected arguments
  observe({
    if (is.null(mlInData())) {
      return(NULL)
    } else {
      ## Just "ml" and either "raw/sum/calc/ratio"
      baseName_v <- paste("ml", mlWhichData(), sep = "_-_")
      
      for (id in c("mlPanel", "mlGate", "mlSamples")) {
        baseName_v <- addName(input, id, baseName_v)
      }
      
      ## Remove spaces and extension
      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".xlsx")
      
      updateTextInput(session, "mlOutName", value = baseName_v)
    }})
  
  mlOutputData <- reactive({if (!is.null(mlInData())){
    currData <- mlData$data
    currData <- subOutput(input, "mlPanel", currData, mlData$c1)
    currData <- subOutput(input, "mlGate", currData, mlData$c2)
    if (!is.null(input$mlSamples)) {
      if (input$mlSamples[1] != "all") {
        currData <- currData[,mget(c(mlData$c1, mlData$c2, mlData$c3, input$mlSamples))]
      } # fi
    } # fi
    return(currData)
  }})
  
  ## Output the table
  output$ml <- DT::renderDataTable({
    if (!is.null(mlInData())) {
      DT::datatable(mlOutputData())
    }})
  
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
      
      baseName_v <- addName(input, "mlSbarSample", baseName_v)
      
      ## Remove spaces and add extension
      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".xlsx")
      
      updateTextInput(session, "mlSbarOutName", value = baseName_v)
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
  
  ####################
  ### SUNBURST TAB ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####################
  
  ## Update grouping variable options
  observe({
    if (is.null(mlCalcData())){
      return(NULL)
    } else {
      data <- mlCalcData()[["ratio"]]
      updateSelectInput(session, "mlSunSample", choices = c("all", setdiff(names(data), c("Subtype", "Cell"))), selected = "all")
    }
  }) # update fSunSample
  
  ## Plotting logic - set to TRUE if "plot" button is pressed, FALSE if 'clear' is pressed. Default is FALSE
  mlSunPlot_logical <- reactiveVal(value = FALSE)
  observeEvent(input$doPlotMLSun, {mlSunPlot_logical(TRUE)})
  observeEvent(input$stopPlotMLSun, {mlSunPlot_logical(FALSE)})
  
  ## Update output filename based on select arguments
  observe({
    if (is.null(mlCalcData())) {
      return(NULL)
    } else {
      
      baseName_v <- "mlSun"
      
      for (id in "mlSunSample"){
        baseName_v <- addName(input, id, baseName_v)
      }
      
      baseName_v <- paste0(gsub(" ", "_", baseName_v), ".pdf")
      
      updateTextInput(session, "mlSunOutName", value = baseName_v)
    }
  }) # update filename
  
  ## Get plot
  outputMLSun <- reactive({
    if (mlSunPlot_logical()) {
      mlSun <- mlSunburstChart(data_dt = mlCalcData()[["ratio"]],
                               color_dt = merge(cellTypeColors_dt, subTypeColors_dt, by = "Cell", sort = F, suffixes = c("_Cell", "_Subtype")),
                               sample_v = input$mlSunSample)
      return(mlSun)
    } else {
      return(NULL)
    }
  })
  
  ## Plot
  output$mlSun <- renderPlot({
    if (mlSunPlot_logical()) {
      sunburstPlot(sunburst_lslsgg = outputMLSun(), type_v = "ml", pct_v = input$mlSunPct)
    }
  })
  
  ## Reactive value
  mlSunMessage_v <- reactiveVal(); mlSunMessage_v("blank")
  
  ## Write horizontal bar
  observeEvent(input$saveMLSun, {
    ## Make name
    out_v <- file.path(input$mlSunOutDir, input$mlSunOutName)
    ## Make message
    mlSunMessage_v(paste0("Saved current view to: ", out_v))
    ## Plot
    pdf(file = out_v)
    sunburstPlot(sunburst_lslsgg = outputMLSun(), type_v = "ml", pct_v = input$mlSunPct)
    dev.off()
  })
  
  ## Update user
  output$mlSunUpdate <- renderText({ if (mlSunMessage_v() == "blank") { return(NULL) } else { mlSunMessage_v() } })
  
}) # shinyServer

shinyApp(ui = ui, server = server)
