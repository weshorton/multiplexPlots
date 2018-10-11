### Functions for server.R

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

getData <- function(input, file, dataPath, fxn, args_ls = NA) {
  #' Get data
  #' @description Shiny reactive to grab data from an input file, using the specified read function
  #' @param input The input argument to the shinyServer() fxn, which is a list of all input variables
  #' @param file character name of input file from ui.R
  #' @param dataPath should be output of getFile() with '$datapath' added. Character vector that is a path to an input file
  #' @param fxn some sort of function to read in 'file'. Should be readRaw usually.
  #' @param args_ls named list of arguments to pass to fxn
  #' @value Some sort of table
  #' @export
  
  if (is.null(input[[file]])) {
    cat(sprintf("%s is NULL in getData\n", file))
    return(NULL)
  } else {
    ## Make args list
    if (is.na(args_ls)){
      args_ls <- list(dataPath)
    } else {
      args_ls <- c("input_xlsx" = dataPath, args_ls)
    } # fi
    data <- do.call(fxn, args = args_ls)
    return(data)
  }
}

calcData <- function(dataFxn, fxn, args_ls = NA) {
  #' Get data
  #' @description Shiny reactive to grab data from an input file, using the specified read function
  #' @param dataFxn should be output of getData()
  #' @param fxn some sort of function to read in 'file'
  #' @param args_ls named list of arguments to pass to fxn
  #' @value Some sort of table
  #' @details Pretty limited in scope right now. Both cases that I use this, the result of getData()
  #' is a list of data.tables and vectors that have one element called 'sum' that is a data.table. 
  #' That is always the input to my two calculation functions. Instead of feeding dataFxn as the arg,
  #' could just make it dataFxn()[["sum"]], that way I could change it. Need to see how that would
  #' effect the first is.null() if statement.
  #' @export
  
  if (is.null(dataFxn())) {
    return(NULL)
  } else {
    ## Make args list
    if (is.na(args_ls)){
      args_ls <- list(dataFxn()[["sum"]])
    } else {
      args_ls <- c("sum_dt" = dataFxn()[["sum"]], args_ls)
    } # fi
    data <- do.call(fxn, args = args_ls)
    return(data)
  }
}

combineData <- function(dataFxn1, dataFxn2, name = NA) {
  #' Combine data
  #' @description Combine two data functions
  #' @param dataFxn1 likely output of getData(). reactive that is a list of data.tables and vectors
  #' @param dataFxn2 likely output of calcData(). reactive that is either a data.table or list of data.tables
  #' @param name character vector to name the list element. Not needed if dataFxn2 is a list
  #' @value reactive that is a combined list of the two inputs
  #' @export
  
  ## Only run if both inputs are not null
  if (!is.null(dataFxn1()) & !is.null(dataFxn2())) {
    ## Create list
    data_ls <- dataFxn1()
    
    ## Get class of the other
    class_v <- class(dataFxn2())
    if ("data.table" %in% class_v){
      data_ls[[name]] <- dataFxn2()
    } else {
      for (i in 1:length(dataFxn2())){
        currName <- names(dataFxn2())[i]
        data_ls[[currName]] <- dataFxn2()[[i]]
      } # for
    } # fi
    return(data_ls)
  } else {
    return(NULL)
  } # fi
} # combineData

addName <- function(input, inputId, base, mainDelim = "_-_", minorDelim = "-") {
  if (!is.null(input[[inputId]])) {
    if (input[[inputId]][1] != "all") {
      temp <- paste(input[[inputId]], collapse = minorDelim)
      base <- paste0(base, mainDelim, temp)
    } # fi
  } # fi
  return(base)
} # subOutput

subOutput <- function(input, inputId, data, col) {
  if (is.null(input[[inputId]])) {
    data <- data
  } else if (input[[inputId]][1] != "all") {
    data <- data[get(col) %in% input[[inputId]],]
  } # fi
  return(data)
} # subOutput

g_legend <- function(a.gplot){
  #' Extract ggplot legend
  #' @description Extract legend as separate table from ggplot object
  #' @param a.gplot a ggplot object with a legend
  #' @value a gtable object of the legend
  #' @export
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} # g_legend

myTableGrob <- function(data_dt, title_v, fontsize_v = 14){
  #' Create custom table grob with title
  #' @description Creates table grob in format that is most common for my usage.
  #' @param data_dt Data.table that the grob will be made out of
  #' @param title_v Title for display
  #' @param fontsize_v Fontsize for title. Default is 14 (goes will with my_theme)
  #' @value gtable object
  #' @export
  
  ## Table
  table_grob <- tableGrob(data_dt, rows = rep('', nrow(data_dt)), theme = ttheme_minimal())
  ## Title
  title_grob <- textGrob(title_v, gp = gpar(fontsize = fontsize_v))
  ## Add title
  table_grob <- gtable_add_rows(table_grob, heights = grobHeight(title_grob) + unit(5,'mm'), pos = 0)
  table_grob <- gtable_add_grob(table_grob, title_grob, 1, 1, 1, ncol(table_grob))
}
