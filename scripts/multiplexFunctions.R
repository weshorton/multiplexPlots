###
### MULTIPLEX FUNCTIONS
###

################
### READ RAW ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################

readRaw <- function(input_xlsx, sheetName_v = "raw data", popCol_v = "Population", gateCol_v = "Gate", infoCol_v = NA) {
  #' Read in raw data from excel file
  #' @description Read in standard excel file containing ROI values for multiple slides
  #' @param input_xlsx character vector - path to input file
  #' @param sheetName_v character vector - name of sheet containing data (default is 'raw data')
  #' @param popCol_v character vector - name of first column that defines the population of cells (e.g. 'CD45+ global population'). Default is 'Population'
  #' @param gateCol_v character vector - name of the second column that contains subsets of the groups based on flow gating (e.g. 'CD45+' or 'CD3+ ICOS+'). Default is 'Gate'
  #' @param infoCol_v only used for ML panel
  #' @value data.table containing same information as excel sheet
  #' @export
  
  ## Dependencies
  require(data.table)
  require(readxl)
  
  ##
  ## GET RAW DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  
  ## Read
  input_dt <- as.data.table(read_excel(path = input_xlsx, sheet = sheetName_v))
  
  ## Handle info col
  if (is.na(infoCol_v)){
    otherCols_v <- c(popCol_v, gateCol_v)
  } else {
    otherCols_v <- c(popCol_v, gateCol_v, infoCol_v)
  } # fi
  
  ## Fix column classes
  dataCols_v <- colnames(input_dt)[!(colnames(input_dt) %in% otherCols_v)]
  for (col_v in otherCols_v) set(input_dt, j = col_v, value = as.character(input_dt[[col_v]]))
  for (col_v in dataCols_v) set(input_dt, j = col_v, value = as.numeric(as.character(input_dt[[col_v]])))
  
  ## Remove spaces from gate names
  input_dt[[gateCol_v]] <- gsub("\\s", "", input_dt[[gateCol_v]])
  
  ##
  ## GET SUMMARIZED DATA ~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  
  ## Make beginning of data.table
  summary_dt <- input_dt[,mget(otherCols_v)]
  
  ## Get slides (and check)
  ## So far, have seen:
    ## S1_ROI1; S1_ROI2; S2_ROI1; etc.
    ## X120.ROI.1; X121.ROI.1; X121.ROI.2; X.125.ROI.3; X.130.ROI.3.
    ## 120 ROI 1; 128 ROI6; 138 ROI 3
    ## syn80_1; syn80_2; syn81_1
  slides_v <- unique(gsub("_ROI[0-9]*$|\\.ROI.*$|[ ]*|ROI[ ]*[0-9]*|syn|_[0-9]*", "", dataCols_v))
  possible_v <- c(paste0("S", 1:1000), paste0("X", 1:1000), 1:1000)
  if (length(which(!(slides_v %in% possible_v)))) {
    stop(sprintf("Not all slide names are 'S[0-9]'. Please check the column names in %s sheet of %s excel file.\n",
                 sheetName_v, input_xlsx))
  } else {
    cat(sprintf("Summarizing Regions of Interest for the following slides: %s.\n", paste(slides_v, collapse = " ")))
  } # fi
  
  ## Summarize each
  sums_mat <- sapply(slides_v, function(x) {
    grep_v <- paste(paste0(x, "\\."), paste0(x, "_"), paste0(x, " "), sep = "|") # Have to have this b/c grepping for S12 would return all of S120,S121, etc.
    cols_v <- grep(grep_v, dataCols_v, value = T)
    if (length(cols_v) == 0) cols_v <- grep(x, dataCols_v, value = T)
    sums_v <- rowSums(input_dt[,mget(cols_v)])
    return(sums_v)
  })
  
  ## Add to summary_dt
  summary_dt <- cbind(summary_dt, sums_mat)
  
  ##
  ## GET OTHER INFO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  
  ## Get all unique groups (also remove blanks)
  uniqGrp_v <- grep("^$", unique(input_dt[[popCol_v]]), value = T, invert = T)
  
  ## Get all unique individual
  uniqInd_v <- unique(input_dt[[gateCol_v]])
  
  ## Check individuals for bad names and remove them (only do this for fnctional panel)
  if (popCol_v %in% c("population", "Population")){
    badStart_v <- grep("^CD|Total", uniqInd_v, invert = T)
    if (length(badStart_v) > 0) {
      whichBad_v <- uniqInd_v[badStart_v]
      uniqInd_v <- uniqInd_v[-badStart_v]
      input_dt <- input_dt[-badStart_v,]
      summary_dt <- summary_dt[-badStart_v,]
      cat(sprintf("Removed %d rows with 'bad' names.\n\tIndexes: %s\n\t%s names: %s\n",
                  length(badStart_v), paste(badStart_v, collapse = "; "), gateCol_v, paste(whichBad_v, collapse = "; ")))
    } # fi
    
    ## Check individuals for positive words
    hasPos_v <- grep("pos", uniqInd_v)
    
    ## Remove them
    if (length(hasPos_v) > 0) {
      whichPos_v <- uniqInd_v[hasPos_v]
      uniqInd_v[hasPos_v] <- gsub("pos", "", uniqInd_v[hasPos_v])
      input_dt[hasPos_v, eval(gateCol_v) := uniqInd_v[hasPos_v]]
      summary_dt[hasPos_v, eval(gateCol_v) := uniqInd_v[hasPos_v]]
      cat(sprintf("Removed %d rows with 'pos' in the name. Please check and make sure there is still a '+'.\n\tIndexes: %s\n\t%s names: %s\n",
                  length(hasPos_v), as.character(paste(hasPos_v, collapse = "; ")),
                  gateCol_v, as.character(paste(whichPos_v, collapse = "; "))))
    }
    
    ## Check individuals for negative words
    hasNeg_v <- grep("neg", uniqInd_v)
    
    if (length(hasNeg_v) > 0) {
      whichNeg_v <- uniqInd_v[hasNeg_v]
      uniqInd_v[hasNeg_v] <- gsub("neg", "", uniqInd_v[hasNeg_v])
      input_dt[hasNeg_v, eval(gateCol_v) := uniqInd_v[hasNeg_v]]
      summary_dt[hasNeg_v, eval(gateCol_v) := uniqInd_v[hasNeg_v]]
      cat(sprintf("Removed %d rows with 'neg' in the name. Please check and make sure there is still a '-'.\n\tIndexes: %s\n\t%s names: %s\n",
                  length(hasNeg_v), as.character(paste(hasNeg_v, collapse = "; ")),
                  gateCol_v, as.character(paste(whichNeg_v, collapse = "; "))))
    } # fi
  } # fi
  
  ##
  ## OUTPUT
  ##
  
  out_ls <- list("raw" = input_dt,
                 "sum" = summary_dt,
                 "slides" = slides_v,
                 "populations" = uniqGrp_v,
                 "gates" = uniqInd_v)
  
  return(out_ls)
} # readRaw

####################
### CALCULATIONS ###
####################

standardCalcs <- function(sum_dt, calcs_v = c("pctCD45", "majorImmune", "PctCD8.CD45", "CD8Functional", "PctKi67", "PctGRZB+", "CD4", "PctIl10_CD3negCD68pos"),
                          slides_v = NULL, popCol_v = "Population", gateCol_v = "Gate") {
  #' Perform different calculations on summed Region of Interest values
  #' @description Using the summed region of interest values for different populations, calculate common percentages for later plotting
  #' @param sum_dt data.table. Rows = gated populations, columns = slides. 1st two columns are "Population" and "Gate", folowing columns are slides. 
  #' This object is the 'sum' element of the list output by readRaw().
  #' @param calcs_v character vector. Name of different calculations to perform.
  #' (1) 'pctCD45" - percentage of all cells that are CD45+
  #' (2) 'majorImmune' - 4 calculations: percentage of CD45+ cells that are (A) CD8+, (B) CD4+, and (C) CD68+; also remaining percentage
  #' (3) 'CD8Functional' - percentage of different CD8 functional groups out of total CD8
  #' (4) 'PctKi67' - percentage of each CD8 functional group that is Ki67+
  #' (5) 'PctGRZB+' - percentage of each CD8 functional group that is Granzyme B+
  #' (6) 'CD4' - percentage of CD4 T cells that are: (A) Ki67+, (B) GRZB+, (C) PD-1+; also percentage of CD4 PD-1+ that are Ki67+
  #' @param slides_v character vector of slides to run calculations on. Must be valid column names of sum_dt. If not specified, will use all slides.
  #' @param popCol_v character vector - name of first column that defines the population of cells (e.g. 'CD45+ global population'). Default is 'Population'
  #' @param gateCol_v character vector - name of the second column that contains subsets of the groups based on flow gating (e.g. 'CD45+' or 'CD3+ ICOS+'). Default is 'Gate'
  #' @value data.table of calculations
  #' @export
  
  ##
  ## SETUP AND CHECKS ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  ## Check calculation names
  possible_v <- c("pctCD45", "majorImmune", "PctCD8.CD45", "CD8Functional", "PctKi67", "PctGRZB+", "CD4", "PctIl10_CD3negCD68pos")
  mismatch_v <- setdiff(calcs_v, possible_v)
  if (length(mismatch_v) > 0) stop(sprintf("At least one element of 'calcs_v' is not supported. Please check your spelling. 
                                           Unsupported element(s): %s\n", paste(mismatch_v, collapse = ' ')))
  
  ## Different row identifications
  cd45Rows_v <- c("pct.CD45.All.Cells" = "CD45+")
  majorImmuneRows_v <- c("pct.CD8.CD45" = "CD45+CD3+CD8+", "pct.CD4.CD45" = "CD45+CD3+CD8-", "pct.CD68+.CD45" = "CD45+CD3-CD68+")
  cd8FxnlRows_v <- c("PD1+ EOMES-" = "CD45+CD3+CD8+PD1+EOMES-", "PD1+ EOMES+" = "CD45+CD3+CD8+PD1+EOMES+",
                     "PD1- EOMES+" = "CD45+CD3+CD8+PD1-EOMES+", "PD1- EOMES-" = "CD45+CD3+CD8+PD1-EOMES-")
  cd8Ki67Rows_v <- c("PD1+ EOMES-" = "CD45+CD3+CD8+PD1+EOMES-Ki67+", "PD1+ EOMES+" = "CD45+CD3+CD8+PD1+EOMES+KI67+",
                     "PD1- EOMES+" = "CD45+CD3+CD8+PD1-EOMES+Ki67+", "PD1- EOMES-" = "CD45+CD3+CD8+PD1-EOMES-Ki67+")
  cd8GRZBRows_v <- c("PD1+ EOMES-" = "CD45+CD3+CD8+PD1+EOMES-GRZB+", "PD1+ EOMES+" = "CD45+CD3+CD8+PD1+EOMES+GRZB+",
                     "PD1- EOMES+" = "CD45+CD3+CD8+PD1-EOMES+GRZB+", "PD1- EOMES-" = "CD45+CD3+CD8+PD1-EOMES-GRZB+")
  cd4Rows_v <- c("Ki67" = "CD45+CD3+CD8-Ki67+", "GRZB+" = "CD45+CD3+CD8-GRZB+", 
                 "PD-1+" = "CD45+CD3+CD8-PD1+")
  cd3Negcd68PosRows_v <- c("CD45+CD3-CD68+IL-10+", "CD45+CD3-CD68+")
  
  ## Get slides - have seen S100, X100, and 100 so far
  if (is.null(slides_v)) slides_v <- grep("[SX][0-9]*$|[0-9]+$", colnames(sum_dt), value = T)
  
  ## Begin output matrix
  out_lsv <- list()
  
  ##
  ## CALCULATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  
  ## Percent CD45
  if ('pctCD45' %in% calcs_v) {
    out_lsv[["pctCD45"]] <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, slides_v = slides_v, rows_v = cd45Rows_v, divisor_v = "Totalcells")
  } # fi
  
  ## Major Immune
  if ('majorImmune' %in% calcs_v) {
    ## Calculate
    majorImmune_dt <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, slides_v = slides_v, rows_v = majorImmuneRows_v, divisor_v = "CD45+")
    ## Get 'other' proportion
    other_lsv <- c("Calc" = "pct.other.CD45", as.list(100 - colSums(majorImmune_dt[,mget(slides_v)])))
    ## Add
    majorImmune_dt <- rbind(majorImmune_dt, other_lsv)
    ## Check
    checkSum(data_dt = majorImmune_dt, slides_v = slides_v, "Major Immune Groups")
    ## Add to final
    out_lsv[['majorImmune']] <- majorImmune_dt
  } # fi
  
  ## CD8 as % CD45
  if ('PctCD8.CD45' %in% calcs_v) {
    other_lsv <- c("Calc" = "pct.other.CD45", as.list(100 - majorImmune_dt[Calc == "pct.CD8.CD45", mget(slides_v)]))
    cd8.cd45_dt <- rbind(majorImmune_dt[Calc == "pct.CD8.CD45",], other_lsv)
    out_lsv[["PctCD8.CD45"]] <- cd8.cd45_dt
  }
  
  ## CD8 Functional
  if ('CD8Functional' %in% calcs_v) {
    ## Get divisor
    cd8fxnlDivisor_v <- colSums(sum_dt[get(gateCol_v) %in% cd8FxnlRows_v, mget(slides_v)])
    ## Calc
    cd8fxnl_dt <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, slides_v = slides_v, rows_v = cd8FxnlRows_v, divisor_v = cd8fxnlDivisor_v)
    ## Check
    checkSum(data_dt = cd8fxnl_dt, slides_v = slides_v, "CD8 Functional state")
    ## Add to final
    out_lsv[["CD8Functional"]] <- cd8fxnl_dt
  } # fi
  
  ## Percent Ki67
  if ("PctKi67" %in% calcs_v) {
    out_lsv[["PctKi67"]] <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, 
                                           slides_v = slides_v, rows_v = cd8Ki67Rows_v, divisor_v = cd8FxnlRows_v)
  } # fi
  
  ## Percent GRZB+
  if ("PctGRZB+" %in% calcs_v) {
    out_lsv[["PctGRZB+"]] <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, 
                                            slides_v = slides_v, rows_v = cd8GRZBRows_v, divisor_v = cd8FxnlRows_v)
  } # fi
  
  ## CD4 T Cells
  if ('CD4' %in% calcs_v) {
    ## Get divisor
    cd4Divisor_v <- sum_dt[get(gateCol_v) == "CD45+CD3+CD8-", mget(slides_v)]
    ## Calc
    cd4_dt <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, slides_v = slides_v, rows_v = cd4Rows_v, divisor_v = cd4Divisor_v)
    ## Get final row
    other_lsv <- c("Calc" = "pct.Ki67.PD_1", as.list(sum_dt[get(gateCol_v) == "CD45+CD3+CD8-PD1+Ki67+", mget(slides_v)] / 
                                                       sum_dt[get(gateCol_v) == "CD45+CD3+CD8-PD1+", mget(slides_v)] * 100))
    ## Add
    cd4_dt <- rbind(cd4_dt, other_lsv)
    ## Add to final
    out_lsv[["CD4"]] <- cd8fxnl_dt
  } # fi
  
  ## % IL-10 out of CD3-CD68+ cells
  if ("PctIl10_CD3negCD68pos" %in% calcs_v) {
    out_lsv[["PctIl10_CD3negCD68pos"]] <- divideROI(sum_dt = sum_dt, gateCol_v = gateCol_v, slides_v = slides_v, 
                                                    rows_v = cd3Negcd68PosRows_v[1], divisor_v = cd3Negcd68PosRows_v[2])
  }
  
  ## Format into data.table
  out_dt <- rbindList(list_lsv = out_lsv, col_v = "Group")
  
  ## Output
  return(out_dt)
  
} # standardCalcs

##################
### RBIND LIST ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################

rbindList <- function(list_lsv, col_v = "Calc", add.names = T) {
  #' Combine list of vectors by rows into data.table
  #' @description special version of rbind that also adds a column for the names of the input list
  #' @param list_lsv - list of vectors. Each element must have the same length as all others.
  #' @param col_v - name of new column to add.
  #' @param add.names - logical. TRUE - add new column with names of list_lsv. FALSE - add V1, V2, ...
  #' @value data.table where nrow(output) = length(rows_v) and ncol(output) = length(list_lsv[[1]]) + 1
  #' @export
  
  ## Turn list to data table
  list_dt <- do.call(rbind, list_lsv)
  
  ## Create name column
  if (add.names) {
    nameVal_v <- unlist(sapply(names(list_lsv), function(x) rep(x, nrow(list_lsv[[x]])), USE.NAMES = F))
  } else {
    nameVal_v <- paste0("V", 1:sum(sapply(list_lsv, nrow)))
  } # fi
  
  ## Add column
  list_dt[[col_v]] <- nameVal_v
  
  ## Re-order
  list_dt <- list_dt[,c(ncol(list_dt),1:(ncol(list_dt)-1)),with = F]
  
  ## Output
  return(list_dt)
} # rbindList

########################
### DIVIDE BY DOUBLE ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########################

divideROI <- function(sum_dt, gateCol_v = "Gate", slides_v = NULL, rows_v, divisor_v, as.pct = T, add.names = T) {
  #' Divide gate ROI values by single reference value
  #' @description Use to get various relative counts of gated ROIs, such as % CD45 of major immune groups, 
  #' relative abundance of CD8 functional state, etc.
  #' @param data_dt data.table - Rows = gated populations, columns = slides. 1st two columns are "Population" and "Gate", folowing columns are slides. 
  #' This object is the 'sum' element of the list output by readRaw().
  #' @param gateCol_v character vector - name of the second column that contains subsets of the groups based on flow gating (e.g. 'CD45+' or 'CD3+ ICOS+'). Default is 'Gate'
  #' @param slides_v character vector of slides to run calculations on. Must be valid column names of sum_dt. If not specified, will use all slides.
  #' @param rows_v character vector (optionally named) - values of different cells in gateCol_v that will be divided by divisor_v.
  #' @param divisor_v can be either character vector or numeric vector. 
  #' if character - values of different cells in gateCol_v to use as divisor. value for rows_v[1] will be divided by value for divisor_v[1]. 
  #' Example: divide cd8 functional rows by ki67+ cd8 functional rows to get % Ki67. length(divisor_v) must equal length(rows_v) or 1.
  #' if  numeric - numeric vector of a value to divide values of rows_v by. e.g. # of total CD45 cells, which can be used to get % CD45 of major immune group rows.
  #' length(divisor_v) must equal length(slides_v).
  #' @param as.pct logical. TRUE - multiply resulting fractions by 100 to create percents. FALSE - leave as fractions.
  #' @param add.names logical. TRUE - add column called 'Calc' containing category names for each calculation. Requires that rows_v is named. FALSE - do not add names.
  #' @value data.table with nrow(output) == length(rows_v) and ncol(output) = length(slides_v) + 'calc' name column.
  #' @export
  
  ## Get slides
  if (is.null(slides_v)) slides_v <- grep("[SX][0-9]*$", colnames(sum_dt), value = T)
  
  ## Check rows
  mismatch_v <- setdiff(rows_v, sum_dt[[gateCol_v]])
  if (length(mismatch_v) > 0) stop(sprintf("At least one element of 'rows_v' is not a valid value for %s column. Bad element(s): %s",
                                           gateCol_v, paste(mismatch_v, collapse = " ")))
  
  ## Different processes for character and numeric
  if (is.character(divisor_v)) {
    ## Check that divisor is valid
    mismatch_v <- setdiff(divisor_v, sum_dt[[gateCol_v]])
    if (length(mismatch_v) > 0) stop(sprintf("At least one element of 'divisor_v' is not a valid value for %s column. Bad element(s): %s",
                                             gateCol_v, paste(mismatch_v, collapse = " ")))
    
    ## Check length and add, if needed
    if (length(divisor_v) == 1 & length(divisor_v) != length(rows_v)) divisor_v <- rep(divisor_v, length(rows_v))
    
    ## Divide each rows_v element by divisor_v (if character) or by divisor_v itself (if numeric)
    divVals_lsv <- mapply(function(x,y) {(sum_dt[get(gateCol_v) == x, mget(slides_v)] / sum_dt[get(gateCol_v) == y, mget(slides_v)])},
                          x = rows_v, y = divisor_v, SIMPLIFY = F)
  } else {
    divVals_lsv <- sapply(rows_v, function(x) {(sum_dt[get(gateCol_v) == x, mget(slides_v)] / divisor_v)}, simplify = F)
  } # fi
  
  ## Change to pct
  if (as.pct) divVals_lsv <- lapply(divVals_lsv, function(x) x * 100)
  
  ## Turn into data.table
  divVals_dt <- rbindList(divVals_lsv)
  
  ## Return
  return(divVals_dt)
} # divideROI

#################
### CHECK SUM ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

checkSum <- function(data_dt, slides_v, name_v = NA) {
  #' Check Calculation
  #' @description For calculations that are a percent of total, check that they sum to 100.
  #' @param data_dt data.table where rows are divisions and columns are samples
  #' @param slides_v column names to select from data_dt when checking sums
  #' @param name_v name to output for warning statement.
  sums_v <- apply(data_dt[,mget(slides_v)], 2, sum)
  badSums_v <- which(!as.character(sums_v) %in% c("100", NA))
  if (length(badSums_v) > 0) warning(sprintf("%s doesn't add up to 100.", name_v))
}

######################
### ML CALCULATION ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################

mlCalculations <- function(sum_dt, slides_v = NULL, panelCol_v = "Panel", gateCol_v = "Gate", infoCol_v = "Info") {
  #' Perform different calcualtions on Myeloid/Lymphoid panel summed ROIs
  #' @description Use summed region of interest values for differing populations to calculate commond percentages for plots.
  #' @param sum_dt data.table. Rows = panel gates, columns = slides. Should be output of readRaw for a myeloid/lymphoid panel
  #' @param calcs_v different calculations to run.
  #' (1) ''
  #' (2) ''
  #' @param slides_v character vector of slides to run calculations on. Must be value column names of sum_dt. If not specified, will use all slides.
  #' @param panelCol_v character vector - name of 1st column that determines lymphoid or myeloid panel
  #' @param gateCol_v character vector - name of 2nd column that defines the gate used
  #' @param infoCol_v character vector - name of 3rd column that has extra info about some gates
  #' @value data.table
  #' @export
  
  ## Get slides
  if (is.null(slides_v)) slides_v <- grep("[SX][0-9]*$|[0-9]+$|^syn|_[0-9]*", colnames(sum_dt), value = T)
  
  ## Output matrix
  out_lsv <- list()
  
  ##
  ## NORMALIZATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  
  for (i in 1:length(slides_v)){
    ## Get slide
    currSlide_v <- slides_v[i]
    
    ## Get numerator and denominator
    currNum_v <- sum_dt[get(infoCol_v) == "M_on_L", get(currSlide_v)]
    currDen_v <- sum_dt[get(infoCol_v) == "L_on_M", get(currSlide_v)]
    
    ## Get myeloid and lymphoid
    currMyeloid_v <- sum_dt[get(panelCol_v) %in% c("MYELOID", "Myeloid", "myeloid"), get(currSlide_v)]
    currLymphoid_v <- sum_dt[get(panelCol_v) %in% c("LYMPHOID", "Lymphoid", "lymphoid"), get(currSlide_v)]
    
    ## Normalize myeloid
    currMyeloid_v <- currMyeloid_v * (currNum_v / currDen_v)
    
    ## Add back
    sum_dt[[currSlide_v]] <- c(currMyeloid_v, currLymphoid_v)
  } # for i
  
  ##
  ## RATIOS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##
  
  ## Make data.table to hold ratio information
  ratio_dt <- data.table("Cell" = c(rep("T-Cell", 6), rep("Lymphoid", 2), "Dendritic Cell", rep("Macrophage", 4), rep("Dendritic Cell", 2), rep("Granulocyte", 2)),
                         "Subtype" = c("Th0", "Treg", "Th17", "Th2", "Th1", "CD8 T Cells", "NK Cells", "B Cells", "myeloid other",
                                       "CD163- myelomono", "CD163+ myelomono", "CD163- TAM", "CD163+ TAM", "immature DCs", "mature DCs", "neutrophil", "mast cell"))
  
  cd68pos_v <- c("CD163- myelomono", "CD163+ myelomono", "CD163- TAM", "CD163+ TAM")
  
  ## Subset sum_dt - We want Th0, Treg, etc. But those values are listed in infoCol_v twice. 
  ## Once for main one, and another for PDL1+ version
  ## We want the main one
  goodCols_v <- grep("PDL1|PD1", sum_dt[[gateCol_v]], value = T, invert = T)
  ratio_dt <- merge(ratio_dt, sum_dt[get(gateCol_v) %in% goodCols_v, mget(c(infoCol_v, slides_v))], 
                    by.x = "Subtype", by.y = infoCol_v, sort = F, all.x = T)
  
  new_mat <- matrix(nrow = 6, ncol = length(slides_v))
  
  for (i in 1:length(slides_v)){
    
    ## Get slide
    currSlide_v <- slides_v[i]
    
    ## Percent CD45
    ratio_dt[[currSlide_v]] <- ratio_dt[[currSlide_v]] / sum_dt[get(panelCol_v) %in% c("LYMPHOID", "Lymphoid", "lymphoid") &
                                                                   get(gateCol_v) == "CD45+", get(currSlide_v)]
    
    ## Other ratios
    new_mat[1, i] <- ratio_dt[Subtype == "Th1", get(currSlide_v)] / ratio_dt[Subtype == "Th2", get(currSlide_v)]
    new_mat[2, i] <- ratio_dt[Subtype == "NK Cells", get(currSlide_v)] / ratio_dt[Subtype == "Treg", get(currSlide_v)]
    new_mat[3, i] <- ratio_dt[Subtype == "Th1", get(currSlide_v)] / ratio_dt[Subtype == "Treg", get(currSlide_v)]
    new_mat[4, i] <- ratio_dt[Subtype == "CD8 T Cells", get(currSlide_v)] / sum(ratio_dt[Subtype %in% cd68pos_v, get(currSlide_v)])
    new_mat[5, i] <- ratio_dt[Subtype == "CD163- TAM", get(currSlide_v)] / ratio_dt[Subtype == "CD163+ TAM", get(currSlide_v)]
    new_mat[6, i] <- ratio_dt[Subtype == "mature DCs", get(currSlide_v)] / ratio_dt[Subtype == "immature DCs", get(currSlide_v)]
    
  } # for i

  ## Add Subtype and Cell columns
  ratioNames_v <- c("Th1:Th2", "NK:Treg", "Th1:Treg", "CD8:CD68+", "CD163-:CD163+ TAMs", "Mature:immature DCs")
  cellNames_v <- rep(NA, 6)
  new_mat <- cbind(ratioNames_v, cellNames_v, new_mat)
  colnames(new_mat) <- c("Subtype", "Cell", slides_v)
  
  ## Add to ratio data
  ratio_dt <- rbind(ratio_dt, new_mat)
  
  ## Fix classes
  for (col_v in slides_v) set(ratio_dt, j = col_v, value = as.numeric(ratio_dt[[col_v]]))
  
  ## Output
  out_lsdt <- list("norm" = sum_dt, "ratio" = ratio_dt)
  return(out_lsdt)
}

