###
### PLOTTING FUNCTIONS
###

library(data.table)
library(ggplot2)

##############
### THEMES ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############

pie_theme <- theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 16))

bar_theme <- theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12, 
                                   margin = unit(c(0.5, -1.2, 0.5, 0.5), "cm"), color = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

stacked_theme <- theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.line.x = element_blank(),
        legend.title = element_blank())

sunburst_theme <- theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank())

#################
### PIE CHART ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

plotPie <- function(data_dt, groupCol_v = "Group", calcCol_v = "Calc", group_v, sample_v, color_dt, pct_v = NA) {
  #' Multiplex Pie Chart
  #' @description Standard pie chart for different immune cell distributions. Will create plots for
  #' (1) a single sample and a single group
  #' (2) multiple samples and a single group
  #' (3) a single sample and multiple groups
  #' @param data_dt data.table with rows = immune cell group and columns are samples. First two columns must be (1)overall immune group, and (2) subset of that group.
  #' @param groupCol_v column name of the immune group column of data_dt. Default is "Group".
  #' @param calcCol_v column name of the immune group subset column of data_dt. Default is "Calc".
  #' @param group_v specific immune group or groups to plot in pie chart. Must be a value listed in groupCol_v of data_dt. If this is multiple, sample_v must be singular.
  #' @param sample_v character vector - sample or samples to plot. Must be a valid column name of data_dt. If this is multiple, group_v must be singular. Can also be "all" to plot all samples.
  #' @param color_dt data.table with rows = immune cell groups and columns of various metadata as well as color specifications. 
  #' Required columns: 'Group' - immune cell group; 'Calc' - group subset; 'Legend' - name of subset for legend; 'SubLegend' - possible extra legend name
  #' 'Hex' - hex code color.
  #' @param pct_v determine printing of pie percentages. one of three possible values: 
  #' (1) character vector of values of calcCol_v to leave blank
  #' (2) numeric vector of indices of calcCol_v to leave blank
  #' (3) NA, will print all percentages larger than 5%.
  #' @value returns a "gg" and "ggplot" object that can be printed to console or saved to a file.
  #' @export
  
  ## Subset data
  sub_dt <- data_dt[get(groupCol_v) %in% group_v,]
  
  ## Merge with colors
  merge_dt <- merge(sub_dt, color_dt, by = c(groupCol_v, calcCol_v), sort = F)
  
  ## Make new legend
  newLegend_v <- sapply(1:nrow(merge_dt), function(x) {
    y <- ifelse(is.na(merge_dt$SubLegend[x]), merge_dt$Legend[x],
                paste0(merge_dt$Legend[x], "\n(", merge_dt$SubLegend[x], ")"))
  })
  merge_dt$Legend <- newLegend_v
  
  ## Handle samples
  if (sample_v == "all"){
    sample_v <- setdiff(colnames(data_dt), c(groupCol_v, calcCol_v))
  }
  
  ## Make title
  title_v <- paste0("Patient: ", sample_v)
  
  ## Make percentage labels
  labels_mat <- sapply(sample_v, function(x) {
    pctLab_v <- paste0(round(merge_dt[[x]]), "%")
    if (is.na(pct_v) | pct_v == ""){
      tooSmall_v <- which(merge_dt[[x]] < 5)
    } else if (is.character(pct_v)) {
      tooSmall_v <- which(merge_dt[[calcCol_v]] %in% pct_v)
    } else if (is.numeric(pct_v)){
      tooSmall_v <- pct_v
    } else {
      stop("Incorrect specification of pct_v")
    }
    pctLab_v[tooSmall_v] <- ""
    return(pctLab_v)
  })
  
  ## Add to data
  colnames(labels_mat) <- paste0(colnames(labels_mat), "_pct")
  merge_dt <- cbind(merge_dt, labels_mat)

  
  ## Plot - different for if multiple samples or groups
  plots_lsgg <- list()
  if (length(group_v) == 1 & length(sample_v) == 1) {
    
    print("One sample and one group")
    name_v <- paste0(sample_v, "_", group_v)
    plots_lsgg[[name_v]] <- pieFxn(merge_dt = merge_dt, groupCol_v = groupCol_v, group_v = group_v, calcCol_v = calcCol_v, sample_v = sample_v, 
                                   pct_v = paste0(sample_v, "_pct"), fill = calcCol_v, title_v = title_v, subtitle_v = "", size = 12)
    
  } else if (length(group_v) > 1 & length(sample_v) > 1) {
    
    cat(sprintf("%s sample(s) and %s group(s)\n", length(sample_v), length(group_v)))
    plots_lslsgg <- lapply(sample_v, function(x) {
      name_v <- paste0(x, "_", group_v)
      title_v <- paste0("Patient: ", x)
      tmp <- lapply(group_v, function(y) pieFxn(merge_dt, groupCol_v, y, calcCol_v, x, paste0(x, "_pct"), calcCol_v, title_v, y, size = 4))
      names(tmp) <- name_v
      return(tmp)
    })
    names(plots_lslsgg) <- sample_v
    for (i in 1:length(plots_lslsgg)) plots_lsgg <- c(plots_lsgg, plots_lslsgg[[i]])
    
  } else if (length(group_v) > 1) {
    
    cat(sprintf("%s sample(s) and %s group(s)\n", length(sample_v), length(group_v)))
    plots_lsgg <- lapply(group_v, function(x) pieFxn(merge_dt, groupCol_v, x, calcCol_v, sample_v, paste0(sample_v, "_pct"), calcCol_v, title_v, x, size = 4))
    names(plots_lsgg) <- paste0(sample_v, "_", group_v)
    
  } else if (length(sample_v) > 1) {
    
    cat(sprintf("%s sample(s) and %s group(s)\n", length(sample_v), length(group_v)))
    plots_lsgg <- lapply(sample_v, function(x) pieFxn(merge_dt, groupCol_v, group_v, calcCol_v, x, paste0(x, "_pct"), calcCol_v, 
                                                      title_v = paste0("Patient: ", x), group_v, size = 4))
    names(plots_lsgg) <- paste0(sample_v, "_", group_v)
      
  } else {
      stop(sprintf("Something went terribly wrong!\n",
                   length(group_v), length(sample_v)))
  } # fi
  
  ## Return
  return(plots_lsgg)
}

pieFxn <- function(merge_dt, groupCol_v, group_v, calcCol_v, sample_v, pct_v, fill, title_v, subtitle_v = "", size = 4) {
  ## Merge_dt must only have one sample and group
  if (length(group_v) != 1 | length(sample_v) != 1) stop("Both group_v and sample_v must have length of 1.")
  ## Subset
  getCols_v <- c(groupCol_v, calcCol_v, sample_v, paste0(sample_v, "_pct"), "Hex", "Legend")
  data_dt <- merge_dt[get(groupCol_v) == group_v[1], mget(getCols_v)]
  ## Fix columns - can't have variables that start with a number
  grepNum_v <- grep("^[0-9]", sample_v)
  if (length(grepNum_v) > 0) {
    whichCols_v <- which(colnames(data_dt) %in% c(sample_v, pct_v))
    sample_v <- paste0("S", sample_v)
    pct_v <- paste0("S", pct_v)
    colnames(data_dt)[whichCols_v] <- paste0("S", colnames(data_dt)[whichCols_v])
  }
  ## Plot
  ggplot(data = data_dt, aes_string(x = groupCol_v, y = sample_v, fill = calcCol_v)) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes_string(label = pct_v), position = position_stack(vjust = 0.5), size = size) +
    coord_polar(theta = "y", start = 0) +
    scale_fill_manual(limits = data_dt[[calcCol_v]], values = data_dt$Hex, labels = data_dt$Legend) +
    labs(fill = NULL,
         title = title_v,
         subtitle = subtitle_v) + 
    pie_theme
}


############################
### HORIZONTAL BAR CHART ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################

horizBar <- function(data_dt, groupCol_v = "Group", calcCol_v = "Calc", group_v, sample_v, color_dt, colors_v = c("red", 'yellow')) {
  #' Multiplex Horizontal Bar Chart
  #' @description Horizontal Bar chart displaying the % subsets for different groups
  #' @param data_dt data.table with rows = immune cell group and columns are samples. First two columns must be (1)overall immune group, and (2) subset of that group.
  #' @param groupCol_v column name of the immune group column of data_dt. Default is "Group".
  #' @param calcCol_v column name of the immune group subset column of data_dt. Default is "Calc".
  #' @param group_v specific immune group or groups to plot in pie chart. Must be a value listed in groupCol_v of data_dt. If this is multiple, sample_v must be singular.
  #' @param sample_v character vector - sample or samples to plot. Must be a valid column name of data_dt. If this is multiple, group_v must be singular.
  #' @param color_dt data.table with rows = immune cell groups and columns of various metadata as well as color specifications. 
  #' Required columns: 'Group' - immune cell group; 'Calc' - group subset; 'Legend' - name of subset for legend; 'SubLegend' - possible extra legend name
  #' 'Hex' - hex code color.
  #' @param colors_v vector of colors for the bars. If only one bar per category, will use grey, otherwise will use the colors specified.
  #' @value returns a "gg" and "ggplot" object that can be printed to console or saved to a file.
  #' @export
   
  ## Dependencies
  library(data.table)
  library(grid)
  library(ggplot2)
  
  ## Subset data
  sub_dt <- data_dt[get(groupCol_v) %in% group_v,]
  
  merge_dt <- merge(sub_dt, color_dt, by = c(groupCol_v, calcCol_v), sort = F)
  
  ## Make new legend
  newLegend_v <- sapply(1:nrow(merge_dt), function(x) {
    y <- ifelse(is.na(merge_dt$SubLegend[x]), merge_dt$Legend[x],
                paste0(merge_dt$Legend[x], "\n(", merge_dt$SubLegend[x], ")"))
  })
  merge_dt$Legend <- newLegend_v
  merge_dt[[calcCol_v]] <- newLegend_v
  
  ## Make title
  title_v <- paste0(sample_v, "_", group_v)
  
  ## Melt data
  melt_dt <- melt(merge_dt[,mget(c("Group", "Calc", "Legend", "Hex", sample_v))], measure.vars = sample_v)
  
  ## Color
  if (length(unique(melt_dt$variable)) == 1) colors_v <- "grey35"
  
  ## Plot
  plotOut <- grid.grabExpr(hbFxn(melt_dt, calcCol_v, title_v, colors_v = colors_v))
  
}

hbFxn <- function(melt_dt, calcCol_v, title_v, colors_v = c("yellow", "red")) {
  
  ## Make base plot
  basePlot <- ggplot(data = melt_dt, aes_string(x = calcCol_v, y = "value", fill = "variable", linetype = "variable")) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_continuous(position = "right", breaks = seq(0, 100, 20), expand = c(0,0), limits = c(-6,101)) +
    coord_flip() + bar_theme +
    geom_col(aes_string(y = -6, fill = calcCol_v), width = 1) +
    scale_fill_manual(limits = melt_dt[[calcCol_v]], values = melt_dt$Hex) +
    geom_abline(intercept = 0, slope = 0) +
    guides(fill = F, linetype = F) +
    labs(y = title_v, x = "CD45+ CD3+ CD8+")
  
  ## Grab data
  basePlot_data <- ggplot_build(basePlot)
  
  ## Switch color
  uniq_v <- unique(basePlot_data$data[[1]]$linetype)
  for (i in 1:length(uniq_v)) basePlot_data$data[[1]][basePlot_data$data[[1]]$linetype == uniq_v[i], "fill"] <- colors_v[i]
  
  ## Print
  plotGrob <- ggplot_gtable(basePlot_data)
  grid.newpage(); grid.draw(plotGrob)
}


#########################
### STACKED BAR CHART ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################

stackedBar <- function(data_dt, panelCol_v = "Cell", gateCol_v = "Subtype", color_dt, sample_v,
                       xlab_v = "CD45+ Cells", ylab_v = "% of CD45+ cells", title_v = "Immune Cell Composition") {
  #' Multiplex Stacked Bar Chart
  #' @description Stacked Bar chart displaying the distribution of subtypes from the lymphoid panel
  #' @param data_dt data.table with rows = immune cell gate and columns are samples. Columns must be:
  #' (1) Panel (2) Gate (3) Info (4) NORMALIZED.VALUES (5) SUM.ROIs (6..n) samples
  #' @param panelCol_v character vector - name of 1st column that determines lymphoid or myeloid panel
  #' @param gateCol_v character vector - name of 2nd column that defines the gate used
  #' @param sample_v character vector - sample or samples to plot. Must be a valid column name of data_dt.
  #' @param color_dt data.table with rows = immune cell groups and columns of various metadata as well as color specifications. 
  #' @param xlab_v character vector - label for x axis. Default is 'CD45+ Cells"
  #' @param ylab_v character vector - label for y axis. Default is "% of CD45+ Cells"
  #' @param title_v character vector - title for plot. Default is "Immune Cell Composition"
  #' Required columns: 
  #' 'Subtype' - immune cell subtype (e.g. Th0, B cell, NK, etc.)
  #' 'Gate' - gating used. Must be equal to the gateCol_v values in data_dt
  #' 'Hex' - hex code color.
  #' @value returns a "gg" and "ggplot" object that can be printed to console or saved to a file.
  #' @export
  
  ## Subset data for samples
  data_dt <- data_dt[,mget(c(panelCol_v, gateCol_v, sample_v))]
  
  ## Merge data and colors
  # merge_dt <- merge(data_dt, color_dt, by = "Subtype", sort = F, all.x = F, all.y = T)
  merge_dt <- merge(data_dt, color_dt, by = gateCol_v, sort = F, all.x = F, all.y = T)
  
  
  ## Melt
  id_v <- setdiff(colnames(merge_dt), sample_v)
  melt_dt <- melt(merge_dt, id.vars = id_v)
  
  ## Factor levels are opposite
  levels_v <- merge_dt[order(merge_dt$Factor, decreasing = T), Subtype]
  melt_dt$Subtype <- factor(melt_dt$Subtype, levels = levels_v)
  
  ## Make plot
  stackedBar_gg <- ggplot(data = melt_dt, aes_string(x = "variable", y = "value", fill = gateCol_v)) +
    geom_bar(position = "fill", stat = "identity", width = 0.5) +
    scale_fill_manual(limits = as.character(melt_dt[[gateCol_v]]), values = melt_dt$Hex) +
    labs(y = "% of CD45+ cells", x = "CD45+ Cells") + ggtitle("Immune Cell Composition") +
    stacked_theme
    
  ## Return
  return(stackedBar_gg)
} # stackedBar


######################
### SUNBURST CHART ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################

mlSunburstChart <- function(data_dt, cellCol_v = "Cell", subCol_v = "Subtype", color_dt, sample_v, type_v = "immuneComp",
                          title_v = NULL) {
  #' Multiplex Sunburst Chart
  #' @description Sunburst chart displaying the hierarchical distribution of different immune types
  #' @param data_dt data.table with rows = immune cell gate and columns are samples. Columns must be:
  #' (1) Panel (2) Gate (3) Info (4) NORMALIZED.VALUES (5) SUM.ROIs (6..n) samples  ### THIS IS STILL FROM STACKED BAR - NEED TO CHANGE
  #' @param cellCol_v character vector - name of column that determines Cell type for 'immuneComp' version and Gate for 'cd4' version.
  #' Usual values are "Cell" for 'immuneComp' version and "Gate" for 'cd4' version.
  #' @param subCol_v character vector - name of column that determines subtype for 'immuneComp' version and Info for 'cd4' version.
  #' Usual values are "Subtype" for 'immuneComp' and "Info" for 'cd4' version.
  #' @param color_dt data.table with rows = immune cell groups and columns of various metadata as well as color specifications.
  #' Required columns: 
  #' 'Subtype' - immune cell subtype (e.g. Th0, B cell, NK, etc.)
  #' 'Gate' - gating used. Must be equal to the subCol_v values in data_dt
  #' 'Hex' - hex code color.
  #' @param sample_v character vector - sample or samples to plot. Must be a valid column name of data_dt.
  #' Should be numbers (e.g. 81, 82, 83) or numbers prepended by S (e.g. S81, S82, S83)
  #' @param type_v character vector - either 'immuneComp' for immune composition or 'cd4' for CD4 groups.
  #' @param title_v character vector - title for plot. Default is "Immune Cell Composition"
  #' @value returns a "gg" and "ggplot" object that can be printed to console or saved to a file.
  #' @export
  
  ### Handle Sample
  if (sample_v == "all") {
    sample_v <- setdiff(colnames(data_dt), c("Panel", cellCol_v, subCol_v))
  } # fi
  
  ## Add an "S" before the sample names, because numeric column names can cause trouble
  newSample_v <- paste0("S", gsub("S", "", sample_v))
  
  ## Adjust in data.table
  whichChange_v <- which(colnames(data_dt) %in% sample_v)
  colnames(data_dt)[whichChange_v] <- newSample_v
  
  ## Handle type
  if (type_v == "immuneComp") {
    
    ## Handle title_v
    title_v <- ifelse(is.null(title_v), "Immune Cell Composition", title_v)
    
    ## Merge with color table
    hexCols_v <- grep("^Hex", colnames(color_dt), value = T)
    panelHex_v <- grep(cellCol_v, hexCols_v, value = T)
    gateHex_v <- grep(subCol_v, hexCols_v, value = T)
    data_dt <- merge(data_dt, color_dt[,mget(c(cellCol_v, subCol_v, hexCols_v))], by = c(subCol_v, cellCol_v), sort = F)
    
  } else if (type_v == "cd4") {
    
    ## Handle title
    title_v <- ifelse(is.null(title_v), "CD4 T Cell Subsets", title_v)
    
    ## Merge with color table
    data_dt <- merge(data_dt, color_dt[,mget(c("SubType", "Hex"))], by.x = subCol_v, by.y = "SubType", sort = F)
    panelHex_v <- gateHex_v <- hexCols_v <- "Hex"
    
  } else {
    
    stop(sprintf("Incorrect value for 'type_v'. Can only be 'immuneComp' or 'cd4', but you have: %s", type_v))
    
  } # fi
  
  ## Lists to hold results
  plot_ls <- legend_ls <- zero_ls <- list()
  
  ## Run for each sample
  for (i in 1:length(newSample_v)) {
    
    ## Get sample and data
    currSample_v <- newSample_v[i]
    currData_dt <- data_dt[,mget(c(cellCol_v, subCol_v, currSample_v, hexCols_v))]
    
    ## Only remove rows for immune composition version
    if (type_v == "immuneComp") {
      zeroRows_v <- which(currData_dt[[currSample_v]] == 0)                                # indices of rows
      zero_dt <- currData_dt[zeroRows_v,]                                                  # subset of data with these rows only
      zeroOut_v <- paste(zero_dt[[cellCol_v]], zero_dt[[subCol_v]], sep = " - ")         # output vector of format 'panel - gate'
    } else { zeroRows_v <- character() }
    
    if (length(zeroRows_v) > 0) {
      cat(sprintf("The following rows have zero counts in %s:\n%s", currSample_v,              # notify user of zero rows
                paste0("\t", paste(zeroOut_v, collapse = "\n\t"))))
      
      #currData_dt <- currData_dt[-zeroRows_v,]                                           # remove rows from data
      
      ### Prep output
      zeroOut_dt <- zero_dt[,mget(c(cellCol_v, subCol_v, currSample_v))]               # only specific columns for output
      zeroOut_dt[[currSample_v]] <- as.character(zeroOut_dt[[currSample_v]])             # change sample col to character, and
      zeroOut_dt[,eval(currSample_v) := currSample_v]                                    # change it to say the sample name
      zero_ls[[currSample_v]] <- zeroOut_dt                                              # add to list for table grob output
    } else {
      zero_ls[[currSample_v]] <- NULL
    } # fi
    
    ## Preparation Calculations for 'cd4' version
    if (type_v == "cd4") {
      ## Split
      pd1Rows_v <- grep("PD1|PDL1", currData_dt[[cellCol_v]])
      temp_dt <- currData_dt[-pd1Rows_v,]
      pd1_dt <- currData_dt[pd1Rows_v,]
      if ((pd1_dt[,.N]+temp_dt[,.N]) != currData_dt[,.N]) stop("Incorrect division of PD1 and non-PD1 rows")
      currData_dt <- merge(temp_dt, pd1_dt[,mget(c(subCol_v, currSample_v))], by = subCol_v, sort = F, suffixes = c("", "_PD1"))
      pd1Col_v <- paste0(currSample_v, "_PD1")
      currData_dt[, "PctPD-1+" := (get(pd1Col_v) / get(currSample_v))*100]
      secondGetCols_v <- c(cellCol_v, subCol_v, panelHex_v, "Sum", "PctPD-1+")
    } else {
      pd1Col_v <- NULL
      secondGetCols_v <- c(cellCol_v, panelHex_v, "Sum")
    }
    
    ## Get first level (overall sum)
    firstLevel <- currData_dt %>% summarize(total = sum(get(currSample_v)))
    
    ##
    ## Second Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##
    
    ## Sum by cell type 
    secondLevel <- currData_dt
    secondLevel <- secondLevel[,Sum := sum(get(currSample_v)), by = cellCol_v]
    
    ## Get desired columns only and also only uniques
    secondLevel <- unique(secondLevel[,mget(secondGetCols_v)])
    
    ## Get percentage of each panel section out of total
    secondLevel$Pct <- secondLevel$Sum / sum(secondLevel$Sum) * 100
    
    ## Change cell to a factor - WATCH THIS!! MAY NEED TO CHANGE IN THE FUTURE
    if (type_v == "immuneComp") {
      secondLevel[[cellCol_v]] <- factor(secondLevel[[cellCol_v]], levels = rev(secondLevel[[cellCol_v]]))
    } else {
      secondLevel[[subCol_v]] <- factor(secondLevel[[subCol_v]], levels = rev(secondLevel[[subCol_v]]))
    }
    
    ## Get position in the middle of each slices (cumSum - currval/2)
    secondLevel$runSum <- cumsum(secondLevel$Sum)
    secondLevel$pos <- secondLevel$runSum - (secondLevel$Sum / 2)
    
    ##
    ## Third Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##
    
    ## New data
    thirdLevel <- currData_dt
    
    if (type_v == "cd4") {
      ## Get remaining non-group values
      thirdLevel[[cellCol_v]] <- "PctPD1"
      thirdLevelOpposite <- thirdLevel
      thirdLevelOpposite[["PctPD-1+"]] <- 100 - thirdLevelOpposite[["PctPD-1+"]]
      thirdLevelOpposite[[pd1Col_v]] <- thirdLevelOpposite[[currSample_v]] - thirdLevelOpposite[[pd1Col_v]]
      thirdLevelOpposite[[cellCol_v]] <- paste0("not_", thirdLevelOpposite[[cellCol_v]])
      thirdLevel <- rbind(thirdLevel, thirdLevelOpposite)
      
      ## Re-order to match second level
      thirdLevel <- thirdLevel[order(match(get(subCol_v), secondLevel[[subCol_v]]))]
      
      ## Minimize values based on percentages
      thirdLevel$pctOfTot <- thirdLevel[[pd1Col_v]] / firstLevel$total * 100
      
      ## Modify values for display
      thirdLevel$display <- as.character(round(thirdLevel[["PctPD-1+"]], digits = 2))
      notRows_v <- grep("not_", thirdLevel[[cellCol_v]])
      thirdLevel[notRows_v, display := ""]
      
      ## Remove any 0's
      thirdLevel[display == "0", display := ""]
      
      ## Get position stuff
      thirdLevel$runSum <- cumsum(thirdLevel[[pd1Col_v]])
      thirdLevel$pos <- thirdLevel$runSum - (thirdLevel[[pd1Col_v]] / 2)
      
      ## Get plot factor
      thirdLevel$plotFactor <- paste(thirdLevel[[subCol_v]], thirdLevel[[cellCol_v]], sep = "_")
      thirdLevel$plotFactor <- factor(thirdLevel$plotFactor, levels = rev(thirdLevel$plotFactor))
      
      ## New colors
      newColor_dt <- data.table("Label" = thirdLevel$plotFactor, "Hex" = rep(c("#EEC62C", "#FFFFFF"), nrow(thirdLevel)/2))
      
      ## Other columns that have to fit with "immuneComp" version
      thirdLevel$Xaxis <- 3
      thirdLevel$newPos <- thirdLevel$pos
      thirdLevel$Xend <- thirdLevel$Xaxis
      
    } else {
      ## Re-order to match second level and fix order
      thirdLevel <- thirdLevel[order(match(get(cellCol_v), secondLevel[[cellCol_v]] ))]
      thirdLevel[[subCol_v]] <- factor(thirdLevel[[subCol_v]], levels = rev(thirdLevel[[subCol_v]]))  ### ALSO WATCH THIS!!
      
      ## Add columns
      thirdLevel$runSum <- cumsum(thirdLevel[[currSample_v]])                    # running sum, used for determining text position
      thirdLevel$pos <- thirdLevel$runSum - (thirdLevel[[currSample_v]] / 2)     # text position around circle - middle of each slice
      thirdLevel$Xaxis <- rep(3, times = nrow(thirdLevel))                       # text position from center to edge - in line w/ slice
      
      ### Original values are "percent" of total (but doesn't sum to 1), want percent of each group
      thirdLevel$Pct <- sapply(seq_along(thirdLevel[[subCol_v]]), function(x) {
        panel_v <- thirdLevel[x, get(cellCol_v)]                                # Find which cell class the subtype belongs to
        sum <- sum(thirdLevel[get(cellCol_v) == panel_v, get(currSample_v)])    # sum the values of all subtypes in that cell class
        pct_v <- thirdLevel[x, get(currSample_v)] / sum * 100                    # get percent of cell class by dividing value by sum
      })
      
      thirdLevel$display <- as.character(round(thirdLevel$Pct, digits = 1))
      thirdLevel[display == "0", display := ""]
      
      ### Offset slices
      thirdLevel$pctOfTot <- thirdLevel[[currSample_v]] / sum(thirdLevel[[currSample_v]]) * 100 # determine overall percentage of slice
      thirdLevel[pctOfTot < 2, Xaxis := 4]                                                      # extend position outward, if slice is too small
      
      ### Offset the extended labels
      for (j in 1:nrow(thirdLevel)) {                                                       # if 1st row, have to use last row as precedent
        k <- ifelse(j == 1, nrow(thirdLevel), j-1)                                         # all others use preceding row
        if (thirdLevel$Xaxis[j] == 3 |                                                     # If this row is 3 (within slice), then keep at 3
            thirdLevel$Xaxis[k] == 3.75) {                                                 # If preceding row is 3.75 (small extend), then keep this row at 4 (large extend)
          next                                                                                           
        } else if (thirdLevel$Xaxis[k] == 4) {                                             # If preceding row is 4 (large extend), then change
          thirdLevel$Xaxis[j] <- 3.75                                                      # this row to 3.75 (small extend)
        } # fi
      } # for j
      
      ### New columns to help determine label spread and line segments
      thirdLevel[Xaxis != 3, extend := "Yes"]                               # column to record if the label is extended (i.e. not 3)
      thirdLevel$adjusted <- rep(FALSE, nrow(thirdLevel))                   # column to record if radial positioning has been adjusted
      thirdLevel$newPos <- thirdLevel$pos                                   # new positioning for offsets
      thirdLevel$Xend <- rep(3, times = nrow(thirdLevel))                   # line segment starts and ends at 3 for labels within slice
      thirdLevel[Xaxis == 4, Xend := 3.85]                                  # extend from 3 to 3.85 for full-extend labels
      thirdLevel[Xaxis == 3.75, Xend := 3.65]                               # extend from 3 to 3.65 for small-extend labels
      lastRow_v <- nrow(thirdLevel)                                         # used as cut-off for offsets
      
      ### Determine new radial positioning for labels that are too close together
      for (j in 1:nrow(thirdLevel)) {
        
        ### Determine group
        if ( !thirdLevel$adjusted[j] &                                      # Only evaluate if the row hasn't been adjusted
             !is.na(thirdLevel$extend[j]) ) {                               # AND the label is extended
          
          isNA_v <- is.na(thirdLevel$extend)                                # vector of if a row is extended (FALSE) or not (TRUE)
          whichNA_v <- which(isNA_v)                                        # vector of positions of non-extended rows
          availNA_v <- whichNA_v[which(whichNA_v > j)]                      # vector of non-extended rows that are past current row
          firstNA_v <- ifelse(length(availNA_v) == 0,                       # returns position of end of group.
                              firstNA_v <- nrow(thirdLevel)+1,              # no avaialable NA's means the current group extends until end of table
                              firstNA_v <- min(availNA_v))                  # If available NA's, take the smallest one to make a single group
          lastNA_v <- max(which(isNA_v))                                    # position of last row that is not extended (only used if j == 1)
          currGrp_v <- j:(firstNA_v - 1)                                    # "extension group" is current row until 1 less than 1st non-extended row
          
          if (j == 1) {                                                     # for 1st row, have to check for extend group to end of circle
            if (lastNA_v == nrow(thirdLevel)) {                             # If last non-extended row is last row of data, no new rows to add
              lastGrp_v <- NULL
            } else {                                                        # If last non-extended row is not last row, then "extension
              lastGrp_v <- (lastNA_v+1):nrow(thirdLevel)                    # group" is 1 more than last non-ext row until the end
            } # fi 
            currGrp_v <- c(lastGrp_v, currGrp_v)                            # add either new extension group or blank
          } # fi 
        } else { 
          currGrp_v <- ""                                                   # if above if statement is false, just make group blank to avoid code below.
        }
        
        if (length(currGrp_v) > 1){
          cat(sprintf("Starting at row %d, current group is: %s\n", 
                      j, paste(currGrp_v, collapse = " ")))
        } # fi
        
        ### Adjust group
        if (length(currGrp_v) > 1) {                                        # only adjust if more than 1 in the group
          
          if (lastRow_v %in% currGrp_v){                                    # last row is a hard cut-off and must be adjusted to a smaller pos, rather than a larger
            lowMid_v <- which(currGrp_v == lastRow_v)                       # If group extends to front of data.table, that will be the shift-right group (even if uneven sizes)
            upMid_v <- lowMid_v + 1
          } else {
            even_v <- ifelse(length(currGrp_v) %% 2 == 0, TRUE, FALSE)      # determine if even or odd length of group
            lowMid_v <- floor( length(currGrp_v) / 2)                       # half for even, 1 less than middle if odd
            upMid_v <- ifelse(even_v, lowMid_v + 1, lowMid_v + 2)           # adjacent if even, skip middle value if odd
          } # fi
          
          adjust_v <- 0.005 * firstLevel$total                              # set/reset adjustment
          add_v <- adjust_v
          
          for (k in lowMid_v:1) {                                           # subtract adjustment, if on low side (left side)
            thirdLevel$newPos[ currGrp_v[k] ] <- thirdLevel$pos[ currGrp_v[k] ] - adjust_v
            adjust_v <- adjust_v + add_v
          } # for k
          
          adjust_v <- 0.005 * firstLevel$total                              # reset adjustment
          add_v <- adjust_v
          
          if (upMid_v <= length(currGrp_v)) {                               # if group ends with last row, there will actually be no right-shifting rows. TODO - is it better to switch from <= to <?
            for (k in upMid_v:length(currGrp_v)){                           # add adjustment, if on high side (right side)
              thirdLevel$newPos[ currGrp_v[k] ] <- thirdLevel$pos[ currGrp_v[k] ] + adjust_v
              adjust_v <- adjust_v + 0.015
            } # for k
          } # fi
          
          thirdLevel[currGrp_v, adjusted := TRUE]                           # Update adjusted column so that these will be skipped
          
        } # fi
        
      } # for j 
      
      ## Final Fixes - If display is masked, have to make sure segment is also
      thirdLevel[display == "", "Xend" := 3]
      thirdLevel[display == "", "newPos" := pos]
      
      ## Final Fixes - If position of 1st row is negative, need to adjust
      negRow_v <- which(thirdLevel$newPos < 0)
      if (length(negRow_v) > 1) warning("Multiple rows have label positions less than zero. Should only be 1 at maximum.")
      thirdLevel[negRow_v, newPos := 0]
    } # fi
    
    ###
    ### PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ###
    
    ## Re-adjust colors
    if (type_v == "immuneComp") {
      temp1_dt <- secondLevel[,mget(c(cellCol_v, panelHex_v))]; colnames(temp1_dt) <- c("Label", "Hex")
      temp2_dt <- thirdLevel[,mget(c(subCol_v, gateHex_v))]; colnames(temp2_dt) <- c("Label", "Hex")
      plotColor_dt <- rbind(temp1_dt, temp2_dt); plotColor_dt$Label <- as.character(plotColor_dt$Label)
    } else {
      plotColor_dt <- color_dt[,mget(c("SubType", "Hex"))]
      colnames(plotColor_dt) <- c("Label", "Hex")
      plotColor_dt <- rbind(plotColor_dt, newColor_dt)
      plotColor_dt$Label <- as.character(plotColor_dt$Label)
    } # fi
    
    ## Make plot
    first_gg <- ggplot(data = firstLevel, aes(x = 1, y = total)) +
      geom_bar(fill = "white", stat = "identity") +
      coord_polar("y") + 
      guides(fill = FALSE) +
      scale_fill_manual(limits = plotColor_dt$Label, values = plotColor_dt$Hex) +
      ggtitle(paste0(title_v, " - ", currSample_v)) +
      sunburst_theme
    
    ## panelCol level (2)
    fillCol_v <- ifelse(type_v == "cd4", subCol_v, cellCol_v)
    second_gg <- first_gg +
      geom_bar(data = secondLevel, aes_string(x = 2, y = "Sum", fill = fillCol_v), 
               stat = "identity", color = "white", position = "stack") +
      geom_text(data = secondLevel, aes(label = round(Pct, digits = 1), x = 2, y = pos))
    
    ## gateCol level (3)
    currY_v <- ifelse(type_v == "cd4", pd1Col_v, currSample_v)
    pctFillCol_v <- ifelse(type_v == "cd4", "plotFactor", subCol_v)
    third_gg <- second_gg +
      geom_bar(data = thirdLevel, aes_string(x = 3, y = currY_v, fill = pctFillCol_v), 
               stat = "identity", color = "white", position = "stack") +
      geom_text(data = thirdLevel, aes(label = display, x = Xaxis, y = newPos)) +
      geom_segment(data = thirdLevel, aes(x = 3, y = pos, xend = Xend, yend = newPos))
    
    ## Legends
    if (type_v == "cd4") {
      legend_dt <- plotColor_dt; legend_dt[1,1] <- legend_dt[1,1]
      legend_dt[grep("PctPD1", Label), Label := "Pct PD-1+"]
      which_v <- c(which(legend_dt$Label %in% secondLevel[[subCol_v]]), which(legend_dt$Label == "Pct PD-1+")[1])
      legend_dt <- legend_dt[which_v,]
      legend_dt$plot <- rep(1, nrow(legend_dt))
      legend_gg <- g_legend(ggplot(data = legend_dt, aes(x = 1, y = plot, fill = Label)) + geom_bar(stat = "identity") +
                              scale_fill_manual(limits = legend_dt$Label, values = legend_dt$Hex) + labs(fill = "T Cell Subset"))
    } else {
      panelLeg_gg <- g_legend(ggplot(data = secondLevel, aes_string(x = 2, y = "Sum", fill = cellCol_v)) + geom_bar(stat = "identity") +
                                scale_fill_manual(limits = as.character(secondLevel[[cellCol_v]]), values = secondLevel[[panelHex_v]]))
      
      ## Gate Legend
      gateLeg_gg <- g_legend(ggplot(data = thirdLevel, aes_string(x = 3, y = currSample_v, fill = subCol_v)) + geom_bar(stat = "identity") +
                               scale_fill_manual(limits = as.character(thirdLevel[[subCol_v]]), values = thirdLevel[[gateHex_v]]) +
                               guides(fill = guide_legend(ncol = 2)))
    }
    
    ###
    ### ARRANGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ###
    
    ## Add plot to list
    plot_ls[[currSample_v]] <- third_gg
    
    ## Add legends to list
    if (i == 1) {
      if (type_v == "immuneComp") {
        legend_ls[[cellCol_v]] <- panelLeg_gg
        legend_ls[[subCol_v]] <- gateLeg_gg
      } else {
        legend_ls[[subCol_v]] <- legend_gg
      } # fi
    } # fi
    
  } # for i
  
  ## merge table of missing measurements
  if (length(zero_ls) == 1) {
    zeroMerge_grob <- myTableGrob(zero_ls[[1]], title_v = "Zero Counts")
  } else if (length(zero_ls) == 0){
    zeroMerge_grob <- nullGrob()
  } else {
    zeroMerge_grob <- myTableGrob(mergeDTs(zero_ls, mergeCol_v = c(cellCol_v, subCol_v)), title_v = "Zero Counts")
  }
  
  ## Final output
  out_ls <- list("plot" = plot_ls, "legend" = legend_ls, "zero" = zeroMerge_grob)
  return(out_ls)
  
} # mlSunburstChart

fxnlSunburstChart <- function(data_dt, groupCol_v = "Group", calcCol_v = "Calc", color_dt, sample_v, groups_v = NULL,
                            legendCol_v = c("#EEC62C", "#0F8012", "#020D80"), title_v = "CD8 T Cell Groups") {
  #' Multiplex Sunburst Chart
  #' @description Sunburst chart displaying the hierarchical distribution of different immune types
  #' @param data_dt data.table with rows = immune cell gate and columns are samples. Columns must be:
  #' (1) Panel (2) Gate (3) Info (4) NORMALIZED.VALUES (5) SUM.ROIs (6..n) samples  ### THIS IS STILL FROM STACKED BAR - NEED TO CHANGE
  #' @param groupCol_v character vector - name of 1st column that determines lymphoid or myeloid panel
  #' @param calcCol_v character vector - name of 2nd column that defines the gate used
  #' @param color_dt data.table with rows = immune cell groups and columns of various metadata as well as color specifications.
  #' @param sample_v character vector - sample or samples to plot. Must be a valid column name of data_dt.
  #' Should be numbers (e.g. 81, 82, 83) or numbers prepended by S (e.g. S81, S82, S83)
  #' @param groups_v character vector - one or more of the values of groupCol_v. Must be a percentage of CD8 Functional groups.
  #' @param legendCol_v character vector - vector of either color names or hex values.
  #' @param title_v character vector - title for plot. Default is "Immune Cell Composition"
  #' Required columns: 
  #' 'Subtype' - immune cell subtype (e.g. Th0, B cell, NK, etc.)
  #' 'Gate' - gating used. Must be equal to the calcCol_v values in data_dt
  #' 'Hex' - hex code color.
  #' @value returns a "gg" and "ggplot" object that can be printed to console or saved to a file.
  #' @export
  
  ### Handle Sample
  if (sample_v == "all") {
    sample_v <- setdiff(colnames(data_dt), c(groupCol_v, calcCol_v))
  } # fi
  
  ## Handle groups
  if (is.null(groups_v) | groups_v == "all") {
    groups_v <- c("PctKi67", "PctGRZB+")
  }
  
  ## Add an "S" before the sample names, because numeric column names can cause trouble
  newSample_v <- paste0("S", gsub("S", "", sample_v))
  
  ## Adjust in data.table
  whichChange_v <- which(colnames(data_dt) %in% sample_v)
  colnames(data_dt)[whichChange_v] <- newSample_v
  
  ## Merge with color table
  colorCols_v <- c("Hex", "Legend", "SubLegend")
  data_dt <- merge(data_dt, color_dt[,mget(c(groupCol_v, calcCol_v, colorCols_v))], by = c(groupCol_v, calcCol_v), sort = F)
  
  ## Lists to hold results
  plot_ls <- legend_ls <- zero_ls <- list()
  
  ## Run for each sample
  for (i in 1:length(newSample_v)) {
    
    ## Get sample and data
    currSample_v <- newSample_v[i]
    currData_dt <- data_dt[,mget(c(groupCol_v, calcCol_v, currSample_v, colorCols_v))]
    
    ## Split data
    currBase_dt <- currData_dt[get(groupCol_v) == "CD8Functional",]
    currFilter_v <- currBase_dt[[calcCol_v]]
    currSecondary_dt <- currData_dt[get(groupCol_v) != "CD8Functional",]
    currSecondary_dt <- currSecondary_dt[get(calcCol_v) %in% currFilter_v,]
    
    ## Get first level (overall sum)
    firstLevel <- currBase_dt %>% summarize(total = sum(get(currSample_v)))
    
    ##
    ## Second Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##
    
    ## Extract main CD8 Functional Groups
    secondLevel <- currBase_dt
    
    ## Change cell to a factor - WATCH THIS!! MAY NEED TO CHANGE IN THE FUTURE
    secondLevel[[calcCol_v]] <- factor(secondLevel[[calcCol_v]], levels = rev(secondLevel[[calcCol_v]]))
    
    ## Get position in the middle of each slices (cumSum - currval/2)
    secondLevel$runSum <- cumsum(secondLevel[[currSample_v]])
    secondLevel$pos <- secondLevel$runSum - (secondLevel[[currSample_v]] / 2)
    
    ## Make display values
    secondLevel$display <- round(secondLevel[[currSample_v]], digits = 2)
    
    ##
    ## Third Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##
    
    ## List
    secondaryGroups_lsdt <- legendColor_lsdt <- list()
    
    for (k in 1:length(groups_v)) {
      
      ## Subset
      currGroup_v <- groups_v[k]
      thirdLevel <- currSecondary_dt[Group == currGroup_v,]
      
      ## Get remaining non-group values
      thirdLevelOpposite <- thirdLevel
      thirdLevelOpposite[[currSample_v]] <- 100 - thirdLevelOpposite[[currSample_v]]
      thirdLevelOpposite[[groupCol_v]] <- paste0("not_", thirdLevelOpposite[[groupCol_v]])
      thirdLevel <- rbind(thirdLevel, thirdLevelOpposite)
      
      ## Order to match second level
      thirdLevel <- thirdLevel[order(match(get(calcCol_v), secondLevel[[calcCol_v]]))]
      
      ### Minimize values based on percentages
      thirdLevel$pctOfTot <- thirdLevel[[currSample_v]]
      
      for (j in 1:nrow(thirdLevel)) {
        currCalc_v <- thirdLevel[[calcCol_v]][j]
        currTotal_v <- secondLevel[get(calcCol_v) == currCalc_v, get(currSample_v)]
        thirdLevel[["pctOfTot"]][j] <- thirdLevel[[currSample_v]][j] * (currTotal_v / 100)
      }
      
      ### Modify values for display
      thirdLevel$display <- as.character(round(thirdLevel[[currSample_v]], digits = 2))
      notRows_v <- grep("not_", thirdLevel[[groupCol_v]])
      thirdLevel[notRows_v, display := ""]
      
      ### Get position stuff
      thirdLevel$runSum <- cumsum(thirdLevel$pctOfTot)
      thirdLevel$pos <- thirdLevel$runSum - (thirdLevel$pctOfTot / 2)
      
      ### Try factor
      thirdLevel$plotFactor <- paste(thirdLevel[[groupCol_v]], thirdLevel[[calcCol_v]], sep = "_")
      thirdLevel$plotFactor <- factor(thirdLevel$plotFactor, levels = rev(thirdLevel$plotFactor))
      
      ### Add to list
      secondaryGroups_lsdt[[currGroup_v]] <- thirdLevel
      
      ### Legend color list
      newColor_dt <- data.table("Label" = thirdLevel$plotFactor, "Hex" = rep(c(legendCol_v[k], "#FFFFFF"), nrow(thirdLevel)/2))
      legendColor_lsdt[[currGroup_v]] <- newColor_dt
      
    } # for k
    
    ###
    ### PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ###
    
    ## Get plot colors for main CD8 groups
    plotColor_dt <- color_dt[,mget(c(calcCol_v, "Hex"))]
    colnames(plotColor_dt) <- c("Label", "Hex")
    goodCol_v <- grep("PD|EOMES", plotColor_dt$Label)
    plotColor_dt <- unique(plotColor_dt[goodCol_v,])
    
    ## Get plot colors for the plotFactor values
    plotFactorColor_dt <- do.call(rbind, legendColor_lsdt)
    
    ## Get legend colors for sub-groups
    subGroupColor_dt <- do.call(rbind, sapply(legendColor_lsdt, function(x) x[1,], simplify = F))
    subGroupColor_dt$Label <- gsub("_.*$", "", subGroupColor_dt$Label)
    
    ## Combine legend data
    legendColor_dt <- rbind(plotColor_dt, subGroupColor_dt)
    legendColor_dt$plot <- rep(1, nrow(legendColor_dt))
    
    ## Combine plot data
    plotColor_dt <- rbind(plotColor_dt, plotFactorColor_dt)
    
    ## Make plot
    first_gg <- ggplot(data = firstLevel, aes(x = 1, y = total)) +
      geom_bar(fill = "grey", stat = "identity") +
      coord_polar("y") + 
      guides(fill = FALSE) +
      scale_fill_manual(limits = as.character(plotColor_dt$Label), values = plotColor_dt$Hex) +
      ggtitle(paste0(title_v, " - ", currSample_v)) +
      sunburst_theme
        
    ## gateCol level (2)
    second_gg <- first_gg +
      geom_bar(data = secondLevel, aes_string(x = 2, y = currSample_v, fill = calcCol_v), 
               stat = "identity", color = "white", position = "stack", width = 1.5) +
      geom_text(data = secondLevel, aes(label = display, x = 2, y = pos))

    ## Subgroup levels
    allLevels_v <- unname(unlist(lapply(secondaryGroups_lsdt, function(x) x$plotFactor)))
    final_gg <- second_gg
    
    ## Determine width
    width_v <- 1 / length(groups_v)
    size_v <- 4 / length(groups_v)
    
    for (k in 1:length(groups_v)) {
      ## Get data
      currGroup_v <- groups_v[k]
      currSubGroup_dt <- secondaryGroups_lsdt[[currGroup_v]]
      
      ## Testing adding more levels
      currSubGroup_dt$plotFactor <- factor(currSubGroup_dt$plotFactor, levels = rev(allLevels_v))
      
      ## Have to re-name plotFactor
      whichCol_v <- grep("plotFactor", colnames(currSubGroup_dt))
      newName_v <- paste0(colnames(currSubGroup_dt)[whichCol_v], k)
      colnames(currSubGroup_dt)[whichCol_v] <- newName_v
      
      ## Also rename display
      whichCol_v <- grep("display", colnames(currSubGroup_dt))
      newDisp_v <- paste0(colnames(currSubGroup_dt)[whichCol_v], k)
      colnames(currSubGroup_dt)[whichCol_v] <- newDisp_v
      
      ## Add layer
      final_gg <- final_gg +
        geom_bar(data = currSubGroup_dt, aes_string(x = 2.5+(k*width_v), y = "pctOfTot", fill = newName_v),
                 stat = "identity", color = "white", position = "stack", width = width_v) +
        geom_text(data = currSubGroup_dt, aes_string(label = newDisp_v, x = 2.5+(k*width_v), y = "pos")) # , size = size_v
    }
    
    ### Make 
    legend_gg <- g_legend(ggplot(data = legendColor_dt, aes(x = 1, y = plot, fill = Label)) + geom_bar(stat = "identity") +
                            scale_fill_manual(limits = as.character(legendColor_dt$Label), values = legendColor_dt$Hex) +
                            labs(fill = "Group"))
    
    
    ###
    ### ARRANGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ###
    
    ## Add plot to list
    plot_ls[[currSample_v]] <- final_gg
    
    ## Add legends to list
    if (i == 1) {
      legend_ls[["legend"]] <- legend_gg
    }
    
  } # for i
  
  ## Final output
  out_ls <- list("plot" = plot_ls, "legend" = legend_ls)
  return(out_ls)
  
} # fxnlSunburstChart

sunburstPlot <- function(sunburst_lslsgg, pct_v = T, type_v) {
  #' Display sunburst chart
  #' @description plot one or more sunburst charts with a common legend
  #' @param sunbusrt_lsgg list of lists output by sunburstChart(). List elements are:
  #' 'plot' - list of ggplot sunburst charts
  #' 'legend' - list of ggplot legend grobs
  #' 'zero' - list of zero-count rows
  #' @param pct_v logical. TRUE (default) display pct on single sunburst outputs. FALSE - don't display percentages.
  #' @param type_v character vector. Either 'immune', 'fxnl', or 'cd4'
  #' multi-sunburst plots always remove percentages.
  #' @export
  
  ### Split objects
  plot_lsgg <- sunburst_lslsgg$plot
  legend_lsgg <- sunburst_lslsgg$legend
  
  ### Get number of plots and legends
  numPlots_v <- length(plot_lsgg)
  numLeg_v <- length(legend_lsgg)
  
  ### Get title text
  if (type_v == "immune") {
    titleText_v <- "Immune Cell Composition"
  } else if (type_v == "fxnl") {
    titleText_v <- "CD8 T Cell Groups"
  } else if (type_v == "cd4") {
    titleText_v <- "CD4 T Cell Subsets"
  } else {
    stop("Incorrect value for 'type_v'")
  }
  
  ### Remove percentages
  if (numPlots_v > 1 | !pct_v) {
    
    for (i in 1:length(plot_lsgg)) {
      currPlot <- plot_lsgg[[i]]
      currRemoveLayers <- which(sapply(currPlot$layers, function(x) {
        ("GeomText" %in% class(x$geom) | "GeomSegment" %in% class(x$geom)) }))
      currPlot$layers <- currPlot$layers[-currRemoveLayers]
      currPlot$labels$title <- NULL
      plot_lsgg[[i]] <- currPlot
    } # for
    
    if (numPlots_v == 1){
      title_v <- textGrob(paste0(titleText_v, " - ", names(plot_lsgg)[1]), gp = gpar(fontsize = 18))
    } else {
      title_v <- textGrob(titleText_v, gp = gpar(fontsize = 18))
    } # fi
    
  } else {
    title_v <- NULL
  } # fi
  
  ### Make matrix
  A <- c(rep(1,6), 2, rep(NA, 2))
  B <- c(rep(1,6), rep(3,3))
  C <- c(rep(1,6), rep(NA,3))
  D <- c(rep(1,6), rep(2, 3))
  if (numLeg_v == 2){
    matrix_mat <- rbind(C,A,A,A,B,B,B,B,B,B)
  } else {
    matrix_mat <- rbind(C,D,D,D,D,D,D,D,D)
  }
  
  
  ### Get labels
  if (numPlots_v > 1) {
    labels_v <- names(plot_lsgg)
  } else {
    labels_v <- NULL
  }
  
  ### Make list of grobs
  grobs_ls <- list(ggarrange(plotlist = plot_lsgg, labels = labels_v))
  for (i in 1:numLeg_v) {
    grobs_ls[[i+1]] <- legend_lsgg[[i]]
  }
  
  ### Output
  grid.arrange(grobs = grobs_ls, layout_matrix = matrix_mat, top = title_v)
  
} # sunburstPlot

