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
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        plot.margin = unit(c(0,6,0,6), "cm"))

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
  #' @param sample_v character vector - sample or samples to plot. Must be a valid column name of data_dt. If this is multiple, group_v must be singular.
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
  hbFxn(melt_dt, calcCol_v, title_v, colors_v = colors_v)
  
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

stackedBar <- function(data_dt, panelCol_v = "Panel", gateCol_v = "Gate", infoCol_v = "Info", plotCol_v = "pctCD45", color_dt,
                       xlab_v = "CD45+ Cells", ylab_v = "% of CD45+ cells", title_v = "Immune Cell Composition") {
  #' Multiplex Stacked Bar Chart
  #' @description Stacked Bar chart displaying the distribution of subtypes from the lymphoid panel
  #' @param data_dt data.table with rows = immune cell gate and columns are samples. Columns must be:
  #' (1) Panel (2) Gate (3) Info (4) NORMALIZED.VALUES (5) SUM.ROIs (6..n) samples
  #' @param panelCol_v character vector - name of 1st column that determines lymphoid or myeloid panel
  #' @param gateCol_v character vector - name of 2nd column that defines the gate used
  #' @param infoCol_v character vector - name of 3rd column that has extra info about some gates
  #' @param plotCol_v character vector - name of column to use as the y-axis.
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
  
  ## Merge data and colors
  merge_dt <- merge(data_dt, color_dt, by = "Gate", sort = F, all.x = F, all.y = T)
  
  ## Add dummy x variable
  merge_dt$x <- rep("a", nrow(merge_dt))
  
  ## Make plot
  stackedBar_gg <- ggplot(data = merge_dt, aes_string(x = "x", y = plotCol_v, fill = "SubType")) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_manual(limits = merge_dt$SubType, values = merge_dt$Hex) +
    labs(y = "% of CD45+ cells", x = "CD45+ Cells") + ggtitle("Immune Cell Composition") +
    stacked_theme
    
  ## Return
  return(stackedBar_gg)
} # stackedBar
