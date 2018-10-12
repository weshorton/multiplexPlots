# Multiplex IHC Shiny App

This repository contains functions and reference data for different visualizations of multiplex IHC data output 
from Coussens Lab functional and myeloid/lymphoid panels. This is for macOS only as of right now.

## Quick Start

1. Make sure you have R installed (https://cran.r-project.org/bin/macosx/)
1. Download/clone this repository
1. Navigate to downloaded location and start the Shiny app using one of the following methods:
    1. In your finder, double-click on `run.command`
    2. From the terminal, `sh run.command`
    3. From your desktop
        1. Copy run.command to Desktop (or preferred location)
	1. Get location of repository download (either `pwd` in your terminal, or from finder, right-click and select "Copy")
	1. Open your `~/.bash_profile` and add `MULTIPLEX_DIR` variable using the above path

		```
		<open terminal app>
		<type> nano ~/.bash_profile
		<scroll to bottom of file>
		<type the following:>
		export MULTIPLEX_DIR=/path/to/repository/download
		<save> (control+X, and then type "Y", and then ENTER)
		```
	1. Double-click on run.command icon from your Desktop
 
3. Load data in the 'View Data' panel
4. View different plots, selecting samples and groups to display

## Description of Contents

1. **data/** - Contains example plotting data as well as standardized colors that are required for different plots.
    1. `ML_stable_batchExport.xlsx` - example myeloid/lymphoid panel.
    2. `cellTypeColors.txt` - Hex/RGB/HSV colors for base cell types identified in myeloid/lymphoid panel.
    3. `functionalColors.txt` - Hex/RGB/HSV colors for standard functional groups.
    4. `functional_stable.xlsx` - example functional panel.
    5. `gateCellSubtypeMapping.txt` - standardized mapping of Gates to their specific subtype and the cell type it belongs to.
    6. `subTypeColors.txt` - Hex/RGB/HSV colors for immune subtypes identified in myeloid/lymphoid panel.
    7. `tCellColors.txt` - Hex/RGB/HSV colors for CD4 T Cell groups.
2. **scripts/** - Contains R code to perform various calculations and also plotting functions
    1. `multiplexFunctions.R` - functions to read in data and perform standard calculations on the raw data
    2. `multiplexPlots.R` - contains all of the plotting functions that are called in `server.R`
3. **fxns.R** - Contains common functions used in the `server.R` script for I/O, data manipulation, etc.
4. **run.command** - shell script to start shiny server. Can run using `sh run.command`, or can double-click the file from
the Desktop, finder, etc.
5. **server.R** - Shiny server function. Loads all dependencies and reference data. Contains all of the information for
how to display data, make changes based on user input, etc.
6. **ui.R** - Shiny ui function. Determines appearance of application.

## Description of Data

This app is relatively limited to a few specific applications at the moment. Below you will find descriptions of the
input data format requirements as well as information on the types of calculations performed and the plots that are available.  

### Functional Data

1. Excel file with `.xlsx` extension.
2. Sheet named "raw data" with:
    1. Rows - different gates (see example data, these must be the same right now. Potentially will support new gates in the future).
    2. Columns:
        1. "Population" - non-unique name that lists the major population (e.g. CD3+ T Cells, CD8 T Cells, etc.).
        2. "Gate" - unique name that lists the gates used for that row (e.g. CD45+PD1+ or CD45+CD3+CD8+GRZB+). 
        3. Columns 3 to end - Individual ROIs. Name should be of the format "S[0-9]+ ROI [0-9]+" or "syn[0-9]+_[0-9]+" (e.g. S120 ROI 1 or syn80_1).
    3. Cells - each cell contains the cell count in that ROI for that particular cell population.

### Myeloid/Lymphoid Data

1. Excel file with `.xlsx` extension.
2. Sheet named "Cohort" with:
    1. Rows - different gates (see example data, these must be the same right now).
    2. Columns
        1. "Panel" - either MYELOID or LYMPHOID.
        2. "Gate" - unique name that lists the gates used for that row.
        3. "Info" - contains information on which rows to use for normalization and also which rows are a part of the immune cell complexity set.
        4. Columns 4 to end - Individual ROIs. Name should be of the format "S[0-9]+ ROI [0-9]+" or "syn[0-9]+_[0-9]+" (e.g. S120 ROI 1 or syn80_1).
    3. Cells - each cell contains the cell count in that ROI for that particular cell populations.

## Description of Calculations

### Functional Data

1. **pctCD45** - Percentage of all cells that are CD45+.
2. **majorImmune** - four calculations for major immune group proportions. Percentage of CD45+ cells that are (A) CD8+, (B) CD4+, (C) CD68+, (D) other.
3. **CD8Functional** - percentage of CD8+ cells that belong to different functional groups.
4. **PctKi67** - percentage of CD8+ functional groups that are Ki67+.
5. **PctGRZB+** - percentage of CD8+ functional groups that are GRZB+.
6. **CD4** - percentage of CD4+ cells that are Ki67+, GRZB+, and PD-1+. Also percentage of CD4+PD-1+ cells that are Ki67+.

### Myeloid/Lymphoid Data

1. **Immune Group Proportions** - Percentage of CD45+ cells that belong to major immune groups (Groups are defined in the "Info" column).
2. **Ratios** - Different ratios of common immune groups:
    1. Th1/Th2
    2. NK/Treg
    3. Th1/Treg
    4. CD8/CD68+
    5. CD163-/CD163+ TAMs
    6. Mature/immature DCs
    
## Description of Plots

### Functional Plots

1. **Pie Charts** - Display different ratios of groups. Common groups to display:.
    1. Major Immune Groups.
    2. Percent of CD45+ that are CD8+.
    3. CD8 Functional Groups (also see sunburst).
2. **Horizontal Bar Charts** - relatively limited. Check sunburst.
    1. Compare percentages of CD8 functional groups between samples.
3. **Sunburst Chart** - Display ratios of CD8 Functional Groups (see pie chart) with additional information.
    1. PctKi67 - display percentage of each functional group that is Ki67+ in an outer ring.
    2. PctGZRB+ - same as PctKi67, but for GRZB.

### Meyloid/Lymphoid Plots

1. **Stacked Bar Chart** - Display proportions of different immune cell groups between samples.
2. **Sunburst Chart** - Display same proportions as above, but in a sunburst format. Includes an inner circle that displays
major cell types that the immune cell groups belong to.


