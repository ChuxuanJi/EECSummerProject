################################
# Counting cell numbers from
# flow cytometry data
# By Tom Smith
# 2022/4/20
# Bertram Ji

rm(list = ls())

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("flowCore")
BiocManager::install("flowViz")
BiocManager::install('flowStats')
library(flowCore) # flow cytometry file reading and manipulation
library(flowViz) # alternative visualisation!
library(flowStats) # cunning flow data functions
install.packages('tidyr')
library(tidyr)
install.packages('dplyr')
library(dplyr)
install.packages('lattice')
library(lattice)

# Functions to run

# this is a gate for cells on FL2 fluoresence from thiazole orange
# based on my (Toms S) experiments with bacterial isolates
# not completely sure if appropriate for all samples...
# should visualise to check
gate.FL2 <- polygonGate("FSC-H" = c(9, 17, 17, 9), 
                        "FL2-A" = c(7, 7, 15, 15))


# function to filter the data based on the gate 
# and return a dataframe with cell counts
filter_data <- function(dataset, gate){
  # filter the data
  filtered.data <- flowCore::filter(dataset, gate)
  # get the cells inside the filter (gate)
  cell.counts <- lapply(filtered.data, function(x) summary(x)$true)
  # reformat
  counts.df <- data.frame(cell.counts) %>%
    pivot_longer(cols = 1:length(cell.counts), names_to = "well", values_to = "cell.count")
  # return the data
  return(counts.df)
}

# main function - read in data and output cell counts for each well
count.cells <- function(data_folder, plot){
  # make a string for the data path
  data_path <- paste("data/", data_folder, "/", sep = "")
  # read the data
  flowdata <- read.flowSet(path = data_path)
  # log it
  tf <- transformList(from=colnames(flowdata)[1:12], tfun=asinh)
  flowdata.log <- tf %on% flowdata
  # perform the filtering function
  cellcounts <- filter_data(flowdata.log, gate.FL2)
  # write a csv
  output_name <- paste("results/", data_folder, ".csv", sep = "")
  write.csv(cellcounts, output_name, row.names = FALSE)
  # plot it, if we want to
  if(plot == TRUE){
    plot_output <- paste("results/", data_folder, ".png", sep = "")
    p <- xyplot(`FL2-A` ~ `FSC-H`, data=flowdata.log, smooth = FALSE, filter = gate.FL2)
    png(file = plot_output, width = 800, height = 800)
    print(p)
    dev.off()
  }
}


############################
### Main code starts here ##
############################

# set working directory
#
# structure should be 3 sub-folders of:
# code (where this R file is located)
# data (where the data is found)
# results (where the results will be saved)
#
# within the data folder, there should be sub-folders
# each containing a set of fcs files

setwd("C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/flow-cytometry/Flow-cytometryApr12")

# list of flow cytometry folders (within /data/ folder):
folder_list <- c("20220420_1_6", "20220420_7_12")

# run the function for all samples
for(i in seq_along(folder_list)){
  count.cells(folder_list[i], plot = TRUE)
}
