#################################
# Title: SBT - North Creek - Transducer data processing
# Author: Jennie Hansson
# Last Updates: July 18, 2024
# Description: Plotting Levelogger and diver data (Pressure, Conductivity and Temperature) along with precipitation data.
## Based on a code provided by Jonathan Kennel

#################################
# Activate libraries 
library(data.table)
library(plotly)
library(ggplot2)
library(remotes)
library(readxl)
library(viridis)
library(rsk)
library(transducer)
library(hydrorecipes)
library(data.table)
library(tidyverse)
library(fs)

#remotes::install_github("jkennel/rsk")
#remotes::install_github("jkennel/transducer")
#remotes::install_github("jkennel/hydrorecipes")

#Convert dbar to m of water if needed, FYI not needed for Leveloggers: https://www.convertunits.com/from/decibar/to/meter+of+head
#dbar_to_m <- 1.0199773

#Covert time and date to the right format.
start <- as.POSIXct("2024-06-07", tz = "UTC")
end <- as.POSIXct("2024-07-03", tz = "UTC")

# Set file directory:
file_dir <- "data/"

# Read excel file: 
#filepath <- c("data/")
# loc <-readxl::read_xlsx(path=paste0(filepath,"Input_North Creek.xlsx"),
#                         sheet = 'Sheet1',
#                         range ="A1:J41")
loc <-readxl::read_xlsx("data/Input_North Creek.xlsx",
                        sheet = 'Sheet1',
                        range ="A1:J41")

#Set DataTable 
#Make a list of the file names in the folder (TURE= entire path, FALSE= file name only:
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.csv")
fn <- fn[basename(fn) %in% loc$file_name]

#set file path to read in all csv files in the folder:
file_paths <- fs::dir_ls("data/")

#read all csv files in the folder:
# 
# df <- rbindlist (lapply (
#   list.files(path = "data/", pattern = "*.csv"),
#   fread, select = c('file_name')
# )
# )

# create a user defined function
readdata <- function(x){
  dt_all <- fread(x,
                  sep=",",
                  header = TRUE,
                  skip = 13)
  return(dt_all)}
              
mylist <- lapply(fn, readdata,setnames(fn(pattern="file_name")))

################################################################
#have this melt function included before making it to a datatable to include the baro here too!!! data.table::melt(setDT())
mydata <- rbindlist(mylist, fill = TRUE)
# bandaid soln to excluding those fucked up baro cols
#mydata <- mydata[,list(Date,Time,ms,LEVEL,TEMPERATURE,CONDUCTIVITY)]

#ignore rows (no manipulation), in cal, beside the file_name col, add the following substitution: 
# for all substitutions, type "data/" followed by file_name, use exact string matching using the existing column:
mydata[,file_name := gsub('data/', '', mylist, fixed = TRUE)]

###########################################################
#Merge the two data tables together Loc and Mydata: Bringing in the loc DT to mydata and match the data with file_name. 

datamerged <- loc[mydata, on = "file_name"]

###########################################################

#Choosing one variable to plot: 
##Pressure
#redefine pr to look in the variable column for pressure/(level) and looking for the exact string match. 
#pr <- head(select(mydata, LEVEL)
     #      )





pr <- loc[file_content, on = "file_name"]
#dt[, file_name := gsub('data/', '', file_name, fixed = TRUE)]


