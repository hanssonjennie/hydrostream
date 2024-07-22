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
setDT(loc)
loc <- loc[site =="SBT" & well == "UT1-FCT-008-3"]

# search for matches to argument pattern within each element of a character vector:
loc <-loc[grep(".csv", file_name)]

#Make a list of the file names in the folder (TURE= entire path, FALSE= file name only:
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.csv")
fn <- fn[basename(fn) %in% loc$file_name]

#set file path to read in all csv files in the folder:
file_paths <- fs::dir_ls("data/")

file_contents <- list()

for (i in seq_along(file_paths)) {
  file_contents[[i]] <- read_csv(
    file = file_paths[[i]],
    skip = 13
  )
}

file_contents <- set_names(file_contents, file_paths)
file_paths %>% 
  map(function (path) {
    read_csv(path)
  })

dataset <- as.data.frame(file_contents)

#Read the transducer data specified based on the file name, and file number in the table uploaded to R:
##pressure <- read.csv("./Data/1094394_FCT-008-3_2024_07_04_232952.csv",
                    # header = TRUE, skip = 13)

#Choosing one variable to plot: 
##Pressure 

#pressure <- pressure[variable %in% c("LEVEL")]

#pressure <- pressure %in% ("LEVEL")

#pressure[, file_name := gsub('../../hydrostream/data/', '', file_name, fixed = TRUE)]

#pr <- loc[pr, on = "file_name"]

baro <-[port == "baro_nc"]
wl <- pr[!port %in% c("baro_nc")]
wl <- ba[, list(datetime, baro = value)][wl, on = "datetime", nomatch = 0]
