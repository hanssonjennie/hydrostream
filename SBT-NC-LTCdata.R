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

#remotes::install_github("jkennel/rsk")
#remotes::install_github("jkennel/transducer")
#remotes::install_github("jkennel/hydrorecipes")

#Convert dbar to m of water if needed, FYI not needed for Leveloggers: https://www.convertunits.com/from/decibar/to/meter+of+head
#dbar_to_m <- 1.0199773

#Covert time and date to the right format.
s <- as.POSIXct("2024-06-07", tz = "UTC")
e <- as.POSIXct("2024-07-03", tz = "UTC")

# Set file directory:
file_dir <- "C:/Users/jhansson/OneDrive - University of Guelph/Sulfolane-hansson workfolder/02 RScripts/Transducer data/North Creek/hydrostream/data/"

# Read excel file: 
filepath <- c("C:/Users/jhansson/OneDrive - University of Guelph/Sulfolane-hansson workfolder/02 RScripts/Transducer data/North Creek/hydrostream/data/")
loc <-readxl::read_xlsx(path=paste0(filepath,"Input_North Creek.xlsx"),
                        sheet = 'Sheet1',
                        range ="A1:J41")

#Set DataTable 
setDT(loc)
loc <- loc[site =="SBT" & well == "UT1-FCT-008-3"]

# search for matches to argument pattern within each element of a character vector:
loc <-loc[grep("CSV", file_name)]

#Make a list of the file names in the folder (TURE= entire path, FALSE= file name only:
#file name <- list.files(file_dir, full.names = TRUE, pattern = "*.csv")

fn <- list.files("./data", full.names = TRUE, pattern = "*.csv")
fn <- fn[basename(fn) %in% loc$file_name]

#Read the transducer data specified based on the file name, and file number in the table uploaded to R:
 
transducer::read_transducer(fn[21])

pressure <- read.csv("1094394_FCT-008-3_2024_07_04_232952.csv",
                     header = TRUE)
pressure <-fread()

pressure <-read_csv(fn[1:6],
                    return_data_table = FALSE,
                    include_params = c('file_name'),
                    keep_raw = TRUE,
                    raw = TRUE)


pressure <- pr[variable %in% c("LEVEL")]
pressure[, file_name := gsub('../../hydrostream/data/', '', file_name, fixed = TRUE)]
#### committing to test 1 
#test 2
