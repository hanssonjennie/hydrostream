#################################
# Title: SBT - North Creek - Transducer data processing
# Author: Jennie Hansson
# Date created: July 18, 2024
# Last Update: August 2, 2024
# Description: Plotting Levelogger data exclusively (Pressure, Conductivity and Temperature) along with precipitation data.
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
library(tidyr)

#remotes::install_github("jkennel/rsk")
#remotes::install_github("jkennel/transducer")
#remotes::install_github("jkennel/hydrorecipes")

#Convert dbar to m of water if needed, FYI not needed for Leveloggers: https://www.convertunits.com/from/decibar/to/meter+of+head
#dbar_to_m <- 1.0199773

#Covert time and date to the right format and decide the starting and ending time of the analysis.

start <- as.POSIXct("2024-06-06 12:00:00", tz= "UTC")
end <- as.POSIXct("2024-07-02 15:00:00", tz = "UTC")

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

#Generate a datatable using the metadata file we just read in

setDT(loc)

#use grep to only include csv files from the file_name column if needed: We are disregarding this function here because we want to include all CSV files so we can pick and choose the data to analyze lower down in the code.
#loc <- loc[grep("csv",file_name)]

#Set DataTable 
#Make a list of the file names in the folder (TURE= entire path, FALSE= file name only:

fn <- list.files(file_dir, full.names = TRUE, pattern = "*.csv")

# Cross referencing the data files to the data table and file_name column generating a character list 

fn <- fn[basename(fn) %in% loc$file_name]

#set file path to read in all csv files in the folder:

file_paths <- fs::dir_ls("data/")


# create a user defined function to read all the csv files in the folder and return the output in a dt.

readdata <- function(x){
  dt_all <- fread(x,
                  sep=",",
                  header = TRUE,      # Header y/n?
                  skip = 13)          # If you need to skip rows to make sure you only have a header.
  return(dt_all)}
              
# Generate a list of the csv files based on the fn and the function to read data.

#mylist <- lapply(fn, readdata, use.names = TRUE)

mylist <- sapply(fn,readdata, simplify = FALSE, USE.NAMES = TRUE)
#sapply(n,FUN = ...,simplify = FALSE,USE.NAMES = TRUE)


################################################################
################################################################

# Generate a data table for mydata with mylist and generate id columns to be able to identify the file names and add a new column in my data with the file names based on the id column.
# Notice if you have a different number of columns or rows you might need to use a function called "melt" to merge these differences in the cvs files accordingly. JH cheated here.

mydata <- rbindlist(mylist, fill = TRUE, idcol = TRUE)

mydata[, .id := gsub('data/','',.id, fixed = TRUE)]
mydata$file_name <- mydata$.id

#mydata$file_name <-loc[mydata$.id,2]

# Specify columns to show (if necessary)
#mydata <- mydata[,list(Date,Time,ms,LEVEL,TEMPERATURE,CONDUCTIVITY)]

# Add the infromation in dt 'loc' to mydata dt based on the file name. Here I'm generating a new table instead of replacing/ adding rows in already existing dt.

data_all <- loc[mydata, on = "file_name"]


##############################################################
##############################################################

############# convert time format to UTC: HH:MM:SS ###########

############# reference code - broken down into steps ########

# time24hr <- strptime(newtable$Time, format = "%I:%M:%S %p")
# time24hrformatted <- format(time24hr, format = "%H:%M:%S")
### create new column of 24 hr time by stripping format away, reformatting time ###
# newtable$time24hr = format(strptime(newtable$Time, "%I:%M:%S %p"), format = "%H:%M:%S")
# newtable$date_adj = format(strptime(newtable$Date,"%d/%m/%Y"), format = "%Y-%m-%d")

# Merge Date and time to one column and create a new column with the the adjusted datetime format:

data_all$datetime = paste(data_all$Date, data_all$Time, sep = " ")
data_all$datetime = format(strptime(data_all$datetime,"%m/%d/%Y %I:%M:%S %p"), format = "%Y-%m-%d %H:%M:%S")
data_all$datetime_adj = as.POSIXct(data_all$datetime, tz = "UTC")

# Create a baro data table from mydata to subset using condition when port is equal to baro_nc

baro <- data_all[port == "baro_nc"]


# create wl dt from data_all subset using condition that excludes all ports equal to baros or liners

wl <- data_all[!port %in% c("baro_nc", "liner")]

# this brings in the baro value to line up with port data, no rows will be returned if no match. 
# Use baro dt and datetime to match exact columns between both data tables to the wl data table, create a new colum baro that has the baro value and if no match = no value

wl <- baro[,list(datetime_adj, baro = LEVEL)][wl,on="datetime_adj", nomatch= 0]

# Calculate water height above transducer from pressure, baro pr, port depth and make a new col called "head"

wl[, head := (LEVEL - baro) * monitoring_location]

# Sort wl data table by time

setkey(wl, datetime_adj)


# subset the wl table with specified time interval for analysis 

wl_sub <- wl[datetime_adj %between% c(start,end)]

# make a new column in dt, calculation is pressure - the first pressure entry (TYPE VALUE HERE)

wl_sub[, value_adj := LEVEL - LEVEL[1], by = well]

# use a if function to make sure to get all conductivity values 

wl_sub$conductivity_adj <- ifelse(is.na(wl_sub$CONDUCTIVITY), wl_sub$CON_UCTIVITY, wl_sub$CONDUCTIVITY)


# Specify the wells you want to plot:

wl_sub <- wl_sub[wl_sub$well %in% c("UT1-FCT-005-3", "UT1-FCT-005-2", "UT1-FCT-005-1")]
setkey(wl_sub, datetime_adj)
    #################################################################
#######################TIME TO PLOT######################################
   ##################################################################
# show a subset in the plot. Set 60 entries to 0, this means looking at every minute data data
# Start with plotting value adjusted and here change to head if you want to look at the water column. 
p1 <- plot_ly(wl_sub[as.numeric(datetime_adj) %% 60 == 0],
              x = wl_sub$datetime_adj,
              y = wl_sub$head,
              color = wl_sub$well,
              colors = viridis(20),
              #name = ~well,
              type = "scatter", mode = "lines")
  
# call the plot:
# p1


# Plot two will include conductivity
p2 <- plot_ly(wl_sub[as.numeric(datetime_adj) %% 60 == 0],
              x = wl_sub$datetime_adj,
              y = wl_sub$conductivity_adj,
              color = wl_sub$well,
              colors = viridis(20),
              #name = ~well,
              type = "scatter", mode = "lines", showlegend=FALSE)
  
# call the plot:
# 
p2

# Plot three will include temperature
p3 <- plot_ly(wl_sub[as.numeric(datetime_adj) %% 60 == 0],
               x = wl_sub$datetime_adj,
               y = wl_sub$TEMPERATURE,
               color = wl_sub$well,
               colors = viridis(20),
               #name = ~well,
               type = "scatter", mode = "lines", showlegend=FALSE)

# call the plot:
# p3

# Plot the baro!
p_baro <- plot_ly(wl_sub[as.numeric(datetime_adj) %% 60 == 0],
              x = wl_sub$datetime_adj,
              y = wl_sub$baro,
              name = "Baro",
              type = "scatter", mode = "line")

#p_baro

# Show numerous plots in a single view

subplot(p1,p_baro,p2,p3, shareX = TRUE, nrows = 4)%>%
          layout(
            title = list(text="UT1-FCT-005",font = list(size = 20), xref='paper', xref='paper', xanchor = 'center', yanchor = 'top'),
            legend=list(title=list(text='Well ID')),
            xaxis = list(title = "Date and time",
                         tickangle = "30",
                         ticks='outside',
                         nticks=10,
                         minorticks=5,
                         tickcolor = 'gray80',
                         gridcolor = 'gray80',
                         showgrid = TRUE), # Ensure grid lines are shown
            grid =list(x=10, y='auto'),
            yaxis = list(title = "Head"),# range = list(-0.2, 0.1)),
            yaxis2 = list(title = 'Pressure'),
            yaxis3 = list(title ="Conductivity"),# range = list(100,600)),
            yaxis4 = list(title ="Temperature")#, range = list(2,5.5))
            
          )



##########################################################################

