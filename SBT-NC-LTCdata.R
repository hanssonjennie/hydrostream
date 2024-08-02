#################################
# Title: SBT - North Creek - Transducer data processing
# Author: Jennie Hansson
# Date created: July 18, 2024
# Last Update: August 1, 2024
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
start <- as.POSIXct("2024-06-06 12:00:00", tz= "UTC")
end <- as.POSIXct("2024-07-03 18:00:00", tz = "UTC")

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
#Generate a datatable using the metadata file we just red in
setDT(loc)

#use grep to only include csv files from the file_name column if needed:
#loc <- loc[grep("csv",file_name)]

#Set DataTable 
#Make a list of the file names in the folder (TURE= entire path, FALSE= file name only:
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.csv")
#cross referencing the data files to the data table and file_name column 
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
              
mylist <- lapply(fn, readdata)

################################################################
#have this melt function included before making it to a datatable to include the baro here too!!! data.table::melt(setDT())
mydata <- rbindlist(mylist, fill = TRUE, idcol = TRUE)
mydata$file_name <-loc[mydata$.id,2]
# bandaid soln to excluding those fucked up baro cols
#mydata <- mydata[,list(Date,Time,ms,LEVEL,TEMPERATURE,CONDUCTIVITY)]

################################################################
#ignore rows (no manipulation), in cal, beside the file_name col, add the following substitution: 
# for all substitutions, type "data/" followed by file_name, use exact string matching using the existing column:
#mydata[,file_name := gsub('data/', '', mylist, fixed = TRUE)]
###############################################################

###########################################################
#Merge the two data tables together Loc and Mydata: Bringing in the loc DT to mydata and match the data with file_name. 

##datamerged <- loc[mydata, on = "file_name"]

###########################################################
#Choosing one variable to plot: 
##Pressure and redefine pr to look in the variable column for pressure/(level) and looking for the exact string match. 
#pr <- head(select(mydata, LEVEL)
          # )

#pr <-mydata[mydata %in% c("LEVEL")]
#mydata <-mydata$LEVEL
#pr <- loc[mydata, on = "file_name"]
#dt[, file_name := gsub('data/', '', file_name, fixed = TRUE)]
###########################################################
##########################################################


newtable <-loc[mydata, on = "file_name"]


#### convert time format to UTC: HH:MM:SS ##########

###### reference code - broken down into steps #####
# time24hr <- strptime(newtable$Time, format = "%I:%M:%S %p")
# time24hrformatted <- format(time24hr, format = "%H:%M:%S")
###### create new column of 24 hr time by stripping format away, reformatting time ######
#newtable$time24hr = format(strptime(newtable$Time, "%I:%M:%S %p"), format = "%H:%M:%S")
#newtable$date_adj = format(strptime(newtable$Date,"%d/%m/%Y"), format = "%Y-%m-%d")


#merge Date and time to one column and create a new one:

newtable$datetime_adj = paste(newtable$Date, newtable$Time, sep = " ")
newtable$datetime_adj = format(strptime(newtable$datetime_adj,"%m/%d/%Y %I:%M:%S %p"), format = "%Y-%m-%d %H:%M:%S")
newtable$datetime_adj1 = as.POSIXct(newtable$datetime_adj, tz = "UTC")
##########################################################
# Create a bar data table from mydata to subset using condition when port is equal to baro_nc
baro <- newtable[port == "baro_nc"]


# create wl dt from pr subset using condition that excludes all ports equal to baros or liners

wl <- newtable[!port %in% c("baro_nc", "liner")]

#this brings in the baro value to line up with port data, no rows will be returned if no match. Use baro dt and datetime to match excat columns between both dts to the wl dt, create a new colum baro that has the baro value and if no match = no value
wl <- baro[,list(datetime_adj1, baro = LEVEL)][wl,on="datetime_adj1", nomatch= 0]

#Calculate water height above transducer from pressure, baro pr, port depth and make a new col called "head"

wl[, head := (LEVEL - baro) * monitoring_location]

#Sort wl data table by time by 

setkey(wl, datetime_adj1)


#subset the wl dt by desired times

#wl_sub <- wl[datetime %between% c(start, end)]

####################################################
#make a new column in dt, calculation is pressure - the first pressure entry (TYPE VALUE HERE)



wl_sub <- wl[datetime_adj1 %between% c(start,end)]
wl_sub[, value_adj := LEVEL - LEVEL[1], by = well]
wl_sub <- wl_sub[wl_sub$well %in% c("UT1-FCT-008-3", "UT1-FCT-008-2", "UT1-FCT-008-1")]
setkey(wl_sub, datetime_adj1)

# Specify a time interval  



##############################################################

x1 <- wl_sub[as.numeric(datetime_adj1) %% 60 == 0]
y1 <- wl_sub$value_adj
y2 <- wl_sub$CONDUCTIVITY
#y3 <- wl_sub$baro
#x2 <- wl_sub[as.numeric(datetime_adj1) %% 60 == 0]

fig1 <- plot_ly() %>%
  add_trace(x = x1, y = y1, type ='scatter', mode = 'lines', name = "value adjusted", yaxis = 'y1') %>%
  #add_trace(x = x1, y = y3, type ='scatter', mode = 'lines', name = "Baro", yaxis = 'y3') %>%
  add_trace(x = x1, y = y2, type ='scatter', mode = 'lines', name = "Conductivity", yaxis = 'y2') %>%
  
  layout(
    yaxis = list(title = 'value adjusted', side = 'left'),
    yaxis2 = list(title = 'Conductivity', overlaying = 'y', side = 'right', showgrid = FALSE),
    xaxis = list(title = 'Date and Time'),
    title = "UT1-FCT-008-3",
    showlegend = TRUE
  )

fig1



######Trying to do mutliple axises in the code that day and time works

p3 <- plot_ly(wl_sub[as.numeric(datetime_adj1) %% 60 == 0],
              x1 = wl_sub$datetime_adj1,
              y1 = wl_sub$value_adj,
              y2 = wl_sub$CONDUCTIVITY,
              color = wl_sub$well,
              colors = viridis(20),
              name = wl_sub$well,
              add_trace(x = x1, y = y1, type ='scatter', mode = 'lines', name = "value adjusted", yaxis = 'y1') %>%
              add_trace(x = x1, y = y2, type ='scatter', mode = 'lines', name = "Conductivity", yaxis = 'y2') %>%
                layout(
                  title = "UT1-FCT-008-3",
                  xaxis = list(title = "Date and time"),
                  yaxis =list(title ="value_adj", side = 'left'),
                  yaxis2 = list(title = 'Conductivity', overlaying ='y', side = 'right', showgrid = FALSE),
                  showledgend = TRUE
                  ))

p3
###################################################################
#show a subset in the plot
#set 300 entries to 0, this means looking at every 5 min data

p1 <- plot_ly(wl_sub[as.numeric(datetime_adj1) %% 60 == 0],
              x = wl_sub$datetime_adj1,
              y = wl_sub$value_adj,
              color = wl_sub$well,
              colors = viridis(20),
              #name = ~well,
              type = "scatter", mode = "lines")
  # layout(
  #   title = "UT1-FCT-008-3",
  #   xaxis = list(title = "Date and time"),
  #   yaxis =list(title = "value_adj",
  #               side = 'left'))

#call the plot:
p1

p2 <- plot_ly(wl_sub[as.numeric(datetime_adj1) %% 60 == 0],
              x = wl_sub$datetime_adj1,
              y = wl_sub$CONDUCTIVITY,
              color = wl_sub$well,
              colors = viridis(20),
              #name = ~well,
              type = "scatter", mode = "lines")
  # layout(
  #   title = "UT1-FCT-008-3",
  #   xaxis = list(title = "Date and time"),
  #   yaxis =list(title ="Conductivity",
  #               overlaying ='y',
  #               side = 'right',
  #               showgrid = FALSE))

#call the plot:
p2

 p3 <- plot_ly(wl_sub[as.numeric(datetime_adj1) %% 60 == 0],
               x = wl_sub$datetime_adj1,
               y = wl_sub$TEMPERATURE,
               color = wl_sub$well,
               colors = viridis(20),
               #name = ~well,
               type = "scatter", mode = "lines")
#call the plot:
p3

# Plot baro
p_baro <- plot_ly(wl_sub[as.numeric(datetime_adj1) %% 60 == 0],
              x = wl_sub$datetime_adj1,
              y = wl_sub$baro,
              name = "Baro",
              type = "scatter", mode = "line")
  #layout(title = "UT1-FCT-008-3", xaxis =list(title = "Date and time", yaxis = list(title = "baro pressure", side = 'left')))
p_baro
# Show numerious plots in a single view
subplot(p1,p_baro,p2,p3, shareX = TRUE, nrows = 4)%>%
          layout(
            title = "UT1-FCT-008",
            xaxis = list(title = "Date and time",
                         nticks = 10,   # Number of ticks/grid lines
                        # tick0 = ('2024-06-06'), #starting tick position
                         #dtick ="D5", #tick interval D1 = 1 day
                         tickangle = "30", 
                         showgrid = TRUE), # Ensure grid lines are shown
            yaxis = list(title = "Relative change in pressure"),
            yaxis2 = list(title = 'Pressure'),
            yaxis3 = list(title ="Conductivity"),
            yaxis4 = list(title ="Temperature")
          )


##########################################################################

