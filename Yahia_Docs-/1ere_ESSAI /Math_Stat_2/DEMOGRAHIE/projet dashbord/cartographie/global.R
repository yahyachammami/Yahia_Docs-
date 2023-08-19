#### Load the required packages ####


library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(maps) # for USA states map - boundaries used by ggplot for mapping
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating




# Column names without state. This will be used in the selectinput for choices in the shinydashboard
c1 = fdc@data %>% 
  select(-"del_nom") %>% 
  names()

# Column names without del_nom and del. This will be used in the selectinput for choices in the shinydashboard
c2 = fdc@data %>% 
  select(-"del_nom", -"del") %>% 
  names()



# convert state to lower case
my_data1 = fdc@data %>% 
  mutate(del_nom = tolower(del_nom))  # converting the state names from USArrests dataset to lower case so we can later merge the maps data to our dataset


## Add the latitude, longitude and other info needed to draw the ploygon for the state map
# For the state boundaries available - add the USAArrests info.
# Note that Alaska and Hawaii boundaries are not available, those rows will be omitted in the merged data
# right_join from dplyr package
#merged =right_join(my_data1, state_map,  by=c("del_nom" = "region"))

# Add State Abreviations and center locations of each states. Create a dataframe out of it
#st = data.frame(abb = del_nom, stname=tolower(del_nom.name), x=del_nom.center$x, y=del_nom.center$y)

# Join the state abbreviations and center location to the dataset for each of the observations in the merged dataset
# left_join from dplyr package
# there is no abbreviation available for District of Columbia and hence those rows will be dropped in the outcome
#new_join = left_join(merged, st, by=c("del_nom" = "stname"))