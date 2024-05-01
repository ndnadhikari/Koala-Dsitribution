## Loading pacakges -----------------
# profvis({ # this is used to check memory and speed of data
library(readr)
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(vroom)
library(shinyWidgets)
library(renv)
# renv::init()

# library(scales)
# library(tidyr)
# library(plotly)
# library(devtools)
# library(forcats)

# library(devtools)
# library(profvis)
# library(gridExtra)


## Loading data sets
# df_with_states <- readr::read_csv("df_with_state.csv")
df_with_states <- vroom("df_with_state.csv")
df_states      <- sf::read_sf('df_states.shp')


## Basic clean
df_with_states <- df_with_states %>% 
  dplyr::select("id", "date", "lat", "long", "states", "temp", "precip", "geometry")

# update_date <- as.Date(max(latest_status$update.date)) # date of app update
