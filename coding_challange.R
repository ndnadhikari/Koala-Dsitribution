## Installing packages ===============================
# install.packages("galah")                                    ### if required, install from Cran
# install.packages("remotes")                                  
# remotes::install_github("AtlasOfLivingAustralia/galah-R")    ### if required,  install from github
# install.packages("purrr")
# install.packages("raster")

# install.packages("maptools") # If required
# install.packages("ggmap")    # If required
# install.packages("broom")    # If required
# library(maptools)
# library(ggmap)
# library(broom)

## Version history
version

##==== Loading libraries ===============================
library ("galah")
library("dplyr")
library("tidyverse")
library ("tidyr")
library("purrr")
library('sf')
library("raster")
# ??galah   

## Setting working directory
setwd("~/")
setwd("C:/Users/uqnadhik/OneDrive - The University of Queensland/Desktop/QCIF")
        
## Getting data ==========================================
galah_config(email = "n.adhikari@uq.edu.au", atlas = "Australia")  

atlas_counts()    # summary counts of records 

## Group and summaries record counts by specific fields
galah_call() |>
  galah_group_by(kingdom) |>
  atlas_counts()

## Getting occurrences data of Koala (Phascolarctos cinereus) since 2020
df <- galah::galah_call() |>
  galah_identify("Phascolarctos cinereus") |>
  galah_filter(year > 2020) |>
  atlas_occurrences()

# df <-  df[1:2500, ]

names(df)
# [1] "recordID"         "scientificName"   "taxonConceptID"   "decimalLatitude"  "decimalLongitude" "eventDate"       
# [7] "occurrenceStatus" "dataResourceName"

df <- df %>% 
  dplyr::select("recordID", "eventDate", "decimalLatitude", "decimalLongitude") %>% 
  mutate(lat = decimalLatitude) %>% 
  mutate(long = decimalLongitude) %>% 
  rename(id = recordID) %>% 
  rename(date = eventDate)
  
str(df)

df$date <- as.Date(df$date)  ## Tidying up date into short format 

## Loading shape files for map ========================
df_states <- read_sf('Australia/STE_2021_AUST_GDA2020.shp')
names(states)

df_states  <- df_states %>% 
  rename(states = STE_NAME21) %>% 
  rename (code = STE_CODE21) %>% 
  dplyr::select(states, geometry) %>% 
  filter(states %in% c("New South Wales", "Victoria","Queensland", "South Australia", 
                       "Western Australia", "Tasmania", "Northern Territory", 
                       "Australian Capital Territory"))

str(df_states)

# Convert your dataframe to a spatial dataframe
df_sf <- st_as_sf(df, coords = c( "decimalLongitude", "decimalLatitude"), crs = st_crs(df_states))

# Spatially join your dataframe with the Australia shapefile to get state information
df_with_state <- st_join(df_sf, df_states)

unique(df_with_state$states)
# [1] "Queensland"                   "South Australia"              "New South Wales"              "Victoria"                    
# [5] NA                             "Australian Capital Territory" "Northern Territory"           "Western Australia" 
## Removing Na Columns
df_with_state <- df_with_state %>%  filter(states != "NA")
names((df_with_state))
head(df_with_state, 6)

## Can extract lat and long from geometry
# df_with_state$lat  <- st_coordinates(df_with_state$geometry)[, "Y"]
# df_with_state$long <- st_coordinates(df_with_state$geometry)[, "X"]

## Getting climate data ============================
bbox    <- st_bbox(df_states) # Get the bounding box equivalent to our data with states
extent  <- extent(bbox)

# Load world climate data (example with WorldClim data)
bio     <- raster::getData("worldclim", var = "bio", res = 10)

# Crop the climate data to the bounding box
bio_aus <- crop(bio, extent)

## Adding temperature and precipitation column in the main data set
## Bio 1 to 11 are temperature variables while 12-19 are precipitation according to Worldclim
## https://www.worldclim.org/data/bioclim.html
df_with_state$temp <- extract(bio_aus$bio5, df_with_state[, c("long", "lat")])
# This is WorldClim version 2.1 climate data for 1970-2000. This version was released in January 2020.
df_with_state$precip <- extract(bio_aus$bio12, df_with_state[, c("long", "lat")])

str(df_with_state)


## Visualisation ====================================

## Showing the distribution of Koala by states
# Plot
p1 <- ggplot(df_with_state, aes(x = states)) +
  geom_bar(aes(y = (..count..)/sum(..count..) * 100), fill = "dodgerblue") +
  labs(x = "States", y = "Percentage of Count (%)", 
       title = "Distribution Koala Occurrences Across Various States from 2020 to 2024") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("plots/distribution of Kolah by states.png", plot = p1, width = 10, height = 6, units = "in", dpi = 300)

## Showing the distribution of temperature by states
# Plot
p2 <- ggplot(df_with_state, aes(x = states, y = temp)) +
  geom_boxplot() +
  labs(x = "States", y = "Temperature", title = "Distribution of Temperature by States") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("plots/distribution of temp by states.png", plot = p2, width = 10, height = 6, units = "in", dpi = 300)

# Plot
p3 <- ggplot(df_with_state, aes(x = temp)) +
  geom_histogram(bins = 10, fill = "dodgerblue") +
  facet_wrap(~ states, scales = "free_y") +
  labs(x = "Temperature", y = "Count", title = "Histogram of Temperature Facet by States")
# ggsave("plots/distribution of temp by states2.png", plot = p3 , width = 10, height = 6, units = "in", dpi = 300)

## Showing the distribution of precipitation by states
p4 <- ggplot(df_with_state, aes(x = states, y = precip)) +
  geom_boxplot() +
  labs(x = "States", y = "Precipitation", title = "Distribution of Precipitation by States") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("plots/distribution of precip by states.png", plot = p4 , width = 10, height = 6, units = "in", dpi = 300)

# Plot
p5 <-ggplot(df_with_state, aes(x = precip)) +
  geom_histogram(bins = 10, fill = "dodgerblue") +
  facet_wrap(~ states, scales = "free_y") +
  labs(x = "Precipitation", y = "Count", title = "Histogram of Precipitation Facet by States")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("plots/distribution of precip by states2.png", plot = p5 , width = 10, height = 6, units = "in", dpi = 300)

## Distribution plot of Koala temp in map visualisation
p6 <- ggplot() +
  geom_sf(data = df_states %>% dplyr::select("states","geometry"),  fill = NA) +  # Assuming Australia is an sf object
  geom_point(data = df_with_state, aes(x = long, y = lat, color = temp), size = 1) +
  theme_minimal() +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), 
           ylim = c(bbox$ymin, bbox$ymax)) +  # Set the extent of the map
  theme(axis.title = element_blank(),         # Remove axis titles
        axis.text = element_blank(),          # Remove axis labels
        panel.grid = element_blank()) +       # Remove grid lines 
  labs(color = "Temperature")    +            # Changing legend title
  scale_color_gradient(low = "blue", high = "red")
# ggsave("plots/distribution Koala map.png", plot = p6 , width = 10, height = 6, units = "in", dpi = 300)

## Plot with states
p7 <- ggplot() +
  geom_sf(data = states, fill = NA) + 
  geom_point(data = df_with_state, aes(x = long, y = lat, color = states), size = 1) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), 
           ylim = c(bbox$ymin, bbox$ymax)) +  # Set the extent of the map
  theme_minimal() +
  labs(color = "States") +
  theme(axis.title = element_blank(),   # Remove axis titles
        axis.text = element_blank(),    # Remove axis labels
        panel.grid = element_blank())   # Remove grid lines
# ggsave("plots/distribution Koala map by states.png", plot = p7 , width = 10, height = 6, units = "in", dpi = 300)


# Saving files ======================================
# write.csv(df_with_state %>% dplyr::select(-geometry), "data/df_with_state.csv")
# st_write(df_states, "data/df_states.shp")

