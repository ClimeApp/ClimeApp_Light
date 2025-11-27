#### Setup script for ClimeApp ####
# Sets up user environment and loads required libraries as well as preprocessed data

#### User Preparation ----

Sys.info()[["user"]]

## Working Directory
# Define a function to set up user configurations
setup_user_environment <- function() {
  
  # Define user-specific settings
  user_configs <- list(
    
    # Nik
    nbartlome = list(
      setwd = "C:/Users/nbartlome/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp",
      lib_path = "C:/Users/nbartlome/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp/library"
    ),
    
    nikla = list(
      setwd = "C:/Users/nikla/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp",
      lib_path = "C:/Users/nikla/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp/library"
    ),
    
    "Niklaus Emanuel" = list(
      setwd = "H:/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp",
      lib_path = "H:/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp/library"
    ),
    
    # Richard
    Richard = list(
      setwd = "C:/Users/Richard/OneDrive/ClimeApp_all/ClimeApp",
      lib_path = "library"
    ),
    rw22z389 = list(
      setwd = "C:/Users/rw22z389/OneDrive/ClimeApp_all/ClimeApp",
      lib_path = "C:/Users/rw22z389/OneDrive/ClimeApp_all/ClimeApp/library"
    ),
    
    # Noémie
    noemi = list(
      setwd = "C:/Users/noemi/OneDrive/ClimeApp_all-noemie-hp/ClimeApp/",
      lib_path = "C:/Users/noemi/OneDrive/ClimeApp_all/ClimeApp/library"
    ),
    nw22d367 = list(
      setwd = "C:/Users/nw22d367/OneDrive/ClimeApp_all/ClimeApp/",
      lib_path = "C:/Users/nw22d367/OneDrive/ClimeApp_all/ClimeApp/library"
    ),
    
    # Tanja
    falasca = list(
      setwd = "C:/Users/falasca/OneDrive/ClimeApp_all/ClimeApp/",
      lib_path = "C:/Users/falasca/OneDrive/ClimeApp_all/ClimeApp/library"
    ),
    tanja = list(
      setwd = "C:/Users/tanja/OneDrive/ClimeApp_all/ClimeApp/",
      lib_path = "C:/Users/tanja/OneDrive/ClimeApp_all/ClimeApp/library"
    )
  )
  # Get the current username
  current_user <- Sys.info()[["user"]]
  if (!is.null(user_configs[[current_user]])) {
    user_config <- user_configs[[current_user]]
    setwd(user_config$setwd)
    .libPaths(user_config$lib_path)
    assign("lib_path", user_config$lib_path, envir = .GlobalEnv)
    message("Working directory and library path set for user: ", current_user)
  } else {
    stop("User configuration not found. Please check the user setup.")
  }
}
setup_user_environment()

#### Packages ----

# Server Path
# .libPaths("/home/ClimeApp/R-packages")
# lib_path <- "/home/ClimeApp/R-packages"

# Load only REQUIRED functions/libraries:

  # Load only REQUIRED functions/libraries:
  #Packages
  
  library(shiny, lib.loc = lib_path)
  library(ggplot2, lib.loc = lib_path)
  library(htmltools, lib.loc = lib_path)

  # library(terra, lib.loc = lib_path)
  # library(shinyWidgets, lib.loc = lib_path)
  # library(RColorBrewer, lib.loc = lib_path)
  # library(shinyjs, lib.loc = lib_path)
  # library(bslib, lib.loc = lib_path)
  # library(readxl, lib.loc = lib_path)
  # library(DT, lib.loc = lib_path)
  # library(colourpicker, lib.loc = lib_path)
  # library(sf, lib.loc = lib_path)
  # library(shinylogs, lib.loc = lib_path)
  # library(shinycssloaders, lib.loc = lib_path)
  # library(openxlsx, lib.loc = lib_path)  # Don't change order!
  # library(dplyr, lib.loc = lib_path)
  # library(markdown, lib.loc = lib_path)
  # library(dplR, lib.loc = lib_path)
  # library(burnr, lib.loc = lib_path)
  # library(shinyjqui, lib.loc = lib_path)
  # library(ncdf4, lib.loc = lib_path)
  # library(maps, lib.loc = lib_path)
  # library(mapdata, lib.loc = lib_path)
  # library(xlsx, lib.loc = lib_path)
  # library(tmaptools, lib.loc = lib_path)
  # library(ggpattern, lib.loc = lib_path)
  # library(ggtext, lib.loc = lib_path)
  # library(ggrepel, lib.loc = lib_path)
  # library(rnaturalearth, lib.loc = lib_path)
  # library(rnaturalearthdata, lib.loc = lib_path)
  # library(viridisLite, lib.loc = lib_path)
  # library(viridis, lib.loc = lib_path)
  # library(leaflet, lib.loc = lib_path)
  # library(leaflet.providers, lib.loc = lib_path)
  # library(zoo, lib.loc = lib_path)
  # library(tidyterra, lib.loc = lib_path)

#### Design ----

# Source for images
addResourcePath(prefix = 'pics', directoryPath = "www")
addResourcePath(prefix = 'videos', directoryPath = "videos")

# Choosing theme and making colouring changes
#my_theme <- bslib::bs_theme(version = 5, bootswatch = "united", primary = "#094030")

# Colour palette and variable names for ModE-RA source leaflet
type_list <- c("bivalve_proxy", "coral_proxy", "documentary_proxy", "glacier_ice_proxy", "ice_proxy", "instrumental_data", "lake_sediment_proxy", "other_proxy", "speleothem_proxy", "tree_proxy")
type_names <-c("Bivalve", "Coral", "Documentary", "Glacier ice", "Ice", "Instrumental", "Lake sediment", "Other", "Speleothem", "Tree")
named_types <- setNames(type_names, type_list)
# Create a Factor Palette with Paul Tol's Muted Colour List for Colour Blind People
pal_type <- leaflet::colorFactor(c('#AA4499', '#CC6677', '#44AA99', '#332288', '#5dbae8', '#882255', '#d0b943', '#000000', '#717126', '#117733'), type_list)

variable_list <- c("sea_level_pressure", "no_of_rainy_days", "pressure", "precipitation", "temperature", "historical_proxy", "natural_proxy")
variable_names <- c("Sea level pressure", "No. of rainy days", "Pressure", "Precipitation", "Temperature", "Historical proxy", "Natural proxy")
named_variables <- setNames(variable_names, variable_list)

# Spinner configurations
spinner_image = "pics/ClimeApp_Loading_V3.gif"
spinner_width = 310
spinner_height = 200

#### Preprocseeing ----
# Load pre-processed data
# annual_temp_nc = ncdf4::nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_year.nc")
# DJF_temp_nc = ncdf4::nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_djf.nc")
# MAM_temp_nc = ncdf4::nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_mam.nc")
# JJA_temp_nc = ncdf4::nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_jja.nc")
# SON_temp_nc = ncdf4::nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_son.nc")
# 
# annual_prec_nc = ncdf4::nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_year.nc")
# DJF_prec_nc = ncdf4::nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_djf.nc")
# MAM_prec_nc = ncdf4::nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_mam.nc")
# JJA_prec_nc = ncdf4::nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_jja.nc")
# SON_prec_nc = ncdf4::nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_son.nc")

## Create dataframe of preprocessed yearly variables
## - pp_data[[season]][[variable]] where
##   season IDs: 1=DJF, 2=MAM, 3=JJA, 4=SON, 5=annual)
##   variable IDs: 1=temp, 2=prec, 3=SLP, 4=Z500)

grid_nc <- ncdf4::nc_open(
  "data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_year.nc"
)

lon <- grid_nc$dim[[3]]$vals  # longitudes
lat <- grid_nc$dim[[4]]$vals  # latitudes

ncdf4::nc_close(grid_nc)


## Create dataframe of continent lon/lats and Set initial latlon values
Europe = c(-30,40,30,75) 
Asia = c(25,170,5,80)
Australasia = c(90,180,-55,20)
Africa = c(-25,55,-40,40)
N_America = c(-175,-10,5,85)
S_America = c(-90,-30,-60,15)

continent_lonlat_values = data.frame(Europe,Asia,Australasia,Africa,N_America,S_America)
row.names(continent_lonlat_values) = c("lon_min","lon_max","lat_min","lat_max")

random_map = sample(1:6,1)

initial_lon_values = continent_lonlat_values[1:2,random_map]
initial_lat_values = continent_lonlat_values[3:4,random_map]

## Create dataframe of centuries and set initial year values
C15 = c(1422,1500)
C16 = c(1500,1600)
C17 = c(1600,1700)
C18 = c(1700,1800)
C19 = c(1800,1900)
C20 = c(1900,2000)

century_years = data.frame(C15,C16,C17,C18,C19,C20)
row.names(century_years) = c("year_min","year_max")

random_century = sample(1:6,1)

initial_year_values = century_years[,random_century]


## Load grid square weights for calculating means
latlon_weights = as.matrix(read.csv("data/latlon_weights.csv"))

coast <- readRDS("data/geodata_maps/coast.rds")
countries <- readRDS("data/geodata_maps/countries.rds")
oceans <- readRDS("data/geodata_maps/oceans.rds")
land <- readRDS("data/geodata_maps/land.rds")

# Load RDS Files for map customization (rnaturalearth)
lakes <- readRDS("data/geodata_custom_maps/lakes.rds")
mountains <- readRDS("data/geodata_custom_maps/mountains.rds")
rivers <- readRDS("data/geodata_custom_maps/rivers.rds")


