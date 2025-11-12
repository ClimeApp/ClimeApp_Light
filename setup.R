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
      setwd = "C:/Users/Niklaus Emanuel/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp",
      lib_path = "C:/Users/Niklaus Emanuel/OneDrive/1_Universit\u00E4t/4_PhD/10_R with R/Shiny R/ClimeApp_all/ClimeApp/library"
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
#Packages
library(shiny, lib.loc = lib_path)
library(ncdf4, lib.loc = lib_path)
library(shinyWidgets, lib.loc = lib_path)
library(RColorBrewer, lib.loc = lib_path)
library(shinyjs, lib.loc = lib_path)
library(bslib, lib.loc = lib_path)
library(readxl, lib.loc = lib_path)
library(DT, lib.loc = lib_path)
library(zoo, lib.loc = lib_path)
library(colourpicker, lib.loc = lib_path)
library(ggplot2, lib.loc = lib_path)
library(sf, lib.loc = lib_path)
library(shinycssloaders, lib.loc = lib_path)
library(openxlsx, lib.loc = lib_path)  # Don't change order!
library(leaflet, lib.loc = lib_path)
library(htmltools, lib.loc = lib_path)
library(dplyr, lib.loc = lib_path)

library(terra, lib.loc = lib_path)
library(tidyterra, lib.loc = lib_path)

library(ggpattern, lib.loc = lib_path)
library(viridisLite, lib.loc = lib_path)
library(viridis, lib.loc = lib_path)
library(markdown, lib.loc = lib_path)
library(tmaptools, lib.loc = lib_path)
library(ggtext, lib.loc = lib_path)
library(dplR, lib.loc = lib_path)
library(burnr, lib.loc = lib_path)
library(shinyjqui, lib.loc = lib_path)
library(ggrepel, lib.loc = lib_path)

library(memoise, lib.loc = lib_path) # Memoization utilities (avoid recomputing heavy steps)
library(qs, lib.loc = lib_path)

# library(plotly, lib.loc = lib_path)
# library(xlsx, lib.loc = lib_path)
# library(shinylogs, lib.loc = lib_path)
# library(maps, lib.loc = lib_path)
# library(mapdata, lib.loc = lib_path)
# library(rnaturalearth, lib.loc = lib_path)
# library(rnaturalearthdata, lib.loc = lib_path)

#### Design ----

# Source for images
addResourcePath(prefix = 'pics', directoryPath = "www")

# Choosing theme and making colouring changes
my_theme <- bs_theme(version = 5, bootswatch = "united", primary = "#094030")

# Colour palette and variable names for ModE-RA source leaflet
type_list <- c("bivalve_proxy", "coral_proxy", "documentary_proxy", "glacier_ice_proxy", "ice_proxy", "instrumental_data", "lake_sediment_proxy", "other_proxy", "speleothem_proxy", "tree_proxy")
type_names <-c("Bivalve", "Coral", "Documentary", "Glacier ice", "Ice", "Instrumental", "Lake sediment", "Other", "Speleothem", "Tree")
named_types <- setNames(type_names, type_list)
# Create a Factor Palette with Paul Tol's Muted Colour List for Colour Blind People
pal_type <- colorFactor(c('#AA4499', '#CC6677', '#44AA99', '#332288', '#5dbae8', '#882255', '#d0b943', '#000000', '#717126', '#117733'), type_list)

variable_list <- c("sea_level_pressure", "no_of_rainy_days", "pressure", "precipitation", "temperature", "historical_proxy", "natural_proxy")
variable_names <- c("Sea level pressure", "No. of rainy days", "Pressure", "Precipitation", "Temperature", "Historical proxy", "Natural proxy")
named_variables <- setNames(variable_names, variable_list)

# Spinner configurations
spinner_image = "pics/ClimeApp_Loading_V3.gif"
spinner_width = 310
spinner_height = 200

# #### Preprocseeing ----
# 
# # Load pre-processed data
# annual_temp_nc = nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_year.nc")
# DJF_temp_nc = nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_djf.nc")
# MAM_temp_nc = nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_mam.nc")
# JJA_temp_nc = nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_jja.nc")
# SON_temp_nc = nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_son.nc")
# 
# annual_prec_nc = nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_year.nc")
# DJF_prec_nc = nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_djf.nc")
# MAM_prec_nc = nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_mam.nc")
# JJA_prec_nc = nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_jja.nc")
# SON_prec_nc = nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_son.nc")
# 
# annual_slp_nc = nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_year.nc")
# DJF_slp_nc = nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_djf.nc")
# MAM_slp_nc = nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_mam.nc")
# JJA_slp_nc = nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_jja.nc")
# SON_slp_nc = nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_son.nc")
# 
# ## Create dataframe of preprocessed yearly variables
# ## - pp_data[[season]][[variable]] where
# ##   season IDs: 1=DJF, 2=MAM, 3=JJA, 4=SON, 5=annual)
# ##   variable IDs: 1=temp, 2=prec, 3=SLP, 4=Z500)
# 
# pp_data = list(vector("list", 4),vector("list", 4),vector("list", 4),vector("list", 4),vector("list", 4))
# 
# pp_data[[5]][[1]] = ncvar_get(annual_temp_nc,varid="temp2")-273.15
# pp_data[[5]][[2]] = ncvar_get(annual_prec_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month
# pp_data[[5]][[3]] = ncvar_get(annual_slp_nc,varid="slp")/100
# 
# pp_data[[1]][[1]] = ncvar_get(DJF_temp_nc,varid="temp2")-273.15
# pp_data[[1]][[2]] = ncvar_get(DJF_prec_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month
# pp_data[[1]][[3]] = ncvar_get(DJF_slp_nc,varid="slp")/100
# 
# pp_data[[2]][[1]] = ncvar_get(MAM_temp_nc,varid="temp2")-273.15
# pp_data[[2]][[2]] = ncvar_get(MAM_prec_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month
# pp_data[[2]][[3]] = ncvar_get(MAM_slp_nc,varid="slp")/100
# 
# pp_data[[3]][[1]] = ncvar_get(JJA_temp_nc,varid="temp2")-273.15
# pp_data[[3]][[2]] = ncvar_get(JJA_prec_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month
# pp_data[[3]][[3]] = ncvar_get(JJA_slp_nc,varid="slp")/100
# 
# pp_data[[4]][[1]] = ncvar_get(SON_temp_nc,varid="temp2")-273.15
# pp_data[[4]][[2]] = ncvar_get(SON_prec_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month
# pp_data[[4]][[3]] = ncvar_get(SON_slp_nc,varid="slp")/100
# 
# # Extract list of longitudes/latitudes
# lon = annual_temp_nc$dim[[3]]$vals
# lat = annual_temp_nc$dim[[4]]$vals
# 
# ## Close pre-processed netCDF files
# nc_close(annual_temp_nc)
# nc_close(DJF_temp_nc)
# nc_close(MAM_temp_nc)
# nc_close(JJA_temp_nc)
# nc_close(SON_temp_nc)
# 
# nc_close(annual_prec_nc)
# nc_close(DJF_prec_nc)
# nc_close(MAM_prec_nc)
# nc_close(JJA_prec_nc)
# nc_close(SON_prec_nc)
# 
# nc_close(annual_slp_nc)
# nc_close(DJF_slp_nc)
# nc_close(MAM_slp_nc)
# nc_close(JJA_slp_nc)
# nc_close(SON_slp_nc)




# ---- Cache für pp_data ----
dir.create("cache", showWarnings = FALSE)

build_pp_cache <- function() {

  # Open pre-processed data
  annual_temp_nc <- nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_year.nc")
  DJF_temp_nc    <- nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_djf.nc")
  MAM_temp_nc    <- nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_mam.nc")
  JJA_temp_nc    <- nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_jja.nc")
  SON_temp_nc    <- nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_temp2_abs_1421-2008_son.nc")

  annual_prec_nc <- nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_year.nc")
  DJF_prec_nc    <- nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_djf.nc")
  MAM_prec_nc    <- nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_mam.nc")
  JJA_prec_nc    <- nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_jja.nc")
  SON_prec_nc    <- nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_totprec_abs_1421-2008_son.nc")

  annual_slp_nc  <- nc_open("data/ModE-RA/year/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_year.nc")
  DJF_slp_nc     <- nc_open("data/ModE-RA/djf/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_djf.nc")
  MAM_slp_nc     <- nc_open("data/ModE-RA/mam/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_mam.nc")
  JJA_slp_nc     <- nc_open("data/ModE-RA/jja/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_jja.nc")
  SON_slp_nc     <- nc_open("data/ModE-RA/son/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_slp_abs_1421-2008_son.nc")

  ## Create dataframe of preprocessed yearly variables
  ## - pp_data[[season]][[variable]] where
  ##   season IDs: 1=DJF, 2=MAM, 3=JJA, 4=SON, 5=annual)
  ##   variable IDs: 1=temp, 2=prec, 3=SLP, 4=Z500)
  pp_data <- vector("list", 5)
  for (i in 1:5) pp_data[[i]] <- vector("list", 4)

  pp_data[[5]][[1]] <- ncvar_get(annual_temp_nc, "temp2") - 273.15
  pp_data[[5]][[2]] <- ncvar_get(annual_prec_nc, "totprec") * 2629756.8
  pp_data[[5]][[3]] <- ncvar_get(annual_slp_nc,  "slp")/100

  pp_data[[1]][[1]] <- ncvar_get(DJF_temp_nc, "temp2") - 273.15
  pp_data[[1]][[2]] <- ncvar_get(DJF_prec_nc, "totprec") * 2629756.8
  pp_data[[1]][[3]] <- ncvar_get(DJF_slp_nc,  "slp")/100

  pp_data[[2]][[1]] <- ncvar_get(MAM_temp_nc, "temp2") - 273.15
  pp_data[[2]][[2]] <- ncvar_get(MAM_prec_nc, "totprec") * 2629756.8
  pp_data[[2]][[3]] <- ncvar_get(MAM_slp_nc,  "slp")/100

  pp_data[[3]][[1]] <- ncvar_get(JJA_temp_nc, "temp2") - 273.15
  pp_data[[3]][[2]] <- ncvar_get(JJA_prec_nc, "totprec") * 2629756.8
  pp_data[[3]][[3]] <- ncvar_get(JJA_slp_nc,  "slp")/100

  pp_data[[4]][[1]] <- ncvar_get(SON_temp_nc, "temp2") - 273.15
  pp_data[[4]][[2]] <- ncvar_get(SON_prec_nc, "totprec") * 2629756.8
  pp_data[[4]][[3]] <- ncvar_get(SON_slp_nc,  "slp")/100


  # Extract list of longitudes/latitudes
  lon <- annual_temp_nc$dim[[3]]$vals
  lat <- annual_temp_nc$dim[[4]]$vals

  # Close pre-processed netCDF files
  lapply(list(annual_temp_nc,DJF_temp_nc,MAM_temp_nc,JJA_temp_nc,SON_temp_nc,
              annual_prec_nc,DJF_prec_nc,MAM_prec_nc,JJA_prec_nc,SON_prec_nc,
              annual_slp_nc,DJF_slp_nc,MAM_slp_nc,JJA_slp_nc,SON_slp_nc), nc_close)

  # Serializer
  if (!requireNamespace("qs", quietly = TRUE))
    install.packages("qs", lib = lib_path)
  qs::qsave(list(pp_data = pp_data, lon = lon, lat = lat),
            "cache/pp_data_v1.qs")
}

# On startup: load cache (or build it once)
if (file.exists("cache/pp_data_v1.qs")) {
  obj <- qs::qread("cache/pp_data_v1.qs")
  pp_data <- obj$pp_data
  lon <- obj$lon
  lat <- obj$lat
} else {
  build_pp_cache()
  obj <- qs::qread("cache/pp_data_v1.qs")
  pp_data <- obj$pp_data
  lon <- obj$lon
  lat <- obj$lat
}

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

# Load shapefiles for maps (rnaturalearth)
# coast <- st_read("data/geodata_maps/coast.shp")
# countries <- st_read("data/geodata_maps/countries.shp")
# oceans <- st_read("data/geodata_maps/oceans.shp")
# land <- st_read("data/geodata_maps/land.shp")

coast <- readRDS("data/geodata_maps/coast.rds")
countries <- readRDS("data/geodata_maps/countries.rds")
oceans <- readRDS("data/geodata_maps/oceans.rds")
land <- readRDS("data/geodata_maps/land.rds")

# Load RDS Files for map customization (rnaturalearth)
lakes <- readRDS("data/geodata_custom_maps/lakes.rds")
mountains <- readRDS("data/geodata_custom_maps/mountains.rds")
rivers <- readRDS("data/geodata_custom_maps/rivers.rds")

