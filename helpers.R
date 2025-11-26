# Helper Functions of ClimeApp 
#### Caching helpers for maps ####
# Use ragg for crisp text/lines in Shiny plots
options(shiny.useragg = TRUE)

# In-memory GeoTIFF builder cached by its arguments
.create_geotiff_mem <- memoise::memoise(function(md, lonlat) {
  create_geotiff(map_data = md)
})

# Read and normalize a vector layer once and reuse it.
.read_shape_mem <- memoise::memoise(function(path) {
  sh <- sf::st_read(path, quiet = TRUE)
  if (is.na(sf::st_crs(sh))) sh <- sf::st_set_crs(sh, 4326) else sh <- sf::st_transform(sh, 4326)
  sh
})

# Serialize overlay data (points/highlights) into a compact, stable key
overlay_key <- function(df) {
  if (is.null(df) || !NROW(df)) return("")
  lon <- df[[ if ("lon" %in% names(df)) "lon" else 1 ]]
  lat <- df[[ if ("lat" %in% names(df)) "lat" else 2 ]]
  # sort for stable ordering, round to reduce noise
  ord <- order(lon, lat)
  paste(paste(round(lon[ord], 3), round(lat[ord], 3), sep=","), collapse=";")
}

# Logos and pictures
current_month_day <- format(Sys.Date(), "%m-%d")

logo_src   <- 'pics/Logo_ClimeApp_Weiss_Font1.png'
logo_id    <- "ClimeApp"
logo_width <- "110px"

if (current_month_day >= "01-01" && current_month_day <= "01-03") {
  logo_src <- 'pics/Clim-year.png'
  logo_id  <- "Clim-year"
  logo_width <- "142px"
} else if (current_month_day >= "03-22" && current_month_day <= "04-09") {
  logo_src <- 'pics/Clim-ster.png'
  logo_id  <- "Clim-ster"
  logo_width <- "142px"
} else if (current_month_day == "04-10") {
  logo_src <- 'pics/Clim-vol.png'
  logo_id  <- "Clim-vol"
  logo_width <- "142px"
} else if (current_month_day == "05-02") {
  logo_src <- 'pics/Clim-bledore.png'
  logo_id  <- "Clim-bledore"
  logo_width <- "142px"
} else if (current_month_day == "05-04") {
  logo_src <- 'pics/Clim-wars.png'
  logo_id  <- "Clim-wars"
  logo_width <- "142px"
} else if (current_month_day == "05-15") {
  logo_src <- 'pics/Clim-day.png'
  logo_id  <- "Clim-day"
  logo_width <- "142px"
} else if (current_month_day == "09-22") {
  logo_src <- 'pics/Clim-lord.png'
  logo_id  <- "Clim-lord"
  logo_width <- "142px"
} else if (current_month_day >= "10-15" && current_month_day <= "11-02") {
  logo_src <- 'pics/Clim-ween.png'
  logo_id  <- "Clim-ween"
  logo_width <- "142px"
} else if (current_month_day >= "12-01" && current_month_day <= "12-31") {
  logo_src <- 'pics/Clim-mas.png'
  logo_id  <- "Clim-mas"
  logo_width <- "142px"
}

#### Internal Functions ####
# (Functions used ONLY by other functions)

#' GENERATE TITLE MONTHS
#'
#' This function generates a title string based on a specified month range.
#' It returns "Annual" if the full year is selected (January to December),
#' a concatenated string of month initials for a partial range, or "Invalid" if the input is not valid.
#'
#' @param MR Numeric vector. A month range, typically of the form c(start_month, end_month), where months are 1–12 (January–December). 
#'
#' @return A character string. "Annual" for a full-year selection, a string of month initials (e.g., "JFMAMJ") for partial ranges, or "Invalid" for invalid input.

generate_title_months = function(MR){
  if (!is.null(MR) && length(MR) >= 2 && !any(is.na(MR)) && MR[1] == 1 && MR[2] == 12){
    title_months = "Annual"
  } else if (!is.null(MR) && length(MR) >= 2 && !any(is.na(MR))) {
    month_letters  = c("D","J","F","M","A","M","J","J","A","S","O","N","D")
    title_months = paste(month_letters[(MR[1]:MR[2])+1], collapse = "")
  } else {
    title_months = "Invalid"
  }
  return(title_months)
}


#### General Functions ####

## Projections
laea_proj = paste0("+proj=laea +lat_0=", 0, " +lon_0=", 0, " +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

#' Generate Orthographic Projection String
#'
#' Creates a PROJ.4-compatible string for an orthographic map projection centered on a given latitude and longitude.
#' Useful for setting up spatial projections in mapping packages (e.g., `sf`, `rgdal`, `terra`).
#'
#' @param center_lat Numeric. Central latitude of the projection (in degrees).
#' @param center_lon Numeric. Central longitude of the projection (in degrees).
#'
#' @return Character string. A complete PROJ.4 string for an orthographic projection centered at the specified location.

ortho_proj <- function(center_lat, center_lon) {
  paste0(
    "+proj=ortho ",
    "+lat_0=", center_lat, " ",
    "+lon_0=", center_lon, " ",
    "+x_0=4321000 ",
    "+y_0=3210000 ",
    "+ellps=GRS80 ",
    "+units=m ",
    "+no_defs"
  )
}


#' Transform Coordinates in a Data Frame
#'
#' Transforms point coordinates from one projection to another using PROJ.4 strings.
#' Supports backward compatibility for named projections like `"Robinson"` or `"Orthographic"`.
#' Used for reprojecting coordinate columns in a data frame.
#'
#' @param df Data frame. Input data containing point coordinates.
#' @param xcol Character. Name of the column containing x (longitude) values.
#' @param ycol Character. Name of the column containing y (latitude) values.
#' @param projection_from Character. PROJ.4 string of the source projection (default is WGS84).
#' @param projection_to Character. PROJ.4 string of the target projection (if known).
#' @param projection Character. Optional named projection (`"Robinson"`, `"Orthographic"`, `"LAEA"`) for compatibility.
#' @param center_lat Numeric. Central latitude for `"Orthographic"` projection (if used).
#' @param center_lon Numeric. Central longitude for `"Orthographic"` projection (if used).
#'
#' @return Data frame. Same as input `df`, but with transformed coordinates in the `xcol` and `ycol` columns.

transform_points_df <- function(df,
                                xcol,
                                ycol,
                                projection_from = "+proj=longlat +datum=WGS84",
                                projection_to = NULL,
                                projection = NULL,  # for backward compatibility
                                center_lat = 0,
                                center_lon = 0) {
  # Backward compatibility: if 'projection' is given and 'projection_to' is NULL
  if (!is.null(projection) && is.null(projection_to)) {
    if (projection == "UTM (default)")
      return(df)
    
    projection_from <- "+proj=longlat +datum=WGS84"
    projection_to <- switch(
      projection,
      "Robinson" = "+proj=robin",
      "Orthographic" = ortho_proj(center_lat, center_lon),
      "LAEA" = laea_proj
    )
  }
  
  # Convert input to sf object using source CRS
  sf_obj <- sf::st_as_sf(df, coords = c(xcol, ycol), crs = projection_from)
  
  # Transform to target CRS
  sf_trans <- sf::st_transform(sf_obj, crs = projection_to)
  coords <- sf::st_coordinates(sf_trans)
  
  # Overwrite x/y columns
  df[[xcol]] <- coords[, 1]
  df[[ycol]] <- coords[, 2]
  return(df)
}


#' Transform Bounding Box Coordinates in a Data Frame
#'
#' Transforms rectangular box coordinates (x1/y1 to x2/y2) between spatial projections.
#' Supports named projections like `"Robinson"` and `"Orthographic"` for compatibility.
#' Applies corner-wise transformation and updates each box's extent.
#'
#' @param df Data frame. Input containing bounding box coordinates.
#' @param x1col Character. Name of column for left x-coordinate (default `"x1"`).
#' @param x2col Character. Name of column for right x-coordinate (default `"x2"`).
#' @param y1col Character. Name of column for bottom y-coordinate (default `"y1"`).
#' @param y2col Character. Name of column for top y-coordinate (default `"y2"`).
#' @param projection_from Character. PROJ.4 string of source projection (default is WGS84).
#' @param projection_to Character. PROJ.4 string of target projection.
#' @param projection Character. Optional named projection (`"Robinson"`, `"Orthographic"`, `"LAEA"`).
#' @param center_lat Numeric. Central latitude for `"Orthographic"` projection (if used).
#' @param center_lon Numeric. Central longitude for `"Orthographic"` projection (if used).
#'
#' @return Data frame. Same as input `df`, but with transformed box coordinates in `x1`, `x2`, `y1`, and `y2`.

transform_box_df <- function(df,
                             x1col = "x1",
                             x2col = "x2",
                             y1col = "y1",
                             y2col = "y2",
                             projection_from = "+proj=longlat +datum=WGS84",
                             projection_to = NULL,
                             projection = NULL,
                             center_lat = 0,
                             center_lon = 0) {
  if (!is.null(projection) && is.null(projection_to)) {
    if (projection == "UTM (default)")
      return(df)
    
    projection_from <- "+proj=longlat +datum=WGS84"
    projection_to <- switch(
      projection,
      "Robinson" = "+proj=robin",
      "Orthographic" = ortho_proj(center_lat, center_lon),
      "LAEA" = laea_proj
    )
  }
  
  # Construct a point matrix from each corner of the boxes
  box_coords <- data.frame(
    x = c(df[[x1col]], df[[x1col]], df[[x2col]], df[[x2col]]),
    y = c(df[[y1col]], df[[y2col]], df[[y1col]], df[[y2col]]),
    id = rep(seq_len(nrow(df)), 4)
  )
  
  # Convert to sf and transform
  sf_obj <- sf::st_as_sf(box_coords, coords = c("x", "y"), crs = projection_from)
  sf_trans <- sf::st_transform(sf_obj, crs = projection_to)
  coords <- sf::st_coordinates(sf_trans)
  
  # Extract transformed corners back into each row
  for (i in seq_len(nrow(df))) {
    idx <- which(box_coords$id == i)
    x_vals <- coords[idx, 1]
    y_vals <- coords[idx, 2]
    df[[x1col]][i] <- min(x_vals)
    df[[x2col]][i] <- max(x_vals)
    df[[y1col]][i] <- min(y_vals)
    df[[y2col]][i] <- max(y_vals)
  }
  
  return(df)
}


#' (General) CREATE MONTH RANGE
#'
#' This function creates a numeric vector representing the minimum and maximum month indices
#' based on a character vector of month names. It is typically used to derive a month range 
#' for further filtering or plotting.
#'
#' @param month_names_vector Character vector. A vector of month names selected from the predefined list:
#' "December (prev.)", "January", "February", "March", "April", "May", "June",
#' "July", "August", "September", "October", "November", "December".
#'
#' @return A numeric vector of length 2 indicating the start and end months, where
#' January = 1, February = 2, ..., December = 12, and "December (prev.)" = 0.

create_month_range = function(month_names_vector){
  month_names_list = c("December (prev.)", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  month_range_full = match(month_names_vector,month_names_list)-1
  month_range = c(month_range_full[1],tail(month_range_full,n=1))
  
  return(month_range)
}


#' (General)CREATE SUBSET LONGITUDE IDs
#'
#' This function creates a subset of longitude indices (`IDs`) for use in plotting, 
#' tables, or reading data. It extends the selected longitude range by one grid point 
#' (±2.8125°) on either end to allow for smooth cutting and interpolation.
#'
#' @param lon_range Numeric vector of length 2. The minimum and maximum longitude values.
#'
#' @return A numeric vector of indices corresponding to the selected longitudes,
#' including one grid point outside the specified range on both sides.

create_subset_lon_IDs = function(lon_range){
  subset_lon_IDs = which((lon >= lon_range[1]-2.8125) & (lon <= lon_range[2]+2.8125))
  return(subset_lon_IDs)
}


#' (General) CREATE SUBSET LATITUDE IDs 
#'
#' This function creates a subset of latitude indices (`IDs`) for use in plotting, 
#' tables, or reading data. It extends the selected latitude range by one grid point 
#' (±2.774456°) on either end to allow for smooth cutting and interpolation.
#'
#' @param lat_range Numeric vector of length 2. The minimum and maximum latitude values.
#'
#' @return A numeric vector of indices corresponding to the selected latitudes,
#' including one grid point outside the specified range on both sides.


create_subset_lat_IDs = function(lat_range){
  subset_lat_IDs = which((lat >= lat_range[1]-2.774456) & (lat <= lat_range[2]+2.774456))
  return(subset_lat_IDs)
}


#' (General) GENERATE MAP DIMENSIONS 
#'
#' This function calculates optimal on-screen and download dimensions for a map display 
#' based on the selected longitude and latitude subsets, available screen space, 
#' and whether axis labels are shown.
#'
#' The function returns a numeric vector containing:
#' 1. On-screen width,
#' 2. On-screen height,
#' 3. Download width,
#' 4. Download height,
#' 5. Longitude-to-latitude aspect ratio.
#'
#' @param subset_lon_IDs Numeric vector. Longitude indices of the selected map region.
#' @param subset_lat_IDs Numeric vector. Latitude indices of the selected map region.
#' @param output_width Numeric. Width of the map output container (in pixels), usually from `session$clientData$output_<<outputId>>_width`.
#' @param output_height Numeric. Height of the map output container (in pixels), usually from `session$clientData$output_<<outputId>>_height`.
#' @param hide_axis Logical. Whether to hide axis labels and reduce margin space.
#'
#' @return A numeric vector of length 5: c(on-screen width, on-screen height, download width, download height, lon/lat aspect ratio).

generate_map_dimensions = function(subset_lon_IDs,
                                   subset_lat_IDs,
                                   output_width,
                                   output_height,
                                   hide_axis) {
  
  lon_over_lat = ((length(subset_lon_IDs) - 1) * 1.875) / ((length(subset_lat_IDs) -
                                                              1) * 1.849638)
  
  # Width of left axis + key in pixels
  if (hide_axis == TRUE) {
    w_ax = 175
  } else {
    w_ax = 70 # TO CHECK!!!
  }
  # Height of bottom axis + title in pixels
  h_ax = 110
  
  # Set width and height when map is BELOW input tab
  if (output_width < 750) {
    w = output_width
    h = ((w - w_ax) * (1 / lon_over_lat)) + h_ax
    
    # Set w/h by height (i.e for a tall map)
  } else if ((output_width / output_height) > lon_over_lat) {
    h = output_height * 0.8
    w = ((h - h_ax) * (lon_over_lat)) + w_ax
    # Deal with too large widths
    if (w > output_width) {
      w = output_width * 0.75
      h = ((w - w_ax) * (1 / lon_over_lat)) + h_ax
      # Deal with very tiny widths
    } else if (w < 500) {
      w = 500
    }
    
    # Set w/h by width (i.e.for wide map)
  } else {
    w = output_width * 0.9
    h = ((w - w_ax) * (1 / lon_over_lat)) + h_ax
    # Deal with too large heights
    if (h > output_height) {
      h = output_height * 0.75
      w = ((h - h_ax) * (lon_over_lat)) + w_ax
      # Deal with very tiny heights
    } else if (h < 175) {
      h = 175
    }
  }
  
  # Generate download dimensions
  total_image_size = 6000000
  m = sqrt(total_image_size / (w * h))
  w_d = m * w
  h_d = m * h
  
  return(c(w, h, w_d, h_d, lon_over_lat))
}


#' (General) GENERATE DATA ID
#'
#' Creates a reference vector for ModE data selections, encoding whether the data is 
#' pre-processed, which dataset is used, the selected variable, and the seasonal period. 
#' This is used internally to manage data access and plotting logic.
#'
#' The returned vector contains:
#' 1. Pre-processed data flag:
#'    - `0` = Not available
#'    - `1` = Available and preloaded
#'    - `2` = Available but needs to be loaded
#' 2. Dataset code:
#'    - `1` = "ModE-RA"
#'    - `2` = "ModE-Sim"
#'    - `3` = "ModE-RAclim"
#'    - `4` = "SD ratio"
#' 3. Variable code:
#'    - `1` = "Temperature"
#'    - `2` = "Precipitation"
#'    - `3` = "SLP"
#'    - `4` = "Z500"
#' 4. Season code:
#'    - `1` = DJF (c(0, 2))
#'    - `2` = MAM (c(3, 5))
#'    - `3` = JJA (c(6, 8))
#'    - `4` = SON (c(9, 11))
#'    - `5` = Annual (c(1, 12))
#'
#' @param dataset Character. One of: "ModE-RA", "ModE-Sim", "ModE-RAclim", "SD ratio".
#' @param variable Character. One of: "Temperature", "Precipitation", "SLP", "Z500".
#' @param month_range Numeric vector of length 2. Month range for seasonal selection.
#'
#' @return Numeric vector of length 4: c(pre-processing flag, dataset ID, variable ID, season ID).

# generate_data_ID = function(dataset,
#                             variable,
#                             month_range){
#   
#   # Generate dataset reference
#   if (dataset == "ModE-RA"){
#     dataset_ref = 1
#   } else if (dataset == "ModE-Sim"){
#     dataset_ref = 2
#   } else if (dataset == "ModE-RAclim"){
#     dataset_ref = 3
#   } else { # dataset == "SD ratio"
#     dataset_ref = 4   
#   }
#   
#   # Generate variable reference
#   if (variable == "Temperature"){
#     variable_ref = 1      
#   } else if (variable == "Precipitation"){
#     variable_ref = 2
#   } else if (variable == "SLP"){
#     variable_ref = 3
#   } else { # (variable == "Z500")
#     variable_ref = 4
#   }
#   
#   # Helper: map month_range to (season_ref, pp_available) for a given pp flag
#   set_season_and_pp <- function(month_range, pp_flag_if_match) {
#     if (identical(month_range, c(1,12))) {
#       season_ref <- 5
#       pp_available <- pp_flag_if_match
#     } else if (identical(month_range, c(0,2))) {
#       season_ref <- 1
#       pp_available <- pp_flag_if_match
#     } else if (identical(month_range, c(3,5))) {
#       season_ref <- 2
#       pp_available <- pp_flag_if_match
#     } else if (identical(month_range, c(6,8))) {
#       season_ref <- 3
#       pp_available <- pp_flag_if_match
#     } else if (identical(month_range, c(9,11))) {
#       season_ref <- 4
#       pp_available <- pp_flag_if_match
#     } else {
#       season_ref <- NA
#       pp_available <- 0
#     }
#     list(season_ref = season_ref, pp_available = pp_available)
#   }
#   
#   # Check for preprocessed data and add season reference
#   if (dataset == "ModE-RA" && variable %in% c("Temperature", "Precipitation")) {
#     
#     # ModE-RA Temperature & Precipitation:
#     # preprocessed AND preloaded in pp_data -> pp_available = 1
#     res <- set_season_and_pp(month_range, pp_flag_if_match = 1)
#     season_ref   <- res$season_ref
#     pp_available <- res$pp_available
#     
#   } else if (dataset == "ModE-RA" && variable == "SLP") {
#     
#     # ModE-RA SLP:
#     # preprocessed exists (seasonal/year files) BUT not preloaded in pp_data
#     # -> pp_available = 2
#     res <- set_season_and_pp(month_range, pp_flag_if_match = 2)
#     season_ref   <- res$season_ref
#     pp_available <- res$pp_available
#     
#   } else {
#     
#     # Other data (ModE-Sim, ModE-RAclim, SD ratio, or Z500 for any dataset):
#     # may be preprocessed, but is never preloaded in pp_data
#     # -> pp_available = 2 for standard seasons/year
#     res <- set_season_and_pp(month_range, pp_flag_if_match = 2)
#     season_ref   <- res$season_ref
#     pp_available <- res$pp_available
#   }
#   
#   # Create data reference:
#   #  pp_available:
#   #    0 = no preprocessed data
#   #    1 = preprocessed & preloaded (pp_data)
#   #    2 = preprocessed exists but must be loaded
#   data_ref = c(pp_available, dataset_ref, variable_ref, season_ref)
#   
#   return(data_ref)
# }

generate_data_ID = function(dataset,
                            variable,
                            month_range){
  
  # Generate dataset reference
  if (dataset == "ModE-RA"){
    dataset_ref = 1
  } else if (dataset == "ModE-Sim"){
    dataset_ref = 2
  } else if (dataset == "ModE-RAclim"){
    dataset_ref = 3
  } else { # dataset == "SD ratio"
    dataset_ref = 4   
  }
  
  # Generate variable reference
  if (variable == "Temperature"){
    variable_ref = 1      
  } else if (variable == "Precipitation"){
    variable_ref = 2
  } else if (variable == "SLP"){
    variable_ref = 3
  } else { # (variable == "Z500")
    variable_ref = 4
  }
  
  # Helper: map month_range to (season_ref, pp_available = 2 if standard season, else 0)
  map_season <- function(month_range) {
    if (identical(month_range, c(1,12))) {
      season_ref <- 5
      pp_available <- 2
    } else if (identical(month_range, c(0,2))) {
      season_ref <- 1
      pp_available <- 2
    } else if (identical(month_range, c(3,5))) {
      season_ref <- 2
      pp_available <- 2
    } else if (identical(month_range, c(6,8))) {
      season_ref <- 3
      pp_available <- 2
    } else if (identical(month_range, c(9,11))) {
      season_ref <- 4
      pp_available <- 2
    } else {
      season_ref <- NA
      pp_available <- 0
    }
    list(season_ref = season_ref, pp_available = pp_available)
  }
  
  res <- map_season(month_range)
  season_ref   <- res$season_ref
  pp_available <- res$pp_available
  
  # Create data reference
  # pp_available:
  #   0 = no preprocessed data (use monthly via load_ModE_data)
  #   2 = preprocessed exists on disk (use load_preprocessed_data)
  data_ref = c(pp_available, dataset_ref, variable_ref, season_ref)
  
  return(data_ref)
}


#' (General) LOAD FULL ModE DATA
#'
#' Loads full ModE monthly data (ModE-RA, ModE-Sim, ModE-RAclim, or SD ratio) for the selected variable.
#' Automatically opens and processes the relevant NetCDF file and returns the complete 3D data array.
#' Applies appropriate unit conversions depending on the variable.
#'
#' ## Supported Datasets:
#' - `"ModE-RA"`: Absolute values (e.g., temperature in °C, precipitation in mm/month).
#' - `"ModE-Sim"`: Absolute values; first year (1420) is removed.
#' - `"ModE-RAclim"`: Anomalies relative to the 1901–2000 climatology.
#' - `"SD ratio"`: Standard deviation ratio of ModE-RA to climate reference.
#'
#' ## Supported Variables:
#' - `"Temperature"`: Converted from Kelvin to Celsius.
#' - `"Precipitation"`: Converted from kg/m²/s to mm/month using factor 2629756.8.
#' - `"SLP"`: Converted from Pa to hPa.
#' - `"Z500"`: Geopotential height at 500 hPa (no conversion).
#'
#' @param dataset Character. Name of dataset to load. One of `"ModE-RA"`, `"ModE-Sim"`, `"ModE-RAclim"`, `"SD ratio"`.
#' @param variable Character. Name of variable. One of `"Temperature"`, `"Precipitation"`, `"SLP"`, `"Z500"`.
#'
#' @return 3D numeric array (lon × lat × time) containing the full ModE dataset for the selected variable.
load_ModE_data = function(dataset,
                          variable){
  # Mod E-RA
  if (dataset == "ModE-RA"){
    # get variable name and open file
    vname =  switch(variable,
                    "Temperature"   = "temp2",
                    "Precipitation" = "totprec",
                    "SLP"           = "slp",
                    "Z500"          = "geopoth_50000")
    
    data_nc = ncdf4::nc_open(paste0("data/ModE-RA/Monthly/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_",vname,"_abs_1421-2008_mon.nc"))
    
    # extract data and convert units if necessary                  
    if (variable == "Temperature"){
      data_output = ncdf4::ncvar_get(data_nc,varid="temp2")-273.15 
    } else if (variable == "Precipitation"){
      data_output = ncdf4::ncvar_get(data_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month 
    } else if (variable == "SLP"){
      data_output = ncdf4::ncvar_get(data_nc,varid="slp")/100 
    } else {
      data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
    }
    
    ncdf4::nc_close(data_nc)
  } 
  # ModE-SIM
  else if (dataset == "ModE-Sim"){
    # get variable name and open file
    vname =  switch(variable,
                    "Temperature"   = "temp2",
                    "Precipitation" = "totprec",
                    "SLP"           = "slp",
                    "Z500"          = "geopoth_50000")
    
    data_nc = ncdf4::nc_open(paste0("data/ModE-SIM/Monthly/ModE-Sim_ensmean_",vname,"_abs_1420-2009.nc"))
    
    # extract data and convert units if necessary                  
    if (variable == "Temperature"){
      data_output = ncdf4::ncvar_get(data_nc,varid="temp2")-273.15 
    } else if (variable == "Precipitation"){
      data_output = ncdf4::ncvar_get(data_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month 
    } else if (variable == "SLP"){
      data_output = ncdf4::ncvar_get(data_nc,varid="slp")/100 
    } else {
      data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
    }
    
    # remove the first year (1421)
    data_output = data_output[,,13:7080]
    
    ncdf4::nc_close(data_nc)
  }
  # ModE-RAclim
  else if (dataset == "ModE-RAclim"){
    # get variable name and open file
    vname =  switch(variable,
                    "Temperature"   = "temp2",
                    "Precipitation" = "totprec",
                    "SLP"           = "slp",
                    "Z500"          = "geopoth_50000")
    data_nc = ncdf4::nc_open(paste0("data/ModE-RAclim/Monthly/ModE-RAclim_ensmean_",vname,"_anom_1421-2008_mon.nc"))
    
    # extract data and convert units if necessary                  
    if (variable == "Temperature"){
      data_output = ncdf4::ncvar_get(data_nc,varid="temp2") 
    } else if (variable == "Precipitation"){
      data_output = ncdf4::ncvar_get(data_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month 
    } else if (variable == "SLP"){
      data_output = ncdf4::ncvar_get(data_nc,varid="slp")/100 
    } else {
      data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
    }
    
    ncdf4::nc_close(data_nc)
  }
  
  # SDratio
  else if (dataset == "SD ratio"){
    # get variable name and open file
    vname =  switch(variable,
                    "Temperature"   = "temp2",
                    "Precipitation" = "totprec",
                    "SLP"           = "slp",
                    "Z500"          = "geopoth_50000")
    data_nc = ncdf4::nc_open(paste0("data/SD_ratio/Monthly/ModE-RA_lowres_20mem_Set_1420-3_1850-1_sdratio_",vname,"_anom_wrt_1901-2000_1421-2008_mon.nc"))
    
    # extract data and convert units if necessary                  
    if (variable == "Temperature"){
      data_output = ncdf4::ncvar_get(data_nc,varid="temp2") 
    } else if (variable == "Precipitation"){
      data_output = ncdf4::ncvar_get(data_nc,varid="totprec")
    } else if (variable == "SLP"){
      data_output = ncdf4::ncvar_get(data_nc,varid="slp")
    } else {
      data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
    }
    
    ncdf4::nc_close(data_nc)
  }
  
  
  return(data_output)
}


#' (General) Load Pre-Processed ModE Data
#'
#' Loads pre-processed ModE climate data for a chosen variable and month range.
#'
#' @param data_ID Integer vector specifying dataset, variable, and month range.
#' 
#' @return A 3D numeric array (longitude x latitude x time) of the requested climate data.

# load_preprocessed_data = function(data_ID){
#   
#   # Mod E-RA
#   if (data_ID[2] == 1){ # ModE-RA
#     # get variable and month names
#     vname = "geopoth_50000"
#     mname = switch(data_ID[4],
#                    "1" = "djf",
#                    "2" = "mam",
#                    "3" = "jja",
#                    "4" = "son",
#                    "5" = "year")
#     
#     # Open file
#     data_nc = ncdf4::nc_open(paste0("data/ModE-RA/",mname,"/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_",vname,"_abs_1421-2008_",mname,".nc"))
#     
#     # extract data and convert units if necessary                  
#     data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
#     
#     ncdf4::nc_close(data_nc)
#   } 
#   
#   # ModE-SIM
#   else if (data_ID[2] == 2){ # ModE-Sim
#     # get variable name and month names
#     vname =  switch(data_ID[3],
#                     "1" = "temp2",
#                     "2" = "totprec",
#                     "3" = "slp",
#                     "4" = "geopoth_50000")
#     mname = switch(data_ID[4],
#                    "1" = "djf",
#                    "2" = "mam",
#                    "3" = "jja",
#                    "4" = "son",
#                    "5" = "year")
#     
#     # open file
#     data_nc = ncdf4::nc_open(paste0("data/ModE-SIM/",mname,"/ModE-Sim_lowres_20mem_Set_1420-3_1850-1_ensmean_",vname,"_abs_1420-2009_",mname,".nc"))
#     
#     # extract data and convert units if necessary                  
#     if (data_ID[3] == 1){
#       data_output = ncdf4::ncvar_get(data_nc,varid="temp2")-273.15 
#     } else if (data_ID[3] == 2){
#       data_output = ncdf4::ncvar_get(data_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month 
#     } else if (data_ID[3] == 3){
#       data_output = ncdf4::ncvar_get(data_nc,varid="slp")/100 
#     } else {
#       data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
#     }
#     
#     # remove the first year (1421)
#     data_output = data_output[,,2:589]
#     
#     ncdf4::nc_close(data_nc)
#   }
#   
#   # ModE-RAclim
#   else if (data_ID[2] == 3) { # ModE-RAclim
#     # get variable name and month names
#     vname =  switch(data_ID[3],
#                     "1" = "temp2",
#                     "2" = "totprec",
#                     "3" = "slp",
#                     "4" = "geopoth_50000")
#     mname = switch(data_ID[4],
#                    "1" = "djf",
#                    "2" = "mam",
#                    "3" = "jja",
#                    "4" = "son",
#                    "5" = "year")
#     
#     # Open file
#     data_nc = ncdf4::nc_open(paste0("data/ModE-RAclim/",mname,"/ModE-RAclim_ensmean_",vname,"_anom_1421-2008_",mname,".nc"))
#     
#     # extract data and convert units if necessary                  
#     if (data_ID[3] == 1){
#       data_output = ncdf4::ncvar_get(data_nc,varid="temp2") 
#     } else if (data_ID[3] == 2){
#       data_output = ncdf4::ncvar_get(data_nc,varid="totprec")*2629756.8 # Multiply by 30.437*24*60*60 to convert Kg m-2 s-2 to get mm/month 
#     } else if (data_ID[3] == 3){
#       data_output = ncdf4::ncvar_get(data_nc,varid="slp")/100 
#     } else {
#       data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
#     }
#     
#     ncdf4::nc_close(data_nc)
#   }
#   
#   # SDratio
#   else if (data_ID[2] == 4){
#     # get variable name and month names
#     vname =  switch(data_ID[3],
#                     "1" = "temp2",
#                     "2" = "totprec",
#                     "3" = "slp",
#                     "4" = "geopoth_50000")
#     mname = switch(data_ID[4],
#                    "1" = "djf",
#                    "2" = "mam",
#                    "3" = "jja",
#                    "4" = "son",
#                    "5" = "year")
#     
#     # Open file
#     data_nc = ncdf4::nc_open(paste0("data/SD_ratio/",mname,"/ModE-RA_lowres_20mem_Set_1420-3_1850-1_sd_ratio_",vname,"_abs_1421-2008_",mname,".nc"))
#     
#     # extract data and convert units if necessary                  
#     if (data_ID[3] == 1){
#       data_output = ncdf4::ncvar_get(data_nc,varid="temp2") 
#     } else if (data_ID[3] == 2){
#       data_output = ncdf4::ncvar_get(data_nc,varid="totprec")
#     } else if (data_ID[3] == 3){
#       data_output = ncdf4::ncvar_get(data_nc,varid="slp")
#     } else {
#       data_output = ncdf4::ncvar_get(data_nc,varid="geopoth")
#     }
#     
#     ncdf4::nc_close(data_nc)
#     
#     # Add an extra year at the start for djf
#     if(mname == "djf"){
#       empty_year = array(NA, dim = c(192,96,588))
#       empty_year[,,c(2:588)] = data_output
#       data_output = empty_year
#     }
#   }
#   
#   return(data_output)
#   
# }

load_preprocessed_data = function(data_ID){
  
  # ModE-RA
  if (data_ID[2] == 1){ # ModE-RA
    # get variable and month names
    vname = switch(as.character(data_ID[3]),
                   "1" = "temp2",
                   "2" = "totprec",
                   "3" = "slp",
                   "4" = "geopoth_50000")
    mname = switch(as.character(data_ID[4]),
                   "1" = "djf",
                   "2" = "mam",
                   "3" = "jja",
                   "4" = "son",
                   "5" = "year")
    
    # Open file
    data_nc = ncdf4::nc_open(paste0(
      "data/ModE-RA/", mname, "/ModE-RA_lowres_20mem_Set_1420-3_1850-1_ensmean_",
      vname, "_abs_1421-2008_", mname, ".nc"
    ))
    
    # extract data and convert units if necessary                  
    if (data_ID[3] == 1){
      data_output = ncdf4::ncvar_get(data_nc, varid = "temp2") - 273.15 
    } else if (data_ID[3] == 2){
      data_output = ncdf4::ncvar_get(data_nc, varid = "totprec") * 2629756.8
    } else if (data_ID[3] == 3){
      data_output = ncdf4::ncvar_get(data_nc, varid = "slp") / 100 
    } else {
      data_output = ncdf4::ncvar_get(data_nc, varid = "geopoth")
    }
    
    ncdf4::nc_close(data_nc)
  } 
  
  # ModE-SIM
  else if (data_ID[2] == 2){ # ModE-Sim
    vname = switch(as.character(data_ID[3]),
                   "1" = "temp2",
                   "2" = "totprec",
                   "3" = "slp",
                   "4" = "geopoth_50000")
    mname = switch(as.character(data_ID[4]),
                   "1" = "djf",
                   "2" = "mam",
                   "3" = "jja",
                   "4" = "son",
                   "5" = "year")
    
    data_nc = ncdf4::nc_open(paste0(
      "data/ModE-SIM/", mname, "/ModE-Sim_lowres_20mem_Set_1420-3_1850-1_ensmean_",
      vname, "_abs_1420-2009_", mname, ".nc"
    ))
    
    if (data_ID[3] == 1){
      data_output = ncdf4::ncvar_get(data_nc, varid = "temp2") - 273.15 
    } else if (data_ID[3] == 2){
      data_output = ncdf4::ncvar_get(data_nc, varid = "totprec") * 2629756.8
    } else if (data_ID[3] == 3){
      data_output = ncdf4::ncvar_get(data_nc, varid = "slp") / 100 
    } else {
      data_output = ncdf4::ncvar_get(data_nc, varid = "geopoth")
    }
    
    # remove the first year (1421)
    data_output = data_output[,,2:589]
    
    ncdf4::nc_close(data_nc)
  }
  
  # ModE-RAclim
  else if (data_ID[2] == 3) { # ModE-RAclim
    vname = switch(as.character(data_ID[3]),
                   "1" = "temp2",
                   "2" = "totprec",
                   "3" = "slp",
                   "4" = "geopoth_50000")
    mname = switch(as.character(data_ID[4]),
                   "1" = "djf",
                   "2" = "mam",
                   "3" = "jja",
                   "4" = "son",
                   "5" = "year")
    
    data_nc = ncdf4::nc_open(paste0(
      "data/ModE-RAclim/", mname, "/ModE-RAclim_ensmean_",
      vname, "_anom_1421-2008_", mname, ".nc"
    ))
    
    if (data_ID[3] == 1){
      data_output = ncdf4::ncvar_get(data_nc, varid = "temp2") 
    } else if (data_ID[3] == 2){
      data_output = ncdf4::ncvar_get(data_nc, varid = "totprec") * 2629756.8
    } else if (data_ID[3] == 3){
      data_output = ncdf4::ncvar_get(data_nc, varid = "slp") / 100 
    } else {
      data_output = ncdf4::ncvar_get(data_nc, varid = "geopoth")
    }
    
    ncdf4::nc_close(data_nc)
  }
  
  # SD ratio
  else if (data_ID[2] == 4){
    vname = switch(as.character(data_ID[3]),
                   "1" = "temp2",
                   "2" = "totprec",
                   "3" = "slp",
                   "4" = "geopoth_50000")
    mname = switch(as.character(data_ID[4]),
                   "1" = "djf",
                   "2" = "mam",
                   "3" = "jja",
                   "4" = "son",
                   "5" = "year")
    
    data_nc = ncdf4::nc_open(paste0(
      "data/SD_ratio/", mname, "/ModE-RA_lowres_20mem_Set_1420-3_1850-1_sd_ratio_",
      vname, "_abs_1421-2008_", mname, ".nc"
    ))
    
    if (data_ID[3] == 1){
      data_output = ncdf4::ncvar_get(data_nc, varid = "temp2") 
    } else if (data_ID[3] == 2){
      data_output = ncdf4::ncvar_get(data_nc, varid = "totprec")
    } else if (data_ID[3] == 3){
      data_output = ncdf4::ncvar_get(data_nc, varid = "slp")
    } else {
      data_output = ncdf4::ncvar_get(data_nc, varid = "geopoth")
    }
    
    ncdf4::nc_close(data_nc)
    
    # Add an extra year at the start for djf
    if (mname == "djf"){
      empty_year = array(NA, dim = c(192, 96, 588))
      empty_year[,,2:588] = data_output
      data_output = empty_year
    }
  }
  
  return(data_output)
}


#' (General) Create Geographic Subset of ModE Data
#'
#' Subsets a ModE dataset to a reduced geographic area based on longitude and latitude indices.
#'
#' @param data_input Numeric array of climate data to subset.
#' @param data_ID Integer vector specifying dataset details.
#' @param subset_lon_IDs Integer vector of longitude indices to include.
#' @param subset_lat_IDs Integer vector of latitude indices to include.
#' 
#' @return A numeric array subsetted by the specified longitude and latitude indices.

# create_latlon_subset = function(data_input,
#                                 data_ID,
#                                 subset_lon_IDs,
#                                 subset_lat_IDs){
#   if (data_ID[1] == 1){
#     # subset preprocessed data
#     data_subset = pp_data[[data_ID[4]]][[data_ID[3]]][subset_lon_IDs,subset_lat_IDs,]
#   } 
#   else {
#     # subset base/loaded preprocessed data
#     data_subset = data_input[subset_lon_IDs,subset_lat_IDs,]
#   }
#   return(data_subset)
# }

create_latlon_subset = function(data_input,
                                data_ID,
                                subset_lon_IDs,
                                subset_lat_IDs){
  # Always subset the provided data_input
  data_subset = data_input[subset_lon_IDs, subset_lat_IDs, ]
  return(data_subset)
}


#' (General) Create Year and Month Subset with Mean Yearly Values
#'
#' Returns a dataset of mean yearly values within a specified year and month range.
#'
#' @param data_input Numeric array of climate data.
#' @param data_ID Integer vector specifying dataset details.
#' @param year_range Integer vector of length 2 specifying start and end years.
#' @param month_range Integer vector of length 2 specifying start and end months.
#'
#' @return A numeric array of mean yearly values for the specified time range.

create_yearly_subset = function(data_input,
                                data_ID,
                                year_range,
                                month_range){
  # Check for preprocessed subset
  if (data_ID[1] != 0){
    year_IDs = (year_range[1]-1420):(year_range[2]-1420)
    data_subset = data_input[,,year_IDs]
  }
  # Calculate subset from scratch
  else {
    dim_data = dim(data_input)
    years = year_range[1]:year_range[2]
    
    data_subset = array(NA,dim=c(dim_data[1],dim_data[2], length(years)))
    
    for (i in 1:length(years)){
      Y = years[i]
      M1 = ((12*(Y-1421))+month_range[1]) ; M2 = ((12*(Y-1421))+month_range[2])
      data_subset[,,i] = apply(data_input[,,M1:M2],c(1:2),mean)
    }
  }
  
  return(data_subset)
}


#' (General) Convert Absolute Yearly Subset to Anomalies
#'
#' Converts an absolute yearly subset dataset into anomalies relative to a reference dataset.
#'
#' @param data_input Numeric array of absolute yearly data.
#' @param ref_data Numeric array of averaged data for the reference period.
#'
#' @return A numeric array of anomaly values.

convert_subset_to_anomalies = function(data_input,
                                       ref_data){
  
  # find dimensions of the reference data
  dim_data = dim(data_input)
  
  # Repeat ref data if year range > 1 year
  if (length(dim_data) == 2){
    baseline_array = ref_data
  } else {
    # duplicate into an array
    baseline_array = array(ref_data,dim= c(dim_data[1],dim_data[2], dim_data[3]))
  }
  
  # Take baseline data from absolute data to calculate anomalies
  anomaly_data = data_input-baseline_array
  
  return(anomaly_data)
}


#' (General) Generate Map, Timeseries, and File Titles
#'
#' Creates a dataframe containing titles and subtitles for maps, timeseries plots, file names, and NetCDF data based on input parameters.
#'
#' @param tab Character; type of title set to generate ("general", "composites", "reference", or "sdratio").
#' @param dataset Character; dataset name (e.g., "ModE-RA", "ModE-Sim", "ModE-RAclim").
#' @param variable Character; climate variable name.
#' @param mode Character; data mode ("Absolute", "Anomaly", "Fixed reference", "X years prior", etc.).
#' @param map_title_mode Character; either "Default" or "Custom" to select map title mode.
#' @param ts_title_mode Character; either "Default" or "Custom" to select timeseries title mode.
#' @param month_range Numeric vector; selected months.
#' @param year_range Numeric vector; selected years.
#' @param baseline_range Numeric vector or NA; baseline reference years if applicable.
#' @param baseline_years_before Numeric or NA; baseline years prior if applicable.
#' @param lon_range Numeric vector; longitude range.
#' @param lat_range Numeric vector; latitude range.
#' @param map_custom_title1 Character or NA; user-defined map title.
#' @param map_custom_title2 Character or NA; user-defined map subtitle.
#' @param ts_custom_title1 Character or NA; user-defined timeseries title.
#' @param ts_custom_title2 Character or NA; user-defined timeseries subtitle.
#' @param map_title_size Numeric; font size for map title.
#' @param ts_title_size Numeric; font size for timeseries title.
#' @param ts_data Numeric vector or dataframe; timeseries data for subtitle statistics.
#'
#' @return A dataframe with columns:
#' \describe{
#'   \item{map_title}{Character; main title for the map.}
#'   \item{map_subtitle}{Character; subtitle for the map.}
#'   \item{ts_title}{Character; title for the timeseries plot.}
#'   \item{ts_subtitle}{Character; subtitle for the timeseries plot, including statistics if available.}
#'   \item{ts_axis}{Character; label for the timeseries plot axis.}
#'   \item{file_title}{Character; sanitized string suitable for filenames.}
#'   \item{netcdf_title}{Character; title for NetCDF output files.}
#'   \item{map_title_size}{Numeric; font size for the map title.}
#'   \item{ts_title_size}{Numeric; font size for the timeseries title.}
#'   \item{v_unit}{Character; variable unit string.}
#' }

generate_titles = function(tab,
                           dataset,
                           variable,
                           mode,
                           map_title_mode = "Default",
                           ts_title_mode = "Default",
                           month_range,
                           year_range,
                           baseline_range = NA,
                           baseline_years_before = NA,
                           lon_range,
                           lat_range,
                           map_custom_title1 = NA,
                           map_custom_title2 = NA,
                           ts_custom_title1 = NA,
                           ts_custom_title2 = NA,
                           map_title_size = 18,
                           ts_title_size = 18,
                           ts_data = NA) {
  
  
  # Generate title months
  title_months = generate_title_months(MR = month_range)
  
  # Create map_title & map_subtitle:
  # Averages and Anomalies titles
  if (tab == "general") {
    if (mode == "Absolute") {
      if (length(year_range) == 1 || year_range[1] == year_range[2]) {
        map_title <- paste(dataset, " ", title_months, " ", variable, " ", year_range[1], sep = "")
      } else {
        map_title <- paste(dataset, " ", title_months, " ", variable, " ", year_range[1], "-", year_range[2], sep = "")
      }
      map_subtitle <- ""
    } else {  # Anomaly
      if (length(year_range) == 1 || year_range[1] == year_range[2]) {
        map_title <- paste(dataset, " ", title_months, " ", variable, " Anomaly ", year_range[1], sep = "")
      } else {
        map_title <- paste(dataset, " ", title_months, " ", variable, " Anomaly ", year_range[1], "-", year_range[2], sep = "")
      }
      
      if (length(baseline_range) == 1 || baseline_range[1] == baseline_range[2]) {
        map_subtitle <- paste("Ref. = ", baseline_range[1], sep = "")
      } else {
        map_subtitle <- paste("Ref. = ", baseline_range[1], "-", baseline_range[2], sep = "")
      }
    }
  }
  
  
  
  # Composites titles
  else if (tab=="composites"){
    if (mode == "Absolute"){
      map_title = paste(dataset," ",title_months," ",variable," Absolute values (Composite years)", sep = "")
      map_subtitle = ""
    } else if (mode == "Fixed reference") {
      map_title = paste(dataset," ",title_months," ",variable," Anomaly (Composite years)", sep = "")
      map_subtitle = paste("Ref. = ",baseline_range[1],"-",baseline_range[2], sep = "") 
    } else if (mode == "X years prior") {
      map_title = paste(dataset," ",title_months," ",variable," Anomaly (Composite years)", sep = "")
      map_subtitle = paste("Ref. = ",baseline_years_before," yrs prior", sep = "")
    } else {
      map_title = paste(dataset," ",title_months," ",variable," Anomaly (Composite years)", sep = "")
      map_subtitle = paste("Ref. = Reference years")  
    }
  }
  
  # Reference period titles
  else if (tab == "reference") {
    if (length(year_range) == 1 || year_range[1] == year_range[2]) {
      map_title <- paste(dataset, " ", title_months, " ", variable, " ", year_range[1], " (Reference year)", sep = "")
    } else {
      map_title <- paste(dataset, " ", title_months, " ", variable, " ", year_range[1], "-", year_range[2], " (Reference years)", sep = "")
    }
    map_subtitle <- ""
  }
  
  # SD ratio titles
  else if (tab == "sdratio") {
    if (is.na(year_range[1])) {
      map_title <- paste(dataset, " ", title_months, " SD ratio (Composite years)", sep = "")
      map_subtitle <- ""
    } else if (length(year_range) == 1 || year_range[1] == year_range[2]) {
      map_title <- paste(dataset, " ", title_months, " SD ratio ", year_range[1], sep = "")
      map_subtitle <- ""
    } else {
      map_title <- paste(dataset, " ", title_months, " SD ratio ", year_range[1], "-", year_range[2], sep = "")
      map_subtitle <- ""
    }
  }
  
  
  # Create Timeseries title, subtitle and axis titles
  if (tab=="composites"){
    ts_title = paste(substr(map_title, 1, nchar(map_title) - 18),
                     " [",lon_range[1],":",lon_range[2],"\u00B0E, ",lat_range[1],":",lat_range[2],"\u00B0N]", sep = "")
  } else {
    ts_title = paste(substr(map_title, 1, nchar(map_title) - 10),
                     " [",lon_range[1],":",lon_range[2],"\u00B0E, ",lat_range[1],":",lat_range[2],"\u00B0N]", sep = "")
  }
  
  v_unit <- get_variable_properties(variable)$unit
  
  if (mode == "Absolute"){
    ts_axis = paste(title_months," Mean ",variable," [",v_unit, "]",sep = "")
  } else {
    ts_axis = paste(title_months," ",variable," Anomaly [",v_unit, "]",sep = "")
  }
  
  if (is.data.frame(ts_data)){# using is.null doesn't work because is.null(NA) = FALSE, using is.na doesn't work because is.na(data.frame) = c(FALSE, FALSE, FALSE, ...)
    stats = generate_stats_ts(as.vector(ts_data$Mean)) #transform into vector, because generate_stats_ts expects a vector
    
    ts_subtitle <- paste(
      "Mean = ",
      signif(stats$mean, 3),
      v_unit,
      "   Range = ",
      signif(stats$min, 3),
      v_unit,
      ":",
      signif(stats$max, 3),
      v_unit,
      "   SD = ",
      signif(stats$sd, 3),
      v_unit,
      sep = ""
    )
    if (map_subtitle != "") {
      ts_subtitle = paste0(ts_subtitle, "\n", map_subtitle)
    }
  } else {
    if (map_subtitle != "") {
      ts_subtitle <- map_subtitle
    } else {
      ts_subtitle <- ""
    }
  }
  
  # Replace with custom titles
  if (map_title_mode == "Custom"){
    if(map_custom_title1 != ""){
      map_title = map_custom_title1
    }
    if(map_custom_title2 != ""){
      map_subtitle = map_custom_title2
    }
  }
  
  if (ts_title_mode == "Custom"){
    if(ts_custom_title1 != ""){
      ts_title = ts_custom_title1
    }
    ts_subtitle = ""
  }
  
  # Create title for filenames
  tf1 = gsub("[[:punct:]]", "", map_title)
  tf2 = gsub(" ","-",tf1)
  file_title = iconv(tf2, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Netcdf title
  netcdf_title = gsub(paste(variable),"NCDF-Data",file_title)
  
  # Combine into dataframe
  m_titles = data.frame(
    map_title,
    map_subtitle,
    ts_title,
    ts_subtitle,
    ts_axis,
    file_title,
    netcdf_title,
    map_title_size,
    ts_title_size,
    v_unit
  )
  
  return(m_titles)
}


#' (General) Generate Statistics from Timeseries Data
#'
#' Calculates basic statistics (mean, standard deviation, minimum, and maximum) from a numeric timeseries vector.
#'
#' @param data Numeric vector; timeseries data.
#'
#' @return A dataframe with columns:

generate_stats_ts = function(data){
  
  mean = mean(data, na.rm = TRUE)
  sd = sd(data, na.rm = TRUE)
  min = min(data, na.rm = TRUE)
  max = max(data, na.rm = TRUE)
  
  ts_stats = data.frame(mean, sd, min, max)
  return(ts_stats)
}


#' (General) SET DEFAULT/CUSTOM AXIS VALUES
#' 
#' @param data_input Numeric vector or array for mapping.
#' @param mode Character string, either "Absolute" or "Anomalies".
#' 
#' @return Numeric vector of length 2 with axis min and max values.

#For Map Plots
set_axis_values = function(data_input,
                           mode){
  
  if (mode == "Absolute"){
    minmax = c(min(data_input),max(data_input))  
  } else {
    z_max = max(abs(data_input))
    minmax = c(-z_max,z_max)
  }
  
  minmax = signif(minmax,digits = 3)
  
  return(minmax)
}


#' (General) SET DEFAULT TIME SERIES AXIS VALUES
#' 
#' @param data_input Numeric vector of timeseries data.
#' 
#' @return Numeric vector of length 2 with axis min and max values.

#FOr TS Plots
set_ts_axis_values = function(data_input) {
  minmax = range(data_input, na.rm = TRUE)
  return(signif(minmax, digits = 3))
}


#' (General) Plot map with ggplot2, including averaging dataset if needed
#'
#' This function creates a map plot using ggplot2 based on provided spatial data,
#' with options for different variables, modes, projections, and overlays.
#'
#' @param data_input Spatial raster data to plot.
#' @param lon_lat_range Numeric vector defining longitude and latitude plotting limits.
#' @param variable Character specifying the variable to plot (e.g., "Temperature", "Precipitation", "SLP", "Z500", "SD ratio").
#' @param mode Character specifying the mode of plotting ("Absolute", "Correlation", "Regression_coefficients", etc.).
#' @param titles List containing titles and subtitle text and sizes.
#' @param axis_range Numeric vector defining axis limits for color scales.
#' @param hide_axis Logical, whether to hide the colorbar axis.
#' @param points_data Data frame with points to add on the map.
#' @param highlights_data Data frame with highlighted areas to add on the map.
#' @param stat_highlights_data Data frame with statistical highlight points.
#' @param c_borders Logical, whether to plot country borders.
#' @param white_ocean Logical, whether to fill oceans with white/gray color.
#' @param white_land Logical, whether to fill land with white/gray color.
#' @param shpOrder Character vector of shapefile names to add to the plot.
#' @param plotOrder Character vector defining the order of shapefiles to plot.
#' @param input List of user inputs for shapefile colors.
#' @param plotType Character prefix for shapefile color input IDs.
#' @param projection Character specifying the map projection ("UTM (default)", "Robinson", "Orthographic", "LAEA").
#' @param center_lat Numeric latitude for centered projections.
#' @param center_lon Numeric longitude for centered projections.
#' @param show_rivers Logical, whether to show rivers on the map.
#' @param label_rivers Logical, whether to label rivers.
#' @param show_lakes Logical, whether to show lakes on the map.
#' @param label_lakes Logical, whether to label lakes.
#' @param show_mountains Logical, whether to show mountains on the map.
#' @param label_mountains Logical, whether to label mountains.
#'
#' @return A ggplot object representing the map.

# Plot map with ggplot2
# plot_map <- function(data_input,
#                      lon_lat_range,
#                      variable = NULL,
#                      mode = NULL,
#                      titles = NULL,
#                      axis_range = NULL,
#                      hide_axis = FALSE,
#                      points_data = data.frame(),
#                      highlights_data = data.frame(),
#                      stat_highlights_data = data.frame(),
#                      c_borders = TRUE,
#                      white_ocean = FALSE,
#                      white_land = FALSE,
#                      shpOrder = NULL,
#                      plotOrder = NULL,
#                      input = NULL,
#                      plotType = "default",
#                      projection = "UTM (default)",
#                      center_lat = 0,
#                      center_lon = 0,
#                      show_rivers = FALSE,
#                      label_rivers = FALSE,
#                      show_lakes = FALSE,
#                      label_lakes = FALSE,
#                      show_mountains = FALSE,
#                      label_mountains = FALSE) {
#   
#   # Define the color palette and unit based on the variable and mode
#   if (!is.null(variable)) {
#     if (variable == "Temperature") {
#       v_col <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
#       v_unit <- "\u00B0C"
#     } else if (variable == "Precipitation") {
#       v_col <- RColorBrewer::brewer.pal(11, "BrBG")
#       v_unit <- "mm/mon."
#     } else if (variable == "SLP") {
#       v_col <- rev(RColorBrewer::brewer.pal(11, "PRGn"))
#       v_unit <- "hPa"
#     } else if (variable == "Z500") {
#       v_col <- rev(RColorBrewer::brewer.pal(11, "PRGn"))
#       v_unit <- "m"
#     }
#   }
#   
#   # Handle the 'mode' condition as well
#   if (!is.null(mode)) {
#     if (mode == "Correlation") {
#       v_col <- rev(RColorBrewer::brewer.pal(11, "PuOr"))
#       v_unit <- "r"
#       
#     } else if (mode == "Regression_coefficients") {
#       v_col <- viridis::viridis(11, option = "turbo")
#       v_unit <- "Coefficient"
#       titles$map_title <- titles$map_title_coeff
#       titles$map_subtitle <- titles$map_subtitle_coeff
#       titles$map_title_size <- titles$map_title_size_coeff
#       
#     } else if (mode == "Regression_p_values") {
#       v_col <- rev(RColorBrewer::brewer.pal(5, "Greens"))
#       v_unit <- "p\nValues"
#       v_lev <- c(0, 0.01, 0.05, 0.1, 0.2, 1)
#       titles$map_title <- titles$map_title_pvals
#       titles$map_subtitle <- titles$map_subtitle_pvals
#       titles$map_title_size <- titles$map_title_size_pvals
#       
#     } else if (mode == "Regression_residuals") {
#       v_unit <- paste("Residuals\n", v_unit)
#       titles$map_title <- titles$map_title_res
#       titles$map_subtitle <- titles$map_subtitle_res
#       titles$map_title_size <- titles$map_title_size_res
#       
#     } else if (mode == "SD ratio") {
#       v_col <- rev(RColorBrewer::brewer.pal(9, "Greens"))
#       v_unit <- ""
#     }
#   }
#   if (is.null(axis_range)) {
#     if (!is.null(mode) && mode == "Regression_p_values") {
#       axis_range <- c(1e-3, 1)
#     } else if (!is.null(mode) && mode == "SD ratio") {
#       axis_range <- c(0, 1)
#     } else {
#       max_abs_z <- max(abs(terra::values(data_input)), na.rm = TRUE)
#       axis_range <- c(-max_abs_z, max_abs_z)
#     }
#   }
#   
#   p <- ggplot() +
#     tidyterra::geom_spatraster_contour_filled(data = data_input, aes(fill = after_stat(level_mid)), bins = 20) +
#     labs(fill = v_unit) +
#     guides(
#       fill = if (hide_axis)
#         "none"
#       else
#         guide_colorbar(
#           barwidth = 2,
#           barheight = unit(0.75, "npc"),
#           title = v_unit,
#           title.position = "top",
#           title.hjust = 0.25,
#           display = "rectangles",
#           draw.ulim = FALSE,
#           draw.llim = FALSE,
#           label.theme = element_text(size = titles$map_title_size / 1.6),
#           title.theme = element_text(size = titles$map_title_size / 1.6),
#           frame.colour = "black",
#           frame.linewidth = 0.5,
#           ticks.colour = "black",
#           ticks.linewidth = 0.5
#         )
#     )
#   
#   if (!is.null(mode) && mode == "Regression_p_values") {
#     p <- p + scale_fill_gradientn(
#       colors = v_col,
#       trans = "log10",
#       limits = axis_range,
#       breaks = c(0.001, 0.01, 0.1, 1),
#       labels = scales::label_comma(accuracy = 0.001)
#     )
#   } else {
#     p <- p + scale_fill_stepsn(
#       limits = axis_range,
#       colors = v_col,
#       n.breaks = 20,
#       nice.breaks = TRUE,
#       labels = function(breaks) {
#         labels <- as.character(round(breaks, 2))
#         labels[1] <- paste0("< ", labels[1])
#         labels[length(labels)] <- paste0("> ", labels[length(labels)])
#         labels
#       }
#     )
#   }
#   
#   # Theme
#   p <- p + if (projection == "UTM (default)")
#     theme_bw()
#   else
#     theme_minimal()
#   
#   # Add coastline and country borders without inheriting x and y aesthetics
#   p <- p + geom_sf(data = coast, color = "#333333", size = 0.5, inherit.aes = FALSE)
#   if (white_ocean) p <- p + geom_sf(data = oceans, fill = "#DDDDDD", size = 0.5, inherit.aes = FALSE)
#   if (white_land)  p <- p + geom_sf(data = land,   fill = "#DDDDDD", size = 0.5, inherit.aes = FALSE)
#   if (c_borders)   p <- p + geom_sf(data = countries, color = "#333333", fill = NA, size = 0.5, inherit.aes = FALSE)
#   
#   if (show_rivers) {
#     p <- p + geom_sf(data = rivers, color = "#2A52BE", size = 0.2, inherit.aes = FALSE)
#     if (label_rivers && "name" %in% names(rivers)) {
#       p <- p + geom_text(
#         data = sf::st_centroid(rivers),
#         aes(label = name, geometry = geometry),
#         stat = "sf_coordinates", size = 5, color = "black", inherit.aes = FALSE
#       )
#     }
#   }
#   if (show_lakes) {
#     p <- p + geom_sf(data = lakes, fill = "#DDDDDD", color = NA, inherit.aes = FALSE)
#     if (label_lakes && "name" %in% names(lakes)) {
#       p <- p + geom_text(
#         data = sf::st_point_on_surface(lakes),
#         aes(label = name, geometry = geometry),
#         stat = "sf_coordinates", size = 5, color = "black", nudge_x = -1,
#         inherit.aes = FALSE
#       )
#     }
#   }
#   if (show_mountains) {
#     p <- p + geom_sf(data = mountains, color = "#333333", size = 3, shape = 17, inherit.aes = FALSE)
#     if (label_mountains && "name" %in% names(mountains)) {
#       p <- p + geom_text(
#         data = mountains,
#         aes(label = name, geometry = geometry),
#         stat = "sf_coordinates", size = 5, color = "black", nudge_y = 0.75,
#         inherit.aes = FALSE
#       )
#     }
#   }
#   # Add shapefiles (if provided) based on plotOrder and shpPickers
#   
#   # Define color picker prefix for shapefiles
#   color_picker_prefix <- plotType
#   if (!is.null(shpOrder) && !is.null(plotOrder) && length(shpOrder) > 0 && length(plotOrder) > 0) {
#     # keep once, preserve order-of-first-appearance
#     shpOrder <- shpOrder[!duplicated(shpOrder)]
#     selected_files <- plotOrder[match(shpOrder, tools::file_path_sans_ext(basename(plotOrder)))]
#     
#     for (file in selected_files) {
#       file_name <- tools::file_path_sans_ext(basename(file))
#       message(paste("Adding shapefile to plot:", file_name))
#       shape <- sf::st_read(file, quiet = TRUE)
#       if (is.na(sf::st_crs(shape))) shape <- sf::st_set_crs(shape, sf::st_crs(4326)) else shape <- sf::st_transform(shape, 4326)
#       
#       col_pick <- input[[paste0(color_picker_prefix, file_name)]]
#       if (is.null(col_pick) || is.na(col_pick) || !nzchar(col_pick)) col_pick <- "black"
#       
#       geom_type <- as.character(sf::st_geometry_type(shape))
#       if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
#         p <- p + geom_sf(data = shape, fill = NA, colour = col_pick, linewidth = 0.5, inherit.aes = FALSE)
#       } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
#         p <- p + geom_sf(data = shape, colour = col_pick, linewidth = 0.5, inherit.aes = FALSE)
#       } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
#         p <- p + geom_sf(data = shape, colour = col_pick, size = 2, inherit.aes = FALSE)
#       }
#     }
#   }
#   
#   
#   # Set projection
#   if (projection == "Robinson") {
#     p <- p + coord_sf(crs = sf::st_crs("+proj=robin"), clip = "on")
#   } else if (projection == "Orthographic") {
#     formula <- paste0("+proj=ortho +lat_0=", center_lat, " +lon_0=", center_lon,
#                       " +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     p <- p + coord_sf(crs = sf::st_crs(formula), clip = "on")
#   } else if (projection == "LAEA") {
#     formula <- paste0("+proj=laea +lat_0=0 +lon_0=0 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     p <- p + coord_sf(crs = sf::st_crs(formula), clip = "on")
#   } else {
#     if (lon_lat_range[1]==lon_lat_range[2]) { lon_lat_range[1] <- lon_lat_range[1]-0.5; lon_lat_range[2] <- lon_lat_range[2]+0.5 }
#     if (lon_lat_range[3]==lon_lat_range[4]) { lon_lat_range[3] <- lon_lat_range[3]-0.5; lon_lat_range[4] <- lon_lat_range[4]+0.5 }
#     lon_lat_range[1] <- max(lon_lat_range[1], -179.0625)
#     lon_lat_range[2] <- min(lon_lat_range[2],  177.1875)
#     lon_lat_range[3] <- max(lon_lat_range[3],  -87.64735)
#     lon_lat_range[4] <- min(lon_lat_range[4],   87.64735)
#     p <- p + coord_sf(xlim = lon_lat_range[1:2], ylim = lon_lat_range[3:4], expand = FALSE, clip = "on")
#   }
#   
#   # Add title and subtitle if provided
#   if (!is.null(titles)) {
#     if (titles$map_title != " " || titles$map_subtitle != " ") {
#       p <- p + labs(
#         title = ifelse(titles$map_title != " ", titles$map_title, NULL),
#         subtitle = ifelse(titles$map_subtitle != " ", titles$map_subtitle, NULL)
#       )
#     }
#     
#     p <- p + theme(
#       plot.title = ggtext::element_textbox_simple(size = titles$map_title_size, face = "bold", margin = margin(5,0,5,0)),
#       plot.subtitle = ggtext::element_textbox_simple(size = titles$map_title_size/1.3, face = "plain", margin = margin(9,0,12,0)),
#       axis.text = element_text(size = titles$map_title_size / 1.6)
#     )
#   }
#   
#   if (projection != "UTM (default)" &&
#       nrow(stat_highlights_data) > 0 &&
#       all(c("x_vals","y_vals") %in% names(stat_highlights_data))) {
#     stat_highlights_data <- transform_points_df(
#       df = stat_highlights_data,
#       xcol = "x_vals", ycol = "y_vals",
#       projection = projection, center_lat = center_lat, center_lon = center_lon
#     )
#   }
#   
#   built <- ggplot_build(p)
#   pp <- built$layout$panel_params[[1]]
#   get_range <- function(pp, axis = c("x","y")) {
#     axis <- match.arg(axis)
#     if (!is.null(pp[[paste0(axis, ".range")]])) {
#       pp[[paste0(axis, ".range")]]
#     } else if (!is.null(pp[[axis]]$range$range)) {
#       pp[[axis]]$range$range
#     } else if (!is.null(pp[[axis]]$range)) {
#       pp[[axis]]$range
#     } else c(-Inf, Inf)
#   }
#   xlim_cur <- get_range(pp, "x")
#   ylim_cur <- get_range(pp, "y")
#   
#   # Transform statistical highlight points if projection is active
#   if (projection != "UTM (default)" &&
#       nrow(points_data) > 0 &&
#       all(c("x_value","y_value") %in% names(points_data))) {
#     points_data <- transform_points_df(
#       df = points_data,
#       xcol = "x_value", ycol = "y_value",
#       projection = projection, center_lat = center_lat, center_lon = center_lon
#     )
#   }
#   
#   # Hard-filter points to the panel bounding box
#   if (nrow(points_data) > 0 && all(c("x_value","y_value","color","shape","size","label") %in% names(points_data))) {
#     
#     points_data$x_value <- as.numeric(points_data$x_value)
#     points_data$y_value <- as.numeric(points_data$y_value)
#     
#     inside <- with(points_data,
#                    !is.na(x_value) & !is.na(y_value) &
#                      x_value >= xlim_cur[1] & x_value <= xlim_cur[2] &
#                      y_value >= ylim_cur[1] & y_value <= ylim_cur[2])
#     points_in <- points_data[inside, , drop = FALSE]
#     
#     if (nrow(points_in) > 0) {
#       p <- p +
#         geom_point(
#           data = points_in,
#           aes(x = x_value, y = y_value, colour = I(color), shape = I(shape), size = I(size)),
#           show.legend = FALSE, inherit.aes = FALSE
#         ) +
#         ggrepel::geom_text_repel(
#           data = points_in,
#           aes(x = x_value, y = y_value, label = label),
#           size = 5, fontface = "bold",
#           bg.color = "white", bg.r = 0.2,
#           point.padding = 10,
#           xlim = xlim_cur, ylim = ylim_cur,
#           na.rm = TRUE, max.overlaps = Inf,
#           show.legend = FALSE, inherit.aes = FALSE
#         )
#     }
#     
#   }
#   
#   # Transform highlight box coordinates if necessary
#   if (nrow(stat_highlights_data) > 0) {
#     filtered_stat_highlights_data <- subset(stat_highlights_data, criteria_vals == 1)
#     if (nrow(filtered_stat_highlights_data) > 0) {
#       p <- p +
#         geom_point(
#           data = filtered_stat_highlights_data,
#           aes(x = x_vals, y = y_vals),
#           size = 1, shape = 20, show.legend = FALSE
#         ) +
#         labs(x = NULL, y = NULL)
#     }
#   }
#   
#   # Boxes
#   if (projection != "UTM (default)" &&
#       nrow(highlights_data) > 0 &&
#       all(c("x1","x2","y1","y2") %in% names(highlights_data))) {
#     highlights_data <- transform_box_df(
#       df = highlights_data,
#       x1col = "x1", x2col = "x2", y1col = "y1", y2col = "y2",
#       projection = projection, center_lat = center_lat, center_lon = center_lon
#     )
#   }
#   if (nrow(highlights_data) > 0 && all(c("x1","x2","y1","y2","color","type") %in% colnames(highlights_data))) {
#     if (any(highlights_data$type == "Box")) {
#       p <- p +
#         geom_rect(
#           data = subset(highlights_data, type == "Box"),
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, colour = I(color)),
#           fill = NA, size = 1, show.legend = FALSE, inherit.aes = FALSE
#         )
#     }
#     
#     if (any(highlights_data$type == "Filled")) {
#       p <- p +
#         geom_rect(
#           data = subset(highlights_data, type == "Filled"),
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = I(color)),
#           colour = NA, alpha = 0.5, show.legend = FALSE, inherit.aes = FALSE
#         )
#     }
#     
#     if (any(highlights_data$type == "Hatched")) {
#       p <- p +
#         geom_rect_pattern(
#           data = subset(highlights_data, type == "Hatched"),
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
#           pattern = "stripe",
#           pattern_fill = subset(highlights_data, type == "Hatched")$color,
#           pattern_spacing = 0.01, pattern_size = 0.1,
#           pattern_colour = NA, fill = NA, colour = NA,
#           show.legend = FALSE, inherit.aes = FALSE
#         )
#     }
#     
#   }
#   
#   return(p)
# }



plot_map <- function(data_input,
                     lon_lat_range,
                     variable = NULL,
                     mode = NULL,
                     titles = NULL,
                     axis_range = NULL,
                     hide_axis = FALSE,
                     points_data = data.frame(),
                     highlights_data = data.frame(),
                     stat_highlights_data = data.frame(),
                     c_borders = TRUE,
                     white_ocean = FALSE,
                     white_land = FALSE,
                     shpOrder = NULL,
                     plotOrder = NULL,
                     input = NULL,
                     plotType = "default",
                     projection = "UTM (default)",
                     center_lat = 0,
                     center_lon = 0,
                     show_rivers = FALSE,
                     label_rivers = FALSE,
                     show_lakes = FALSE,
                     label_lakes = FALSE,
                     show_mountains = FALSE,
                     label_mountains = FALSE) {
  
  # --- sichere Defaults (verhindert 'v_col'/'v_unit' Fehler) ---
  v_col  <- viridis::viridis(11)
  v_unit <- ""
  title_size <- if (!is.null(titles) && !is.null(titles$map_title_size)) titles$map_title_size else 12
  
  # --- Paletten/Einheiten je nach variable/mode überschreiben (wie zuvor) ---
  if (!is.null(variable)) {
    if (variable == "Temperature") {
      v_col <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
      v_unit <- "\u00B0C"
    } else if (variable == "Precipitation") {
      v_col <- RColorBrewer::brewer.pal(11, "BrBG")
      v_unit <- "mm/mon."
    } else if (variable == "SLP") {
      v_col <- rev(RColorBrewer::brewer.pal(11, "PRGn"))
      v_unit <- "hPa"
    } else if (variable == "Z500") {
      v_col <- rev(RColorBrewer::brewer.pal(11, "PRGn"))
      v_unit <- "m"
    }
  }
  
  if (!is.null(mode)) {
    if (mode == "Correlation") {
      v_col <- rev(RColorBrewer::brewer.pal(11, "PuOr"))
      v_unit <- "r"
      
    } else if (mode == "Regression_coefficients") {
      v_col <- viridis::viridis(11, option = "turbo")
      v_unit <- "Coefficient"
      if (!is.null(titles)) {
        titles$map_title <- titles$map_title_coeff
        titles$map_subtitle <- titles$map_subtitle_coeff
        title_size <- titles$map_title_size_coeff %||% title_size
      }
      
    } else if (mode == "Regression_p_values") {
      v_col <- rev(RColorBrewer::brewer.pal(5, "Greens"))
      v_unit <- "p\nValues"
      if (!is.null(titles)) {
        titles$map_title <- titles$map_title_pvals
        titles$map_subtitle <- titles$map_subtitle_pvals
        title_size <- titles$map_title_size_pvals %||% title_size
      }
      
    } else if (mode == "Regression_residuals") {
      v_unit <- paste("Residuals\n", v_unit)
      if (!is.null(titles)) {
        titles$map_title <- titles$map_title_res
        titles$map_subtitle <- titles$map_subtitle_res
        title_size <- titles$map_title_size_res %||% title_size
      }
      
    } else if (mode == "SD ratio") {
      v_col <- rev(RColorBrewer::brewer.pal(9, "Greens"))
      v_unit <- ""
    }
  }
  
  # --- Achsenbereich schnell & identisch (kein terra::values) ---
  if (is.null(axis_range)) {
    if (!is.null(mode) && mode == "Regression_p_values") {
      axis_range <- c(1e-3, 1)
    } else if (!is.null(mode) && mode == "SD ratio") {
      axis_range <- c(0, 1)
    } else {
      g <- terra::global(data_input, fun = c("min","max"), na.rm = TRUE)
      max_abs_z <- max(abs(unlist(g)))
      axis_range <- c(-max_abs_z, max_abs_z)
    }
  }
  

  # ---- Smart, artifact-safe clipping & simplification ----
  
  .should_clip <- function(lon_lat_range, projection) {
    # never clip for non-UTM projections (orthographic/robin/laea often cross seams)
    if (!identical(projection, "UTM (default)")) return(FALSE)
    
    lon_min <- lon_lat_range[1]; lon_max <- lon_lat_range[2]
    lat_min <- lon_lat_range[3]; lat_max <- lon_lat_range[4]
    
    # treat huge extents as "world": don't clip (prevents continent cuts & seam lines)
    lon_span <- abs(lon_max - lon_min)
    lat_span <- abs(lat_max - lat_min)
    if (lon_span >= 120 || lat_span >= 80) return(FALSE)
    
    # if bbox touches antimeridian or crosses it -> don't clip
    if (lon_min <= -170 || lon_max >= 170) return(FALSE)
    
    TRUE
  }
  
  .safe_crop <- function(x, lon_lat_range, projection) {
    if (is.null(x) || !.should_clip(lon_lat_range, projection)) return(x)
    bbox <- sf::st_bbox(
      c(xmin = lon_lat_range[1], xmax = lon_lat_range[2],
        ymin = lon_lat_range[3], ymax = lon_lat_range[4]),
      crs = sf::st_crs(4326)
    )
    # Crop in planar math to avoid s2/antimeridian artifacts;
    # switch off s2 just for the crop, then restore previous setting.
    old_s2 <- sf::sf_use_s2()
    on.exit(sf::sf_use_s2(old_s2), add = TRUE)
    sf::sf_use_s2(FALSE)
    suppressWarnings({
      y <- tryCatch(sf::st_crop(x, bbox), error = function(e) x)
      # Make valid to prevent hairline issues from invalid rings
      tryCatch(sf::st_make_valid(y), error = function(e) y)
    })
  }
  
  # Simplify only when zoomed in a bit; tolerance tied to bbox span
  .maybe_simplify <- function(x, lon_lat_range, projection) {
    if (is.null(x) || nrow(x) == 0) return(x)
    if (!.should_clip(lon_lat_range, projection)) return(x)
    
    lon_span <- abs(lon_lat_range[2] - lon_lat_range[1])
    # modest tolerance (degrees); smaller at tight zooms, larger at mid zooms
    tol <- max(0.02, min(0.2, lon_span / 200))  # ~0.02°–0.2°
    suppressWarnings(sf::st_simplify(x, dTolerance = tol, preserveTopology = TRUE))
  }
  
  # Unified helper used everywhere
  crop_sf <- function(x) {
    x <- .safe_crop(x, lon_lat_range, projection)
    .maybe_simplify(x, lon_lat_range, projection)
  }
  
  # 1) Crop-only helper: NO simplify (avoids s2 path entirely)
  crop_sf_nosimplify <- function(x) {
    .safe_crop(x, lon_lat_range, projection)
  }
  
  # 2) Robust fixer for uploaded shapefiles (planar, make valid, drop Z/M)
  .fix_user_geom <- function(g) {
    if (is.null(g)) return(g)
    old <- sf::sf_use_s2()
    on.exit(sf::sf_use_s2(old), add = TRUE)
    sf::sf_use_s2(FALSE)  # avoid s2 during repair
    
    g <- tryCatch(sf::st_make_valid(g), error = function(e) g)
    # drop Z/M if present (some historic sets have Z/M that confuse simplification)
    g <- tryCatch(sf::st_zm(g, drop = TRUE, what = "ZM"), error = function(e) g)
    # if it’s a geometry collection, extract polygons/lines safely
    g <- tryCatch(sf::st_collection_extract(g, "GEOMETRY"), error = function(e) g)
    
    g
  }

  r <- data_input
  if (terra::ncell(r) > 3e5) {          
    r <- terra::aggregate(r, fact = 3) 
  }
  
  p <- ggplot() +
    tidyterra::geom_spatraster_contour_filled(
      data = r,                           
      aes(fill = after_stat(level_mid)),
      bins = 20                          
    ) +
    labs(fill = v_unit)
  
  
  # --- Guides nur bauen, wenn nicht versteckt; sichere Größen verwenden ---
  if (isTRUE(hide_axis)) {
    p <- p + guides(fill = "none")
  } else {
    p <- p + guides(
      fill = guide_colorbar(
        barwidth = 2,
        barheight = grid::unit(0.75, "npc"),
        title = v_unit,
        title.position = "top",
        title.hjust = 0.25,
        display = "rectangles",
        draw.ulim = FALSE, draw.llim = FALSE,
        label.theme = ggplot2::element_text(size = title_size / 1.6),
        title.theme = ggplot2::element_text(size = title_size / 1.6),
        frame.colour = "black", frame.linewidth = 0.5,
        ticks.colour = "black", ticks.linewidth = 0.5
      )
    )
  }
  
  # --- Skalen (wie zuvor) ---
  if (!is.null(mode) && mode == "Regression_p_values") {
    p <- p + scale_fill_gradientn(
      colors = v_col, trans = "log10", limits = axis_range,
      breaks = c(0.001, 0.01, 0.1, 1),
      labels = scales::label_comma(accuracy = 0.001)
    )
  } else {
    p <- p + scale_fill_stepsn(
      limits = axis_range, colors = v_col, n.breaks = 20, nice.breaks = TRUE,
      labels = function(breaks) {
        labels <- as.character(round(breaks, 2))
        labels[1] <- paste0("< ", labels[1])
        labels[length(labels)] <- paste0("> ", labels[length(labels)])
        labels
      }
    )
  }
  
  # --- Theme (unverändert semantisch) ---
  p <- p + if (projection == "UTM (default)") ggplot2::theme_bw() else ggplot2::theme_minimal()
  
  # --- statische Layer (vorher bbox-clipping) ---
  p <- p + ggplot2::geom_sf(data = crop_sf(coast), color = "#333333", size = 0.5, inherit.aes = FALSE)
  if (isTRUE(white_ocean)) p <- p + ggplot2::geom_sf(data = crop_sf(oceans), fill = "#DDDDDD", size = 0.5, inherit.aes = FALSE)
  if (isTRUE(white_land))
    p <- p + ggplot2::geom_sf(data = crop_sf_nosimplify(land),
                              fill = "#DDDDDD", color = NA, inherit.aes = FALSE)
  if (isTRUE(c_borders))   p <- p + ggplot2::geom_sf(data = crop_sf(countries), color = "#333333", fill = NA, size = 0.5, inherit.aes = FALSE)
  
  # --- optionale Features (robust gegen NA via isTRUE) ---
  if (isTRUE(show_rivers)) {
    p <- p + ggplot2::geom_sf(data = crop_sf(rivers), color = "#2A52BE", size = 0.2, inherit.aes = FALSE)
    if (isTRUE(label_rivers) && "name" %in% names(rivers)) {
      p <- p + ggplot2::geom_text(
        data = sf::st_centroid(rivers),
        ggplot2::aes(label = name, geometry = geometry),
        stat = "sf_coordinates", size = 5, color = "black", inherit.aes = FALSE
      )
    }
  }
  if (isTRUE(show_lakes)) {
    lk <- crop_sf(lakes)
    p <- p + ggplot2::geom_sf(data = lk, fill = "#DDDDDD", color = NA, inherit.aes = FALSE)
    if (isTRUE(label_lakes) && "name" %in% names(lakes)) {
      p <- p + ggplot2::geom_text(
        data = sf::st_point_on_surface(lakes),
        ggplot2::aes(label = name, geometry = geometry),
        stat = "sf_coordinates", size = 5, color = "black", nudge_x = -1,
        inherit.aes = FALSE
      )
    }
  }
  if (isTRUE(show_mountains)) {
    mt <- crop_sf(mountains)
    p <- p + ggplot2::geom_sf(data = mt, color = "#333333", size = 3, shape = 17, inherit.aes = FALSE)
    if (isTRUE(label_mountains) && "name" %in% names(mountains)) {
      p <- p + ggplot2::geom_text(
        data = mountains,
        ggplot2::aes(label = name, geometry = geometry),
        stat = "sf_coordinates", size = 5, color = "black", nudge_y = 0.75,
        inherit.aes = FALSE
      )
    }
  }
  
  # --- Shapefiles wie gehabt (kein Interface-Change); nur gezeichnet, wenn gesetzt ---
  color_picker_prefix <- plotType
  if (!is.null(shpOrder) && !is.null(plotOrder) && length(shpOrder) > 0 && length(plotOrder) > 0) {
    shpOrder <- shpOrder[!duplicated(shpOrder)]
    selected_files <- plotOrder[match(shpOrder, tools::file_path_sans_ext(basename(plotOrder)))]
    for (file in selected_files) {
      file_name <- tools::file_path_sans_ext(basename(file))
      message(paste("Adding shapefile to plot:", file_name))
      shape <- sf::st_read(file, quiet = TRUE)
      if (is.na(sf::st_crs(shape))) shape <- sf::st_set_crs(shape, sf::st_crs(4326)) else shape <- sf::st_transform(shape, 4326)
      
      shape <- .fix_user_geom(shape)          # <-- new
      shape <- crop_sf_nosimplify(shape)      # <-- new (no simplify for user layers)
      
      col_pick <- input[[paste0(color_picker_prefix, file_name)]]
      if (is.null(col_pick) || is.na(col_pick) || !nzchar(col_pick)) col_pick <- "black"
      
      geom_type <- as.character(sf::st_geometry_type(shape))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        p <- p + ggplot2::geom_sf(data = shape, fill = NA, colour = col_pick, linewidth = 0.5, inherit.aes = FALSE)
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        p <- p + ggplot2::geom_sf(data = shape, colour = col_pick, linewidth = 0.5, inherit.aes = FALSE)
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        p <- p + ggplot2::geom_sf(data = shape, colour = col_pick, size = 2, inherit.aes = FALSE)
      }
    }
  }
  
  # --- Projektion / Ausschnitt (wie zuvor) ---
  if (projection == "Robinson") {
    p <- p + ggplot2::coord_sf(crs = sf::st_crs("+proj=robin"), clip = "on")
  } else if (projection == "Orthographic") {
    formula <- paste0("+proj=ortho +lat_0=", center_lat, " +lon_0=", center_lon,
                      " +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(formula), clip = "on")
  } else if (projection == "LAEA") {
    formula <- paste0("+proj=laea +lat_0=0 +lon_0=0 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(formula), clip = "on")
  } else {
    if (lon_lat_range[1]==lon_lat_range[2]) { lon_lat_range[1] <- lon_lat_range[1]-0.5; lon_lat_range[2] <- lon_lat_range[2]+0.5 }
    if (lon_lat_range[3]==lon_lat_range[4]) { lon_lat_range[3] <- lon_lat_range[3]-0.5; lon_lat_range[4] <- lon_lat_range[4]+0.5 }
    lon_lat_range[1] <- max(lon_lat_range[1], -179.0625)
    lon_lat_range[2] <- min(lon_lat_range[2],  177.1875)
    lon_lat_range[3] <- max(lon_lat_range[3],  -87.64735)
    lon_lat_range[4] <- min(lon_lat_range[4],   87.64735)
    p <- p + ggplot2::coord_sf(xlim = lon_lat_range[1:2], ylim = lon_lat_range[3:4], expand = FALSE, clip = "on")
  }
  
  # --- Titel/Untertitel (robust) ---
  if (!is.null(titles)) {
    if (titles$map_title != " " || titles$map_subtitle != " ") {
      p <- p + ggplot2::labs(
        title = ifelse(titles$map_title != " ", titles$map_title, NULL),
        subtitle = ifelse(titles$map_subtitle != " ", titles$map_subtitle, NULL)
      )
    }
    p <- p + ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(size = title_size, face = "bold", margin = ggplot2::margin(5,0,5,0)),
      plot.subtitle = ggtext::element_textbox_simple(size = title_size/1.3, face = "plain", margin = ggplot2::margin(9,0,12,0)),
      axis.text = ggplot2::element_text(size = title_size / 1.6)
    )
  }
  
  # --- ggplot_build() nur falls wirklich für Punkte nötig ---
  need_points <- (nrow(points_data) > 0 && all(c("x_value","y_value","color","shape","size","label") %in% names(points_data)))
  if (need_points) {
    built <- ggplot_build(p)
    pp <- built$layout$panel_params[[1]]
    get_range <- function(pp, axis = c("x","y")) {
      axis <- match.arg(axis)
      if (!is.null(pp[[paste0(axis, ".range")]])) {
        pp[[paste0(axis, ".range")]]
      } else if (!is.null(pp[[axis]]$range$range)) {
        pp[[axis]]$range$range
      } else if (!is.null(pp[[axis]]$range)) {
        pp[[axis]]$range
      } else c(-Inf, Inf)
    }
    xlim_cur <- get_range(pp, "x")
    ylim_cur <- get_range(pp, "y")
    
    # Projection transform
    if (projection != "UTM (default)") {
      points_data <- transform_points_df(
        df = points_data,
        xcol = "x_value", ycol = "y_value",
        projection = projection, center_lat = center_lat, center_lon = center_lon
      )
    }
    
    points_data$x_value <- as.numeric(points_data$x_value)
    points_data$y_value <- as.numeric(points_data$y_value)
    inside <- with(points_data, !is.na(x_value) & !is.na(y_value) &
                     x_value >= xlim_cur[1] & x_value <= xlim_cur[2] &
                     y_value >= ylim_cur[1] & y_value <= ylim_cur[2])
    points_in <- points_data[inside, , drop = FALSE]
    
    if (nrow(points_in) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = points_in,
          ggplot2::aes(x = x_value, y = y_value, colour = I(color), shape = I(shape), size = I(size)),
          show.legend = FALSE, inherit.aes = FALSE
        ) +
        ggrepel::geom_text_repel(
          data = points_in,
          ggplot2::aes(x = x_value, y = y_value, label = label),
          size = 5, fontface = "bold",
          bg.color = "white", bg.r = 0.2,
          point.padding = 10,
          xlim = xlim_cur, ylim = ylim_cur,
          na.rm = TRUE, max.overlaps = Inf,
          show.legend = FALSE, inherit.aes = FALSE
        )
    }
  }
  
  # --- stat highlights / boxes ---
  if (nrow(stat_highlights_data) > 0) {
    filtered_stat_highlights_data <- subset(stat_highlights_data, criteria_vals == 1)
    if (nrow(filtered_stat_highlights_data) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = filtered_stat_highlights_data,
          ggplot2::aes(x = x_vals, y = y_vals),
          size = 1, shape = 20, show.legend = FALSE
        ) +
        ggplot2::labs(x = NULL, y = NULL)
    }
  }
  
  if (projection != "UTM (default)" &&
      nrow(highlights_data) > 0 &&
      all(c("x1","x2","y1","y2") %in% names(highlights_data))) {
    highlights_data <- transform_box_df(
      df = highlights_data,
      x1col = "x1", x2col = "x2", y1col = "y1", y2col = "y2",
      projection = projection, center_lat = center_lat, center_lon = center_lon
    )
  }
  if (nrow(highlights_data) > 0 && all(c("x1","x2","y1","y2","color","type") %in% colnames(highlights_data))) {
    if (any(highlights_data$type == "Box")) {
      p <- p +
        ggplot2::geom_rect(
          data = subset(highlights_data, type == "Box"),
          ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, colour = I(color)),
          fill = NA, size = 1, show.legend = FALSE, inherit.aes = FALSE
        )
    }
    if (any(highlights_data$type == "Filled")) {
      p <- p +
        ggplot2::geom_rect(
          data = subset(highlights_data, type == "Filled"),
          ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = I(color)),
          colour = NA, alpha = 0.5, show.legend = FALSE, inherit.aes = FALSE
        )
    }
    if (any(highlights_data$type == "Hatched")) {
      p <- p +
        ggpattern::geom_rect_pattern(
          data = subset(highlights_data, type == "Hatched"),
          ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
          pattern = "stripe",
          pattern_fill = subset(highlights_data, type == "Hatched")$color,
          pattern_spacing = 0.01, pattern_size = 0.1,
          pattern_colour = NA, fill = NA, colour = NA,
          show.legend = FALSE, inherit.aes = FALSE
        )
    }
  }
  
  return(p)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' (General) CREATE MAP DATATABLE
#' 
#' Generates a labeled 2D matrix (longitude × latitude) of spatial means from a 3D climate dataset.
#' Uses coordinate subsets and adds directional labels.
#' 
#' @param data_input 3D numeric array. Subset of climate data (e.g., yearly mean or anomaly) with dimensions [lon × lat × time].
#' @param subset_lon_IDs Integer vector. Indices of longitudes to retain (e.g., for regional focus).
#' @param subset_lat_IDs Integer vector. Indices of latitudes to retain.
#'
#' @return 2D labeled numeric matrix (latitude × longitude) with spatial means and coordinate labels for use in maps.

create_map_datatable = function(data_input,
                                subset_lon_IDs,
                                subset_lat_IDs){
  
  # find x,y & z values
  x = lon[subset_lon_IDs]
  y = lat[subset_lat_IDs]
  
  data_mean = apply(data_input,c(1:2),mean) # finds mean of input data
  z = data_mean[,rev(1:length(y))]
  
  # Transpose and rotate z
  map_data =t(z)[order(ncol(z):1),]
  
  # Add degree symbols and cardinal directions for longitude and latitude
  x_labels = ifelse(x >= 0,
                    paste(x, "\u00B0E", sep = ""),
                    paste(abs(x), "\u00B0W", sep = ""))
  y_labels = ifelse(y >= 0,
                    paste(round(y, digits = 3), "\u00B0N", sep = ""),
                    paste(abs(round(y, digits = 3)), "\u00B0S", sep = ""))
  
  # Apply labels to map data
  colnames(map_data) = x_labels
  rownames(map_data) = y_labels
  
  return(map_data)
}


#' (General) CREATE BASIC TIMESERIES DATATABLE

#' Generates a basic timeseries data frame (Year, Mean, Min, Max) from a 3D climate array.
#' Handles both year ranges and discrete year sets.
#' Assumes `latlon_weights`, `lat`, and `lon` are globally defined.
#' Trims outer grid cells and applies area-weighted averaging.
#'
#' @param data_input 3D numeric array. Subset of climate data with dimensions [lon × lat × time].
#' @param year_input Integer vector. Either a year range (start/end) or specific year set.
#' @param year_input_type Character. Either `"range"` or `"set"` indicating type of `year_input`.
#' @param subset_lon_IDs Integer vector. Indices of longitudes to include (excluding outer edge).
#' @param subset_lat_IDs Integer vector. Indices of latitudes to include (excluding outer edge).
#'
#' @return Data frame with columns: `Year`, `Mean` (weighted mean), `Min`, and `Max` values for each time step.

create_timeseries_datatable = function(data_input,
                                       year_input,
                                       year_input_type,
                                       subset_lon_IDs,
                                       subset_lat_IDs){
  
  # Remove outer rows and columns from map data
  cut_data_input = data_input[-c(1,dim(data_input)[1]),-c(1,dim(data_input)[2]),]
  cut_subset_lon_IDs = subset_lon_IDs[-c(1,length(subset_lon_IDs))]
  cut_subset_lat_IDs = subset_lat_IDs[-c(1,length(subset_lat_IDs))]
  
  # Create years column
  if (year_input_type == "range"){
    Year = year_input[1]:year_input[2]
  } else {
    Year = year_input
  }
  
  # Check that cut_data is more than a single point
  if (is.null(dim(cut_data_input))){
    Mean = cut_data_input
    Min = cut_data_input
    Max = cut_data_input
  } 
  else {
    # Calculate weighted Mean column
    latlon_weights_reduced = latlon_weights[cut_subset_lat_IDs,cut_subset_lon_IDs]
    weight_function = function(df,llwr){df_weighted = (df*llwr)/sum(llwr)}
    data_weighted = apply(cut_data_input,c(3),weight_function, t(latlon_weights_reduced))
    Mean = apply(data_weighted,c(2),sum)
    
    # create Min and Max columns
    Min = apply(cut_data_input,c(3),min)
    Max = apply(cut_data_input,c(3),max)
  }
  
  # Create dataframe
  timeseries_data = data.frame(Year,Mean,Min,Max)
  
  return(timeseries_data)
}


#' (General) ADD STATISTICS COLUMNS TO TS DATATABLE
#'
#' Adds optional `Moving_Average` and percentile columns to a timeseries datatable.
#' Supports static or moving percentile bands and allows flexible window/align settings.
#' Assumes normality is tested for dynamic percentile calculation.
#'
#' @param data_input Data frame. Output from `create_timeseries_datatable()`.
#' @param add_moving_average Logical. Whether to add a moving average column.
#' @param moving_average_range Integer. Size of the moving average window (e.g., 5, 11, 21).
#' @param moving_average_alignment Character. One of `"before"`, `"center"`, `"right"`.
#' @param add_percentiles Logical. Whether to add percentile threshold columns.
#' @param percentiles Numeric vector. One or more values among `0.9`, `0.95`, `0.99`.
#' @param use_MA_percentiles Logical. If `TRUE`, percentile bands are centered on the moving average.
#'
#' @return Data frame including original timeseries plus added columns for `Moving_Average` and requested percentiles.

add_stats_to_TS_datatable = function(data_input,
                                     add_moving_average,
                                     moving_average_range,
                                     moving_average_alignment,
                                     add_percentiles,
                                     percentiles,
                                     use_MA_percentiles){
  # Set up variables
  Year = data_input[,1]
  Mean = data_input[,2]
  
  # Create moving average column
  Moving_Average = NULL
  if (add_moving_average == TRUE){
    Moving_Average = zoo::rollmean(Mean, k=moving_average_range, fill=NA, align=moving_average_alignment)
  }
  
  # Create percentile columns
  Percentile_0.005 = NULL ; Percentile_0.995 = NULL
  Percentile_0.025 = NULL ; Percentile_0.975 = NULL
  Percentile_0.05 = NULL ; Percentile_0.95 = NULL
  
  if (add_percentiles == TRUE){
    
    # Check to see if there there are more than 10 year values
    if (length(Year) > 10){
      # Test data for normality
      p_value = shapiro.test(Mean)[[2]]
    } else {
      p_value = 0
    }
    
    # 0.9 Percentile 
    if (0.9 %in% percentiles){
      # Use normal distribution to find percentiles
      if(p_value>0.05){
        upper_percentile_value = qnorm(.95,mean(Mean),sd(Mean))
        lower_percentile_value = qnorm(.05,mean(Mean),sd(Mean))
      }
      # Find percentiles for non-normal data
      else {
        upper_percentile_value = as.numeric(quantile(Mean, .95)) 
        lower_percentile_value = as.numeric(quantile(Mean, .05))
      }
      
      # Constant percentiles  
      if (use_MA_percentiles == FALSE){
        Percentile_0.95 = rep(upper_percentile_value,length(Year))
        Percentile_0.05 = rep(lower_percentile_value,length(Year))
      }
      # Use moving average for percentiles 
      else {
        p_diff = (upper_percentile_value-lower_percentile_value)/2
        Percentile_0.95 = Moving_Average+p_diff
        Percentile_0.05 = Moving_Average-p_diff
      }
    }
    
    # 0.95 Percentile 
    if (0.95 %in% percentiles){
      # Use normal distribution to find percentiles
      if(p_value>0.05){
        upper_percentile_value = qnorm(.975,mean(Mean),sd(Mean))
        lower_percentile_value = qnorm(.025,mean(Mean),sd(Mean))
      }
      # Find percentiles for non-normal data
      else {
        upper_percentile_value = as.numeric(quantile(Mean, .975)) 
        lower_percentile_value = as.numeric(quantile(Mean, .025))
      }
      
      # Constant percentiles  
      if (use_MA_percentiles == FALSE){
        Percentile_0.975 = rep(upper_percentile_value,length(Year))
        Percentile_0.025 = rep(lower_percentile_value,length(Year))
      }
      # Use moving average for percentiles 
      else {
        p_diff = (upper_percentile_value-lower_percentile_value)/2
        Percentile_0.975 = Moving_Average+p_diff
        Percentile_0.025 = Moving_Average-p_diff
      }
    }
    
    # 0.99 Percentile 
    if (0.99 %in% percentiles){
      # Use normal distribution to find percentiles
      if(p_value>0.05){
        upper_percentile_value = qnorm(.995,mean(Mean),sd(Mean))
        lower_percentile_value = qnorm(.005,mean(Mean),sd(Mean))
      }
      # Find percentiles for non-normal data
      else {
        upper_percentile_value = as.numeric(quantile(Mean, .995)) 
        lower_percentile_value = as.numeric(quantile(Mean, .005))
      }
      
      # Constant percentiles  
      if (use_MA_percentiles == FALSE){
        Percentile_0.995 = rep(upper_percentile_value,length(Year))
        Percentile_0.005 = rep(lower_percentile_value,length(Year))
      }
      # Use moving average for percentiles 
      else {
        p_diff = (upper_percentile_value-lower_percentile_value)/2
        Percentile_0.995 = Moving_Average+p_diff
        Percentile_0.005 = Moving_Average-p_diff
      }
    }
  }
  
  # Update Timeseries data
  timeseries_data = data_input
  
  if (!is.null(Moving_Average)){
    timeseries_data = data.frame(timeseries_data,Moving_Average)
  }
  if (!is.null(Percentile_0.005)){
    timeseries_data = data.frame(timeseries_data,Percentile_0.005)
  }
  if (!is.null(Percentile_0.025)){
    timeseries_data = data.frame(timeseries_data,Percentile_0.025)
  }
  if (!is.null(Percentile_0.05)){
    timeseries_data = data.frame(timeseries_data,Percentile_0.05)
  }
  if (!is.null(Percentile_0.95)){
    timeseries_data = data.frame(timeseries_data,Percentile_0.95)
  }
  if (!is.null(Percentile_0.975)){
    timeseries_data = data.frame(timeseries_data,Percentile_0.975)
  }
  if (!is.null(Percentile_0.995)){
    timeseries_data = data.frame(timeseries_data,Percentile_0.995)
  }
  
  return(timeseries_data)
}


#' PLOT TIMESERIES 
#'
#' This function sets up time series plots based on the specified type.
#'
#' @param type Character. "Anomaly", "Composites", "Monthly", "Correlation","Regression_Trend" or "Regression_Residual".
#' @param data data.frame. The main time series data table.
#' @param variable Character. Variable name: "Temperature", "Precipitation", "SLP", "Z500", or "other" (user data).
#' @param data_v1 data.frame. Time series data table for variable1 (only for "Correlation").
#' @param data_v2 data.frame. Time series data table for variable2 (only for "Correlation").
#' @param variable1 Character. Variable name for first correlation dataset (only for "Correlation").
#' @param variable2 Character. Variable name for second correlation dataset (only for "Correlation").
#' @param ref Numeric. Mean value of the reference data (default: NA).
#' @param year_range Numeric vector. Start and end year.
#' @param month_range_1 Numeric vector. Start and end month (only for "Monthly").
#' @param month_range_2 Numeric vector. Start and end month (only for "Monthly").
#' @param titles Character vector. Contains titles, subtitles, axis titles, and custom titles/subtitles.
#' @param show_key Logical. TRUE or FALSE, whether to show a key.
#' @param key_position Character. Position of the key: "top", "bottom", "left", "right", "none".
#' @param show_ticks Show yearly Ticks: Logical. TRUE or FALSE, whether to change ticks
#' @param tick_interval Numeric. Interval of ticks for X Axis
#' @param moving_ave Logical. TRUE or FALSE, whether to show a moving average.
#' @param moving_ave_year Numeric. Number of years for moving average calculation (e.g., 11-year moving average).
#' @param custom_percentile Logical. TRUE or FALSE, whether to show custom percentiles.
#' @param percentiles Numeric. Percentile value (e.g., c(0.9, 0.95, 0.99)).
#' @param highlights data.frame. Data frame containing highlight information.
#' @param lines data.frame. Data frame containing line information.
#' @param points data.frame. Data frame containing point information.
#'
#' @return A ggplot object.

# plot_timeseries <- function(type,
#                             mode = NA,
#                             data = NA,
#                             variable = NA,
#                             data_v1 = NA,
#                             data_v2 = NA,
#                             variable1 = NA,
#                             variable2 = NA,
#                             ref = NA,
#                             year_range = NA,
#                             month_range_1 = NA,
#                             month_range_2 = NA,
#                             titles = NA,
#                             #titles_mode=NA, titles_corr=NA, titles_reg=NA, titles_custom=NA,
#                             show_key = FALSE,
#                             key_position = NA,
#                             show_ref = FALSE,
#                             show_ticks = FALSE,
#                             tick_interval = NA,
#                             moving_ave = FALSE,
#                             moving_ave_year = NA,
#                             custom_percentile = FALSE,
#                             percentiles = NA,
#                             highlights = NA,
#                             lines = NA,
#                             points = NA,
#                             axis_range = NULL)
# {
#   
#   # Create empty dataframes for Lines, Points, Fills and boxes  
#   lines_data = data.frame(matrix(ncol = 8, nrow = 0))
#   colnames(lines_data) = c("ID","label","x","y","lcolor","ltype","lwidth","key_show")
#   
#   points_data = data.frame(matrix(ncol = 8, nrow = 0))
#   colnames(points_data) = c("ID","label","x","y","pcolor","pshape","psize","key_show")
#   
#   fills_data = data.frame(matrix(ncol = 8, nrow = 0))
#   colnames(points_data) = c("ID","label","x1","x2","y1","y2","fill","key_show")
#   
#   boxes_data = data.frame(matrix(ncol = 8, nrow = 0))
#   colnames(points_data) = c("ID","label","x1","x2","y1","y2","bcolor","key_show")
#   
#   # Set up inital row IDs
#   lID = 1 ; pID = 1 ; fID = 1 ; bID = 1
#   
#   # Extract ylimits from data
#   if (type == "Correlation") {
#     y_min = min(data_v1[, 2], na.rm = TRUE)
#     y_max = max(data_v1[, 2], na.rm = TRUE)
#   } else if (type == "Regression_Trend") {
#     y_min = min(data[, c(2, 3)])
#     y_max = max(data[, c(2, 3)])
#   } else if (type == "Regression_Residual") {
#     y_min = min(data[, 4])
#     y_max = max(data[, 4])
#   } else {
#     y_min = min(data[, 2])
#     y_max = max(data[, 2])
#   }
# 
#   # Axis Range Setting with optional padding
#   if (!is.null(axis_range) && length(axis_range) == 2 && !any(is.na(axis_range))) {
#     # Apply padding even to user-specified range
#     y_range <- axis_range[2] - axis_range[1]
#     y_Min <- axis_range[1] - (0.1 * y_range)
#     y_Max <- axis_range[2] + (0.1 * y_range)
#   } else {
#     # Auto mode
#     y_range <- y_max - y_min
#     y_Min <- y_min - (0.1 * y_range)
#     y_Max <- y_max + (0.1 * y_range)
#   }
#   
#   # Edit year range if required:
#   if (type == "Composites"){
#     year_range = range(year_range)
#   }
#   
#   # Add a vertical white line to set axis limits 
#   new_line = data.frame(
#     ID = lID,
#     label = NA,
#     x = year_range[1],
#     y = c(y_Min,y_Max),
#     lcolor = "white",
#     ltype = "solid",
#     lwidth = 1,
#     key_show = FALSE
#   )
#   lines_data = rbind(lines_data,new_line)
#   lID = lID+1
#   
#   # Add custom lines to data:
#   if (!is.null(lines) && nrow(lines) > 0) {
#     
#     for (i in 1:nrow(lines)) {
#       line_data <- lines[i, ]
#       
#       if (line_data$orientation == "Vertical"){
#         x_line = line_data$location
#         y_line = c(y_Min,y_Max)
#       } else { # Orientation == "Horizontal"
#         x_line = year_range
#         y_line = line_data$location
#       }
#       
#       new_line = data.frame(
#         ID = lID,
#         label = line_data$label,
#         x = x_line,
#         y = y_line,
#         lcolor = line_data$color,
#         ltype = line_data$type,
#         lwidth = 1,
#         key_show = line_data$key_show
#       )
#       lines_data = rbind(lines_data,new_line)
#       lID = lID+1
#     }
#   }
#   
#   # Add Percentile Lines
#   if(custom_percentile){
#     # Find column for percentiles
#     if (moving_ave){
#       percentile_column = 6  
#     } else {
#       percentile_column = 5  
#     }
#     
#     # Add lower percentile
#     lower_percentile = data.frame(
#       ID = lID,
#       label = gsub("_", " ",gsub("\\[.*?\\]", "", colnames(data)[percentile_column])),
#       x = data$Year,
#       y = data[,percentile_column],
#       lcolor = "gold2",
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,lower_percentile)
#     lID = lID+1
#     
#     # Add upper percentile
#     upper_percentile = data.frame(
#       ID = lID,
#       label = gsub("_", " ",gsub("\\[.*?\\]", "", colnames(data)[percentile_column+1])),
#       x = data$Year,
#       y = data[,percentile_column+1],
#       lcolor = "firebrick4",
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,upper_percentile)
#     lID = lID+1
#   }
#   
#   # Add main line/points to data:
#   if (type == "Anomaly" || type == "Monthly") {
#     new_line = data.frame(
#       ID = lID,
#       label = paste(generate_title_months(MR = month_range_1), variable, type),
#       x = data$Year,
#       y = data[,2],
#       lcolor = get_variable_properties(variable)$color,
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,new_line)
#     lID = lID+1
#     
#   } else if (type == "Correlation") {
#     new_line = data.frame(
#       ID = lID,
#       label = titles$V1_axis_label,
#       x = data_v1$Year,
#       y = data_v1[,2],
#       lcolor = titles$V1_color,
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,new_line)
#     lID = lID+1
#   } else if (type == "Composites") {
#     new_point = data.frame(
#       ID = pID,
#       label = paste(generate_title_months(MR = month_range_1), variable, type),
#       x = as.double(data$Year),
#       y = data[,2],
#       pcolor = get_variable_properties(variable)$color,
#       pshape = "●",
#       psize = as.integer(4),
#       key_show = TRUE
#     )
#     points_data = rbind(points_data,new_point)
#     pID = pID+1
#   } else if (type =="Regression_Trend"){
#     new_line = data.frame(
#       ID = lID,
#       label = "Original",
#       x = data$Year,
#       y = data[,2],
#       lcolor = titles$color_d,
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,new_line)
#     lID = lID+1  
#   } else if (type =="Regression_Residual"){
#     new_line = data.frame(
#       ID = lID,
#       label = "Residual",
#       x = data$Year,
#       y = data[,4],
#       lcolor = titles$color_d,
#       ltype = "dashed",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,new_line)
#     lID = lID+1  
#   } else {
#     stop("Invalid plot type")
#   }# else if (type == "Regression"){} TBC #
#   
#   # Add secondary line to data:
#   if (type == "Correlation") {
#     # Convert data
#     min_v1 <- y_min
#     max_v1 <- y_max
#     min_v2 <- min(data_v2[,2], na.rm = TRUE)
#     max_v2 <- max(data_v2[,2], na.rm = TRUE)
#     
#     v2_adjusted <- (data_v2[,2] - min_v2) / (max_v2 - min_v2)  # normalize to [0, 1]
#     v2_adjusted <- v2_adjusted * (max_v1 - min_v1) + min_v1     # scale to v1 range
#     
#     # add to lines
#     new_line = data.frame(
#       ID = lID,
#       label = titles$V2_axis_label,
#       x = data_v2$Year,
#       y = v2_adjusted,
#       lcolor = titles$V2_color,
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,new_line)
#     lID = lID+1
#   }
#   
#   if (type =="Regression_Trend"){
#     new_line = data.frame(
#       ID = lID,
#       label = "Trend",
#       x = data$Year,
#       y = data[,3],
#       lcolor = "black",
#       ltype = "solid",
#       lwidth = 1,
#       key_show = TRUE
#     )
#     lines_data = rbind(lines_data,new_line)
#     lID = lID+1  
#   }
#   
#   # Add moving average line to data 
#   if(moving_ave){
#     # Add moving average data
#     if (type=="Correlation"){
#       average_line = data.frame(
#         ID = lID,
#         label = paste0(moving_ave_year,"-Yr Moving Average "),
#         x = data_v1$Year,
#         y = data_v1[,5],
#         lcolor = titles$V1_color,
#         ltype = "solid",
#         lwidth = 1.5,
#         key_show = TRUE
#       )
#       lines_data = rbind(lines_data,average_line)
#       lID = lID+1
#       
#       average_line = data.frame(
#         ID = lID,
#         label = paste0(moving_ave_year,"-Yr Moving Average"),
#         x = data_v2$Year,
#         y = data_v2[,5],
#         lcolor = titles$V2_color,
#         ltype = "solid",
#         lwidth = 1.5,
#         key_show = TRUE
#       )
#       lines_data = rbind(lines_data,average_line)
#       lID = lID+1
#       
#     } else {
#       average_line = data.frame(
#         ID = lID,
#         label = paste0(moving_ave_year,"-Yr Moving Average"),
#         x = data$Year,
#         y = data[,5],
#         lcolor = "black",
#         ltype = "solid",
#         lwidth = 1.5,
#         key_show = TRUE
#       )
#       lines_data = rbind(lines_data,average_line)
#       lID = lID+1
#     }
#   }
#   
#   # Add custom points to data:
#   if (!is.null(points) && nrow(points) > 0) {
#     
#     for (i in 1:nrow(points)) {
#       point_data <- points[i, ]
#       
#       new_point = data.frame(
#         ID = pID,
#         label = point_data$label,
#         x = point_data$x_value,
#         y = point_data$y_value,
#         pcolor = point_data$color,
#         pshape = point_data$shape,
#         psize = point_data$size,
#         key_show = FALSE
#       )
#       points_data = rbind(points_data,new_point)
#       pID = pID+1
#     }
#   }
#   
#   # Add custom highlights to data:
#   if (!is.null(highlights) && nrow(highlights) > 0) {
#     
#     if(any(highlights$type == "Fill")) {
#       fills <- subset(highlights, type == "Fill")
#       
#       for (i in 1:nrow(fills)) {
#         fill_data <- fills[i, ]
#         
#         new_fill = data.frame(
#           ID = fID,
#           label = fill_data$label,
#           x1 = fill_data$x1,
#           x2 = fill_data$x2,
#           y1 = fill_data$y1,
#           y2 = fill_data$y2,
#           fill = fill_data$color,
#           key_show = fill_data$key_show
#         )
#         fills_data = rbind(fills_data,new_fill)
#         fID = fID+1
#       }
#     }
#     
#     if(any(highlights$type == "Box")) {
#       boxes <- subset(highlights, type == "Box")
#       
#       for (i in 1:nrow(boxes)) {
#         box_data <- boxes[i, ]
#         
#         new_box = data.frame(
#           ID = bID,
#           label = box_data$label,
#           x1 = box_data$x1,
#           x2 = box_data$x2,
#           y1 = box_data$y1,
#           y2 = box_data$y2,
#           bcolor = box_data$color,
#           key_show = box_data$key_show
#         )
#         boxes_data = rbind(boxes_data,new_box)
#         bID = bID+1
#       }
#     }
#     
#   }
#   
#   # Plot data 
#   p = ggplot()
#   
#   # Plot fills
#   if (nrow(fills_data>0)){
#     
#     #Plot
#     for (i in 1:(fID-1)){
#       fill_data = subset(fills_data, ID == i)
#       
#       if (fill_data$key_show){ # Show on legend
#         p = p +  geom_rect(
#           data = fill_data,
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = label),
#           alpha = 0.5,
#           inherit.aes = FALSE
#         ) 
#       } else { # Don't show on legend
#         p = p +  geom_rect(
#           data = fill_data,
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
#           fill = fill_data$fill,
#           alpha = 0.5,
#           inherit.aes = FALSE
#         )  
#       }
#     }
#     
#     # Add legend
#     fills_legend = unique(subset(fills_data, key_show == TRUE)[,c("ID","label","fill")])
#     if (!is.null(fills_legend) && nrow(fills_legend)>0){
#       p = p + scale_fill_manual(values = setNames(fills_legend$fill,fills_legend$label))
#     }
#   }
#   
#   # Plot lines
#   if (nrow(lines_data>0)){
#     
#     #Plot
#     for (i in 1:(lID-1)){
#       line_data = subset(lines_data, ID == i)
#       
#       if (line_data$key_show[1]){ # Show on legend
#         p = p + geom_line(
#           data = line_data,
#           aes(x = x, y = y, linetype = label),
#           linewidth = line_data$lwidth[1],
#           color = line_data$lcolor[1]
#         ) 
#       } else { # Don't show on legend
#         p = p + geom_line(
#           data = line_data,
#           aes(x = x, y = y),
#           linetype = line_data$ltype[1],
#           linewidth = line_data$lwidth[1],
#           color = line_data$lcolor[1]
#         ) 
#       }
#     }
#     
#     # Add legend
#     lines_legend = unique(subset(lines_data, key_show == TRUE)[,c("ID","label","ltype")])
#     if (!is.null(lines_legend) && nrow(lines_legend)>0){
#       p = p + scale_linetype_manual(values = setNames(lines_legend$ltype,lines_legend$label))
#     }
#   }
#   
#   # Plot Boxes
#   if (nrow(boxes_data>0)){
#     
#     #Plot
#     for (i in 1:(bID-1)){
#       box_data = subset(boxes_data, ID == i)
#       
#       if (box_data$key_show){ # Show on legend
#         p = p +  geom_rect(
#           data = box_data,
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, color = label),
#           fill = "white", alpha = 0, linewidth = 1,
#           inherit.aes = FALSE
#         ) 
#       } else { # Don't show on legend
#         p = p +  geom_rect(
#           data = box_data,
#           aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
#           color = box_data$bcolor,
#           fill = "white", alpha = 0, linewidth = 1,
#           inherit.aes = FALSE
#         )  
#       }
#     }
#     
#     # Add legend
#     boxes_legend = unique(subset(boxes_data, key_show == TRUE)[,c("ID","label","bcolor")])
#     if (!is.null(boxes_legend) && nrow(boxes_legend)>0){
#       p = p + scale_color_manual(values = setNames(boxes_legend$bcolor,boxes_legend$label))
#     }
#   }
#   
#   # Plot points
#   if (nrow(points_data)>0){
#     
#     for (i in 1:nrow(points_data)){
#       point_data = points_data[i,]
#       
#       if (point_data$key_show){ # Show on legend
#         p = p + geom_point(
#           data = point_data,
#           aes(x = x, y = y, shape = label),
#           size = point_data$psize,
#           color = point_data$pcolor
#         ) 
#       } else { # Don't show on legend
#         p = p + geom_point(
#           data = point_data,
#           aes(x = x, y = y),
#           shape = point_data$pshape,
#           size = point_data$psize,
#           color = point_data$pcolor
#         )
#       }  
#       # Add labels
#       if (type == "Composites"){start_ID = 1} else {start_ID = 0}
#       
#       if (point_data$ID > start_ID) {
#         p <- p + geom_text(
#           data = point_data,
#           aes(x = x, y = y, label = label),
#           vjust = -1.5,   # vertical adjustment
#           hjust = 0.5,  # horizontal adjustment
#           size = 5,     # text size
#           color = "black"  # or any color you want
#         )
#       }
#     }
#     
#     # Add legend
#     points_legend = unique(subset(points_data, key_show == TRUE)[,c("ID","label","pshape")])
#     if (!is.null(points_legend) && nrow(points_legend)>0){
#       p = p + scale_shape_manual(values = setNames(points_legend$pshape,points_legend$label))
#     }
#   }
#   
#   
#   # Add theme
#   p <- p + theme_bw(base_size = 18)
#   
#   # Add title and subtitle if provided
#   if (!is.null(titles)) {
#     p <- p + labs(x = "Year", y = titles$ts_axis)
#     
#     # Add secondary axis for correlation
#     if (type =="Correlation"){
#       p = p + scale_y_continuous(
#         name = sub(";.*$", "", titles$V1_axis_label),
#         limits = c(y_Min,y_Max),expand = c(0, 0),
#         sec.axis = sec_axis(~ ( . - min_v1) / (max_v1 - min_v1) * (max_v2 - min_v2) + min_v2,
#                             name = sub(";.*$", "", titles$V2_axis_label)) # Shift and scale the secondary axis
#       ) +
#         
#         theme(
#           axis.title.y = element_text(color = titles$V1_color),
#           axis.title.y.right = element_text(color = titles$V2_color)
#         ) 
#     }
#     
#     # Get rid of titles for Regression Residual TS
#     if (type =="Regression_Residual"){
#       titles$ts_title = "  "
#       titles$ts_subtitle = "  "
#     }
#     
#     if (titles$ts_title != " ") {
#       p <- p + ggtitle(titles$ts_title)
#     }
#     
#     # if (!is.null(titles$ts_subtitle)) {
#     #   p <- p + labs(subtitle = titles$ts_subtitle)
#     # }
#     
#     if (!is.null(titles$ts_subtitle) && !is.na(titles$ts_subtitle) && titles$ts_subtitle != "") {
#       p <- p + labs(subtitle = titles$ts_subtitle)
#     }
#     
#     p <- p + theme(
#       plot.title = element_text(size = titles$ts_title_size, face = "bold"),
#       plot.subtitle = element_text(size = titles$ts_title_size / 1.3, face = "plain"),
#       axis.text=element_text(size = titles$ts_title_size / 1.6),
#     )
#   } 
#   
#   # Set legend groupings
#   if (type == "Composites") {
#     p <- p + guides(
#       fill = guide_legend(title = NULL, order = 3),
#       color = guide_legend(title = NULL, order = 4),
#       linetype = guide_legend(title = NULL, order = 2),
#       shape = guide_legend(title = "Legend", order = 1)
#     )
#   } else {
#     p <- p + guides(
#       fill = guide_legend(title = NULL, order = 3),
#       color = guide_legend(title = NULL, order = 4),
#       linetype = guide_legend(title = "Legend", order = 1),
#       shape = guide_legend(title = NULL, order = 2)
#     )
#   }
#   
#   # Set legend visibility and position
#   if (show_key) {
#     p <- p + theme(legend.position = key_position)  # Apply the position from `key_position`
#   } else {
#     p <- p + theme(legend.position = "none")  # Remove the legend if `show_key == FALSE`
#   }
#   
#   # Optional: If you want to make sure legends don’t appear for other components when `show_key == FALSE`
#   if (!show_key) {
#     p <- p + guides(fill= "none",color= "none", linetype = "none",shape= "none")  # Ensure guides are turned off
#   }
#   
#   # Remove whitespace
#   p <- p + scale_x_continuous(limits = year_range,expand = c(0, 0))
#   if(type != "Correlation"){
#     p = p + scale_y_continuous(limits = c(y_Min,y_Max),expand = c(0, 0))
#   }
#   
#   # Add tick marks
#   if (show_ticks) {
#     p <- p + scale_x_continuous(
#       limits = year_range,
#       expand = c(0, 0),
#       breaks = seq(year_range[1], year_range[2], by = tick_interval)
#     )
#   } else {
#     p <- p + scale_x_continuous(
#       limits = year_range,
#       expand = c(0, 0)
#     )
#   }
#   
#   # Add reference line
#   if (show_ref){
#     p = p + 
#       geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +  
#       annotate("text", 
#                x = year_range[1] + diff(year_range)/25.5,  # mimic text_x offset
#                y = 0,                                      # label at line
#                label = paste0(ref, titles$v_unit),
#                vjust = -0.5,                               # just above line
#                size = 5)
#   }
#   
#   # Return the final plot
#   return(p)
# }

plot_timeseries <- function(type,
                            mode = NA,
                            data = NA,
                            variable = NA,
                            data_v1 = NA,
                            data_v2 = NA,
                            variable1 = NA,
                            variable2 = NA,
                            ref = NA,
                            year_range = NA,
                            month_range_1 = NA,
                            month_range_2 = NA,
                            titles = NA,
                            #titles_mode=NA, titles_corr=NA, titles_reg=NA, titles_custom=NA,
                            show_key = FALSE,
                            key_position = NA,
                            show_ref = FALSE,
                            show_ticks = FALSE,
                            tick_interval = NA,
                            moving_ave = FALSE,
                            moving_ave_year = NA,
                            custom_percentile = FALSE,
                            percentiles = NA,
                            highlights = NA,
                            lines = NA,
                            points = NA,
                            axis_range = NULL)
{
  # ---------- tiny helpers (local; no API change) ----------
  nz <- function(x) !is.null(x) && length(x) > 0 && !all(is.na(x))
  safe_min <- function(x) suppressWarnings(min(x, na.rm = TRUE))
  safe_max <- function(x) suppressWarnings(max(x, na.rm = TRUE))
  
  # ---------- Init containers ----------
  lines_data  <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(lines_data) <- c("ID","label","x","y","lcolor","ltype","lwidth","key_show")
  
  points_data <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(points_data) <- c("ID","label","x","y","pcolor","pshape","psize","key_show")
  
  fills_data  <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(fills_data) <- c("ID","label","x1","x2","y1","y2","fill","key_show")  # FIX
  
  boxes_data  <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(boxes_data) <- c("ID","label","x1","x2","y1","y2","bcolor","key_show") # FIX
  
  lID <- 1 ; pID <- 1 ; fID <- 1 ; bID <- 1
  
  # ---------- Y-range detection (fast & robust) ----------
  if (identical(type, "Correlation")) {
    y_min <- safe_min(data_v1[, 2])
    y_max <- safe_max(data_v1[, 2])
  } else if (identical(type, "Regression_Trend")) {
    y_min <- safe_min(data[, c(2, 3)])
    y_max <- safe_max(data[, c(2, 3)])
  } else if (identical(type, "Regression_Residual")) {
    y_min <- safe_min(data[, 4])
    y_max <- safe_max(data[, 4])
  } else {
    y_min <- safe_min(data[, 2])
    y_max <- safe_max(data[, 2])
  }
  
  # Axis Range Setting with optional padding
  if (!is.null(axis_range) && length(axis_range) == 2 && !any(is.na(axis_range))) {
    y_range <- axis_range[2] - axis_range[1]
    if (!is.finite(y_range) || y_range == 0) y_range <- 1
    y_Min <- axis_range[1] - (0.1 * y_range)
    y_Max <- axis_range[2] + (0.1 * y_range)
  } else {
    y_range <- y_max - y_min
    if (!is.finite(y_range) || y_range == 0) y_range <- 1
    y_Min <- y_min - (0.1 * y_range)
    y_Max <- y_max + (0.1 * y_range)
  }
  
  # Edit year range if required:
  if (identical(type, "Composites") && nz(year_range)) {
    year_range <- range(year_range, na.rm = TRUE)
  }
  
  # ---------- "white" line to enforce axis limits (unchanged semantics) ----------
  if (nz(year_range)) {
    new_line <- data.frame(
      ID = lID,
      label = NA,
      x = year_range[1],
      y = c(y_Min, y_Max),
      lcolor = "white",
      ltype  = "solid",
      lwidth = 1,
      key_show = FALSE
    )
    lines_data <- rbind(lines_data, new_line)
    lID <- lID + 1
  }
  
  # ---------- Custom lines (vectorize input; no plot loops) ----------
  if (nz(lines) && nrow(lines) > 0 && nz(year_range)) {
    lin <- lines
    is_vert <- lin$orientation == "Vertical"
    # vertical: x constant, y spans; horizontal: y constant, x spans
    if (any(is_vert, na.rm = TRUE)) {
      nl <- data.frame(
        ID = lID + seq_len(sum(is_vert)) - 1,
        label = lin$label[is_vert],
        x     = lin$location[is_vert],
        y     = I(Map(function(y) c(y_Min, y_Max), lin$location[is_vert])), # dummy; replaced below
        lcolor= lin$color[is_vert],
        ltype = lin$type[is_vert],
        lwidth= 1,
        key_show = lin$key_show[is_vert]
      )
      # expand rows into 2 rows per ID for y-min/max
      nl_exp <- do.call(rbind, lapply(seq_len(nrow(nl)), function(i) {
        r <- nl[i, ]
        r1 <- r; r1$y <- y_Min
        r2 <- r; r2$y <- y_Max
        rbind(r1, r2)
      }))
      lines_data <- rbind(lines_data, nl_exp)
      lID <- lID + sum(is_vert)
    }
    if (any(!is_vert, na.rm = TRUE)) {
      nl <- data.frame(
        ID = lID + seq_len(sum(!is_vert)) - 1,
        label = lin$label[!is_vert],
        x     = NA_real_,
        y     = lin$location[!is_vert],
        lcolor= lin$color[!is_vert],
        ltype = lin$type[!is_vert],
        lwidth= 1,
        key_show = lin$key_show[!is_vert]
      )
      # expand into 2 points per line for x range
      nl_exp <- do.call(rbind, lapply(seq_len(nrow(nl)), function(i) {
        r <- nl[i, ]
        r1 <- r; r1$x <- year_range[1]
        r2 <- r; r2$x <- year_range[2]
        rbind(r1, r2)
      }))
      lines_data <- rbind(lines_data, nl_exp)
      lID <- lID + sum(!is_vert)
    }
  }
  
  # ---------- Percentile lines (only if requested) ----------
  if (isTRUE(custom_percentile) && nz(data)) {
    percentile_column <- if (isTRUE(moving_ave)) 6 else 5
    if (ncol(data) >= percentile_column + 1) {
      lower_percentile <- data.frame(
        ID = lID,
        label = gsub("_", " ", gsub("\\[.*?\\]", "", colnames(data)[percentile_column])),
        x = data$Year,
        y = data[, percentile_column],
        lcolor = "gold2",
        ltype  = "solid",
        lwidth = 1,
        key_show = TRUE
      )
      lines_data <- rbind(lines_data, lower_percentile); lID <- lID + 1
      
      upper_percentile <- data.frame(
        ID = lID,
        label = gsub("_", " ", gsub("\\[.*?\\]", "", colnames(data)[percentile_column + 1])),
        x = data$Year,
        y = data[, percentile_column + 1],
        lcolor = "firebrick4",
        ltype  = "solid",
        lwidth = 1,
        key_show = TRUE
      )
      lines_data <- rbind(lines_data, upper_percentile); lID <- lID + 1
    }
  }
  
  # ---------- Main series ----------
  if (identical(type, "Anomaly") || identical(type, "Monthly")) {
    new_line <- data.frame(
      ID = lID,
      label = paste(generate_title_months(MR = month_range_1), variable, type),
      x = data$Year,
      y = data[, 2],
      lcolor = get_variable_properties(variable)$color,
      ltype  = "solid",
      lwidth = 1,
      key_show = TRUE
    )
    lines_data <- rbind(lines_data, new_line); lID <- lID + 1
    
  } else if (identical(type, "Correlation")) {
    new_line <- data.frame(
      ID = lID,
      label = titles$V1_axis_label,
      x = data_v1$Year,
      y = data_v1[, 2],
      lcolor = titles$V1_color,
      ltype  = "solid",
      lwidth = 1,
      key_show = TRUE
    )
    lines_data <- rbind(lines_data, new_line); lID <- lID + 1
    
  } else if (identical(type, "Composites")) {
    new_point <- data.frame(
      ID = pID,
      label = paste(generate_title_months(MR = month_range_1), variable, type),
      x = as.double(data$Year),
      y = data[, 2],
      pcolor = get_variable_properties(variable)$color,
      pshape = "●",
      psize  = as.integer(4),
      key_show = TRUE
    )
    points_data <- rbind(points_data, new_point); pID <- pID + 1
    
  } else if (identical(type, "Regression_Trend")) {
    new_line <- data.frame(
      ID = lID,
      label = "Original",
      x = data$Year,
      y = data[, 2],
      lcolor = titles$color_d,
      ltype  = "solid",
      lwidth = 1,
      key_show = TRUE
    )
    lines_data <- rbind(lines_data, new_line); lID <- lID + 1
    
  } else if (identical(type, "Regression_Residual")) {
    new_line <- data.frame(
      ID = lID,
      label = "Residual",
      x = data$Year,
      y = data[, 4],
      lcolor = titles$color_d,
      ltype  = "dashed",
      lwidth = 1,
      key_show = TRUE
    )
    lines_data <- rbind(lines_data, new_line); lID <- lID + 1
    
  } else {
    stop("Invalid plot type")
  }
  
  # ---------- Secondary line for Correlation ----------
  if (identical(type, "Correlation")) {
    min_v1 <- y_min; max_v1 <- y_max
    min_v2 <- safe_min(data_v2[, 2]); max_v2 <- safe_max(data_v2[, 2])
    
    # normalize to v1 range
    denom <- (max_v2 - min_v2); if (!is.finite(denom) || denom == 0) denom <- 1
    v2_adjusted <- ((data_v2[, 2] - min_v2) / denom) * (max_v1 - min_v1) + min_v1
    
    new_line <- data.frame(
      ID = lID,
      label = titles$V2_axis_label,
      x = data_v2$Year,
      y = v2_adjusted,
      lcolor = titles$V2_color,
      ltype  = "solid",
      lwidth = 1,
      key_show = TRUE
    )
    lines_data <- rbind(lines_data, new_line); lID <- lID + 1
  }
  
  # ---------- Trend overlay for Regression_Trend ----------
  if (identical(type, "Regression_Trend")) {
    new_line <- data.frame(
      ID = lID,
      label = "Trend",
      x = data$Year,
      y = data[, 3],
      lcolor = "black",
      ltype  = "solid",
      lwidth = 1,
      key_show = TRUE
    )
    lines_data <- rbind(lines_data, new_line); lID <- lID + 1
  }
  
  # ---------- Moving averages ----------
  if (isTRUE(moving_ave)) {
    if (identical(type, "Correlation")) {
      average_line <- data.frame(
        ID = lID,
        label = paste0(moving_ave_year, "-Yr Moving Average "),
        x = data_v1$Year,
        y = data_v1[, 5],
        lcolor = titles$V1_color,
        ltype  = "solid",
        lwidth = 1.5,
        key_show = TRUE
      )
      lines_data <- rbind(lines_data, average_line); lID <- lID + 1
      
      average_line <- data.frame(
        ID = lID,
        label = paste0(moving_ave_year, "-Yr Moving Average"),
        x = data_v2$Year,
        y = data_v2[, 5],
        lcolor = titles$V2_color,
        ltype  = "solid",
        lwidth = 1.5,
        key_show = TRUE
      )
      lines_data <- rbind(lines_data, average_line); lID <- lID + 1
      
    } else {
      average_line <- data.frame(
        ID = lID,
        label = paste0(moving_ave_year, "-Yr Moving Average"),
        x = data$Year,
        y = data[, 5],
        lcolor = "black",
        ltype  = "solid",
        lwidth = 1.5,
        key_show = TRUE
      )
      lines_data <- rbind(lines_data, average_line); lID <- lID + 1
    }
  }
  
  # ---------- Custom points ----------
  if (nz(points) && nrow(points) > 0) {
    pts <- data.frame(
      ID     = pID + seq_len(nrow(points)) - 1,
      label  = points$label,
      x      = points$x_value,
      y      = points$y_value,
      pcolor = points$color,
      pshape = points$shape,
      psize  = points$size,
      key_show = FALSE
    )
    points_data <- rbind(points_data, pts)
    pID <- pID + nrow(points)
  }
  
  # ---------- Highlights: fills & boxes ----------
  if (nz(highlights) && nrow(highlights) > 0) {
    if (any(highlights$type == "Fill", na.rm = TRUE)) {
      fills <- subset(highlights, type == "Fill")
      new_f <- data.frame(
        ID = fID + seq_len(nrow(fills)) - 1,
        label = fills$label,
        x1 = fills$x1, x2 = fills$x2,
        y1 = fills$y1, y2 = fills$y2,
        fill = fills$color,
        key_show = fills$key_show
      )
      fills_data <- rbind(fills_data, new_f)
      fID <- fID + nrow(fills)
    }
    if (any(highlights$type == "Box", na.rm = TRUE)) {
      boxes <- subset(highlights, type == "Box")
      new_b <- data.frame(
        ID = bID + seq_len(nrow(boxes)) - 1,
        label = boxes$label,
        x1 = boxes$x1, x2 = boxes$x2,
        y1 = boxes$y1, y2 = boxes$y2,
        bcolor = boxes$color,
        key_show = boxes$key_show
      )
      boxes_data <- rbind(boxes_data, new_b)
      bID <- bID + nrow(boxes)
    }
  }
  
  # ---------- Build plot (vectorized geoms; no per-ID loops) ----------
  p <- ggplot()
  
  # Fills
  if (nrow(fills_data) > 0) {
    # legend entries
    fills_leg <- subset(fills_data, key_show %in% TRUE)
    if (nrow(fills_leg) > 0) {
      p <- p + geom_rect(
        data = fills_leg,
        aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = label),
        alpha = 0.5, inherit.aes = FALSE
      )
      # manual legend mapping
      p <- p + scale_fill_manual(
        values = setNames(fills_leg$fill, fills_leg$label)
      )
    }
    # non-legend entries
    fills_noleg <- subset(fills_data, !key_show %in% TRUE)
    if (nrow(fills_noleg) > 0) {
      p <- p + geom_rect(
        data = fills_noleg,
        aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
        fill = fills_noleg$fill, alpha = 0.5,
        inherit.aes = FALSE, show.legend = FALSE
      )
    }
  }
  
  # Lines
  if (nrow(lines_data) > 0) {
    # legend lines (map linetype by label; linewidth from data; color per row)
    l_leg <- subset(lines_data, key_show %in% TRUE)
    if (nrow(l_leg) > 0) {
      p <- p + geom_line(
        data = l_leg,
        aes(x = x, y = y, group = ID,
            linetype = label,
            linewidth = lwidth,
            colour = I(lcolor))
      )
      map_types <- unique(l_leg[, c("label","ltype")])
      p <- p + scale_linetype_manual(values = setNames(map_types$ltype, map_types$label)) +
        scale_linewidth_identity()
    }
    
    # non-legend lines (use each row’s ltype/width; no legend)
    l_noleg <- subset(lines_data, !key_show %in% TRUE)
    if (nrow(l_noleg) > 0) {
      p <- p + geom_line(
        data = l_noleg,
        aes(x = x, y = y, group = ID,
            linetype = I(ltype),
            linewidth = I(lwidth),
            colour = I(lcolor)),
        show.legend = FALSE
      ) + scale_linewidth_identity()
    }
  }
  
  
  # Boxes
  if (nrow(boxes_data) > 0) {
    b_leg <- subset(boxes_data, key_show %in% TRUE)
    if (nrow(b_leg) > 0) {
      p <- p + geom_rect(
        data = b_leg,
        aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, color = label),
        fill = "white", alpha = 0, linewidth = 1, inherit.aes = FALSE
      )
      # manual legend mapping
      map_cols <- unique(b_leg[, c("label","bcolor")])
      p <- p + scale_color_manual(values = setNames(map_cols$bcolor, map_cols$label))
    }
    b_noleg <- subset(boxes_data, !key_show %in% TRUE)
    if (nrow(b_noleg) > 0) {
      p <- p + geom_rect(
        data = b_noleg,
        aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
        color = b_noleg$bcolor, fill = "white", alpha = 0, linewidth = 1,
        inherit.aes = FALSE, show.legend = FALSE
      )
    }
  }
  
  # Points
  if (nrow(points_data) > 0) {
    pt_leg <- subset(points_data, key_show %in% TRUE)
    if (nrow(pt_leg) > 0) {
      p <- p + geom_point(
        data = pt_leg,
        aes(x = x, y = y, shape = label),
        size = pt_leg$psize, color = pt_leg$pcolor
      )
      map_shapes <- unique(pt_leg[, c("label","pshape")])
      p <- p + scale_shape_manual(values = setNames(map_shapes$pshape, map_shapes$label))
    }
    pt_noleg <- subset(points_data, !key_show %in% TRUE)
    if (nrow(pt_noleg) > 0) {
      p <- p + geom_point(
        data = pt_noleg,
        aes(x = x, y = y),
        shape = pt_noleg$pshape, size = pt_noleg$psize, color = pt_noleg$pcolor,
        show.legend = FALSE
      )
    }
    
    # Labels for composites or custom points (same semantics)
    start_ID <- if (identical(type, "Composites")) 1 else 0
    lab_df <- subset(points_data, ID > start_ID & nz(label))
    if (nrow(lab_df) > 0) {
      p <- p + geom_text(
        data = lab_df,
        aes(x = x, y = y, label = label),
        vjust = -1.5, hjust = 0.5, size = 5, color = "black", show.legend = FALSE
      )
    }
  }
  
  # Theme
  p <- p + theme_bw(base_size = 18)
  
  # Axes, titles, subtitles, secondary axis (Correlation)
  if (!is.null(titles)) {
    p <- p + labs(x = "Year", y = titles$ts_axis)
    
    if (identical(type, "Correlation")) {
      # min_v1/max_v1/min_v2/max_v2 were set above in correlation branch
      p <- p + scale_y_continuous(
        name   = sub(";.*$", "", titles$V1_axis_label),
        limits = c(y_Min, y_Max), expand = c(0, 0),
        sec.axis = sec_axis(~ ( . - min_v1) / (max_v1 - min_v1) * (max_v2 - min_v2) + min_v2,
                            name = sub(";.*$", "", titles$V2_axis_label))
      ) +
        theme(
          axis.title.y = element_text(color = titles$V1_color),
          axis.title.y.right = element_text(color = titles$V2_color)
        )
    }
    
    if (identical(type, "Regression_Residual")) {
      titles$ts_title <- "  "
      titles$ts_subtitle <- "  "
    }
    
    if (!is.null(titles$ts_title) && titles$ts_title != " ") {
      p <- p + ggtitle(titles$ts_title)
    }
    if (!is.null(titles$ts_subtitle) && !is.na(titles$ts_subtitle) && nz(titles$ts_subtitle) && titles$ts_subtitle != "") {
      p <- p + labs(subtitle = titles$ts_subtitle)
    }
    
    p <- p + theme(
      plot.title = element_text(size = titles$ts_title_size, face = "bold"),
      plot.subtitle = element_text(size = titles$ts_title_size / 1.3, face = "plain"),
      axis.text = element_text(size = titles$ts_title_size / 1.6)
    )
  }
  
  # Legend grouping & visibility (unchanged behavior)
  if (identical(type, "Composites")) {
    p <- p + guides(
      fill = guide_legend(title = NULL, order = 3),
      color = guide_legend(title = NULL, order = 4),
      linetype = guide_legend(title = NULL, order = 2),
      shape = guide_legend(title = "Legend", order = 1)
    )
  } else {
    p <- p + guides(
      fill = guide_legend(title = NULL, order = 3),
      color = guide_legend(title = NULL, order = 4),
      linetype = guide_legend(title = "Legend", order = 1),
      shape = guide_legend(title = NULL, order = 2)
    )
  }
  
  if (isTRUE(show_key)) {
    if (!is.null(key_position)) p <- p + theme(legend.position = key_position)
  } else {
    p <- p + theme(legend.position = "none") +
      guides(fill = "none", color = "none", linetype = "none", shape = "none")
  }
  
  # X/Y limits and ticks (avoid duplicate scale_x calls)
  p <- p + scale_x_continuous(limits = year_range, expand = c(0, 0))
  if (!identical(type, "Correlation")) {
    p <- p + scale_y_continuous(limits = c(y_Min, y_Max), expand = c(0, 0))
  }
  if (isTRUE(show_ticks) && nz(tick_interval)) {
    p <- p + scale_x_continuous(
      limits = year_range, expand = c(0, 0),
      breaks = seq(year_range[1], year_range[2], by = tick_interval)
    )
  }
  
  # Reference line (unchanged)
  if (isTRUE(show_ref)) {
    p <- p +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 1) +
      annotate("text",
               x = year_range[1] + diff(year_range)/25.5,
               y = 0,
               label = paste0(ref, titles$v_unit),
               vjust = -0.5,
               size = 5)
  }
  
  return(p)
}


#' (General) GET VARIABLE PROPERTIES
#'
#' Returns display properties for a climate variable, including its unit and color.
#' Supports alternate color schemes for secondary variables in correlation plots.
#' Useful for consistent labeling and theming in plots.
#'
#' @param variable Character. Name of the variable (e.g., `"Temperature"`, `"Precipitation"`, `"SLP"`, `"Z500"`).
#' @param secondary Logical. If `TRUE`, returns alternate color scheme for secondary variables (default `FALSE`).
#'
#' @return Named list with:
#'   - `unit`: Character string (e.g., `"°C"`, `"mm/month"`).
#'   - `color`: Character string with a base R color name.


get_variable_properties <- function(variable,
                                    secondary = FALSE) {
  # Determine unit
  unit <- switch(variable,
                 "Temperature" = "\u00B0C",
                 "Precipitation" = "mm/month",
                 "SLP" = "hPa",
                 "Z500" = "m",
                 "other" = "units")  # Default for custom variables
  
  # Determine color
  if (secondary == FALSE){
    color <- switch(variable,
                    "Temperature" = "red3",
                    "Precipitation" = "turquoise4",
                    "SLP" = "purple4",
                    "Z500" = "green4",
                    "other" = "darkorange2",  # Default for custom variables
                    "black")  # Fallback
  } else { #use different colors for Correlation if the variables are the same
    color <- switch(variable,
                    "Temperature" = "red2",
                    "Precipitation" = "turquoise2",
                    "SLP" = "purple2",
                    "Z500" = "green2",
                    "other" = "saddlebrown",  # Default for custom variables
                    "black")  # Fallback
  }
  
  # Return both unit and color
  return(list(unit = unit, color = color))
}


#' (General) REWRITE MAPTABLE
#'
#' Reformats a labeled map data table by stripping degree symbols and inserting numeric lat/lon values.
#' Handles correlation/regression cases where subset IDs are `NA`.
#' Used for exporting or post-processing tables for CSV or Excel.
#'
#' @param maptable Matrix or data frame. Output from `create_map_datatable()` with labeled row/column names.
#' @param subset_lon_IDs Numeric vector or `NA`. Longitude indices used in the original subset. Set to `NA` for correlation/regression mode.
#' @param subset_lat_IDs Numeric vector or `NA`. Latitude indices used in the original subset. Set to `NA` for correlation/regression mode.
#'
#' @return Matrix. Reformatted version of `maptable` with numeric lat/lon row and column headers.

rewrite_maptable = function(maptable,
                            subset_lon_IDs,
                            subset_lat_IDs){
  
  if (is.na(subset_lon_IDs[1])){
    rnames = rownames(maptable)
    cnames = colnames(maptable)
    
    maptable1 = rbind(as.numeric(substr(cnames, 1, nchar(cnames) - 1)),as.matrix(maptable))
    maptable2 = cbind(c("Lat/Lon", as.numeric(substr(rnames, 1, nchar(rnames) - 1))), maptable1)
    colnames(maptable2)<- NULL
    
  } else {
    maptable1 = rbind(round(lon[subset_lon_IDs],digits = 3),as.matrix(maptable))
    maptable2 = cbind(c("Lat/Lon", round(lat[subset_lat_IDs],digits = 3)), maptable1)
  }
  
  return(maptable2)
}


#' (General) REWRITE TS TABLE
#'
#' Reformats a timeseries datatable by rounding values and appending units to column headers.
#' Variable-specific units are applied to all columns except the year.
#' Intended for display or export of processed timeseries data.
#'
#' @param tstable Data frame. Output from `create_timeseries_datatable()` or similar.
#' @param variable Character. Name of the variable (`"Temperature"`, `"Precipitation"`, `"SLP"`, `"Z500"`, or custom).
#'
#' @return Data frame. Rounded timeseries table with units included in column names.

rewrite_tstable = function(tstable,
                           variable){
  
  # Create units
  if (variable == "Temperature"){
    v_unit = "[\u00B0C]"
  } else if (variable == "Precipitation"){
    v_unit = "[mm/month]"
  } else if (variable == "SLP"|variable == "Z500"){
    v_unit = "[m]"
  } else {
    v_unit = ""
  }
  
  newnames = paste(colnames(tstable),"",v_unit, sep = "")
  colnames(tstable) = c("Year",newnames[-1])
  
  new_tstable = round(tstable, 2)
  
  return(new_tstable)
}


#' (General) LOAD MODE-RA SOURCE DATA
#'
#' Loads ModE-RA feedback source data for a given year and season.
#' Assumes input files are stored in `data/feedback_archive_fin/` with filenames matching the pattern `season + year + .csv`.
#'
#' @param year Integer. The year of interest (e.g., `1972`).
#' @param season Character. Either `"summer"` or `"winter"` (lowercase).
#'
#' @return Data frame. Contents of the source data CSV file for the selected year and season.


load_modera_source_data = function(year,
                                   season){
  # Load data
  feedback_data = read.csv(paste0("data/feedback_archive_fin/",season,year,".csv"))  
}


#' (General) PLOT MODE-RA SOURCES
#'
#' Creates a map of ModE-RA observation sources for a given year and season.
#' Supports full-globe or zoomed plotting using a bounding box.
#'
#' @param ME_source_data Data frame. Source data loaded via `load_modera_source_data()`.
#' @param year Integer. Year of interest (e.g., `1972`).
#' @param season Character. Either `"summer"` or `"winter"`.
#' @param minmax_lonlat Numeric vector of length 4. Bounding box: `c(min_lon, max_lon, min_lat, max_lat)`.
#' @param base_size Numeric (optional). Base font size for plot theme.
#'
#' @return A `ggplot` object displaying ModE-RA sources by type and variable.

plot_modera_sources = function(ME_source_data,
                               year,
                               season,
                               minmax_lonlat,
                               base_size = NULL){
  
  # Load data
  world = map_data("world")
  
  # Sum total sources and observations
  if (identical(minmax_lonlat, c(-180, 180, -90, 90))) {
    total_observations = sum(ME_source_data$Omitted_Duplicates) + nrow(ME_source_data)
    visible_sources = nrow(ME_source_data)
    cut_sources = sum(ME_source_data$Omitted_Duplicates)
  } else {
    in_boundary = ME_source_data$LON > minmax_lonlat[1] & ME_source_data$LON < minmax_lonlat[2] &
      ME_source_data$LAT > minmax_lonlat[3] & ME_source_data$LAT < minmax_lonlat[4]
    total_observations = sum(ME_source_data$Omitted_Duplicates[in_boundary]) + sum(in_boundary)
    visible_sources = sum(in_boundary)
    cut_sources = sum(ME_source_data$Omitted_Duplicates[in_boundary])
  }
  
  # Create Season & Year title
  if (season == "summer"){
    season_title = "Apr. to Sept."
    yr = year
  } else {
    season_title = "Oct. to Mar."
    yr = paste0((year - 1), "/", year)
  }
  
  # Set color scheme
  type_list = c("ice_proxy","glacier_ice_proxy","lake_sediment_proxy","tree_proxy",
                "coral_proxy","instrumental_data","documentary_proxy","speleothem_proxy",
                "bivalve_proxy","other_proxy")
  color_list = c('#5dbae8', '#332288', '#d0b943', '#117733', '#CC6677',
                 '#882255', '#44AA99', '#717126', '#AA4499', '#000000')
  named_colors = setNames(color_list, type_list)
  
  # Clean labels for TYPE
  type_labels = setNames(
    tools::toTitleCase(gsub("_", " ", type_list)),
    type_list
  )
  
  # Set shape scheme
  variable_list = c("sea_level_pressure","no_of_rainy_days","pressure",
                    "precipitation","temperature","historical_proxy","natural_proxy")
  shape_list = c(3,2,4,6,5,0,1)
  named_shapes = setNames(shape_list, variable_list)
  
  # Clean labels for VARIABLE
  variable_labels = setNames(
    tools::toTitleCase(gsub("_", " ", variable_list)),
    variable_list
  )
  
  # Plot
  ggplot() +
    geom_polygon(
      data = world,
      aes(x = long, y = lat, group = group),
      fill = "#e8e8e8",
      color = "darkgrey"
    ) +
    geom_sf() +
    coord_sf(
      xlim = minmax_lonlat[c(1, 2)],
      ylim = minmax_lonlat[c(3, 4)],
      crs = sf::st_crs(4326),
      expand = FALSE
    ) +
    geom_point(
      data = ME_source_data,
      aes(
        x = LON,
        y = LAT,
        color = TYPE,
        shape = VARIABLE
      ),
      alpha = 1,
      size = 2,
      stroke = 1
    ) +
    labs(
      title = paste0("Assimilated Observations - ", season_title, " ", yr),
      subtitle = paste0("Total Sources = ", visible_sources),
      x = "",
      y = ""
    ) +
    scale_colour_manual(values = named_colors, labels = type_labels) +
    scale_shape_manual(values = named_shapes, labels = variable_labels) +
    guides() +
    theme_classic(base_size = ifelse(is.null(base_size), 11, base_size)) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
}


#' (General) DOWNLOAD MODE-RA SOURCES DATA
#'
#' Extracts a spatial subset of ModE-RA source data based on longitude and latitude bounds.
#'
#' @param global_data Data frame. Full ModE-RA source dataset (e.g., from `load_modera_source_data()`).
#' @param lon_range Numeric vector of length 2. Longitude bounds: `c(min_lon, max_lon)`.
#' @param lat_range Numeric vector of length 2. Latitude bounds: `c(min_lat, max_lat)`.
#'
#' @return Data frame containing the subset of sources within the specified region.

download_feedback_data = function(global_data,
                                  lon_range,
                                  lat_range) {
  
  # Subset data based on lon and lat range
  subset_data = global_data[(global_data$LON > lon_range[1]) & (global_data$LON < lon_range[2]) &
                              (global_data$LAT > lat_range[1]) & (global_data$LAT < lat_range[2]), ]
}


#' (General) FORMAT Y-AXIS LABELS WITH APOSTROPHE
#'
#' Formats numeric values with apostrophes as thousands separators (e.g., 10'000).
#'
#' @param x Numeric vector. Values to format.
#'
#' @return Character vector with formatted numbers using apostrophes.

comma_apostrophe <- function(x) {
  format(x, big.mark = "'", scientific = FALSE)
}


#' (General) PLOT MODE-RA SOURCES AS TIME SERIES
#'
#' Creates an interactive time series plot of ModE-RA source counts by season and total.
#'
#' @param data Data frame. Preprocessed data for plotting (must include year and source columns).
#' @param year_column Character. Name of the year column (typically `"Year"`).
#' @param selected_columns Character vector. Column names to plot as individual lines.
#' @param line_titles Named list. Titles for the legend corresponding to `selected_columns`.
#' @param title Character. Title for the plot.
#' @param x_label Character. Label for the x-axis.
#' @param y_label Character. Label for the y-axis.
#' @param x_ticks_every Integer. Number of years between x-axis ticks.
#' @param year_range Numeric vector of length 2. Min and max year to display on the x-axis.
#'
#' @return A `plotly` interactive line chart object displaying source counts over time.

# Function to create the time series plot for selected lines with a legend
plot_ts_modera_sources <- function(data,
                                   year_column,
                                   selected_columns,
                                   line_titles,
                                   title,
                                   x_label,
                                   y_label,
                                   x_ticks_every,
                                   year_range) {
  # Filter data based on the selected year range
  data <- data |> dplyr::filter(get(year_column) >= year_range[1], get(year_column) <= year_range[2])
  
  # Define custom colors for each line (adjust as needed)
  line_colors <- c(
    "Global sources (Apr. - Sept.)" = "#882255",
    "Global sources (Oct. - Mar.)" = "#332288",
    "Global sources total" = "#117733"
  )
  
  # Calculate the maximum value of the selected columns
  max_value <- data |> dplyr::select(dplyr::all_of(selected_columns)) |> max(na.rm = TRUE)
  
  # Determine appropriate breaks based on the maximum value (rounded up to next whole number)
  y_breaks <- ceiling(seq(0, max_value, length.out = 10))
  
  # Determine x-axis tick marks dynamically based on year range
  if ((year_range[2] - year_range[1]) <= 10) {
    x_ticks_every <- 2  # Tick every year if range is 10 years or less
  } else if ((year_range[2] - year_range[1]) <= 20) {
    x_ticks_every <- 5  # Tick every 5 years if range is between 11 and 20 years
  } else {
    x_ticks_every <- 20  # Default tick interval if range is more than 20 years
  }
  
  # Create an empty plot object using ggplot
  p <- ggplot(data) +
    labs(title = title, x = x_label, y = y_label, color = "Legend (select individual lines below)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, color = "#094030"),
      axis.title.x = element_text(size = 15, color = "#094030"),
      axis.title.y = element_text(size = 15, color = "#094030"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 12, face = "bold", color = "#094030")  # Make the legend title bold
    )
  
  # Add each selected line to the plot with customized titles and colors
  for (col in selected_columns) {
    title <- line_titles[[col]]
    p <- p + geom_line(aes_string(x = year_column, y = col, color = shQuote(title)), linewidth = 1)
  }
  
  # Adjust x-axis tick marks
  if (x_ticks_every > 1) {
    p <- p + scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = x_ticks_every))
  }
  
  # Format y-axis labels with apostrophe for thousands separator
  p <- p + scale_y_continuous(labels = comma_apostrophe, breaks = y_breaks)
  
  # Set manual color scale with consistent colors
  p <- p + scale_color_manual(values = line_colors)
  
  # Convert ggplot to plotly for interactivity
  p <- plotly::ggplotly(p, tooltip = c("x","y"))
  
  return(p)
}


#' (General) GENERATE CUSTOM NETCDF
#'
#' Creates a user-defined NetCDF file from selected ModE-RA data based on tab, variables, mode, and year/baseline ranges.
#'
#' @param data_input 3D array. Primary variable data to include in the NetCDF.
#' @param tab Character. Either `"general"` or `"composites"` to determine processing mode.
#' @param dataset Character. Dataset source for fallback loading of other variables.
#' @param ncdf_ID Character. Unique identifier for naming the NetCDF file.
#' @param variable Character. Name of the primary selected variable.
#' @param user_nc_variables Character vector. List of ModE-RA variables to include (e.g., `c("Temperature", "SLP")`).
#' @param mode Character. One of `"Absolute"`, `"Anomalies"`, `"Anomaly_fixed"`, `"Anomaly_yrs_prior"`.
#' @param subset_lon_IDs Integer vector. Subset of longitude indices.
#' @param subset_lat_IDs Integer vector. Subset of latitude indices.
#' @param month_range Integer vector. Range of months used to create seasonal subsets.
#' @param year_range Integer vector. Either a year range or specific year set, depending on `tab`.
#' @param baseline_range Integer vector or NA. Reference period for anomaly calculation (for `"Anomalies"` and `"Anomaly_fixed"`).
#' @param baseline_years_before Integer or NA. Number of years prior to event for baseline (used with `"Anomaly_yrs_prior"`).
#'
#' @return None. A NetCDF file is written to `user_ncdf/netcdf_<ncdf_ID>.nc`.

generate_custom_netcdf = function(data_input,
                                  tab,
                                  dataset,
                                  ncdf_ID,
                                  variable,
                                  user_nc_variables,
                                  mode,
                                  subset_lon_IDs,
                                  subset_lat_IDs,
                                  month_range,
                                  year_range,
                                  baseline_range,
                                  baseline_years_before){
  
  # Create NA variables for each ModE-RA variable
  temp_variable_data = NA
  prec_variable_data = NA
  SLP_variable_data = NA
  Z500_variable_data = NA
  
  for (i in user_nc_variables){
    if (i == variable){
      variable_data = data_input
    } else {
      
      # Generate data_ID for new variable
      data_ref = generate_data_ID(dataset,i,month_range)
      
      # Access variable base data (if pp data not available)
      if (data_ref[1]==0){
        data1 = load_ModE_data(dataset,i)
      } else if (data_ref[1]==2){
        data1 = load_preprocessed_data(data_ref)
      } else {
        data1 = NA
      }
      # Generate latlon subset data
      data2 =  create_latlon_subset(data1, data_ref, subset_lon_IDs, subset_lat_IDs)
      # Generate yearly subset data
      if (tab == "general"){
        data3 = create_yearly_subset(data2, data_ref, year_range, month_range)
      }
      else if (tab == "composites"){
        data3 =  create_yearly_subset_composite(data2, data_ref, year_range, month_range) 
      }
      # Generate reference data 
      if (tab == "general"){
        refd = create_yearly_subset(data2, data_ref, baseline_range, month_range)
        data4 = apply(refd,c(1:2),mean)
      }
      else if (tab == "composites"){ # TBC if used for COMPOSITES!
        refd =  create_yearly_subset_composite(data2, data_ref, baseline_range, month_range) 
        data4 = apply(refd,c(1:2),mean)
      }
      
      # Generate anomalies data
      if (mode == "Absolute"){ # TBC
        variable_data = data3
      } else if (mode == "Anomaly_yrs_prior"){ #TBC
        variable_data = convert_composite_to_anomalies(data3,data2,data_ref,year_range,month_range,baseline_years_before)
      } else {
        variable_data = convert_subset_to_anomalies(data3,data4)
      }
    }
    # Save variable data
    if (i == "Temperature"){temp_variable_data = variable_data}
    else if (i == "Precipitation"){prec_variable_data = variable_data}
    else if (i == "SLP"){SLP_variable_data = variable_data}
    else if (i == "Z500"){Z500_variable_data = variable_data}
  }
  
  # Define dimensions
  londim <- ncdf4::ncdim_def("longitude","degrees_east",as.double(lon[subset_lon_IDs])) 
  latdim <- ncdf4::ncdim_def("latitude","degrees_north",as.double(lat[subset_lat_IDs])) 
  if (tab == "general"){
    timedim <- ncdf4::ncdim_def("time","year",as.double(seq(year_range[1],year_range[2],1)))  
  } else { # for composites
    timedim <- ncdf4::ncdim_def("time","year",as.double(year_range)) 
  }
  
  # Create variable long_name additions:
  if (mode == "Absolute"){
    ln_extension = ""
  } else if (mode == "Anomaly_yrs_prior"){
    ln_extension = paste(", anomaly compared to",baseline_years_before,"years prior")
  } else {
    ln_extension = paste(", anomaly compared to",baseline_range[1],"to",baseline_range[2],"reference period")
  }
  
  # Define variables and create list of variable defs
  variable_def_list = list()
  
  if ("Temperature" %in% user_nc_variables){
    dlname <- paste("air temperature at 2m",ln_extension, sep = "")
    temp_def <- ncdf4::ncvar_def("air_temperature","deg_C",list(londim,latdim,timedim),missval=NULL,dlname,prec="single")
    variable_def_list = append(variable_def_list,list(temp_def))
  }
  if ("Precipitation" %in% user_nc_variables){
    dlname <- paste("total_precipitation (rain and snow)",ln_extension, sep = "")
    prec_def <- ncdf4::ncvar_def("total_precipitation","mm/month",list(londim,latdim,timedim),missval=NULL,dlname,prec="single")
    variable_def_list = append(variable_def_list,list(prec_def))
  }
  if ("SLP" %in% user_nc_variables){
    dlname <- paste("air pressure at sea level",ln_extension, sep = "")
    SLP_def <- ncdf4::ncvar_def("air_pressure_at_sea_level","hPa",list(londim,latdim,timedim),missval=NULL,dlname,prec="single")
    variable_def_list = append(variable_def_list,list(SLP_def))
  }
  if ("Z500" %in% user_nc_variables){
    dlname <- paste("500 hPa geopotential height",ln_extension, sep = "") 
    Z500_def <- ncdf4::ncvar_def("geopotential_height","m",list(londim,latdim,timedim),missval=NULL,dlname,prec="single")# to check
    variable_def_list = append(variable_def_list,list(Z500_def))
  }  
  
  # Create netCDF file 
  ncfname <- paste("user_ncdf/netcdf_",ncdf_ID,".nc", sep="")
  ncout <- ncdf4::nc_create(ncfname,variable_def_list,force_v4=TRUE)
  
  # Add variable data to netcdf file
  if ("Temperature" %in% user_nc_variables){
    ncdf4::ncvar_put(ncout,temp_def,temp_variable_data)
  }
  if ("Precipitation" %in% user_nc_variables){
    ncdf4::ncvar_put(ncout,prec_def,prec_variable_data)
  }
  if ("SLP" %in% user_nc_variables){
    ncdf4::ncvar_put(ncout,SLP_def,SLP_variable_data)
  }
  if ("Z500" %in% user_nc_variables){
    ncdf4::ncvar_put(ncout,Z500_def,Z500_variable_data)
  }
  
  # Add additional attributes into dimension variables
  ncdf4::ncatt_put(ncout,"longitude","axis","X") 
  ncdf4::ncatt_put(ncout,"latitude","axis","Y")
  ncdf4::ncatt_put(ncout,"time","axis","T")
  
  # Add global attributes
  ncdf4::ncatt_put(ncout,0,"data","ModE-RA Project")
  ncdf4::ncatt_put(ncout,0,"grid","1.875 deg_longitude, 1.865 deg_latitude")
  ncdf4::ncatt_put(ncout,0,"institution","Institute of Geography,University of Bern")
  ncdf4::ncatt_put(ncout,0,"source","ClimeApp") 
  #ncatt_put(ncout,0,"references","tbc") # to be completed
  history <- paste("Created", date(), sep=", ")
  ncdf4::ncatt_put(ncout,0,"history",history)
  ncdf4::ncatt_put(ncout,0,"Conventions","CF-1.4")
  
  # Export netcdf
  ncdf4::nc_close(ncout)
}


#' (General) CREATE A GEOREFERENCED TIFF RASTER FROM THE MAP DATATABLE
#'
#' Converts a labeled map datatable into a georeferenced raster and optionally saves it as a GeoTIFF.
#'
#' @param map_data 2D numeric matrix. Output from `create_map_datatable()`, with coordinate-labeled rows and columns.
#' @param output_file Character (optional). File path to save the raster as a `.tif` file. If `NULL`, the raster is not written to disk.
#'
#' @return A `SpatRaster` object (from the `terra` package) with WGS84 georeferencing.

create_geotiff <- function(map_data,
                           output_file = NULL) {
  
  # Extract longitudes and latitudes from column and row names and retaining original sign
  x <- as.numeric(gsub("°[EW]", "", colnames(map_data))) *
    ifelse(grepl("E", colnames(map_data)), 1, -1)
  
  y <- as.numeric(gsub("°[NS]", "", rownames(map_data))) *
    ifelse(grepl("N", rownames(map_data)), 1, -1)
  
  # Check if dimensions of the matrix match the lon/lat lengths
  if (ncol(map_data) != length(x) || nrow(map_data) != length(y)) {
    stop("Matrix dimensions do not match the provided longitude/latitude ranges")
  }
  
  r <- terra::rast(as.matrix(map_data))
  terra::ext(r) <- terra::ext(min(x), max(x), min(y), max(y)) # define raster extent
  
  terra::crs(r) <- "EPSG:4326"  # EPSG:4326 = WGS84
  
  # Save the raster as a georeferenced TIFF file (terra package)
  # This option is only executed if the argument output_file is provided (used for the download)
  if(!is.null(output_file)) {
    terra::writeRaster(r, output_file, filetype = "GTiff", overwrite = TRUE)
  }
  
  return(r)
}


#' (General) GENERATE METADATA FROM CUSTOMIZATION INPUTS FOR ANOMALIES PLOTS
#'
#' Collects and flattens all relevant user inputs (shared, map, timeseries) into a single-row metadata table.
#' 
#' @param ALL Inputs and reactive vals relevant for plots
#'
#' @return Data frame. A single-row `data.frame` containing all selected customization inputs, formatted for export or upload to composite modules.

generate_metadata_anomalies <- function(
    
  # Common / input data
  range_years = NA,
  dataset_selected = NA,
  range_latitude = NA,
  range_longitude = NA,
  range_months = NA,
  ref_period_sg = NA,
  ref_period = NA,
  ref_single_year = NA,
  season_selected = NA,
  variable_selected = NA,
  single_year = NA,
  range_years_sg = NA,
  axis_input = NA,
  axis_mode = NA,
  
  # Map settings
  center_lat = NA,
  center_lon = NA,
  custom_map = NA,
  custom_topo = NA,
  download_options = NA,
  file_type_map_sec = NA,
  file_type_map = NA,
  hide_axis = NA,
  hide_borders = NA,
  label_lakes = NA,
  label_mountains = NA,
  label_rivers = NA,
  projection = NA,
  ref_map_mode = NA,
  sd_ratio = NA,
  show_lakes = NA,
  show_mountains = NA,
  show_rivers = NA,
  title_mode = NA,
  title_size_input = NA,
  title1_input = NA,
  title2_input = NA,
  white_land = NA,
  white_ocean = NA,
  custom_statistic = NA,
  enable_custom_statistics = NA,
  
  # Time series plot inputs
  axis_input_ts = NA,
  axis_mode_ts = NA,
  custom_ts = NA,
  download_options_ts = NA,
  enable_custom_statistics_ts = NA,
  file_type_timeseries = NA,
  key_position_ts = NA,
  show_key_ts = NA,
  show_ticks_ts = NA,
  title_mode_ts = NA,
  title_size_input_ts = NA,
  title1_input_ts = NA,
  xaxis_numeric_interval_ts = NA,
  custom_percentile_ts = NA,
  percentile_ts = NA,
  show_ref_ts = NA,
  custom_average_ts = NA,
  moving_percentile_ts = NA,
  year_moving_ts = NA,
  
  # Reactive Values / DFs
  ts_points_data = NA,
  ts_highlights_data = NA,
  ts_lines_data = NA,
  
  map_points_data = NA,
  map_highlights_data = NA,
  
  plotOrder = NA,
  availableLayers = NA,
  
  lonlat_vals = NA
) {
  # Helper to flatten all variable-length inputs
  collapse_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    paste(as.character(x), collapse = ", ")
  }
  
  meta <- data.frame(
    
    # Common / input data
    range_years       = collapse_or_na(range_years),
    dataset_selected  = dataset_selected,
    range_latitude    = collapse_or_na(range_latitude),
    range_longitude   = collapse_or_na(range_longitude),
    range_months      = collapse_or_na(range_months),
    ref_period_sg     = ref_period_sg,
    ref_period        = collapse_or_na(ref_period),
    ref_single_year   = ref_single_year,
    season_selected   = season_selected,
    variable_selected = variable_selected,
    single_year       = single_year,
    range_years_sg    = range_years_sg,
    axis_input        = collapse_or_na(axis_input),
    axis_mode         = axis_mode,
    
    # Map settings
    center_lat              = center_lat,
    center_lon              = center_lon,
    custom_map              = custom_map,
    custom_topo             = custom_topo,
    download_options        = download_options,
    file_type_map_sec       = file_type_map_sec,
    file_type_map           = file_type_map,
    hide_axis               = hide_axis,
    hide_borders            = hide_borders,
    label_lakes             = label_lakes,
    label_mountains         = label_mountains,
    label_rivers            = label_rivers,
    projection              = projection,
    ref_map_mode            = ref_map_mode,
    sd_ratio                = sd_ratio,
    show_lakes              = show_lakes,
    show_mountains          = show_mountains,
    show_rivers             = show_rivers,
    title_mode              = title_mode,
    title_size_input        = title_size_input,
    title1_input            = title1_input,
    title2_input            = title2_input,
    white_land              = white_land,
    white_ocean             = white_ocean,
    custom_statistic        = custom_statistic,
    enable_custom_statistics = enable_custom_statistics,
    
    # Time series
    axis_input_ts           = collapse_or_na(axis_input_ts),
    axis_mode_ts            = axis_mode_ts,
    custom_ts               = custom_ts,
    download_options_ts     = download_options_ts,
    enable_custom_statistics_ts = enable_custom_statistics_ts,
    file_type_timeseries    = file_type_timeseries,
    key_position_ts         = key_position_ts,
    show_key_ts             = show_key_ts,
    show_ticks_ts           = show_ticks_ts,
    title_mode_ts           = title_mode_ts,
    title_size_input_ts     = title_size_input_ts,
    title1_input_ts         = title1_input_ts,
    xaxis_numeric_interval_ts = xaxis_numeric_interval_ts,
    custom_percentile_ts    = collapse_or_na(custom_percentile_ts),
    percentile_ts           = collapse_or_na(percentile_ts),
    show_ref_ts             = show_ref_ts,
    custom_average_ts       = custom_average_ts,
    moving_percentile_ts    = moving_percentile_ts,
    year_moving_ts          = year_moving_ts,
    
    # Reactive values
    lonlat_vals     = collapse_or_na(as.vector(lonlat_vals)),
    plotOrder       = collapse_or_na(plotOrder),
    availableLayers = collapse_or_na(availableLayers),
    
    stringsAsFactors = FALSE
  )
  
  return(meta)
}


#' (General) PROCESS UPLOADED METADATA FOR MAP/TS VISUALIZATION
#'
#' Reads a metadata Excel file and updates Shiny inputs and reactiveVals for map or timeseries visualizations.
#'
#' @param file_path Character. Path to the uploaded `.xlsx` file containing metadata and optional data sheets.
#' @param mode Character. Either `"map"` or `"ts"` to indicate the target visualization module.
#' @param metadata_sheet Character. Name of the Excel sheet containing metadata (default is `"custom_meta"`).
#' @param df_ts_points Character (optional). Sheet name for uploaded TS points data.
#' @param df_ts_highlights Character (optional). Sheet name for uploaded TS highlights data.
#' @param df_ts_lines Character (optional). Sheet name for uploaded TS lines data.
#' @param df_map_points Character (optional). Sheet name for uploaded map points data.
#' @param df_map_highlights Character (optional). Sheet name for uploaded map highlights data.
#' @param rv_plotOrder ReactiveVal (optional). Stores the ordering of shapefile overlays for maps.
#' @param rv_availableLayers ReactiveVal (optional). Stores the available shapefile layer IDs.
#' @param rv_lonlat_vals ReactiveVal (optional). Stores the selected lon/lat values (numeric vector).
#' @param map_points_data Function (reactive setter). Updates the stored map points data.
#' @param map_highlights_data Function (reactive setter). Updates the stored map highlights data.
#' @param ts_points_data Function (reactive setter). Updates the stored timeseries points data.
#' @param ts_highlights_data Function (reactive setter). Updates the stored timeseries highlights data.
#' @param ts_lines_data Function (reactive setter). Updates the stored timeseries lines data.
#'
#' @return None. This function updates Shiny inputs and reactive values in-place based on the uploaded metadata file.

process_uploaded_metadata <- function(
    file_path,
    mode = c("map", "ts"),
    metadata_sheet = "custom_meta",
    df_ts_points = NULL,
    df_ts_highlights = NULL,
    df_ts_lines = NULL,
    df_map_points = NULL,
    df_map_highlights = NULL,
    rv_plotOrder = NULL,
    rv_availableLayers = NULL,
    rv_lonlat_vals = NULL,
    map_points_data = NULL,
    map_highlights_data = NULL,
    ts_points_data = NULL,
    ts_highlights_data = NULL,
    ts_lines_data = NULL
) {
  mode <- match.arg(mode)
  meta <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet)
  session <- getDefaultReactiveDomain()
  
  # Helpers
  split_numeric_range <- function(value) {
    if (is.null(value) || is.na(value) || !is.character(value)) return(c(NA, NA))
    as.numeric(strsplit(value, ",\\s*")[[1]])
  }
  split_text_vector <- function(value) {
    if (is.null(value) || is.na(value)) return(character(0))
    strsplit(value, ",\\s*")[[1]]
  }
  
  # === Shared Inputs ===
  updateSelectInput(session, "dataset_selected", selected = meta[1, "dataset_selected"])
  updateSelectInput(session, "variable_selected", selected = meta[1, "variable_selected"])
  updateNumericRangeInput(session, "range_years", value = split_numeric_range(meta[1, "range_years"]))
  updateNumericRangeInput(session, "range_latitude", value = split_numeric_range(meta[1, "range_latitude"]))
  updateNumericRangeInput(session, "range_longitude", value = split_numeric_range(meta[1, "range_longitude"]))
  shinyWidgets::updateSliderTextInput(session, "range_months", selected = split_text_vector(meta[1, "range_months"]))
  updateNumericInput(session, "ref_period_sg", value = meta[1, "ref_period_sg"])
  updateNumericRangeInput(session, "ref_period", value = split_numeric_range(meta[1, "ref_period"]))
  updateCheckboxInput(session, "ref_single_year", value = as.logical(meta[1, "ref_single_year"]))
  updateRadioButtons(session, "season_selected", selected = meta[1, "season_selected"])
  updateCheckboxInput(session, "single_year", value = as.logical(meta[1, "single_year"]))
  updateNumericInput(session, "range_years_sg", value = meta[1, "range_years_sg"])
  
  # === Map Only ===
  if (mode == "map") {
    
    updateRadioButtons(session, "axis_mode", selected = meta[1, "axis_mode"])
    updateNumericRangeInput(session, "axis_input", value = split_numeric_range(meta[1, "axis_input"]))
    updateCheckboxInput(session, "hide_axis", value = as.logical(meta[1, "hide_axis"]))
    updateRadioButtons(session, "title_mode", selected = meta[1, "title_mode"])
    updateTextInput(session, "title1_input", value = meta[1, "title1_input"])
    updateTextInput(session, "title2_input", value = meta[1, "title2_input"])
    updateNumericInput(session, "title_size_input", value = meta[1, "title_size_input"])
    updateRadioButtons(session, "custom_statistic", selected = meta[1, "custom_statistic"])
    updateNumericInput(session, "sd_ratio", value = meta[1, "sd_ratio"])
    updateCheckboxInput(session, "hide_borders", value = as.logical(meta[1, "hide_borders"]))
    updateCheckboxInput(session, "white_ocean", value = as.logical(meta[1, "white_ocean"]))
    updateCheckboxInput(session, "white_land", value = as.logical(meta[1, "white_land"]))
    
    updateNumericInput(session, "center_lat", value = meta[1, "center_lat"])
    updateNumericInput(session, "center_lon", value = meta[1, "center_lon"])
    updateCheckboxInput(session, "custom_map", value = as.logical(meta[1, "custom_map"]))
    updateCheckboxInput(session, "custom_topo", value = as.logical(meta[1, "custom_topo"]))
    updateCheckboxInput(session, "download_options", value = as.logical(meta[1, "download_options"]))
    updateRadioButtons(session, "file_type_map", selected = meta[1, "file_type_map"])
    updateRadioButtons(session, "file_type_map_sec", selected = meta[1, "file_type_map_sec"])
    updateCheckboxInput(session, "label_lakes", value = as.logical(meta[1, "label_lakes"]))
    updateCheckboxInput(session, "label_rivers", value = as.logical(meta[1, "label_rivers"]))
    updateCheckboxInput(session, "label_mountains", value = as.logical(meta[1, "label_mountains"]))
    updateCheckboxInput(session, "show_lakes", value = as.logical(meta[1, "show_lakes"]))
    updateCheckboxInput(session, "show_rivers", value = as.logical(meta[1, "show_rivers"]))
    updateCheckboxInput(session, "show_mountains", value = as.logical(meta[1, "show_mountains"]))
    updateSelectInput(session, "projection", selected = meta[1, "projection"])
    updateRadioButtons(session, "ref_map_mode", selected = meta[1, "ref_map_mode"])
    updateCheckboxInput(session, "enable_custom_statistics", value = as.logical(meta[1, "enable_custom_statistics"]))
    
    # === Update Map DataFrames ===
    if (!is.null(df_map_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_points)
      if (!is.null(df) && nrow(df) > 0) map_points_data(df)
    }
    if (!is.null(df_map_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_highlights)
      if (!is.null(df) && nrow(df) > 0) map_highlights_data(df)
    }
    
    
    # === Update Map Shp Plots ===
    if (!is.null(rv_plotOrder)) {
      rv_plotOrder(character(0))
    }
    if (!is.null(rv_availableLayers)) {
      rv_availableLayers(character(0))
    }
  }
  
  # === TS Only ===
  if (mode == "ts") {
    updateNumericRangeInput(session, "axis_input_ts", value = split_numeric_range(meta[1, "axis_input_ts"]))
    updateRadioButtons(session, "axis_mode_ts", selected = meta[1, "axis_mode_ts"])
    updateCheckboxInput(session, "custom_ts", value = as.logical(meta[1, "custom_ts"]))
    updateCheckboxInput(session, "download_options_ts", value = as.logical(meta[1, "download_options_ts"]))
    updateCheckboxInput(session, "enable_custom_statistics_ts", value = as.logical(meta[1, "enable_custom_statistics_ts"]))
    updateRadioButtons(session, "file_type_timeseries", selected = meta[1, "file_type_timeseries"])
    updateRadioButtons(session, "key_position_ts", selected = meta[1, "key_position_ts"])
    updateCheckboxInput(session, "show_key_ts", value = as.logical(meta[1, "show_key_ts"]))
    updateCheckboxInput(session, "show_ticks_ts", value = as.logical(meta[1, "show_ticks_ts"]))
    updateRadioButtons(session, "title_mode_ts", selected = meta[1, "title_mode_ts"])
    updateNumericInput(session, "title_size_input_ts", value = meta[1, "title_size_input_ts"])
    updateTextInput(session, "title1_input_ts", value = meta[1, "title1_input_ts"])
    updateNumericInput(session, "xaxis_numeric_interval_ts", value = meta[1, "xaxis_numeric_interval_ts"])
    updateCheckboxInput(session, "custom_percentile_ts", value = as.logical(meta[1, "custom_percentile_ts"]))
    updateRadioButtons(session, "percentile_ts", selected = meta[1, "percentile_ts"])
    updateCheckboxInput(session, "show_ref_ts", value = as.logical(meta[1, "show_ref_ts"]))
    updateCheckboxInput(session, "custom_average_ts", value = as.logical(meta[1, "custom_average_ts"]))
    updateCheckboxInput(session, "moving_percentile_ts", value = as.logical(meta[1, "moving_percentile_ts"]))
    updateNumericInput(session, "year_moving_ts", value = meta[1, "year_moving_ts"])
    
    # === Update TS DataFrames ===
    if (!is.null(df_ts_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_points)
      if (!is.null(df) && nrow(df) > 0) ts_points_data(df)
    }
    
    if (!is.null(df_ts_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_highlights)
      if (!is.null(df) && nrow(df) > 0) ts_highlights_data(df)
    }
    
    if (!is.null(df_ts_lines)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_lines)
      if (!is.null(df) && nrow(df) > 0) ts_lines_data(df)
    }
  }
  
  # === ReactiveVals ===
  if (!is.null(rv_lonlat_vals)) {
    lon_vals <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals)) rv_lonlat_vals(lon_vals)
  }
}


#' (General) UPDATES THE SELECTED VALUE OF A GROUP OF LINKED RADIO BUTTONS
#'
#' Updates multiple radio button inputs simultaneously with a shared selected value.
#'
#' @param selected_value Character. The value to be set as selected across all target inputs.
#' @param inputIds Character vector. List of input IDs for the radio buttons to be updated.
#'
#' @return None. The function directly updates radio button inputs within the active Shiny session.

updateRadioButtonsGroup <- function(selected_value,
                                    inputIds) {
  session <- getDefaultReactiveDomain()
  lapply(inputIds, function(inputId) {
    updateRadioButtons(
      session = session,
      inputId = inputId,
      label = NULL,
      selected = selected_value
    )
  })
}

#### Plot Features Functions #### 

# USEFUL CODE for adding/taking away data:
# df <- df[-nrow(df),]

# USEFUL CODE for implementation in shiny:
# point_shapes_numeric = sapply(point_shapes,switch,
#                              "circle" = 16,
#                              "triangle" = 17,
#                              "square" = 15)


#' (Plot Features) CREATE SD RATIO DATA
#'
#' Generates standard deviation ratio data for the selected year range, season, and region.
#'
#' @param data_input 3D numeric array. Preprocessed or raw ModE-RA input data.
#' @param data_ID Numeric vector. Data identifier used to guide processing steps.
#' @param tab Character. Either `"general"` or `"composites"` to define subset method.
#' @param variable Character. Variable name used for consistency in calling.
#' @param subset_lon_IDs, subset_lat_IDs Integer vectors. Subsets of longitude/latitude indices for the region.
#' @param month_range Integer vector. Range of months used to create seasonal subset.
#' @param year_range Integer vector. Range or set of years (must be within 1422–2008).
#'
#' @return 3D array. Subsetted SD ratio data (lon × lat × time) for the specified region and time range.

create_sdratio_data = function(data_input,
                               data_ID,
                               tab,
                               variable,
                               subset_lon_IDs,
                               subset_lat_IDs,
                               month_range,
                               year_range){
  
  # Change data_ID to identify data as preprocessed but not preloaded:
  if (data_ID[1] == 1) { 
    data_ID[1] = 2 
  } 
  
  # Lat/lon subset:
  SD_data1 = create_latlon_subset(data_input, data_ID, subset_lon_IDs, subset_lat_IDs)  
  
  # Yearly subset only if year_range is valid:
  if (all(year_range >= 1422 & year_range <= 2008)) {
    if (tab == "general") {
      SD_data2 = create_yearly_subset(SD_data1, data_ID, year_range, month_range)
    } else {
      SD_data2 = create_yearly_subset_composite(SD_data1, data_ID, year_range, month_range)
    }
  } else {
    stop(paste0("Invalid year range: ", paste(year_range, collapse = "–"), 
                ". Valid years are between 1422 and 2008."))
  }
  
  return(SD_data2)
}


#' (Plot Features) CREATE STATISTICAL HIGHLIGHTS DATA
#'
#' Creates a dataframe of points to highlight on anomaly maps based on SD ratio or percent sign agreement.
#'
#' @param data_input 3D array. Anomaly data (subsetted) from ModE-RA.
#' @param sd_data 3D array. SD ratio data from `create_sdratio_data()`.
#' @param stat_highlight Character. One of `"None"`, `"% sign match"`, or `"SD ratio"`.
#' @param sdratio Numeric. Threshold between 0 and 1 for SD ratio filtering.
#' @param percent Numeric. Percent threshold (1–100) for sign match agreement.
#' @param subset_lon_IDs, subset_lat_IDs Integer vectors. Longitude and latitude indices of the selected region.
#'
#' @return Data frame with `x_vals`, `y_vals`, and `criteria_vals` columns for plotting map highlights.

create_stat_highlights_data = function(data_input,
                                       sd_data,
                                       stat_highlight,
                                       sdratio,
                                       percent,
                                       subset_lon_IDs,
                                       subset_lat_IDs){
  if (stat_highlight != "None"){
    if (stat_highlight == "% sign match"){
      # Create sign_check function
      matching_sign_check = function(anom_data,chosen_p){
        pos = sum(anom_data>0)
        neg = sum(anom_data<0)
        if (pos>neg){
          sign.percent = (pos/(pos+neg))*100
        } else {
          sign.percent = (neg/(pos+neg))*100
        }
        if (sign.percent>chosen_p){
          criteria.match = 1 # set to preferred point size
        } else {
          criteria.match = 0
        }
        return(criteria.match)
      }
      
      # Apply sign_check function across input data
      criteria_vals = c(apply(data_input, c(1:2),matching_sign_check,percent))
    }
    
    else if (stat_highlight == "SD ratio"){
      # Mean data:
      SD_data3 = apply(sd_data,c(1,2),mean)
      
      # SD ratio check:
      sd_ratio_check = function(sd_data,chosen_sdr){
        if (sd_data > chosen_sdr){
          criteria.match = 0
        } else {
          criteria.match = 1
        }
        return(criteria.match)
      }
      
      # Apply ratio check function across SD ratio data
      criteria_vals = c(apply(SD_data3,c(1,2),sd_ratio_check,sdratio))
    }
    
    # Create vectors of x and y values
    x = lon[subset_lon_IDs]
    y = lat[subset_lat_IDs]
    
    x_vals = c(array(rep(x,length(y)),dim = c(length(x),length(y)))) 
    y_vals = c(t(array(y,dim = c(length(y),length(x)))))
    
    # Combine into dataframe
    S_H_data = data.frame(x_vals,y_vals,criteria_vals)  
    
  }
  
  else {
    S_H_data = data.frame()
  }
  
  return(S_H_data)
}


#' (Plot Features) CREATE NEW HIGHLIGHTS DATA
#'
#' Generates a single-row dataframe defining a custom highlight box for use on plots.
#'
#' @param highlight_x_values Numeric vector of length 2. Horizontal range: `c(x1, x2)` for box edges.
#' @param highlight_y_values Numeric vector of length 2. Vertical range: `c(y1, y2)` for box edges.
#' @param highlight_color Character. Fill or border color of the highlight box.
#' @param highlight_type Character. One of `"Fill"`, `"Box"`, or `"Hatched"`.
#' @param show_highlight_on_key Logical. Whether to include the highlight in the legend (`FALSE` by default).
#' @param highlight_label Character. Label used in the legend (`""` by default).
#'
#' @return Data frame. A single-row data frame containing all highlight properties for use in plotting.

create_new_highlights_data = function(highlight_x_values,
                                      highlight_y_values,
                                      highlight_color,
                                      highlight_type,
                                      show_highlight_on_key,
                                      highlight_label) {
  
  x1 = highlight_x_values[1]
  x2 = highlight_x_values[2]
  y1 = highlight_y_values[1]
  y2 = highlight_y_values[2]
  
  color = highlight_color
  type = highlight_type
  key_show = show_highlight_on_key
  label = highlight_label
  
  # Combine into a dataframe
  new_h_data = data.frame(x1,x2,y1,y2,color,type,key_show,label)
  
  return(new_h_data)
}

#' (Plot Features) CREATE NEW LINES DATA
#'
#' Creates a dataframe of custom vertical or horizontal reference lines for plotting.
#'
#' @param line_orientation Character. Either `"Horizontal"` or `"Vertical"`.
#' @param line_locations Character. Comma-separated numeric string of line positions (e.g., `"1823,1824"`).
#' @param line_color Character. Color of the lines.
#' @param line_type Character. Linetype, e.g., `"solid"` or `"dashed"`.
#' @param show_line_on_key Logical. Whether the line should appear in the legend (`FALSE` by default).
#' @param line_label Character. Label to display in the legend (`""` by default).
#'
#' @return Data frame. A dataframe with one row per line containing all line properties.

create_new_lines_data = function(line_orientation,
                                 line_locations,
                                 line_color,
                                 line_type,
                                 show_line_on_key,
                                 line_label){
  
  # Create location coordinates from string
  location = as.numeric(unlist(strsplit(line_locations,",")))
  
  # Repeat other values to match location length
  orientation = rep(line_orientation,length(location))
  color = rep(line_color,length(location))
  type = rep(line_type,length(location))
  key_show = rep(show_line_on_key,length(location))
  label = rep(line_label,length(location))
  
  # Combine into a dataframe
  new_l_data = data.frame(orientation,location,color,type,key_show,label)
  
  return(new_l_data)
}


#' (Plot Features) CREATE NEW POINTS DATA
#'
#' Creates a dataframe of custom points for plotting, including shape, color, size, and optional labels.
#'
#' @param point_x_values Character. A single numeric value or comma-separated string of values (e.g., `"30.5, 40.2"`).
#' @param point_y_values Character. A single numeric value or comma-separated string of values (e.g., `"30.5, 40.2"`).
#' @param point_label Character. Label to assign to all points (used in legends or tooltips).
#' @param point_shape Character or numeric. Unicode symbol (`"●"`, `"▲"`, `"■"`) or shape ID (16=circle, 17=triangle, 15=square).
#' @param point_color Character. Color of the points.
#' @param point_size Numeric. Size of the points (suggested range: 0–10).
#'
#' @return Data frame. A dataframe with one row per point containing all point properties.

create_new_points_data = function(point_x_values,
                                  point_y_values,
                                  point_label,
                                  point_shape,
                                  point_color,
                                  point_size) {
  # Create x/y values from strings
  x_value = as.numeric(unlist(strsplit(point_x_values, ",")))
  y_value = as.numeric(unlist(strsplit(point_y_values, ",")))
  # Repeat other values to match x/y length
  label = rep(point_label, length(x_value))
  shape_unicode = rep(point_shape, length(x_value))
  
  # Convert Unicode to numeric shape codes
  shape = dplyr::recode(shape_unicode,
                        "\u25CF" = 16, # ●
                        "\u25B2" = 17, # ▲
                        "\u25A0" = 15) # ■
  
  color = rep(point_color, length(x_value))
  size = rep(point_size, length(x_value))
  # Combine into a dataframe
  new_p_data = data.frame(x_value, y_value, label, shape, color, size)
  
  return(new_p_data)
}




#### Composite Functions ####

#' (Composite) READ USER YEAR DATA AND CONVERT INTO A YEAR_SET VECTOR
#'
#' Reads composite years from either a manual string input or a file and returns a filtered numeric vector.
#'
#' @param data_input_manual Character. A comma-separated string of years (e.g., `"1455,1532,1782"`).
#' @param data_input_filepath Character. File path to a `.csv` or Excel file containing years (no header).
#' @param year_input_mode Character. Either `"Manual"` or another value indicating file upload.
#'
#' @return Numeric vector. A `year_set` filtered to include only values within the valid ModE-RA range (1422–2008).

read_composite_data = function(data_input_manual,
                               data_input_filepath,
                               year_input_mode){
  
  # Create year_set vector from manual or uploaded years
  if(year_input_mode == "Manual"){
    year_set = as.integer(unlist(strsplit(data_input_manual,",")))
    # Check if file is excel or csv
  } else if (is.null(data_input_filepath)){
    year_set = c(1815,1816)
  } else if (grepl(".csv",data_input_filepath, fixed = TRUE)==TRUE){
    year_set = read.csv(data_input_filepath)[[1]]
  } else {
    year_set = readxl::read_excel(data_input_filepath)[[1]]
  }
  
  # Check for header and remove
  if (is.character(year_set[1])){
    year_set = as.numeric(year_set[-1])
  }
  
  # Cut years outside the range of ModE-RA
  year_set = subset(year_set,year_set>=1422 & year_set<=2008)
  
  return(year_set)
}


#' (Composite) CALCULATE YEARLY SUBSET GIVEN A SET OF YEARS
#'
#' Generates a composite data subset by averaging over months for each selected year.
#'
#' @param data_input 3D numeric array. Output from `create_latlon_subset()`.
#' @param data_ID Numeric vector. Data identifier used to determine processing mode (raw vs. preprocessed).
#' @param year_set Integer vector. Set of years to include (e.g., from `read_composite_data()`).
#' @param month_range Integer vector of length 2. Month range to average over (e.g., `c(4,9)`).
#'
#' @return 3D numeric array. Composite subset with dimensions [lon × lat × year_set].

create_yearly_subset_composite = function(data_input,
                                          data_ID,
                                          year_set,
                                          month_range){
  
  # Check for preprocessed subset
  if (data_ID[1] != 0){
    year_IDs = year_set-1420 
    data_subset = data_input[,,year_IDs]
  } 
  # or calculate from data_input
  else {
    dim_data = dim(data_input)
    years = year_set
    
    data_subset = array(NA,dim=c(dim_data[1],dim_data[2], length(years)))
    
    for (i in 1:length(years)){
      Y = years[i]
      M1 = ((12*(Y-1421))+month_range[1]) ; M2 = ((12*(Y-1421))+month_range[2]) 
      data_subset[,,i] = apply(data_input[,,M1:M2],c(1:2),mean)
    }
  }
  
  return(data_subset)
}


#' (Composite) CALCULATE COMPOSITE ANOMALIES COMPARED TO X YEARS BEFORE
#'
#' Computes anomalies for each year in a composite set, relative to the mean of a number of preceding years.
#'
#' @param data_input 3D numeric array. Output from `create_yearly_subset_composite()`.
#' @param ref_data 3D numeric array. Corresponding output from `create_latlon_subset()` used for baseline calculations.
#' @param data_ID Numeric vector. Data identifier used to determine if preprocessed data is used.
#' @param year_set Integer vector. List of years for which anomalies are computed.
#' @param month_range Integer vector of length 2. Month range to include in the averaging.
#' @param baseline_year_before Integer. Number of years prior to each year in `year_set` to use for the reference mean.
#'
#' @return 3D numeric array. Anomaly values for each year, compared to its preceding-year baseline.

convert_composite_to_anomalies = function(data_input,
                                          ref_data,
                                          data_ID,
                                          year_set,
                                          month_range,
                                          baseline_year_before){
  
  # Remove years where year_set - baseline_years_before < 1422
  subset_year_IDs = which(year_set-baseline_year_before >= 1422)
  subset_year_set = year_set[subset_year_IDs]
  subset_data_input = data_input[,,subset_year_IDs]
  
  # Create empty array for all baseline values 
  dim_data = dim(subset_data_input)
  baseline_array = array(NA,dim=c(dim_data[1],dim_data[2], length(subset_year_set)))
  
  # Use PP data to calculate baselines
  if (data_ID[1]!=0){
    for (i in 1:length(subset_year_set)){
      year_list = c()
      for (j in 1:baseline_year_before){ # Note: years_before COULD be a chosen range (i.e.10-20 years before)
        Y_ID = (subset_year_set[i]-j)-1421
        year_list = c(year_list,Y_ID)
      }
      baseline_array[,,i] = apply(ref_data[,,year_list],c(1:2),mean)
    }  
  }
  # Calculate baselines from scratch
  else {
    for (i in 1:length(subset_year_set)){
      month_list = c()
      for (j in 1:baseline_year_before){ # Note: years_before COULD be a chosen range (i.e.10-20 years before)
        Y = subset_year_set[i]-j
        M1 = ((12*(Y-1421))+month_range[1]) ; M2 = ((12*(Y-1421))+month_range[2]) 
        month_list = c(month_list,M1:M2)
      }
      baseline_array[,,i] = apply(ref_data[,,month_list],c(1:2),mean)
    }
  }
  
  anomaly_data = subset_data_input-baseline_array
  
  return(anomaly_data)
}

#' (Composite) GENERATE METADATA FROM CUSTOMIZATION INPUTS FOR COMPOSITE PLOTS
#'
#' Collects and flattens relevant user inputs from the composite plotting interface (shared, map, and timeseries),
#' returning a one-row metadata `data.frame` for export or upload into other modules.
#'
#' @param ALL Inputs and reactive values relevant for plotting.
#'
#' @return A single-row metadata `data.frame` summarizing all composite customization inputs.


generate_metadata_composite <- function(
    
  # Common / input data
  range_years2 = NA,
  range_years2a = NA,
  dataset_selected2 = NA,
  range_latitude2 = NA,
  range_longitude2 = NA,
  range_months2 = NA,
  ref_period_sg2 = NA,
  ref_period2 = NA,
  ref_single_year2 = NA,
  season_selected2 = NA,
  variable_selected2 = NA,
  enter_upload2 = NA,
  enter_upload2a = NA,
  mode_selected2 = NA,
  prior_years2 = NA,
  
  # Map settings
  axis_input2 = NA,
  axis_mode2 = NA,
  center_lat2 = NA,
  center_lon2 = NA,
  custom_map2 = NA,
  custom_statistic2 = NA,
  custom_topo2 = NA,
  download_options2 = NA,
  enable_custom_statistics2 = NA,
  file_type_map_sec2 = NA,
  file_type_map2 = NA,
  hide_axis2 = NA,
  hide_borders2 = NA,
  label_lakes2 = NA,
  label_mountains2 = NA,
  label_rivers2 = NA,
  percentage_sign_match2 = NA,
  projection2 = NA,
  ref_map_mode2 = NA,
  sd_ratio2 = NA,
  show_lakes2 = NA,
  show_mountains2 = NA,
  show_rivers2 = NA,
  title_mode2 = NA,
  title_size_input2 = NA,
  title1_input2 = NA,
  title2_input2 = NA,
  white_land2 = NA,
  white_ocean2 = NA,
  
  # TS settings
  axis_input_ts2 = NA,
  axis_mode_ts2 = NA,
  custom_percentile_ts2 = NA,
  custom_ts2 = NA,
  download_options_ts2 = NA,
  enable_custom_statistics_ts2 = NA,
  file_type_timeseries2 = NA,
  key_position_ts2 = NA,
  percentile_ts2 = NA,
  show_key_ts2 = NA,
  show_ref_ts2 = NA,
  show_ticks_ts2 = NA,
  title_mode_ts2 = NA,
  title_size_input_ts2 = NA,
  title1_input_ts2 = NA,
  xaxis_numeric_interval_ts2 = NA,
  
  # Reactive values
  plotOrder = NA,
  availableLayers = NA,
  
  lonlat_vals = NA
) {
  collapse_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    paste(as.character(x), collapse = ", ")
  }
  
  meta <- data.frame(
    
    # Common / input data
    range_years2 = collapse_or_na(range_years2),
    range_years2a = collapse_or_na(range_years2a),
    dataset_selected2 = dataset_selected2,
    range_latitude2 = collapse_or_na(range_latitude2),
    range_longitude2 = collapse_or_na(range_longitude2),
    range_months2 = collapse_or_na(range_months2),
    ref_period_sg2 = ref_period_sg2,
    ref_period2 = collapse_or_na(ref_period2),
    ref_single_year2 = ref_single_year2,
    season_selected2 = season_selected2,
    variable_selected2 = variable_selected2,
    enter_upload2 = enter_upload2,
    enter_upload2a = enter_upload2a,
    mode_selected2 = mode_selected2,
    prior_years2 = prior_years2,
    
    # Map settings
    axis_input2 = collapse_or_na(axis_input2),
    axis_mode2 = axis_mode2,
    center_lat2 = center_lat2,
    center_lon2 = center_lon2,
    custom_map2 = custom_map2,
    custom_statistic2 = custom_statistic2,
    custom_topo2 = custom_topo2,
    download_options2 = download_options2,
    enable_custom_statistics2 = enable_custom_statistics2,
    file_type_map_sec2 = file_type_map_sec2,
    file_type_map2 = file_type_map2,
    hide_axis2 = hide_axis2,
    hide_borders2 = hide_borders2,
    label_lakes2 = label_lakes2,
    label_mountains2 = label_mountains2,
    label_rivers2 = label_rivers2,
    percentage_sign_match2 = percentage_sign_match2,
    projection2 = projection2,
    ref_map_mode2 = ref_map_mode2,
    sd_ratio2 = sd_ratio2,
    show_lakes2 = show_lakes2,
    show_mountains2 = show_mountains2,
    show_rivers2 = show_rivers2,
    title_mode2 = title_mode2,
    title_size_input2 = title_size_input2,
    title1_input2 = title1_input2,
    title2_input2 = title2_input2,
    white_land2 = white_land2,
    white_ocean2 = white_ocean2,
    
    # TS settings
    axis_input_ts2 = collapse_or_na(axis_input_ts2),
    axis_mode_ts2 = axis_mode_ts2,
    custom_percentile_ts2 = custom_percentile_ts2,
    custom_ts2 = custom_ts2,
    download_options_ts2 = download_options_ts2,
    enable_custom_statistics_ts2 = enable_custom_statistics_ts2,
    file_type_timeseries2 = file_type_timeseries2,
    key_position_ts2 = key_position_ts2,
    percentile_ts2 = collapse_or_na(percentile_ts2),
    show_key_ts2 = show_key_ts2,
    show_ref_ts2 = show_ref_ts2,
    show_ticks_ts2 = show_ticks_ts2,
    title_mode_ts2 = title_mode_ts2,
    title_size_input_ts2 = title_size_input_ts2,
    title1_input_ts2 = title1_input_ts2,
    xaxis_numeric_interval_ts2 = xaxis_numeric_interval_ts2,
    
    # Reactive values
    lonlat_vals = collapse_or_na(as.vector(lonlat_vals)),
    plotOrder = collapse_or_na(plotOrder),
    availableLayers = collapse_or_na(availableLayers),
    
    stringsAsFactors = FALSE
  )
  
  return(meta)
}


#' (Composite) Process Uploaded Metadata for Composite Visualization
#'
#' Reads composite metadata and optional data frames from an uploaded Excel file
#' and updates corresponding Shiny UI inputs and reactive values for composite 
#' map or timeseries visualizations. Expects composite-style input IDs (e.g., *_2, *_ts2).
#'
#' @param file_path Character. Path to the uploaded Excel `.xlsx` file containing metadata and optional data sheets.
#' @param mode Character. Either `"map"` or `"ts"`; determines which UI group to update.
#' @param metadata_sheet Character. Name of the Excel sheet containing metadata (default: `"custom_meta"`).
#' @param df_ts_points Character (optional). Name of the sheet containing timeseries points data.
#' @param df_ts_highlights Character (optional). Name of the sheet containing timeseries highlights data.
#' @param df_ts_lines Character (optional). Name of the sheet containing timeseries lines data.
#' @param df_map_points Character (optional). Name of the sheet containing map points data.
#' @param df_map_highlights Character (optional). Name of the sheet containing map highlights data.
#' @param rv_plotOrder ReactiveVal (optional). Reactive value storing the order of plotted map layers.
#' @param rv_availableLayers ReactiveVal (optional). Reactive value storing available shapefile layer names.
#' @param rv_lonlat_vals ReactiveVal (optional). Reactive value storing selected lon/lat points for highlighting.
#' @param map_points_data Function (optional). Reactive setter function for map points data.
#' @param map_highlights_data Function (optional). Reactive setter function for map highlights data.
#' @param ts_points_data Function (optional). Reactive setter function for timeseries points data.
#' @param ts_highlights_data Function (optional). Reactive setter function for timeseries highlights data.
#' @param ts_lines_data Function (optional). Reactive setter function for timeseries lines data.
#'
#' @return None. This function performs side effects by updating Shiny inputs and reactive values.

process_uploaded_metadata_composite <- function(
    file_path,
    mode = c("map", "ts"),
    metadata_sheet = "custom_meta",
    df_ts_points = NULL,
    df_ts_highlights = NULL,
    df_ts_lines = NULL,
    df_map_points = NULL,
    df_map_highlights = NULL,
    rv_plotOrder = NULL,
    rv_availableLayers = NULL,
    rv_lonlat_vals = NULL,
    map_points_data = NULL,
    map_highlights_data = NULL,
    ts_points_data = NULL,
    ts_highlights_data = NULL,
    ts_lines_data = NULL
) {
  mode <- match.arg(mode)
  meta <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet)
  session <- getDefaultReactiveDomain()
  
  # Helpers
  split_numeric_range <- function(value) {
    if (is.null(value) || is.na(value) || !is.character(value)) return(c(NA, NA))
    as.numeric(strsplit(value, ",\\s*")[[1]])
  }
  split_text_vector <- function(value) {
    if (is.null(value) || is.na(value)) return(character(0))
    strsplit(value, ",\\s*")[[1]]
  }
  
  # === Shared Inputs ===
  updateTextInput(session, "range_years2", value = meta[1, "range_years2"])
  updateTextInput(session, "range_years2a", value = meta[1, "range_years2a"])
  updateSelectInput(session, "dataset_selected2", selected = meta[1, "dataset_selected2"])
  updateNumericRangeInput(session, "range_latitude2", value = split_numeric_range(meta[1, "range_latitude2"]))
  updateNumericRangeInput(session, "range_longitude2", value = split_numeric_range(meta[1, "range_longitude2"]))
  shinyWidgets::updateSliderTextInput(session, "range_months2", selected = split_text_vector(meta[1, "range_months2"]))
  updateNumericInput(session, "ref_period_sg2", value = meta[1, "ref_period_sg2"])
  updateNumericRangeInput(session, "ref_period2", value = split_numeric_range(meta[1, "ref_period2"]))
  updateCheckboxInput(session, "ref_single_year2", value = as.logical(meta[1, "ref_single_year2"]))
  updateRadioButtons(session, "season_selected2", selected = meta[1, "season_selected2"])
  updateSelectInput(session, "variable_selected2", selected = meta[1, "variable_selected2"])
  updateRadioButtons(session, "enter_upload2", selected = meta[1, "enter_upload2"])
  updateRadioButtons(session, "enter_upload2a", selected = meta[1, "enter_upload2a"])
  updateRadioButtons(session, "mode_selected2", selected = meta[1, "mode_selected2"])
  updateNumericInput(session, "prior_years2", value = meta[1, "prior_years2"])
  
  if (mode == "map") {
    # === Map UI ===
    updateRadioButtons(session, "axis_mode2", selected = meta[1, "axis_mode2"])
    updateNumericRangeInput(session, "axis_input2", value = split_numeric_range(meta[1, "axis_input2"]))
    updateNumericInput(session, "center_lat2", value = meta[1, "center_lat2"])
    updateNumericInput(session, "center_lon2", value = meta[1, "center_lon2"])
    updateCheckboxInput(session, "custom_map2", value = as.logical(meta[1, "custom_map2"]))
    updateRadioButtons(session, "custom_statistic2", selected = meta[1, "custom_statistic2"])
    updateCheckboxInput(session, "custom_topo2", value = as.logical(meta[1, "custom_topo2"]))
    updateCheckboxInput(session, "download_options2", value = as.logical(meta[1, "download_options2"]))
    updateCheckboxInput(session, "enable_custom_statistics2", value = as.logical(meta[1, "enable_custom_statistics2"]))
    updateRadioButtons(session, "file_type_map_sec2", selected = meta[1, "file_type_map_sec2"])
    updateRadioButtons(session, "file_type_map2", selected = meta[1, "file_type_map2"])
    updateCheckboxInput(session, "hide_axis2", value = as.logical(meta[1, "hide_axis2"]))
    updateCheckboxInput(session, "hide_borders2", value = as.logical(meta[1, "hide_borders2"]))
    updateCheckboxInput(session, "label_lakes2", value = as.logical(meta[1, "label_lakes2"]))
    updateCheckboxInput(session, "label_mountains2", value = as.logical(meta[1, "label_mountains2"]))
    updateCheckboxInput(session, "label_rivers2", value = as.logical(meta[1, "label_rivers2"]))
    updateNumericInput(session, "percentage_sign_match2", value = meta[1, "percentage_sign_match2"])
    updateSelectInput(session, "projection2", selected = meta[1, "projection2"])
    updateRadioButtons(session, "ref_map_mode2", selected = meta[1, "ref_map_mode2"])
    updateNumericInput(session, "sd_ratio2", value = meta[1, "sd_ratio2"])
    updateCheckboxInput(session, "show_lakes2", value = as.logical(meta[1, "show_lakes2"]))
    updateCheckboxInput(session, "show_mountains2", value = as.logical(meta[1, "show_mountains2"]))
    updateCheckboxInput(session, "show_rivers2", value = as.logical(meta[1, "show_rivers2"]))
    updateRadioButtons(session, "title_mode2", selected = meta[1, "title_mode2"])
    updateNumericInput(session, "title_size_input2", value = meta[1, "title_size_input2"])
    updateTextInput(session, "title1_input2", value = meta[1, "title1_input2"])
    updateTextInput(session, "title2_input2", value = meta[1, "title2_input2"])
    updateCheckboxInput(session, "white_land2", value = as.logical(meta[1, "white_land2"]))
    updateCheckboxInput(session, "white_ocean2", value = as.logical(meta[1, "white_ocean2"]))
    
    # === Update Map DataFrames ===
    if (!is.null(df_map_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_points)
      if (!is.null(df) && nrow(df) > 0) map_points_data(df)
    }
    if (!is.null(df_map_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_highlights)
      if (!is.null(df) && nrow(df) > 0) map_highlights_data(df)
    }
    
    
    # === Update Map Shp Plots ===
    if (!is.null(rv_plotOrder)) {
      rv_plotOrder(character(0))
    }
    if (!is.null(rv_availableLayers)) {
      rv_availableLayers(character(0))
    }
    
  }
  
  if (mode == "ts") {
    # === TS UI ===
    updateNumericRangeInput(session, "axis_input_ts2", value = split_numeric_range(meta[1, "axis_input_ts2"]))
    updateRadioButtons(session, "axis_mode_ts2", selected = meta[1, "axis_mode_ts2"])
    updateCheckboxInput(session, "custom_percentile_ts2", value = as.logical(meta[1, "custom_percentile_ts2"]))
    updateCheckboxInput(session, "custom_ts2", value = as.logical(meta[1, "custom_ts2"]))
    updateCheckboxInput(session, "download_options_ts2", value = as.logical(meta[1, "download_options_ts2"]))
    updateRadioButtons(session, "file_type_timeseries2", selected = meta[1, "file_type_timeseries2"])
    updateCheckboxInput(session, "enable_custom_statistics_ts2", value = as.logical(meta[1, "enable_custom_statistics_ts2"]))
    updateRadioButtons(session, "key_position_ts2", selected = meta[1, "key_position_ts2"])
    updateRadioButtons(session, "percentile_ts2", selected = meta[1, "percentile_ts2"])
    updateCheckboxInput(session, "show_key_ts2", value = as.logical(meta[1, "show_key_ts2"]))
    updateCheckboxInput(session, "show_ref_ts2", value = as.logical(meta[1, "show_ref_ts2"]))
    updateCheckboxInput(session, "show_ticks_ts2", value = as.logical(meta[1, "show_ticks_ts2"]))
    updateRadioButtons(session, "title_mode_ts2", selected = meta[1, "title_mode_ts2"])
    updateNumericInput(session, "title_size_input_ts2", value = meta[1, "title_size_input_ts2"])
    updateTextInput(session, "title1_input_ts2", value = meta[1, "title1_input_ts2"])
    updateNumericInput(session, "xaxis_numeric_interval_ts2", value = meta[1, "xaxis_numeric_interval_ts2"])
    
    # === Update TS DataFrames ===
    if (!is.null(df_ts_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_points)
      if (!is.null(df) && nrow(df) > 0) ts_points_data(df)
    }
    
    if (!is.null(df_ts_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_highlights)
      if (!is.null(df) && nrow(df) > 0) ts_highlights_data(df)
    }
    
    if (!is.null(df_ts_lines)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_lines)
      if (!is.null(df) && nrow(df) > 0) ts_lines_data(df)
    }
    
  }
  
  # === ReactiveVals ===
  if (!is.null(rv_lonlat_vals)) {
    lon_vals <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals)) rv_lonlat_vals(lon_vals)
  }
}


#### Reg/Cor Functions ####


#' (Regression/Correlation) Load User data
#'
#' Loads user-supplied data from a CSV or Excel file for regression or correlation analysis.
#' Replaces placeholder values (-999.9) with `NA` for proper handling of missing data.
#'
#' @param data_input_filepath Character. Path to a `.csv` or Excel `.xlsx` file containing the user data.
#'
#' @return A data frame with user-provided data, with -999.9 replaced by `NA`.

read_regcomp_data = function(data_input_filepath) {
  # Read in user data
  if (grepl(".csv", data_input_filepath, fixed = TRUE) == TRUE) {
    user_data = read.csv(data_input_filepath)
  } else {
    user_data = readxl::read_excel(data_input_filepath)
  }
  
  user_data = replace(user_data, user_data == -999.9, NA)
  
  return(user_data)
}

#' (Regression/Correlation) Extract Shared Year Range
#'
#' Computes the shared valid year range between two variables (ModE-RA or user-supplied),
#' accounting for lag settings. Useful for defining valid `year_range` slider inputs.
#'
#' @param variable1_source Character. Source of variable 1: either `"ModE-RA"` or `"User data"`.
#' @param variable2_source Character. Source of variable 2: either `"ModE-RA"` or `"User data"`.
#' @param variable1_data_filepath Character. Path to the user file for variable 1, if applicable.
#' @param variable2_data_filepath Character. Path to the user file for variable 2, if applicable.
#' @param variable1_lag Numeric. Number of years to lag variable 1 (default = 0).
#' @param variable2_lag Numeric. Number of years to lag variable 2 (default = 0).
#'
#' @return A numeric vector with six elements:
#'   \describe{
#'     \item{[1]}{Shared year range start (after lag adjustment)}
#'     \item{[2]}{Shared year range end (after lag adjustment)}
#'     \item{[3]}{Variable 1 min year (raw)}
#'     \item{[4]}{Variable 1 max year (raw)}
#'     \item{[5]}{Variable 2 min year (raw)}
#'     \item{[6]}{Variable 2 max year (raw)}
#'   }

extract_year_range = function(variable1_source,
                              variable2_source,
                              variable1_data_filepath,
                              variable2_data_filepath,
                              variable1_name = NULL,
                              variable2_name = NULL,
                              variable1_lag = 0,
                              variable2_lag = 0) {
  
  # Default values
  V1_min = 1422; V1_max = 2008
  V2_min = 1422; V2_max = 2008
  V1_min_tot = 1422; V1_max_tot = 2008
  V2_min_tot = 1422; V2_max_tot = 2008
  
  # Helper function to extract full and variable-specific year ranges
  get_year_ranges <- function(filepath, variable_name) {
    if (is.null(filepath)) return(list(var = NULL, all = NULL))
    
    # Read file
    if (grepl(".xls", filepath, fixed = TRUE)) {
      df <- readxl::read_excel(filepath)
    } else if (grepl(".csv", filepath, fixed = TRUE)) {
      df <- read_csv(filepath, show_col_types = FALSE)
    } else return(list(var = NULL, all = NULL))
    
    # Identify Year column
    year_col <- names(df)[tolower(names(df)) == "year"]
    if (length(year_col) != 1) return(list(var = NULL, all = NULL))
    df$Year <- as.numeric(df[[year_col]])
    
    # Full year range
    all_years <- df$Year[!is.na(df$Year)]
    range_all <- if (length(all_years) > 0) c(min(all_years), max(all_years)) else NULL
    
    # Year range for selected variable(s)
    range_var <- NULL
    if (!is.null(variable_name)) {
      # Keep only valid variable names
      variable_name <- variable_name[variable_name %in% names(df)]
      
      if (length(variable_name) > 0) {
        # Collect all years where any selected variable has non-NA values
        valid_years <- unlist(lapply(variable_name, function(v) df$Year[!is.na(df[[v]])]))
        if (length(valid_years) > 0) {
          range_var <- c(min(valid_years), max(valid_years))
        }
      }
    }
    
    return(list(var = range_var, all = range_all))
  }
  
  
  # Variable 1
  if (variable1_source == "User data") {
    ranges1 <- get_year_ranges(variable1_data_filepath, variable1_name)
    if (!is.null(ranges1$var)) {
      V1_min <- ranges1$var[1]
      V1_max <- ranges1$var[2]
    }
    if (!is.null(ranges1$all)) {
      V1_min_tot <- ranges1$all[1]
      V1_max_tot <- ranges1$all[2]
    }
  }
  
  # Variable 2
  if (variable2_source == "User data") {
    ranges2 <- get_year_ranges(variable2_data_filepath, variable2_name)
    if (!is.null(ranges2$var)) {
      V2_min <- ranges2$var[1]
      V2_max <- ranges2$var[2]
    }
    if (!is.null(ranges2$all)) {
      V2_min_tot <- ranges2$all[1]
      V2_max_tot <- ranges2$all[2]
    }
  }
  
  # Apply lag
  V1_min_adjusted <- V1_min - variable1_lag
  V1_max_adjusted <- V1_max - variable1_lag
  V2_min_adjusted <- V2_min - variable2_lag
  V2_max_adjusted <- V2_max - variable2_lag
  
  # Shared (overlapping) range after lag
  YR_min <- max(c(V1_min_adjusted, V2_min_adjusted))
  YR_max <- min(c(V1_max_adjusted, V2_max_adjusted))
  
  # Return all three types of year ranges
  return(c(
    V1_min,     # [1]
    V1_max,     # [2]
    V2_min,     # [3]
    V2_max,     # [4]
    
    YR_min,     # [5]
    YR_max,     # [6]
    
    V1_min_tot, # [7]
    V1_max_tot, # [8]
    V2_min_tot, # [9]
    V2_max_tot  # [10]
  ))
}


#' (Regression/Correlation) Create User data Subset
#'
#' Subsets user-uploaded data to match a specified year range and lag setting.
#' Adjusts the years based on the lag and ensures variable values are numeric, replacing invalid entries with `NA`.
#'
#' @param data_input Data frame. Loaded user data, typically from `read_regcomp_data()`.
#' @param variable Character. The name of the column (variable) to extract from user data.
#' @param year_range Numeric vector of length 2. The desired year range (e.g., `c(1850, 1900)`).
#' @param lag Numeric. Number of years the data should be lagged by (default = 0).
#'
#' @return A data frame with two columns: `"Year"` (adjusted for lag) and the selected variable values (numeric).

create_user_data_subset = function(data_input,
                                   variable,
                                   year_range,
                                   lag = 0) {
  lagged_year_range = year_range + lag
  UD_ss_year_range = subset(data_input,
                            data_input[, 1] >= lagged_year_range[1] &
                              data_input[, 1] <= lagged_year_range[2])
  UD_ss_variable = UD_ss_year_range[, variable]
  UD_ss_year = UD_ss_year_range[, 1] - lag
  
  UD_ss = data.frame(UD_ss_year, UD_ss_variable)
  colnames(UD_ss) = c("Year", variable)
  
  # Replace missing values an text with NAs
  UD_ss[, 2] = as.numeric(UD_ss[, 2])
  
  return(UD_ss)
}


#' (Regression/Correlation) Extract Shared LonLat Values
#'
#' Identifies the overlapping longitude and latitude range between two input variables, used for plotting correlation/regression maps.
#' If one variable is a timeseries, the lon/lat range of the other variable is used directly.
#'
#' @param variable1_type Character. Type of variable 1 ("Timeseries" or "Map").
#' @param variable2_type Character. Type of variable 2 ("Timeseries" or "Map").
#' @param variable1_lon_range Numeric vector of length 2. Longitude range for variable 1 (e.g., `c(-10, 30)`).
#' @param variable1_lat_range Numeric vector of length 2. Latitude range for variable 1 (e.g., `c(35, 65)`).
#' @param variable2_lon_range Numeric vector of length 2. Longitude range for variable 2.
#' @param variable2_lat_range Numeric vector of length 2. Latitude range for variable 2.
#'
#' @return A numeric vector of length 4: `c(shared_lon_min, shared_lon_max, shared_lat_min, shared_lat_max)`.

extract_shared_lonlat = function(variable1_type,
                                 variable2_type,
                                 variable1_lon_range,
                                 variable1_lat_range,
                                 variable2_lon_range,
                                 variable2_lat_range){
  
  # If variable 1 is a timeseries:
  if (variable1_type == "Timeseries"){
    shared_lon_range = variable2_lon_range
    shared_lat_range = variable2_lat_range
  } else if (variable2_type == "Timeseries"){
    shared_lon_range = variable1_lon_range
    shared_lat_range = variable1_lat_range
  } else{
    shared_lon_range = c(max(c(variable1_lon_range[1],variable2_lon_range[1])),
                         min(c(variable1_lon_range[2],variable2_lon_range[2])))
    shared_lat_range = c(max(c(variable1_lat_range[1],variable2_lat_range[1])),
                         min(c(variable1_lat_range[2],variable2_lat_range[2])))
  }
  
  shared_lonlat = c(shared_lon_range,shared_lat_range)
  
  return(shared_lonlat)
}


#### Correlation Functions  ####

## Special Input required:
## variable1/2_type = "Timeseries" or "Field" ("Timeseries" by default)
##                    Choice appears, and selection is updated to "Field" IF
##                    variable is ModE-RA AND length(subset_lon & subset_lat) > 2 
##                    (i.e. NOT a single point)


#' (Correlation) Plot User Timeseries
#'
#' Plots a single user-provided timeseries with color customization. Also tests for normality and displays range and standard deviation (if applicable).
#'
#' @param data_input Data frame. Subset of user data created with `create_user_data_subset()`, must include a "Year" column and one variable column.
#' @param color Character. Color used for the line plot (e.g., `"darkorange2"` for Variable 1, `"saddlebrown"` for Variable 2).
#'
#' @return None. The function draws a base R line plot with annotation for value range and standard deviation.

plot_user_timeseries = function(data_input,
                                color) {
  # Calculate y statistics
  y = data_input[, 2]
  y_range = range(y, na.rm = TRUE)
  
  # Test data for normality
  p_value = shapiro.test(y)[[2]]
  if (p_value > 0.05) {
    y_sd = sd(y, na.rm = TRUE)
  } else {
    y_sd = NA
  }
  
  # Build title text
  plot_title <- paste(
    "Range = ",
    signif(y_range[1], 3),
    ":",
    signif(y_range[2], 3),
    "   SD = ",
    signif(y_sd, 3),
    sep = ""
  )
  
  # Create plot
  p <- ggplot(data_input, aes(x = Year, y = !!dplyr::sym(colnames(data_input)[2]))) +
    geom_line(color = color, linewidth = 1.2, na.rm = TRUE) +
    labs(
      x = "Year",
      y = colnames(data_input)[2],
      title = plot_title
    ) +
    scale_x_continuous(limits = range(data_input$Year, na.rm = TRUE), expand = c(0, 0)) +  # <--- added here
    theme_bw(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 1, size = 12, face = "plain", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
  
  return(p)
}


#' (Correlation) Generate Correlation Titles
#'
#' Generates a set of titles, axis labels, colors, and metadata strings for correlation plots
#' based on user inputs and the selected ModE-RA or user-supplied variables.
#'
#' @param variable1_source Character. `"ModE-RA"` or `"User data"` for Variable 1.
#' @param variable2_source Character. `"ModE-RA"` or `"User data"` for Variable 2.
#' @param variable1_dataset Character. Name of the dataset used for Variable 1 (e.g., "ModE-RA").
#' @param variable2_dataset Character. Name of the dataset used for Variable 2.
#' @param variable1 Character. Variable name for Variable 1 (e.g., "Temperature").
#' @param variable2 Character. Variable name for Variable 2.
#' @param variable1_type Character. `"Timeseries"` or `"Field"` for Variable 1.
#' @param variable2_type Character. `"Timeseries"` or `"Field"` for Variable 2.
#' @param variable1_mode Character. `"Absolute"` or `"Anomaly"` for Variable 1.
#' @param variable2_mode Character. `"Absolute"` or `"Anomaly"` for Variable 2.
#' @param variable1_month_range Numeric vector. Month range (start and end) for Variable 1.
#' @param variable2_month_range Numeric vector. Month range (start and end) for Variable 2.
#' @param variable1_lon_range Numeric vector. Longitude range for Variable 1.
#' @param variable2_lon_range Numeric vector. Longitude range for Variable 2.
#' @param variable1_lat_range Numeric vector. Latitude range for Variable 1.
#' @param variable2_lat_range Numeric vector. Latitude range for Variable 2.
#' @param year_range Numeric vector. Year range used in the analysis.
#' @param method Character. Correlation method, either `"pearson"` or `"spearman"`.
#' @param map_title_mode Character. `"Custom"` or `"Auto"` title mode for the map.
#' @param ts_title_mode Character. `"Custom"` or `"Auto"` title mode for the time series.
#' @param map_custom_title Character. Custom main title for the map (used if mode is "Custom").
#' @param map_custom_subtitle Character. Custom subtitle for the map (used if mode is "Custom").
#' @param ts_custom_title Character. Custom title for the time series (used if mode is "Custom").
#' @param map_title_size Numeric. Font size for the map title.
#' @param ts_title_size Numeric. Font size for the time series title.
#'
#' @return A data frame containing map title, subtitle, time series title, file-safe title,
#' axis labels, colors, and font sizes for Variable 1 and Variable 2.

generate_correlation_titles = function(variable1_source,
                                       variable2_source,
                                       variable1_dataset,
                                       variable2_dataset,
                                       variable1,
                                       variable2,
                                       variable1_type,
                                       variable2_type,
                                       variable1_mode,
                                       variable2_mode,
                                       variable1_month_range,
                                       variable2_month_range,
                                       variable1_lon_range,
                                       variable2_lon_range,
                                       variable1_lat_range,
                                       variable2_lat_range,
                                       year_range,
                                       method,
                                       map_title_mode,
                                       ts_title_mode,
                                       map_custom_title,
                                       map_custom_subtitle,
                                       ts_custom_title,
                                       map_title_size,
                                       ts_title_size) {
  
  
  # Set values for variable 1:
  if (variable1_source=="User data"){
    V1_axis_label = variable1
    V1_color = "darkorange2"
    V1_TS_title = variable1
    V1_map_title = variable1
    V1_file_title = variable1
    
    # If V1 = ModE-RA
  } else {
    # Generate ME title months and extension
    title_months1 = generate_title_months(MR = variable1_month_range)
    if (variable1_mode == "Absolute"){
      variable1_mode = ""
    }
    # Generate units & color scheme for TS plots
    if (variable1 == "Temperature"){
      V1_color = "red3" ; V1_unit = "[\u00B0C]"
    }
    else if (variable1 == "Precipitation"){
      V1_color = "turquoise4" ; V1_unit = "[mm/month]"
    }
    else if (variable1 == "SLP"){
      V1_color = "purple4" ; V1_unit = "[hPa]"
    }
    else if (variable1 == "Z500"){
      V1_color = "green4" ; V1_unit = "[m]"
    }
    # Generate lon/lat addition
    if (variable1_lon_range[1]==variable1_lon_range[2]) {
      V1_lon_title = paste(variable1_lon_range[1],"\u00B0E",sep = "")
    } else {
      V1_lon_title = paste(variable1_lon_range[1],":",variable1_lon_range[2],"\u00B0E", sep = "")
    }
    if (variable1_lat_range[1]==variable1_lat_range[2]){
      V1_lat_title = paste(variable1_lat_range[1],"\u00B0N",sep = "")
    } else {
      V1_lat_title = paste(variable1_lat_range[1],":",variable1_lat_range[2],"\u00B0N", sep = "")
    }
    V1_lonlat = paste("[",V1_lon_title,", ",V1_lat_title,"]",sep = "")
    
    # Generate titles
    V1_axis_label = paste(title_months1,variable1,variable1_mode,V1_unit)
    V1_TS_title = paste(variable1_dataset,title_months1,variable1,variable1_mode,V1_lonlat)
    if (variable1_type == "Timeseries"){
      V1_Map_title = paste(variable1_dataset,title_months1,variable1,variable1_mode,V1_lonlat)
    } else {
      V1_Map_title = paste(variable1_dataset,title_months1,variable1,variable1_mode)
    }
    V1_file_title = paste(variable1_dataset,title_months1,variable1,variable1_mode)
  }
  
  # Set values for variable 2:
  if (variable2_source=="User data"){
    V2_axis_label = variable2
    V2_color = "saddlebrown"
    V2_TS_title = variable2
    V2_Map_title = variable2
    V2_file_title = variable2
    # If V2 = ModE-RA
  } else {
    # Generate ME title months and extension
    title_months2 = generate_title_months(MR = variable2_month_range)
    if (variable2_mode == "Absolute"){
      variable2_mode = ""
    }
    # Generate units & color scheme
    if (variable2 == "Temperature"){
      V2_color = "red3" ; V2_unit = "[\u00B0C]"
    }
    else if (variable2 == "Precipitation"){
      V2_color = "turquoise4" ; V2_unit = "[mm/month]"
    }
    else if (variable2 == "SLP"){
      V2_color = "purple4" ; V2_unit = "[hPa]"
    }
    else if (variable2 == "Z500"){
      V2_color = "green4" ; V2_unit = "[m]"
    }
    # Generate lon/lat addition
    if (variable2_lon_range[1]==variable2_lon_range[2]) {
      V2_lon_title = paste(variable2_lon_range[1],"\u00B0E",sep = "")
    } else {
      V2_lon_title = paste(variable2_lon_range[1],":",variable2_lon_range[2],"\u00B0E", sep = "")
    }
    if (variable2_lat_range[1]==variable2_lat_range[2]){
      V2_lat_title = paste(variable2_lat_range[1],"\u00B0N",sep = "")
    } else {
      V2_lat_title = paste(variable2_lat_range[1],":",variable2_lat_range[2],"\u00B0N", sep = "")
    }
    V2_lonlat = paste("[",V2_lon_title,", ",V2_lat_title,"]",sep = "")
    
    # Generate titles
    V2_axis_label = paste(title_months2,variable2,variable2_mode,V2_unit)
    V2_TS_title = paste(variable2_dataset,title_months2,variable2,variable2_mode,V2_lonlat)
    if (variable2_type == "Timeseries"){
      V2_Map_title = paste(variable2_dataset,title_months2,variable2,variable2_mode,V2_lonlat)
    } else {
      V2_Map_title = paste(variable2_dataset,title_months2,variable2,variable2_mode)
    }
    V2_file_title = paste(variable2_dataset,title_months2,variable2,variable2_mode)
  }
  
  # Edit colors and titles if v1 and v2 are the same
  if (variable1 == variable2 & variable1_source !="User data" & variable2_source !="User data"){
    if (variable1 == "Temperature"){
      V1_color = "red4" ; V2_color = "red2"
    }
    else if (variable1 == "Precipitation"){
      V1_color = "turquoise4" ; V2_color = "turquoise2"
    }
    else if (variable1 == "SLP"){
      V1_color = "purple4" ; V2_color = "purple2"
    }
    else if (variable1 == "Z500"){
      V1_color = "green4" ; V2_color = "green2"
    }  
    
    V1_axis_label = paste(V1_axis_label,";",V1_lonlat)
    V2_axis_label = paste(V2_axis_label,";",V2_lonlat)
  }
  
  # Generate combined titles:
  if (ts_title_mode == "Custom"){
    ts_title = ts_custom_title
  } else {
    ts_title = paste(V1_TS_title,"&",V2_TS_title)
  }
  
  map_title <- ifelse(map_title_mode == "Custom" & map_custom_title != "", map_custom_title, "Correlation coefficient")
  map_subtitle <- ifelse(map_title_mode == "Custom" & map_custom_subtitle != "", map_custom_subtitle, paste(V1_TS_title,"&",V2_TS_title))
  
  # Generate download titles
  tf0 = paste("Corr",V1_file_title,"&",V2_file_title)
  tf1 = gsub("[[:punct:]]", "", tf0)
  tf2 = gsub(" ","-",tf1)
  file_title = iconv(tf2, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Title font size
  map_title_size = map_title_size
  ts_title_size = ts_title_size
  
  cor_titles = data.frame(
    map_title,
    map_subtitle,
    ts_title,
    file_title,
    map_title_size,
    ts_title_size,
    V1_axis_label,
    V2_axis_label,
    V1_color,
    V2_color
  )
  
  return(cor_titles)
}




#' (Correlation) Correlate Two Timeseries
#'
#' Calculates the correlation between two timeseries using Pearson or Spearman method.
#' Handles missing values by excluding incomplete observations.
#'
#' @param variable1_data Data frame. First variable's data with columns `Year` and values.
#' @param variable2_data Data frame. Second variable's data with columns `Year` and values.
#' @param method Character. Correlation method to use: `"pearson"` (default) or `"spearman"`.
#'
#' @return An object of class `"htest"` returned by `cor.test()`, containing the correlation coefficient, p-value, confidence interval, and other statistics.

correlate_timeseries = function(variable1_data,
                                variable2_data,
                                method) {
  r = cor.test(variable1_data[, 2],
               variable2_data[, 2],
               method = method,
               use = "complete.obs")
  return (r)
}


#' (Correlation) Generate Correlation Map Data
#'
#' Computes grid-based correlation values between a timeseries and a spatial field,
#' or between two spatial fields. Returns map-ready correlation data (`x`, `y`, `z`).
#' Assumes input ModE-RA data has been preprocessed. Not suitable for comparing two timeseries.
#'
#' @param variable1_data Array or data frame. Either a 2D or 3D data set: a user or ModE-RA timeseries or field.
#' @param variable2_data Array or data frame. Second data set to correlate (timeseries or field).
#' @param method Character. Correlation method to use: `"pearson"` (default) or `"spearman"`.
#' @param variable1_type Character. `"Timeseries"` or `"Field"` – type of the first variable.
#' @param variable2_type Character. `"Timeseries"` or `"Field"` – type of the second variable.
#' @param variable1_lon_range Numeric vector of length 2. Longitude range for variable 1.
#' @param variable2_lon_range Numeric vector of length 2. Longitude range for variable 2.
#' @param variable1_lat_range Numeric vector of length 2. Latitude range for variable 1.
#' @param variable2_lat_range Numeric vector of length 2. Latitude range for variable 2.
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{x}{Numeric vector of longitudes.}
#'   \item{y}{Numeric vector of latitudes.}
#'   \item{z}{Matrix of correlation values for each grid cell.}
#' }

generate_correlation_map_data = function(variable1_data,
                                         variable2_data,
                                         method,
                                         variable1_type,
                                         variable2_type,
                                         variable1_lon_range,
                                         variable2_lon_range,
                                         variable1_lat_range,
                                         variable2_lat_range) {
  
  
  # Generate subset lon/lat IDs
  subset_lon_IDs1 = create_subset_lon_IDs(variable1_lon_range)
  subset_lon_IDs2 = create_subset_lon_IDs(variable2_lon_range)
  subset_lat_IDs1 = create_subset_lat_IDs(variable1_lat_range)
  subset_lat_IDs2 = create_subset_lat_IDs(variable2_lat_range)
  
  # Define correlation method:
  my_cor = function(a,b){cor(a,b,method=method,use = "complete.obs")}
  
  # If variable 1 is a timeseries:
  if (variable1_type == "Timeseries"){
    x = lon[subset_lon_IDs2]
    y = rev(lat[subset_lat_IDs2])
    plotfield = apply(variable2_data,c(1,2),my_cor,variable1_data[,2])
    z = plotfield[,rev(1:length(subset_lat_IDs2))]
  }
  # If variable 2 is a timeseries:
  else if (variable2_type == "Timeseries"){
    x = lon[subset_lon_IDs1]
    y = rev(lat[subset_lat_IDs1])
    plotfield = apply(variable1_data,c(1,2),my_cor,variable2_data[,2])
    z = plotfield[,rev(1:length(subset_lat_IDs1))]
  }
  # If variable 1 & 2 are fields
  else {
    # Find shared lon/lat
    shared_lon_IDs = dplyr::intersect(subset_lon_IDs1,subset_lon_IDs2)
    shared_lat_IDs = dplyr::intersect(subset_lat_IDs1,subset_lat_IDs2)
    # Cut data to new shared lon/lat
    cut_variable1_data = variable1_data[match(shared_lon_IDs,subset_lon_IDs1),
                                        match(shared_lat_IDs,subset_lat_IDs1),]
    cut_variable2_data = variable2_data[match(shared_lon_IDs,subset_lon_IDs2),
                                        match(shared_lat_IDs,subset_lat_IDs2),]
    # Set up x, y and z for plotting
    x = lon[shared_lon_IDs]
    y = rev(lat[shared_lat_IDs])
    plotfield = array(NA,dim = dim(cut_variable1_data[,,1]))
    # Cycle through all gridpoints, correlating the two timeseries for each
    for (i in 1:dim(cut_variable1_data)[1]){
      for (j in 1:dim(cut_variable1_data)[2]){
        plotfield[i,j] = my_cor(cut_variable1_data[i,j,],cut_variable2_data[i,j,])
      }
    }
    z = plotfield[,rev(1:length(shared_lat_IDs))]
  }
  
  # Combine x, y and z into a dataframe
  xyz = list(x,y,z)
  
  return(xyz)
}

#' (Correlation) Generate Correlation Map Datatable
#'
#' Converts correlation map output (`x`, `y`, `z`) into a formatted matrix
#' suitable for display as a datatable. Includes rotated and labeled axes.
#'
#' @param data_input List. Correlation map data as returned by `generate_correlation_map_data()`,
#'                   containing elements `x` (longitude), `y` (latitude), and `z` (correlation matrix).
#'
#' @return A matrix with row and column labels including degree symbols and cardinal directions,
#'         representing latitude and longitude respectively.

generate_correlation_map_datatable = function(data_input){
  
  # Extract x,y,z
  x = data_input[[1]]
  y = data_input[[2]]
  z = data_input[[3]]  
  
  # Transpose and rotate z
  
  zt = t(z)[rev(1:length(y)),]
  
  # Add degree symbols and cardinal directions for longitude and latitude
  x_labels = ifelse(x >= 0,
                    paste(x, "\u00B0E", sep = ""),
                    paste(abs(x), "\u00B0W", sep = ""))
  y_labels = ifelse(y >= 0,
                    paste(round(y, digits = 3), "\u00B0N", sep = ""),
                    paste(abs(round(y, digits = 3)), "\u00B0S", sep = ""))
  
  # Apply labels to correlation map data
  colnames(zt) = x_labels
  rownames(zt) = y_labels  
  
  return(zt)
}


#' (Correlation) Generate Metadata from Customization Inputs
#'
#' Collects and flattens all user-specified inputs for correlation plot generation,
#' including shared settings, map and timeseries parameters. Useful for saving or exporting session state.
#'
#' @param ALL Inputs or Reactives Values
#'
#' @return A one-row data frame containing all metadata inputs, flattened for export or saving.

generate_metadata_correlation <- function(
    # Shared inputs
  range_years3 = NA,
  range_latitude_v1 = NA,
  range_longitude_v1 = NA,
  range_latitude_v2 = NA,
  range_longitude_v2 = NA,
  range_months_v1 = NA,
  range_months_v2 = NA,
  dataset_selected_v1 = NA,
  dataset_selected_v2 = NA,
  ME_variable_v1 = NA,
  ME_variable_v2 = NA,
  ref_period_sg_v1 = NA,
  ref_period_sg_v2 = NA,
  ref_period_v1 = NA,
  ref_period_v2 = NA,
  ref_single_year_v1 = NA,
  ref_single_year_v2 = NA,
  season_selected_v1 = NA,
  season_selected_v2 = NA,
  coordinates_type_v1 = NA,
  coordinates_type_v2 = NA,
  mode_selected_v1 = NA,
  mode_selected_v2 = NA,
  source_v1 = NA,
  source_v2 = NA,
  type_v1 = NA,
  type_v2 = NA,
  lagyears_v1_cor = NA,
  lagyears_v2_cor = NA,
  
  # TS
  axis_input_ts3 = NA,
  axis_mode_ts3 = NA,
  cor_method_ts = NA,
  custom_ts3 = NA,
  custom_ref_ts3 = NA,
  custom_average_ts3 = NA,
  enable_custom_statistics_ts3 = NA,
  download_options_ts3 = NA,
  file_type_timeseries3 = NA,
  key_position_ts3 = NA,
  title_mode_ts3 = NA,
  title_size_input_ts3 = NA,
  title1_input_ts3 = NA,
  xaxis_numeric_interval_ts3 = NA,
  year_moving_ts3 = NA,
  add_outliers_ref_ts3 = NA,
  add_trend_ref_ts3 = NA,
  show_key_ts3 = NA,
  show_key_ref_ts3 = NA,
  show_ticks_ts3 = NA,
  sd_input_ref_ts3 = NA,
  trend_sd_input_ref_ts3 = NA,
  
  # Map
  axis_input3 = NA,
  axis_mode3 = NA,
  center_lat3 = NA,
  center_lon3 = NA,
  custom_map3 = NA,
  custom_topo3 = NA,
  download_options3 = NA,
  file_type_map3 = NA,
  file_type_map_sec3 = NA,
  hide_axis3 = NA,
  hide_borders3 = NA,
  label_lakes3 = NA,
  label_mountains3 = NA,
  label_rivers3 = NA,
  projection3 = NA,
  ref_map_mode3 = NA,
  cor_method_map = NA,
  cor_method_map_data = NA,
  show_lakes3 = NA,
  show_mountains3 = NA,
  show_rivers3 = NA,
  title_mode3 = NA,
  title_size_input3 = NA,
  title1_input3 = NA,
  title2_input3 = NA,
  white_land3 = NA,
  white_ocean3 = NA,
  
  # Reactive
  lonlat_vals_v1 = NA,
  lonlat_vals_v2 = NA,
  plotOrder = NA,
  availableLayers = NA
) {
  collapse_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    paste(as.character(x), collapse = ", ")
  }
  
  meta <- data.frame(
    # Shared
    range_years3 = collapse_or_na(range_years3),
    range_latitude_v1 = collapse_or_na(range_latitude_v1),
    range_longitude_v1 = collapse_or_na(range_longitude_v1),
    range_latitude_v2 = collapse_or_na(range_latitude_v2),
    range_longitude_v2 = collapse_or_na(range_longitude_v2),
    range_months_v1 = collapse_or_na(range_months_v1),
    range_months_v2 = collapse_or_na(range_months_v2),
    dataset_selected_v1 = dataset_selected_v1,
    dataset_selected_v2 = dataset_selected_v2,
    ME_variable_v1 = ME_variable_v1,
    ME_variable_v2 = ME_variable_v2,
    ref_period_sg_v1 = ref_period_sg_v1,
    ref_period_sg_v2 = ref_period_sg_v2,
    ref_period_v1 = collapse_or_na(ref_period_v1),
    ref_period_v2 = collapse_or_na(ref_period_v2),
    ref_single_year_v1 = ref_single_year_v1,
    ref_single_year_v2 = ref_single_year_v2,
    season_selected_v1 = season_selected_v1,
    season_selected_v2 = season_selected_v2,
    coordinates_type_v1 = coordinates_type_v1,
    coordinates_type_v2 = coordinates_type_v2,
    mode_selected_v1 = mode_selected_v1,
    mode_selected_v2 = mode_selected_v2,
    source_v1 = source_v1,
    source_v2 = source_v2,
    type_v1 = type_v1,
    type_v2 = type_v2,
    lagyears_v1_cor = lagyears_v1_cor,
    lagyears_v2_cor = lagyears_v2_cor,
    
    # TS
    axis_input_ts3 = collapse_or_na(axis_input_ts3),
    axis_mode_ts3 = axis_mode_ts3,
    cor_method_ts = cor_method_ts,
    custom_ts3 = custom_ts3,
    custom_ref_ts3 = custom_ref_ts3,
    custom_average_ts3 = custom_average_ts3,
    enable_custom_statistics_ts3 = enable_custom_statistics_ts3,
    download_options_ts3 = download_options_ts3,
    file_type_timeseries3 = file_type_timeseries3,
    key_position_ts3 = key_position_ts3,
    title_mode_ts3 = title_mode_ts3,
    title_size_input_ts3 = title_size_input_ts3,
    title1_input_ts3 = title1_input_ts3,
    xaxis_numeric_interval_ts3 = xaxis_numeric_interval_ts3,
    year_moving_ts3 = year_moving_ts3,
    add_outliers_ref_ts3 = add_outliers_ref_ts3,
    add_trend_ref_ts3 = add_trend_ref_ts3,
    show_key_ts3 = show_key_ts3,
    show_key_ref_ts3 = show_key_ref_ts3,
    show_ticks_ts3 = show_ticks_ts3,
    sd_input_ref_ts3 = sd_input_ref_ts3,
    trend_sd_input_ref_ts3 = trend_sd_input_ref_ts3,
    
    # Map
    axis_input3 = collapse_or_na(axis_input3),
    axis_mode3 = axis_mode3,
    center_lat3 = center_lat3,
    center_lon3 = center_lon3,
    custom_map3 = custom_map3,
    custom_topo3 = custom_topo3,
    download_options3 = download_options3,
    file_type_map3 = file_type_map3,
    file_type_map_sec3 = file_type_map_sec3,
    hide_axis3 = hide_axis3,
    hide_borders3 = hide_borders3,
    label_lakes3 = label_lakes3,
    label_mountains3 = label_mountains3,
    label_rivers3 = label_rivers3,
    projection3 = projection3,
    ref_map_mode3 = ref_map_mode3,
    cor_method_map = cor_method_map,
    cor_method_map_data = cor_method_map_data,
    show_lakes3 = show_lakes3,
    show_mountains3 = show_mountains3,
    show_rivers3 = show_rivers3,
    title_mode3 = title_mode3,
    title_size_input3 = title_size_input3,
    title1_input3 = title1_input3,
    title2_input3 = title2_input3,
    white_land3 = white_land3,
    white_ocean3 = white_ocean3,
    
    # Reactive
    lonlat_vals_v1 = collapse_or_na(as.vector(lonlat_vals_v1)),
    lonlat_vals_v2 = collapse_or_na(as.vector(lonlat_vals_v2)),
    plotOrder = collapse_or_na(plotOrder),
    availableLayers = collapse_or_na(availableLayers),
    
    stringsAsFactors = FALSE
  )
  
  return(meta)
}


#' (Correlation) Process Uploaded Metadata
#'
#' Reads and applies user-saved metadata and optional UI-related data tables from an Excel file,
#' updating Shiny UI inputs and reactive values to restore a previous correlation plot configuration.
#'
#' @param file_path Character. Filepath to the Excel metadata file.
#' @param mode Character. Visualization mode: either `"map"` or `"ts"`.
#' @param metadata_sheet Character. Sheet name in Excel file containing the metadata (default: `"custom_meta"`).
#' @param df_ts_points Character. Optional sheet name containing timeseries point data.
#' @param df_ts_highlights Character. Optional sheet name containing timeseries highlight data.
#' @param df_ts_lines Character. Optional sheet name containing timeseries line data.
#' @param df_map_points Character. Optional sheet name containing map point data.
#' @param df_map_highlights Character. Optional sheet name containing map highlight data.
#' @param rv_plotOrder ReactiveVal. Stores the current order of plot layers.
#' @param rv_availableLayers ReactiveVal. Stores the available layers for display.
#' @param rv_lonlat_vals_v1 ReactiveVal. Stores lon/lat coordinates for variable 1.
#' @param rv_lonlat_vals_v2 ReactiveVal. Stores lon/lat coordinates for variable 2.
#' @param map_points_data ReactiveVal. Function to update map point overlay data.
#' @param map_highlights_data ReactiveVal. Function to update map highlight overlay data.
#' @param ts_points_data ReactiveVal. Function to update timeseries point data.
#' @param ts_highlights_data ReactiveVal. Function to update timeseries highlight data.
#' @param ts_lines_data ReactiveVal. Function to update timeseries line data.
#'
#' @return This function is called for its side effects. It updates Shiny inputs and reactive values.

process_uploaded_metadata_correlation <- function(
    file_path,
    mode = c("map", "ts"),
    metadata_sheet = "custom_meta",
    df_ts_points = NULL,
    df_ts_highlights = NULL,
    df_ts_lines = NULL,
    df_map_points = NULL,
    df_map_highlights = NULL,
    rv_plotOrder = NULL,
    rv_availableLayers = NULL,
    rv_lonlat_vals_v1 = NULL,
    rv_lonlat_vals_v2 = NULL,
    map_points_data = NULL,
    map_highlights_data = NULL,
    ts_points_data = NULL,
    ts_highlights_data = NULL,
    ts_lines_data = NULL
) {
  mode <- match.arg(mode)
  meta <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet)
  session <- getDefaultReactiveDomain()
  
  # Helpers
  split_numeric_range <- function(value) {
    if (is.null(value) || is.na(value) || !is.character(value)) return(c(NA, NA))
    as.numeric(strsplit(value, ",\\s*")[[1]])
  }
  split_text_vector <- function(value) {
    if (is.null(value) || is.na(value)) return(character(0))
    strsplit(value, ",\\s*")[[1]]
  }
  
  # === Shared Inputs ===
  updateNumericRangeInput(session, "range_years3", value = split_numeric_range(meta[1, "range_years3"]))
  updateSelectInput(session, "dataset_selected_v1", selected = meta[1, "dataset_selected_v1"])
  updateSelectInput(session, "dataset_selected_v2", selected = meta[1, "dataset_selected_v2"])
  updateSelectInput(session, "ME_variable_v1", selected = meta[1, "ME_variable_v1"])
  updateSelectInput(session, "ME_variable_v2", selected = meta[1, "ME_variable_v2"])
  updateRadioButtons(session, "coordinates_type_v1", selected = meta[1, "coordinates_type_v1"])
  updateRadioButtons(session, "coordinates_type_v2", selected = meta[1, "coordinates_type_v2"])
  updateRadioButtons(session, "mode_selected_v1", selected = meta[1, "mode_selected_v1"])
  updateRadioButtons(session, "mode_selected_v2", selected = meta[1, "mode_selected_v2"])
  updateRadioButtons(session, "season_selected_v1", selected = meta[1, "season_selected_v1"])
  updateRadioButtons(session, "season_selected_v2", selected = meta[1, "season_selected_v2"])
  shinyWidgets::updateSliderTextInput(session, "range_months_v1", selected = split_text_vector(meta[1, "range_months_v1"]))
  shinyWidgets::updateSliderTextInput(session, "range_months_v2", selected = split_text_vector(meta[1, "range_months_v2"]))
  updateNumericRangeInput(session, "range_latitude_v1", value = split_numeric_range(meta[1, "range_latitude_v1"]))
  updateNumericRangeInput(session, "range_latitude_v2", value = split_numeric_range(meta[1, "range_latitude_v2"]))
  updateNumericRangeInput(session, "range_longitude_v1", value = split_numeric_range(meta[1, "range_longitude_v1"]))
  updateNumericRangeInput(session, "range_longitude_v2", value = split_numeric_range(meta[1, "range_longitude_v2"]))
  updateNumericInput(session, "ref_period_sg_v1", value = meta[1, "ref_period_sg_v1"])
  updateNumericInput(session, "ref_period_sg_v2", value = meta[1, "ref_period_sg_v2"])
  updateNumericRangeInput(session, "ref_period_v1", value = split_numeric_range(meta[1, "ref_period_v1"]))
  updateNumericRangeInput(session, "ref_period_v2", value = split_numeric_range(meta[1, "ref_period_v2"]))
  updateCheckboxInput(session, "ref_single_year_v1", value = as.logical(meta[1, "ref_single_year_v1"]))
  updateCheckboxInput(session, "ref_single_year_v2", value = as.logical(meta[1, "ref_single_year_v2"]))
  updateRadioButtons(session, "source_v1", selected = meta[1, "source_v1"])
  updateRadioButtons(session, "source_v2", selected = meta[1, "source_v2"])
  updateRadioButtons(session, "type_v1", selected = meta[1, "type_v1"])
  updateRadioButtons(session, "type_v2", selected = meta[1, "type_v2"])
  updateNumericInput(session, "lagyears_v1_cor", value = meta[1, "lagyears_v1_cor"])
  updateNumericInput(session, "lagyears_v2_cor", value = meta[1, "lagyears_v2_cor"])
  
  if (mode == "map") {
    # === Map UI ===
    updateRadioButtons(session, "axis_mode3", selected = meta[1, "axis_mode3"])
    updateNumericRangeInput(session, "axis_input3", value = split_numeric_range(meta[1, "axis_input3"]))
    updateNumericInput(session, "center_lat3", value = meta[1, "center_lat3"])
    updateNumericInput(session, "center_lon3", value = meta[1, "center_lon3"])
    updateCheckboxInput(session, "custom_map3", value = as.logical(meta[1, "custom_map3"]))
    updateCheckboxInput(session, "custom_topo3", value = as.logical(meta[1, "custom_topo3"]))
    updateCheckboxInput(session, "download_options3", value = as.logical(meta[1, "download_options3"]))
    updateRadioButtons(session, "file_type_map3", selected = meta[1, "file_type_map3"])
    updateRadioButtons(session, "file_type_map_sec3", selected = meta[1, "file_type_map_sec3"])
    updateCheckboxInput(session, "hide_axis3", value = as.logical(meta[1, "hide_axis3"]))
    updateCheckboxInput(session, "hide_borders3", value = as.logical(meta[1, "hide_borders3"]))
    updateCheckboxInput(session, "label_lakes3", value = as.logical(meta[1, "label_lakes3"]))
    updateCheckboxInput(session, "label_rivers3", value = as.logical(meta[1, "label_rivers3"]))
    updateCheckboxInput(session, "label_mountains3", value = as.logical(meta[1, "label_mountains3"]))
    updateCheckboxInput(session, "show_lakes3", value = as.logical(meta[1, "show_lakes3"]))
    updateCheckboxInput(session, "show_rivers3", value = as.logical(meta[1, "show_rivers3"]))
    updateCheckboxInput(session, "show_mountains3", value = as.logical(meta[1, "show_mountains3"]))
    updateSelectInput(session, "projection3", selected = meta[1, "projection3"])
    updateRadioButtons(session, "ref_map_mode3", selected = meta[1, "ref_map_mode3"])
    updateRadioButtons(session, "cor_method_map", selected = meta[1, "cor_method_map"])
    updateRadioButtons(session, "cor_method_map_data", selected = meta[1, "cor_method_map_data"])
    updateRadioButtons(session, "title_mode3", selected = meta[1, "title_mode3"])
    updateTextInput(session, "title1_input3", value = meta[1, "title1_input3"])
    updateTextInput(session, "title2_input3", value = meta[1, "title2_input3"])
    updateNumericInput(session, "title_size_input3", value = meta[1, "title_size_input3"])
    updateCheckboxInput(session, "white_land3", value = as.logical(meta[1, "white_land3"]))
    updateCheckboxInput(session, "white_ocean3", value = as.logical(meta[1, "white_ocean3"]))
    
    # === Update Map DataFrames ===
    if (!is.null(df_map_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_points)
      if (!is.null(df) && nrow(df) > 0) map_points_data(df)
    }
    if (!is.null(df_map_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_highlights)
      if (!is.null(df) && nrow(df) > 0) map_highlights_data(df)
    }
    
    # === Update Map Shp Plots ===
    if (!is.null(rv_plotOrder)) {
      rv_plotOrder(character(0))
    }
    if (!is.null(rv_availableLayers)) {
      rv_availableLayers(character(0))
    }
  }
  
  if (mode == "ts") {
    # TS specific
    updateRadioButtons(session, "axis_mode_ts3", selected = meta[1, "axis_mode_ts3"])
    updateNumericRangeInput(session, "axis_input_ts3", value = split_numeric_range(meta[1, "axis_input_ts3"]))
    updateRadioButtons(session, "cor_method_ts", selected = meta[1, "cor_method_ts"])
    updateCheckboxInput(session, "custom_ts3", value = as.logical(meta[1, "custom_ts3"]))
    updateCheckboxInput(session, "custom_ref_ts3", value = as.logical(meta[1, "custom_ref_ts3"]))
    updateCheckboxInput(session, "custom_average_ts3", value = as.logical(meta[1, "custom_average_ts3"]))
    updateCheckboxInput(session, "enable_custom_statistics_ts3", value = as.logical(meta[1, "enable_custom_statistics_ts3"]))
    updateCheckboxInput(session, "download_options_ts3", value = as.logical(meta[1, "download_options_ts3"]))
    updateRadioButtons(session, "file_type_timeseries3", selected = meta[1, "file_type_timeseries3"])
    updateRadioButtons(session, "key_position_ts3", selected = meta[1, "key_position_ts3"])
    updateRadioButtons(session, "title_mode_ts3", selected = meta[1, "title_mode_ts3"])
    updateTextInput(session, "title1_input_ts3", value = meta[1, "title1_input_ts3"])
    updateNumericInput(session, "title_size_input_ts3", value = meta[1, "title_size_input_ts3"])
    updateNumericInput(session, "xaxis_numeric_interval_ts3", value = meta[1, "xaxis_numeric_interval_ts3"])
    updateNumericInput(session, "year_moving_ts3", value = meta[1, "year_moving_ts3"])
    updateRadioButtons(session, "add_outliers_ref_ts3", selected = meta[1, "add_outliers_ref_ts3"])
    updateCheckboxInput(session, "add_trend_ref_ts3", value = as.logical(meta[1, "add_trend_ref_ts3"]))
    updateCheckboxInput(session, "show_key_ts3", value = as.logical(meta[1, "show_key_ts3"]))
    updateCheckboxInput(session, "show_key_ref_ts3", value = as.logical(meta[1, "show_key_ref_ts3"]))
    updateCheckboxInput(session, "show_ticks_ts3", value = as.logical(meta[1, "show_ticks_ts3"]))
    updateNumericInput(session, "sd_input_ref_ts3", value = meta[1, "sd_input_ref_ts3"])
    updateNumericInput(session, "trend_sd_input_ref_ts3", value = meta[1, "trend_sd_input_ref_ts3"])
    
    # === Update TS DataFrames ===
    if (!is.null(df_ts_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_points)
      if (!is.null(df) && nrow(df) > 0) ts_points_data(df)
    }
    
    if (!is.null(df_ts_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_highlights)
      if (!is.null(df) && nrow(df) > 0) ts_highlights_data(df)
    }
    
    if (!is.null(df_ts_lines)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_lines)
      if (!is.null(df) && nrow(df) > 0) ts_lines_data(df)
    }
  }
  
  # === ReactiveVals ===
  if (!is.null(rv_lonlat_vals_v1)) {
    lon_vals1 <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals_v1"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals1)) rv_lonlat_vals_v1(lon_vals1)
  }
  if (!is.null(rv_lonlat_vals_v2)) {
    lon_vals2 <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals_v2"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals2)) rv_lonlat_vals_v2(lon_vals2)
  }
}

#### Regression Functions ####

#' (Regression) Create ModE-RA Timeseries Data
#'
#' Creates a timeseries dataset for one or more ModE-RA variables to be used as independent variables in regression.
#' Internally applies a sequence of preprocessing steps, including subsetting, anomaly conversion, and spatial weighting.
#' 
#' @param dataset Character. Name of the ModE-RA dataset.
#' @param variables Character vector. One or more variable names to extract timeseries for.
#' @param subset_lon_IDs Integer vector. Longitude indices for spatial subsetting.
#' @param subset_lat_IDs Integer vector. Latitude indices for spatial subsetting.
#' @param mode Character. Either `"Absolute"` or `"Anomaly"` mode for the final output.
#' @param month_range Integer vector. Vector of months used for temporal subsetting (e.g. `1:12`).
#' @param year_range Integer vector of length 2. Range of years to extract timeseries data for (e.g. `c(1901, 2000)`).
#' @param baseline_range Integer vector of length 2. Range of baseline years for anomaly calculation.
#'
#' @return A data frame containing a `Year` column and one column for each variable's weighted annual average.

create_ME_timeseries_data = function(dataset,
                                     variables,
                                     subset_lon_IDs,
                                     subset_lat_IDs,
                                     mode,
                                     month_range,
                                     year_range,
                                     baseline_range){
  
  # Create year column
  Year = year_range[1]:year_range[2]
  MEts_data = data.frame(Year)
  
  # Precalculations for weighting yearly means
  latlon_weights_reduced = latlon_weights[subset_lat_IDs,subset_lon_IDs]
  weight_function = function(df,llwr){df_weighted = (df*llwr)/sum(llwr)}
  
  # Cycle through each variable
  for (i in variables){
    
    # Generate data_ID for new variable
    data_ref = generate_data_ID(dataset,i,month_range)
    
    # Access variable base data (if pp data not available)
    if (data_ref[1]==0){
      data1 = load_ModE_data(dataset,i)
    } else if (data_ref[1]==2){
      data1 = load_preprocessed_data(data_ref)
    } else {
      data1 = NA
    }
    # Generate latlon subset data
    data2 =  create_latlon_subset(data1, data_ref, subset_lon_IDs, subset_lat_IDs)
    # Generate yearly subset data
    data3 = create_yearly_subset(data2, data_ref, year_range, month_range)
    # Generate reference data
    refd = create_yearly_subset(data2, data_ref, baseline_range, month_range)
    data4 = apply(refd,c(1:2),mean)
    # Generate anomalies data
    if (mode == "Absolute"){ # TBC
      data5 = data3
    } else {
      data5 = convert_subset_to_anomalies(data3,data4)
    }
    
    # Weight yearly means
    data_weighted = apply(data5,c(3),weight_function, t(latlon_weights_reduced))
    
    # Create TS data
    variable_ts_data = data.frame(apply(data_weighted,c(2),sum))
    
    # Add to MEts timeseries dataframe
    MEts_data = data.frame(MEts_data,variable_ts_data)
  }
  
  # Name columns
  colnames(MEts_data) = c("Year",variables)
  
  #if (variable == "Residuals"){
  # Weight yearly means
  #data_weighted = apply(data_input,c(1),weight_function, t(latlon_weights_reduced))
  
  # Create TS data
  #ts_data = data.frame(apply(data_weighted,c(2),sum))
  #} 
  
  return(MEts_data)
}


#' (Regression) Generate Regression Titles
#'
#' Creates a dataframe of titles and labels for regression coefficient, p-value, and residual maps.
#' Handles both user and ModE-RA data sources and supports optional customization of map titles and subtitles.
#'
#' @param independent_source Character. `"ModE-RA"` or `"User data"` for the independent variable.
#' @param dependent_source Character. `"ModE-RA"` or `"User data"` for the dependent variable.
#' @param dataset_i Character. Name of the dataset for the independent variable (if ModE-RA).
#' @param dataset_d Character. Name of the dataset for the dependent variable (if ModE-RA).
#' @param modERA_dependent_variable Character. Variable name from ModE-RA for the dependent variable.
#' @param mode_i Character. `"Absolute"` or `"Anomaly"` mode for the independent variable.
#' @param mode_d Character. `"Absolute"` or `"Anomaly"` mode for the dependent variable.
#' @param month_range_i Integer vector. Months used for independent variable.
#' @param month_range_d Integer vector. Months used for dependent variable.
#' @param lon_range_i Numeric vector. Longitude range for the independent variable.
#' @param lon_range_d Numeric vector. Longitude range for the dependent variable.
#' @param lat_range_i Numeric vector. Latitude range for the independent variable.
#' @param lat_range_d Numeric vector. Latitude range for the dependent variable.
#' @param year_range Integer vector of length 2. Range of years used for regression.
#' @param year_selected Integer. Single year selected for residual plotting.
#' @param independent_variables Character vector. List of all independent variables used.
#' @param dependent_variable Character. Name of the dependent variable.
#' @param iv_number_coeff Integer. Index of independent variable used for coefficient plot.
#' @param iv_number_pvals Integer. Index of independent variable used for p-value plot.
#' @param map_title_mode Character. `"Auto"` or `"Custom"` for title handling.
#' @param map_custom_title Character. Custom map title (optional).
#' @param map_custom_subtitle Character. Custom map subtitle (optional).
#' @param title_size Integer. Font size for map titles (default is 18).
#'
#' @return A single-row dataframe containing various titles and formatting options for regression plots.

generate_regression_titles = function(independent_source,
                                      dependent_source,
                                      dataset_i,
                                      dataset_d,
                                      modERA_dependent_variable,
                                      mode_i,
                                      mode_d,
                                      month_range_i,
                                      month_range_d,
                                      lon_range_i,
                                      lon_range_d,
                                      lat_range_i,
                                      lat_range_d,
                                      year_range,
                                      year_selected,
                                      independent_variables,
                                      dependent_variable,
                                      iv_number_coeff,
                                      iv_number_pvals,
                                      map_title_mode,
                                      map_custom_title,
                                      map_custom_subtitle,
                                      title_size=18){
  
  # Create Independent variable titles
  if (independent_source == "User data"){
    title_months_i = ""
    title_mode_i = ""
    title_lonlat_i = ""
  } else {
    # Generate title months
    title_months_i = paste(dataset_i," ",generate_title_months(MR = month_range_i)," ",sep = "")
    
    # Generate title mode addition
    if (mode_i == "Absolute"){
      title_mode_i = ""
    } else {
      title_mode_i = "Anomaly"
    }
    
    # Generate lon/lat addition
    if (lon_range_i[1]==lon_range_i[2]) {
      lon_title = paste(lon_range_i[1],"\u00B0E",sep = "")
    } else {
      lon_title = paste(lon_range_i[1],":",lon_range_i[2],"\u00B0E", sep = "")
    }
    
    if (lat_range_i[1]==lat_range_i[2]) {
      lat_title = paste(lat_range_i[1],"\u00B0E",sep = "")
    } else {
      lat_title = paste(lat_range_i[1],":",lat_range_i[2],"\u00B0N", sep = "")
    }
    
    title_lonlat_i = paste(" [",lon_title,", ",lat_title,"]",sep = "")
  }
  
  # Create Dependent variable titles
  if (dependent_source == "User data"){
    title_months_d = ""
    title_mode_d = ""
    title_lonlat_d = ""
    color_d = "darkorange2"
    unit_d = ""
  } else {
    # Generate title months
    title_months_d = paste(dataset_d," ",generate_title_months(MR = month_range_d)," ",sep = "")
    
    # Generate title mode addition
    if (mode_d == "Absolute"){
      title_mode_d = ""
    } else {
      title_mode_d = " Anomaly"
    }
    
    # Generate lon/lat addition
    if (lon_range_d[1]==lon_range_d[2]) {
      lon_title = paste(lon_range_d[1],"\u00B0E",sep = "")
    } else {
      lon_title = paste(lon_range_d[1],":",lon_range_d[2],"\u00B0E", sep = "")
    }
    
    if (lat_range_d[1]==lat_range_d[2]) {
      lat_title = paste(lat_range_d[1],"\u00B0E",sep = "")
    } else {
      lat_title = paste(lat_range_d[1],":",lat_range_d[2],"\u00B0N", sep = "")
    }
    
    title_lonlat_d = paste(" [",lon_title,", ",lat_title,"]",sep = "")
    
    # Generate timeseries color & unit
    if (modERA_dependent_variable == "Temperature"){
      color_d = "red3" ; unit_d = "[\u00B0C]"
    } else if (modERA_dependent_variable == "Precipitation"){
      color_d = "turquoise4" ; unit_d = "[mm/month]"
    } else if (modERA_dependent_variable == "SLP"){
      color_d = "purple4" ; unit_d = "[hPa]"
    } else if (modERA_dependent_variable == "Z500"){
      color_d = "green4" ; unit_d = "[m]"
    }
  }
  
  # Create year range title
  title_year_range = paste(year_range[1],"-",year_range[2],sep = "")
  
  # Generate regression map titles
  map_title_coeff <- ifelse(map_title_mode == "Custom" & map_custom_title != "", map_custom_title, "Regression Coefficients")
  map_title_pvals <- ifelse(map_title_mode == "Custom" & map_custom_title != "", map_custom_title, "Regression P Values")
  map_title_res <- ifelse(map_title_mode == "Custom" & map_custom_title != "", map_custom_title, "Regression Residuals")
  
  map_subtitle_coeff <- ifelse(map_title_mode == "Custom" & map_custom_subtitle != "", map_custom_subtitle,paste(
    "Independent variable:", title_months_i, independent_variables[iv_number_coeff],
    title_mode_i, title_lonlat_i, "\nDependent variable:",
    title_months_d, dependent_variable, title_mode_d,
    "\nTime range:", title_year_range
  ))
  
  map_subtitle_pvals <- ifelse(map_title_mode == "Custom" & map_custom_subtitle != "", map_custom_subtitle, paste(
    "Independent variable:", title_months_i, independent_variables[iv_number_pvals],
    title_mode_i, "\nDependent variable:",
    title_months_d, dependent_variable, title_mode_d,
    "\nTime range:", title_year_range
  ))
  
  map_subtitle_res <- ifelse(map_title_mode == "Custom" & map_custom_subtitle != "", map_custom_subtitle, paste(
    "Independent variables:", title_months_i, paste(independent_variables, collapse = " ; "),
    title_mode_i, title_lonlat_i, "\nDependent variable:",
    title_months_d, dependent_variable, title_mode_d,
    "\nYear:", year_selected
  ))
  
  # Adapt title size
  map_title_size_coeff <- ifelse(map_title_mode == "Custom", title_size, 18)
  map_title_size_pvals <- ifelse(map_title_mode == "Custom", title_size, 18)
  map_title_size_res <- ifelse(map_title_mode == "Custom", title_size, 18)
  
  # Generate download titles
  tf0 = paste("Reg",title_months_i,"ind. var.", ">", title_months_d, modERA_dependent_variable)
  tf1 = gsub("[[:punct:]]", "", tf0)
  tf2 = gsub(" ","-",tf1)
  file_title = iconv(tf2, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Title font size
  map_title_size = title_size
  
  # Combine all titles into a dataframe
  titles_df = data.frame(
    title_months_i,
    title_mode_i,
    title_lonlat_i,
    title_months_d,
    title_mode_d,
    title_lonlat_d,
    color_d,
    unit_d,
    title_year_range,
    file_title,
    map_title_coeff,
    map_title_pvals,
    map_title_res,
    map_subtitle_coeff,
    map_subtitle_pvals,
    map_subtitle_res,
    map_title_size_coeff,
    map_title_size_pvals,
    map_title_size_res
  )
  
  return(titles_df)
}


#' (Regression) Generate Regression Titles for Timeseries Plots
#'
#' Creates a dataframe of titles, axis labels, and subtitles for regression timeseries plots,
#' including metadata for dependent and independent variables and customization options.
#'
#' @param independent_source Character. `"ModE-RA"` or `"User data"` for the independent variable source.
#' @param dependent_source Character. `"ModE-RA"` or `"User data"` for the dependent variable source.
#' @param dataset_i Character. Dataset name for the independent variable (if ModE-RA).
#' @param dataset_d Character. Dataset name for the dependent variable (if ModE-RA).
#' @param modERA_dependent_variable Character. ModE-RA variable name for the dependent variable.
#' @param mode_i Character. `"Absolute"` or `"Anomaly"` for the independent variable mode.
#' @param mode_d Character. `"Absolute"` or `"Anomaly"` for the dependent variable mode.
#' @param month_range_i Integer vector. Month range for the independent variable.
#' @param month_range_d Integer vector. Month range for the dependent variable.
#' @param lon_range_i Numeric vector. Longitude range for the independent variable.
#' @param lon_range_d Numeric vector. Longitude range for the dependent variable.
#' @param lat_range_i Numeric vector. Latitude range for the independent variable.
#' @param lat_range_d Numeric vector. Latitude range for the dependent variable.
#' @param year_range Integer vector of length 2. Year range for the regression period.
#' @param year_selected Integer. Specific year for residual plot context.
#' @param independent_variables Character vector. Names of all independent variables used.
#' @param dependent_variable Character. Name of the dependent variable.
#' @param iv_number_coeff Integer. Index of the independent variable used for coefficient context.
#' @param iv_number_pvals Integer. Index of the independent variable used for p-value context.
#' @param map_title_mode Character. `"Auto"` or `"Custom"` to control title generation mode.
#' @param map_custom_title Character. Optional custom title for maps and timeseries.
#' @param map_custom_subtitle Character. Optional custom subtitle for maps and timeseries.
#' @param title_size Integer. Font size for titles and subtitles (default: user-defined).
#'
#' @return A single-row dataframe containing metadata used for timeseries regression plot labeling,
#' including axis labels, titles, subtitles, and download-safe file title.

generate_regression_titles_ts = function(independent_source,
                                         dependent_source,
                                         dataset_i,
                                         dataset_d,
                                         modERA_dependent_variable,
                                         mode_i,
                                         mode_d,
                                         month_range_i,
                                         month_range_d,
                                         lon_range_i,
                                         lon_range_d,
                                         lat_range_i,
                                         lat_range_d,
                                         year_range,
                                         year_selected,
                                         independent_variables,
                                         dependent_variable,
                                         iv_number_coeff,
                                         iv_number_pvals,
                                         map_title_mode,
                                         map_custom_title,
                                         map_custom_subtitle,
                                         title_size
){
  
  # Create Independent variable titles
  if (independent_source == "User data"){
    title_months_i = ""
    title_mode_i = ""
    title_lonlat_i = ""
  } else {
    # Generate title months
    title_months_i = paste(dataset_i," ",generate_title_months(MR = month_range_i)," ",sep = "")
    
    # Generate title mode addition
    if (mode_i == "Absolute"){
      title_mode_i = ""
    } else {
      title_mode_i = "Anomaly"
    }
    
    # Generate lon/lat addition
    if (lon_range_i[1]==lon_range_i[2]) {
      lon_title = paste(lon_range_i[1],"\u00B0E",sep = "")
    } else {
      lon_title = paste(lon_range_i[1],":",lon_range_i[2],"\u00B0E", sep = "")
    }
    
    if (lat_range_i[1]==lat_range_i[2]) {
      lat_title = paste(lat_range_i[1],"\u00B0E",sep = "")
    } else {
      lat_title = paste(lat_range_i[1],":",lat_range_i[2],"\u00B0N", sep = "")
    }
    
    title_lonlat_i = paste(" [",lon_title,", ",lat_title,"]",sep = "")
  }
  
  # Create Dependent variable titles
  if (dependent_source == "User data"){
    title_months_d = ""
    title_mode_d = ""
    title_lonlat_d = ""
    color_d = "darkorange2"
    unit_d = ""
  } else {
    # Generate title months
    title_months_d = paste(dataset_d," ",generate_title_months(MR = month_range_d)," ",sep = "")
    
    # Generate title mode addition
    if (mode_d == "Absolute"){
      title_mode_d = ""
    } else {
      title_mode_d = " Anomaly"
    }
    
    # Generate lon/lat addition
    if (lon_range_d[1]==lon_range_d[2]) {
      lon_title = paste(lon_range_d[1],"\u00B0E",sep = "")
    } else {
      lon_title = paste(lon_range_d[1],":",lon_range_d[2],"\u00B0E", sep = "")
    }
    
    if (lat_range_d[1]==lat_range_d[2]) {
      lat_title = paste(lat_range_d[1],"\u00B0E",sep = "")
    } else {
      lat_title = paste(lat_range_d[1],":",lat_range_d[2],"\u00B0N", sep = "")
    }
    
    title_lonlat_d = paste(" [",lon_title,", ",lat_title,"]",sep = "")
    
    # Generate timeseries color & unit
    if (modERA_dependent_variable == "Temperature"){
      color_d = "red3" ; unit_d = "[\u00B0C]"
    } else if (modERA_dependent_variable == "Precipitation"){
      color_d = "turquoise4" ; unit_d = "[mm/month]"
    } else if (modERA_dependent_variable == "SLP"){
      color_d = "purple4" ; unit_d = "[hPa]"
    } else if (modERA_dependent_variable == "Z500"){
      color_d = "green4" ; unit_d = "[m]"
    }
  }
  
  # Create year range title
  title_year_range = paste(year_range[1],"-",year_range[2],sep = "")
  
  # Generate regression map titles for coefficients
  map_subtitle_coeff = paste("Independent variable: ", title_months_i, independent_variables[iv_number_coeff],
                             " ", title_mode_i, title_lonlat_i, "\nDependent variable: ",
                             title_months_d, dependent_variable,
                             title_mode_d, "\nTime range:", title_year_range, sep = "")
  
  # Generate regression map titles for p-values
  map_subtitle_pvals = paste("Independent variable: ", title_months_i, independent_variables[iv_number_pvals],
                             " ", title_mode_i, "\nDependent variable: ",
                             title_months_d, dependent_variable,
                             title_mode_d, "\nTime range:", title_year_range, sep = "")
  
  # Generate regression map titles for residuals
  title_variables_i = paste(independent_variables, collapse = " ; ")
  map_subtitle_res = paste("Independent variables: ", title_months_i, title_variables_i,
                           " ", title_mode_i, title_lonlat_i, "\nDependent variable: ",
                           title_months_d, dependent_variable,
                           title_mode_d, "\nYear:", year_selected, sep = "")
  
  # Generate download titles
  tf0 = paste("Reg",title_months_i,"ind. var.", ">", title_months_d, modERA_dependent_variable)
  tf1 = gsub("[[:punct:]]", "", tf0)
  tf2 = gsub(" ","-",tf1)
  file_title = iconv(tf2, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Generate TS titles
  ts_axis = paste(dependent_variable,unit_d)
  
  if (map_title_mode == "Custom") {
    ts_title = map_custom_title
    ts_subtitle = map_custom_subtitle
  } else {
    ts_title = "Regression Timeseries"
    ts_subtitle = paste("Independent variables: ", title_months_i, title_variables_i,
                        " ", title_mode_i, title_lonlat_i, "\nDependent variable: ",
                        title_months_d, dependent_variable,
                        title_mode_d, sep = "")
  }
  
  ts_title_size = title_size
  
  # Combine all titles into a dataframe
  titles_df = data.frame(
    title_months_i,
    title_mode_i,
    title_lonlat_i,
    title_months_d,
    title_mode_d,
    title_lonlat_d,
    color_d,
    unit_d,
    title_year_range,
    file_title,
    map_subtitle_coeff,
    map_subtitle_pvals,
    map_subtitle_res,
    ts_axis,
    ts_title,
    ts_subtitle,
    ts_title_size
  )
  
  return(titles_df)
}


#' (Regression) Calculate Summary Data
#'
#' Performs linear regression on one or more independent variables against a single
#' dependent variable to generate a regression model object.
#'
#' @param independent_variable_data Data frame. Timeseries data containing one or more independent variables
#' @param dependent_variable_data Data frame. Timeseries data for the dependent variable;
#' @param independent_variables Character vector. Names of the independent variables
#'
#' @return A linear model object (class \code{"lm"}) containing the fitted regression model.
#'   This can be passed to \code{summary()} or \code{broom::tidy()} for detailed output.

create_regression_summary_data = function(independent_variable_data,
                                          dependent_variable_data,
                                          independent_variables) {
  # --- Ensure both datasets have a 'Year' column ---
  if (!"Year" %in% colnames(independent_variable_data) ||
      !"Year" %in% colnames(dependent_variable_data)) {
    stop("Both datasets must contain a 'Year' column.")
  }
  
  # --- Align by Year ---
  merged_data = merge(
    x = dependent_variable_data,
    y = independent_variable_data,
    by = "Year",
    all = FALSE # only keep overlapping years
  )
  
  # --- Extract y (dependent) and x (independent) ---
  y = as.matrix(merged_data[, 2, drop = FALSE])
  x = as.matrix(merged_data[, independent_variables, drop = FALSE])
  
  # --- Perform regression (handles internal NAs gracefully) ---
  regression_data = lm(y ~ x, na.action = na.exclude)
  
  return(regression_data)
}


#' (Regression) Calculate Regression Coefficients for Mapping
#'
#' Computes spatially distributed regression coefficients for each grid cell,
#' where each time series at a grid point is regressed on one or more independent variables.
#'
#' @param independent_variable_data Data frame. Timeseries data for one or more independent variables
#' @param dependent_variable_data 3D array. Yearly ModE-RA data (either absolute or anomaly) with dimensions
#' @param independent_variables Character vector. Names of the independent variables to use from
#'
#' @return A 3D array of regression coefficients (excluding intercepts) with dimensions

create_regression_coeff_data = function(independent_variable_data,
                                        dependent_variable_data,
                                        independent_variables){
  reg_coeffs <- apply(dependent_variable_data, c(1:2), function(fy,fx) lm(fy~fx)$coef,as.matrix(independent_variable_data[,independent_variables]))[-1,,]
  # Note that [-1,,] removes the intercepts from the coef data 
  return(reg_coeffs)
}


#' (Regression) Calculate Regression P-values for Mapping
#'
#' Computes spatially distributed regression p-values for each grid cell,
#' based on linear regression of the grid point time series on one or more
#' independent variables.
#'
#' @param independent_variable_data Data frame. Timeseries data for one or more independent variables.
#' @param dependent_variable_data 3D array. Yearly ModE-RA data (absolute or anomaly).
#' @param independent_variables Character vector. Names of the independent variables from \code{independent_variable_data}.
#'
#' @return 3D array of p-values (excluding intercepts) with dimensions [independent variables, lon, lat].

create_regression_pvalue_data = function(independent_variable_data,
                                         dependent_variable_data,
                                         independent_variables){
  # Define lmp function
  lmp <- function (fy,fx) {
    f1 <- lm(fy~fx)
    f2 <- summary.lm(f1)$coefficients[,4]
    return(f2)
  }
  
  reg_pvalues = apply(dependent_variable_data, c(1:2),lmp,as.matrix(independent_variable_data[,independent_variables]))[-1,,]
  # Note that [-1,,] removes the intercepts from the pvalue data 
  return(reg_pvalues)
}


#' (Regression) Calculate Regression Residuals for Mapping and Timeseries
#'
#' Computes residuals from multiple linear regression between timeseries predictors and a ModE-RA field. 
#' Used for plotting spatial or timeseries regression residuals.
#'
#' @param independent_variable_data A data frame of timeseries for one or more independent variables (columns = variables, rows = years).
#' @param dependent_variable_data A 3D array of ModE-RA data (lon x lat x year) representing the dependent variable.
#' @param independent_variables A character vector of column names in \code{independent_variable_data} to be used as predictors.
#'
#' @return A 3D array (lon x lat x year) of regression residuals excluding model intercepts.

create_regression_residuals = function(independent_variable_data,
                                       dependent_variable_data,
                                       independent_variables){
  reg_residuals <- apply(dependent_variable_data, c(1:2), function(fy,fx) lm(fy~fx)$residuals,as.matrix(independent_variable_data[,independent_variables]))
  return(reg_residuals)
}


#' (Regression) Create Regression Map Datatable
#'
#' Generates a labeled 2D datatable (latitude x longitude) from spatial regression outputs such as 
#' coefficients, p-values, or residuals for display or export purposes.
#'
#' @param data_input A 2D numeric matrix (lon x lat) representing the regression result to be formatted.
#' @param subset_lon_IDs Integer vector of selected longitude indices used in the regression.
#' @param subset_lat_IDs Integer vector of selected latitude indices used in the regression.
#'
#' @return A labeled 2D matrix (lat x lon) with longitude/latitude in degree format as column and row names.

create_regression_map_datatable = function(data_input,
                                           subset_lon_IDs,
                                           subset_lat_IDs){
  
  # find x,y & z values
  x = lon[subset_lon_IDs]
  y = lat[subset_lat_IDs]
  z = data_input
  
  # Transpose
  map_data =t(z)
  
  # Add degree symbols and cardinal directions for longitude and latitude
  x_labels = ifelse(x >= 0,
                    paste(x, "\u00B0E", sep = ""),
                    paste(abs(x), "\u00B0W", sep = ""))
  y_labels = ifelse(y >= 0,
                    paste(round(y, digits = 3), "\u00B0N", sep = ""),
                    paste(abs(round(y, digits = 3)), "\u00B0S", sep = ""))
  
  # Apply labels to map data
  colnames(map_data) = x_labels
  rownames(map_data) = y_labels
  
  return(map_data)
}


#' (Regression) Create Regression Timeseries Datatable
#'
#' Combines the original, trend, and residual timeseries into a labeled dataframe 
#' for display or export following a regression analysis.
#'
#' @param dependent_variable_data Data frame containing the original ModE-RA timeseries for the dependent variable.
#' @param summary_data An object from `create_regression_summary_data`, containing regression summary output.
#' @param regression_titles A data frame from `generate_regression_titles`, used to label columns with units.
#'
#' @return A data frame with columns for Year, Original, Trend, and Residuals, labeled with appropriate units.

create_regression_timeseries_datatable = function(dependent_variable_data,
                                                  summary_data,
                                                  regression_titles) {
  # Extract original timeseries
  years = dependent_variable_data$Year
  original_ts = signif(dependent_variable_data[, 2], digits = 3)
  
  # Extract residuals and fitted values
  residuals_ts = signif(summary_data$residuals, digits = 3)
  fitted_ts = signif(summary_data$fitted.values, digits = 3)
  
  # Align lengths safely
  n = min(length(years), length(fitted_ts), length(residuals_ts))
  years = years[seq_len(n)]
  original_ts = original_ts[seq_len(n)]
  fitted_ts = fitted_ts[seq_len(n)]
  residuals_ts = residuals_ts[seq_len(n)]
  
  # Create dataframe
  regression_ts_df = data.frame(
    Year = years,
    Original = original_ts,
    Trend = fitted_ts,
    Residuals = residuals_ts
  )
  
  # Rename columns with units
  colnames(regression_ts_df) = c(
    "Year",
    paste0("Original", regression_titles$unit_d),
    paste0("Trend", regression_titles$unit_d),
    paste0("Residuals", regression_titles$unit_d)
  )
  
  return(regression_ts_df)
}

#' (Regression) Generate Metadata for Regression Visualization
#'
#' Flattens and consolidates all user selections related to regression map and timeseries plots.
#' Returns a single-row metadata data frame for saving or reloading UI customizations.
#'
#' @param ALL Inputs and Reactive Values from the Plots.
#'
#' @return A single-row data frame containing all flattened metadata used in the regression modules.


generate_metadata_regression <- function(
    # Shared regression inputs
  range_years4 = NA,
  range_latitude_dv = NA,
  range_longitude_dv = NA,
  range_latitude_iv = NA,
  range_longitude_iv = NA,
  range_months_dv = NA,
  range_months_iv = NA,
  dataset_selected_dv = NA,
  dataset_selected_iv = NA,
  ME_variable_dv = NA,
  ME_variable_iv = NA,
  ref_period_sg_dv = NA,
  ref_period_sg_iv = NA,
  ref_period_dv = NA,
  ref_period_iv = NA,
  ref_single_year_dv = NA,
  ref_single_year_iv = NA,
  season_selected_dv = NA,
  season_selected_iv = NA,
  coordinates_type_dv = NA,
  coordinates_type_iv = NA,
  mode_selected_dv = NA,
  mode_selected_iv = NA,
  source_dv = NA,
  source_iv = NA,
  
  # TS regression options
  axis_input_ts4a = NA,
  axis_input_ts4b = NA,
  axis_mode_ts4a = NA,
  axis_mode_ts4b = NA,
  custom_ts4 = NA,
  key_position_ts4 = NA,
  show_ticks_ts4 = NA,
  title_mode_ts4 = NA,
  title_size_input_ts4 = NA,
  title1_input_ts4 = NA,
  title2_input_ts4 = NA,
  xaxis_numeric_interval_ts4 = NA,
  reg_ts_plot_type = NA,
  reg_ts2_plot_type = NA,
  
  # Map regression options
  axis_input_reg = NA,
  axis_mode_reg = NA,
  center_lat_reg = NA,
  center_lon_reg = NA,
  coeff_variable = NA,
  custom_topo_reg = NA,
  download_options = NA,
  hide_axis_reg = NA,
  hide_borders_reg = NA,
  label_lakes_reg = NA,
  label_mountains_reg = NA,
  label_rivers_reg = NA,
  projection_reg = NA,
  reg_plot_type = NA,
  show_lakes_reg = NA,
  show_mountains_reg = NA,
  show_rivers_reg = NA,
  title_mode_reg = NA,
  title_size_input_reg = NA,
  title1_input_reg = NA,
  title2_input_reg = NA,
  white_land_reg = NA,
  white_ocean_reg = NA,
  reg_resi_year = NA,
  
  # Reactive
  lonlat_vals_iv = NA,
  lonlat_vals_dv = NA,
  plotOrder = NA,
  availableLayers = NA
  
) {
  collapse_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    paste(as.character(x), collapse = ", ")
  }
  
  meta <- data.frame(
    # Shared
    range_years4 = collapse_or_na(range_years4),
    range_latitude_dv = collapse_or_na(range_latitude_dv),
    range_longitude_dv = collapse_or_na(range_longitude_dv),
    range_latitude_iv = collapse_or_na(range_latitude_iv),
    range_longitude_iv = collapse_or_na(range_longitude_iv),
    range_months_dv = collapse_or_na(range_months_dv),
    range_months_iv = collapse_or_na(range_months_iv),
    dataset_selected_dv = dataset_selected_dv,
    dataset_selected_iv = collapse_or_na(dataset_selected_iv),
    ME_variable_dv = ME_variable_dv,
    ME_variable_iv = collapse_or_na(ME_variable_iv),
    ref_period_sg_dv = ref_period_sg_dv,
    ref_period_sg_iv = ref_period_sg_iv,
    ref_period_dv = collapse_or_na(ref_period_dv),
    ref_period_iv = collapse_or_na(ref_period_iv),
    ref_single_year_dv = ref_single_year_dv,
    ref_single_year_iv = ref_single_year_iv,
    season_selected_dv = season_selected_dv,
    season_selected_iv = season_selected_iv,
    coordinates_type_dv = coordinates_type_dv,
    coordinates_type_iv = coordinates_type_iv,
    mode_selected_dv = mode_selected_dv,
    mode_selected_iv = mode_selected_iv,
    source_dv = source_dv,
    source_iv = source_iv,
    
    # TS
    axis_input_ts4a = collapse_or_na(axis_input_ts4a),
    axis_input_ts4b = collapse_or_na(axis_input_ts4b),
    axis_mode_ts4a = axis_mode_ts4a,
    axis_mode_ts4b = axis_mode_ts4b,
    custom_ts4 = custom_ts4,
    key_position_ts4 = key_position_ts4,
    show_ticks_ts4 = show_ticks_ts4,
    title_mode_ts4 = title_mode_ts4,
    title_size_input_ts4 = title_size_input_ts4,
    title1_input_ts4 = title1_input_ts4,
    title2_input_ts4 = title2_input_ts4,
    xaxis_numeric_interval_ts4 = xaxis_numeric_interval_ts4,
    reg_ts_plot_type = reg_ts_plot_type,
    reg_ts2_plot_type = reg_ts2_plot_type,
    
    # Map
    axis_input_reg = collapse_or_na(axis_input_reg),
    axis_mode_reg = axis_mode_reg,
    center_lat_reg = center_lat_reg,
    center_lon_reg = center_lon_reg,
    coeff_variable = coeff_variable,
    custom_topo_reg = custom_topo_reg,
    download_options = download_options,
    hide_axis_reg = hide_axis_reg,
    hide_borders_reg = hide_borders_reg,
    label_lakes_reg = label_lakes_reg,
    label_mountains_reg = label_mountains_reg,
    label_rivers_reg = label_rivers_reg,
    projection_reg = projection_reg,
    reg_plot_type = reg_plot_type,
    show_lakes_reg = show_lakes_reg,
    show_mountains_reg = show_mountains_reg,
    show_rivers_reg = show_rivers_reg,
    title_mode_reg = title_mode_reg,
    title_size_input_reg = title_size_input_reg,
    title1_input_reg = title1_input_reg,
    title2_input_reg = title2_input_reg,
    white_land_reg = white_land_reg,
    white_ocean_reg = white_ocean_reg,
    reg_resi_year = reg_resi_year,
    
    # Reactive
    lonlat_vals_iv = collapse_or_na(as.vector(lonlat_vals_iv)),
    lonlat_vals_dv = collapse_or_na(as.vector(lonlat_vals_dv)),
    plotOrder = collapse_or_na(plotOrder),
    availableLayers = collapse_or_na(availableLayers),
    
    stringsAsFactors = FALSE
  )
  
  return(meta)
}


#' (Regression) Process Uploaded Metadata for Regression Visualization
#'
#' Loads regression metadata and optional data sheets from an Excel file to update
#' Shiny UI inputs and reactive values. Supports regression timeseries and map modes:
#' Coefficient, P-Value, and Residual.
#'
#' @param file_path Character. Path to the Excel file containing metadata and optional data.
#' @param mode Character. Regression view mode: one of "ts", "coeff", "pval", or "res".
#' @param metadata_sheet Character. Name of the sheet containing metadata (default is "custom_meta").
#' @param df_ts_points Character or NULL. Sheet name for regression TS point data.
#' @param df_ts_highlights Character or NULL. Sheet name for regression TS highlights.
#' @param df_ts_lines Character or NULL. Sheet name for regression TS line data.
#' @param df_map_points Character or NULL. Sheet name for regression map points.
#' @param df_map_highlights Character or NULL. Sheet name for regression map highlights.
#' @param ts_points_data Reactive function or NULL. Shiny reactive value for storing TS point data.
#' @param ts_highlights_data Reactive function or NULL. Shiny reactive value for TS highlight data.
#' @param ts_lines_data Reactive function or NULL. Shiny reactive value for TS line data.
#' @param map_points_data Reactive function or NULL. Shiny reactive value for map point data.
#' @param map_highlights_data Reactive function or NULL. Shiny reactive value for map highlight data.
#' @param rv_plotOrder Reactive function or NULL. Stores plotting order for map layers.
#' @param rv_availableLayers Reactive function or NULL. Stores available shapefile layers.
#' @param rv_lonlat_vals_iv Reactive function or NULL. Reactive value for IV (independent variable) lon/lat.
#' @param rv_lonlat_vals_dv Reactive function or NULL. Reactive value for DV (dependent variable) lon/lat.
#'
#' @return No return value. The function updates Shiny inputs and reactive values in-place.

process_uploaded_metadata_regression <- function(
    file_path,
    mode = c("ts", "coeff", "pval", "res"),
    metadata_sheet = "custom_meta",
    df_ts_points = NULL,
    df_ts_highlights = NULL,
    df_ts_lines = NULL,
    df_map_points = NULL,
    df_map_highlights = NULL,
    ts_points_data = NULL,
    ts_highlights_data = NULL,
    ts_lines_data = NULL,
    map_points_data = NULL,
    map_highlights_data = NULL,
    rv_plotOrder = NULL,
    rv_availableLayers = NULL,
    rv_lonlat_vals_iv = NULL,
    rv_lonlat_vals_dv = NULL
) {
  mode <- match.arg(mode)
  meta <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet)
  session <- getDefaultReactiveDomain()
  
  # === Helpers ===
  split_numeric_range <- function(value) {
    if (is.null(value) || is.na(value) || !is.character(value)) return(c(NA, NA))
    as.numeric(strsplit(value, ",\\s*")[[1]])
  }
  split_text_vector <- function(value) {
    if (is.null(value) || is.na(value)) return(character(0))
    strsplit(value, ",\\s*")[[1]]
  }
  
  # === Shared Inputs ===
  updateNumericRangeInput(session, "range_years4", value = split_numeric_range(meta[1, "range_years4"]))
  updateSelectInput(session, "dataset_selected_dv", selected = meta[1, "dataset_selected_dv"])
  updateSelectInput(session, "dataset_selected_iv", selected = split_text_vector(meta[1, "dataset_selected_iv"]))
  updateSelectInput(session, "ME_variable_dv", selected = meta[1, "ME_variable_dv"])
  updateSelectInput(session, "ME_variable_iv", selected = split_text_vector(meta[1, "ME_variable_iv"]))
  updateRadioButtons(session, "coordinates_type_dv", selected = meta[1, "coordinates_type_dv"])
  updateRadioButtons(session, "coordinates_type_iv", selected = meta[1, "coordinates_type_iv"])
  updateRadioButtons(session, "mode_selected_dv", selected = meta[1, "mode_selected_dv"])
  updateRadioButtons(session, "mode_selected_iv", selected = meta[1, "mode_selected_iv"])
  updateRadioButtons(session, "season_selected_dv", selected = meta[1, "season_selected_dv"])
  updateRadioButtons(session, "season_selected_iv", selected = meta[1, "season_selected_iv"])
  shinyWidgets::updateSliderTextInput(session, "range_months_dv", selected = split_text_vector(meta[1, "range_months_dv"]))
  shinyWidgets::updateSliderTextInput(session, "range_months_iv", selected = split_text_vector(meta[1, "range_months_iv"]))
  updateNumericRangeInput(session, "range_latitude_dv", value = split_numeric_range(meta[1, "range_latitude_dv"]))
  updateNumericRangeInput(session, "range_longitude_dv", value = split_numeric_range(meta[1, "range_longitude_dv"]))
  updateNumericRangeInput(session, "range_latitude_iv", value = split_numeric_range(meta[1, "range_latitude_iv"]))
  updateNumericRangeInput(session, "range_longitude_iv", value = split_numeric_range(meta[1, "range_longitude_iv"]))
  updateNumericInput(session, "ref_period_sg_dv", value = meta[1, "ref_period_sg_dv"])
  updateNumericInput(session, "ref_period_sg_iv", value = meta[1, "ref_period_sg_iv"])
  updateNumericRangeInput(session, "ref_period_dv", value = split_numeric_range(meta[1, "ref_period_dv"]))
  updateNumericRangeInput(session, "ref_period_iv", value = split_numeric_range(meta[1, "ref_period_iv"]))
  updateCheckboxInput(session, "ref_single_year_dv", value = as.logical(meta[1, "ref_single_year_dv"]))
  updateCheckboxInput(session, "ref_single_year_iv", value = as.logical(meta[1, "ref_single_year_iv"]))
  updateRadioButtons(session, "source_dv", selected = meta[1, "source_dv"])
  updateRadioButtons(session, "source_iv", selected = meta[1, "source_iv"])
  
  # === Timeseries Inputs ===
  if (mode == "ts") {
    updateRadioButtons(session, "axis_mode_ts4a", selected = meta[1, "axis_mode_ts4a"])
    updateRadioButtons(session, "axis_mode_ts4b", selected = meta[1, "axis_mode_ts4b"])
    updateNumericRangeInput(session, "axis_input_ts4a", value = split_numeric_range(meta[1, "axis_input_ts4a"]))
    updateNumericRangeInput(session, "axis_input_ts4b", value = split_numeric_range(meta[1, "axis_input_ts4b"]))
    updateCheckboxInput(session, "custom_ts4", value = as.logical(meta[1, "custom_ts4"]))
    updateRadioButtons(session, "key_position_ts4", selected = meta[1, "key_position_ts4"])
    updateCheckboxInput(session, "show_ticks_ts4", value = as.logical(meta[1, "show_ticks_ts4"]))
    updateRadioButtons(session, "title_mode_ts4", selected = meta[1, "title_mode_ts4"])
    updateNumericInput(session, "title_size_input_ts4", value = meta[1, "title_size_input_ts4"])
    updateTextInput(session, "title1_input_ts4", value = meta[1, "title1_input_ts4"])
    updateTextInput(session, "title2_input_ts4", value = meta[1, "title2_input_ts4"])
    updateNumericInput(session, "xaxis_numeric_interval_ts4", value = meta[1, "xaxis_numeric_interval_ts4"])
    updateRadioButtons(session, "reg_ts_plot_type", selected = meta[1, "reg_ts_plot_type"])
    updateRadioButtons(session, "reg_ts2_plot_type", selected = meta[1, "reg_ts2_plot_type"])
    
    if (!is.null(df_ts_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_points)
      if (!is.null(df) && nrow(df) > 0) ts_points_data(df)
    }
    if (!is.null(df_ts_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_highlights)
      if (!is.null(df) && nrow(df) > 0) ts_highlights_data(df)
    }
    if (!is.null(df_ts_lines)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_ts_lines)
      if (!is.null(df) && nrow(df) > 0) ts_lines_data(df)
    }
  }
  
  # === Map Inputs ===
  if (mode %in% c("coeff", "pval", "res")) {
    suffix <- switch(mode,
                     coeff = "coeff",
                     pval = "pval",
                     res = "res"
    )
    
    updateNumericRangeInput(session, paste0("axis_input_reg_", suffix), value = split_numeric_range(meta[1, "axis_input_reg"]))
    updateRadioButtons(session, paste0("axis_mode_reg_", suffix), selected = meta[1, "axis_mode_reg"])
    updateNumericInput(session, paste0("center_lat_reg_", suffix), value = meta[1, "center_lat_reg"])
    updateNumericInput(session, paste0("center_lon_reg_", suffix), value = meta[1, "center_lon_reg"])
    updateCheckboxInput(session, paste0("custom_topo_reg_", suffix), value = as.logical(meta[1, "custom_topo_reg"]))
    updateCheckboxInput(session, paste0("download_options_", suffix), value = as.logical(meta[1, "download_options"]))
    updateCheckboxInput(session, paste0("hide_axis_reg_", suffix), value = as.logical(meta[1, "hide_axis_reg"]))
    updateCheckboxInput(session, paste0("hide_borders_reg_", suffix), value = as.logical(meta[1, "hide_borders_reg"]))
    updateCheckboxInput(session, paste0("label_lakes_reg_", suffix), value = as.logical(meta[1, "label_lakes_reg"]))
    updateCheckboxInput(session, paste0("label_mountains_reg_", suffix), value = as.logical(meta[1, "label_mountains_reg"]))
    updateCheckboxInput(session, paste0("label_rivers_reg_", suffix), value = as.logical(meta[1, "label_rivers_reg"]))
    updateSelectInput(session, paste0("projection_reg_", suffix), selected = meta[1, "projection_reg"])
    updateRadioButtons(session, paste0("reg_", suffix, "_plot_type"), selected = meta[1, "reg_plot_type"])
    updateCheckboxInput(session, paste0("show_lakes_reg_", suffix), value = as.logical(meta[1, "show_lakes_reg"]))
    updateCheckboxInput(session, paste0("show_mountains_reg_", suffix), value = as.logical(meta[1, "show_mountains_reg"]))
    updateCheckboxInput(session, paste0("show_rivers_reg_", suffix), value = as.logical(meta[1, "show_rivers_reg"]))
    updateRadioButtons(session, paste0("title_mode_reg_", suffix), selected = meta[1, "title_mode_reg"])
    updateNumericInput(session, paste0("title_size_input_reg_", suffix), value = meta[1, "title_size_input_reg"])
    updateTextInput(session, paste0("title1_input_reg_", suffix), value = meta[1, "title1_input_reg"])
    updateTextInput(session, paste0("title2_input_reg_", suffix), value = meta[1, "title2_input_reg"])
    updateCheckboxInput(session, paste0("white_land_reg_", suffix), value = as.logical(meta[1, "white_land_reg"]))
    updateCheckboxInput(session, paste0("white_ocean_reg_", suffix), value = as.logical(meta[1, "white_ocean_reg"]))
    
    var_field <- switch(mode, coeff = "coeff_variable", pval = "pvalue_variable", res = NULL)
    if (!is.null(var_field)) {
      updateSelectInput(session, var_field, selected = meta[1, var_field])
    }
    
    if (mode == "res" && !is.null(meta[1, "reg_resi_year"])) {
      updateNumericInput(session, "reg_resi_year", value = meta[1, "reg_resi_year"])
    }
    
    # === Map DataFrames ===
    if (!is.null(df_map_points)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_points)
      if (!is.null(df) && nrow(df) > 0) map_points_data(df)
    }
    if (!is.null(df_map_highlights)) {
      df <- openxlsx::read.xlsx(file_path, sheet = df_map_highlights)
      if (!is.null(df) && nrow(df) > 0) map_highlights_data(df)
    }
    
    if (!is.null(rv_plotOrder)) rv_plotOrder(character(0))
    if (!is.null(rv_availableLayers)) rv_availableLayers(character(0))
  }
  
  # === ReactiveVals: lonlat_vals ===
  if (!is.null(rv_lonlat_vals_iv)) {
    lon_vals <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals_iv"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals)) rv_lonlat_vals_iv(lon_vals)
  }
  if (!is.null(rv_lonlat_vals_dv)) {
    lon_vals <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals_dv"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals)) rv_lonlat_vals_dv(lon_vals)
  }
}


#### Annual cycles Functions ----

#' (Annual Cycles) Starter Data for Monthly Time Series (Bern, 1815)
#'
#' Provides example monthly temperature data for Bern in the year 1815.
#' Used to initialize the annual cycles plotting interface.
#'
#' @return A data frame containing one row of monthly temperature data for Bern (1815),
#'   including metadata such as dataset, coordinates, mode, and data type.

monthly_ts_starter_data = function(){
  Dataset = "ModE-RA"
  Years = "1815" ; Variable = "Temperature" ; Unit = "\u00B0C"
  Jan = -3.17 ; Feb = 2.84 ; Mar = 5.3
  Apr = 6.97 ; May = 11.3 ; Jun = 13.92
  Jul = 17.11 ; Aug = 17.01 ; Sep = 14.07 
  Oct = 8.46 ; Nov = 0.87 ; Dec = -1.33
  Coordinates = "7.5\u00B0E, 47\u00B0N"
  Mode = "Absolute"
  Ref = NA
  Type = "Individual years"
  
  Bern_data = data.frame(Dataset,Years,Variable,Unit,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,
                         Coordinates,Mode,Ref,Type)
  return(Bern_data)
}


#' (Annual Cycles) Create Monthly Time Series Data
#'
#' Generates a monthly time series data frame from 3D climate data for selected years,
#' spatial extent, and mode (absolute or anomaly). Supports individual years or averaged periods.
#'
#' @param data_input 3D array of climate data [lon, lat, time] from `custom_data()`.
#' @param dataset Character. Name of the dataset (e.g., "ModE-RA").
#' @param variable Character. Variable name (e.g., "Temperature").
#' @param years Character. Year(s) to include (e.g., "1815", "1800,1815", or "1800-1810").
#' @param lon_range Numeric vector. Longitude range (e.g., c(7.5, 7.5)).
#' @param lat_range Numeric vector. Latitude range (e.g., c(47, 47)).
#' @param mode Character. Either "Absolute" or "Anomaly".
#' @param type Character. Either "Average" or "Individual years".
#' @param baseline_range Numeric vector. Reference years used for anomalies (if mode = "Anomaly").
#'
#' @return A data frame containing monthly values with metadata (dataset, variable, coordinates,
#'   unit, mode, type, and reference period), where each row represents a year or averaged period.

create_monthly_TS_data = function(data_input,
                                  dataset,
                                  variable,
                                  years,
                                  lon_range,
                                  lat_range,
                                  mode,
                                  type,
                                  baseline_range){
  
  # read in and interpret "years"
  if (grepl(",",years)){
    year_vector = as.integer(unlist(strsplit(years,",")))
  } else if (grepl("-",years)){
    yr_range = as.integer(unlist(strsplit(years,"-")))
    year_vector = yr_range[1]:yr_range[2]  
  } else {
    year_vector = as.integer(years)
  }
  
  # Find subset lat/lon IDs and cut data to lat/lon subset
  lon_IDs = create_subset_lon_IDs(lon_range)
  lat_IDs = create_subset_lat_IDs(lat_range)
  
  data_subset = data_input[lon_IDs,lat_IDs,]
  
  # Create monthly data for each year and add to dataframe
  year_df = data.frame()
  
  for (Y in year_vector){
    year_data = c()
    for (M in 1:12){
      month_ID = ((12*(Y-1421))+M)
      month_data = mean(data_subset[,,month_ID])
      year_data = c(year_data,month_data)
    }
    year_df = rbind(year_df,year_data)
  }
  
  # Calculate reference data (if required) and remove from year_data
  if (mode == "Anomaly"){
    
    if (length(baseline_range)<3){
      ref_years = baseline_range[1]:baseline_range[2]
      ref_df = data.frame()
    } else {
      ref_years = baseline_range
    }
    
    for (Y in ref_years){
      year_data = c()
      for (M in 1:12){
        month_ID = ((12*(Y-1421))+M)
        month_data = mean(data_subset[,,month_ID])
        year_data = c(year_data,month_data)
      }
      ref_df = rbind(ref_df,year_data)
    }
    
    ref_df = as.data.frame(lapply(ref_df,mean))
    year_df = sweep(year_df,2,as.matrix(ref_df)[1,]) 
  }
  
  # Round and add colnames to year_df
  year_df = round(year_df,digits = 2)
  colnames(year_df) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  # Generate Years column and if type = average, average all years
  if (type == "Average"){
    year_df = as.data.frame(lapply(year_df,mean))
    Years = years
  } else {
    Years = as.character(year_vector)
  } 
  
  # Generate dataset column
  Dataset = rep(dataset,length(Years))
  
  # Generate Variable column
  Variable = rep(variable,length(Years))
  
  # Generate Units column
  if (variable == "Temperature"){
    v_unit = "\u00B0C"
  } else if (variable == "Precipitation"){
    v_unit = "mm/month"
  } else if (variable == "SLP"){
    v_unit = "hPa"
  } else if (variable == "Z500"){
    v_unit = "m"
  } 
  Unit = rep(v_unit,length(Years))
  
  # Generate Coordinates column
  # Lon labels
  if (lon_range[1] == lon_range[2]){
    lon_addition = paste0(lon_range[1],"\u00B0E")
  } else {
    lon_addition = paste0(lon_range[1],":",lon_range[2],"\u00B0E")
  }
  # Lat labels
  if (lat_range[1] == lat_range[2]){
    lat_addition = paste0(lat_range[1],"\u00B0N")
  } else {
    lat_addition = paste0(lat_range[1],":",lat_range[2],"\u00B0N")
  }
  
  coordinate_label = paste0(lon_addition,", ",lat_addition)
  Coordinates = rep(coordinate_label,length(Years))  
  
  # Generate Mode and Type columns
  Mode = rep(mode,length(Years))
  Type = rep(type,length(Years))
  
  # Generate Ref column
  if (mode == "Anomaly"){
    if (baseline_range[1] == baseline_range[2]){
      ref = baseline_range[1]
    } else {
      ref = paste0(baseline_range[1],"-",baseline_range[2])
    }
  } else {
    ref = NA
  }
  
  Ref = rep(ref,length(Years))
  
  # Combine Columns to create dataframe  
  combined_df = data.frame(Dataset,Years,Variable,Unit,year_df,Coordinates,Mode,Ref,Type)
  
  return(combined_df)
}


#' (Annual Cycles) PLOT MONTHLY TIMESERIES 
#' 
#' @param data data.frame. The main monthly time series data table.
#' @param titles Character vector. Contains ts_title and ts_title_size
#' @param show_key Logical. TRUE or FALSE, whether to show a key.
#' @param key_position Character. Position of the key: "top", "bottom", "left", "right", "none".
#' @param highlights data.frame. Data frame containing highlight information.
#' @param lines data.frame. Data frame containing line information.
#' @param points data.frame. Data frame containing point information.
#' 
#' @return A ggplot object.

plot_monthly_timeseries <- function(data = NA,
                                    titles = NA,
                                    show_key = NA,
                                    key_position = NA,
                                    highlights = NA,
                                    lines = NA,
                                    points = NA) {
  
  # Create empty dataframes for Lines, Points, Fills and boxes  
  lines_data = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(lines_data) = c("ID","label","x","y","lcolor","ltype","lwidth","key_show")
  
  points_data = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(points_data) = c("ID","label","x","y","pcolor","pshape","psize","key_show")
  
  fills_data = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(points_data) = c("ID","label","x1","x2","y1","y2","fill","key_show")
  
  boxes_data = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(points_data) = c("ID","label","x1","x2","y1","y2","bcolor","key_show")
  
  # Set up inital row IDs
  lID = 1 ; pID = 1 ; fID = 1 ; bID = 1
  
  # Generate colors
  if (nrow(data) < 3) {
    color_set = RColorBrewer::brewer.pal(3, "Dark2")
  } else if (nrow(data) < 13) {
    color_set = RColorBrewer::brewer.pal(nrow(data), "Dark2")
  } else {
    color_set = colorRampPalette(RColorBrewer::brewer.pal(12, "Dark2"))(nrow(data))
  }
  
  # Convert data to plotting format
  for (i in 1:nrow(data)){
    year_data = data[i,]
    
    # Create variables
    if (year_data$Type == "Average"){
      lwidth = 1.5
    } else {
      lwidth = 1
    }
    
    x = zoo::as.Date(c())
    for (j in 5:16){
      x = c(x,zoo::as.Date(paste0("2000-",j-4,"-01")))
    }
    
    new_line = data.frame(
      ID = lID,
      label = paste0(year_data$Dataset," ",year_data$Years," [",year_data$Coordinates,"]"),
      x = x,
      y = as.numeric(year_data[,5:16]),
      lcolor = color_set[i],
      ltype = "solid",
      lwidth = lwidth,
      key_show = TRUE
    )
    lines_data = rbind(lines_data,new_line)
    lID = lID+1
  }
  
  # Extract ylimits from data
  y_min = min(lines_data$y) ; y_max = max(lines_data$y)
  y_range = y_max-y_min
  y_Min = y_min-(0.1*y_range) ; y_Max = y_max+(0.1*y_range)
  
  # Add a vertical white line to set axis limits 
  new_line = data.frame(
    ID = lID,
    label = NA,
    x = zoo::as.Date("2000-01-01"),
    y = c(y_Min,y_Max),
    lcolor = "white",
    ltype = "solid",
    lwidth = 1,
    key_show = FALSE
  )
  lines_data = rbind(lines_data,new_line)
  lID = lID+1
  
  # Add custom lines to data:
  if (!is.null(lines) && nrow(lines) > 0) {
    
    for (i in 1:nrow(lines)) {
      line_data <- lines[i, ]
      
      if (line_data$orientation == "Vertical"){
        x_line = line_data$location
        y_line = c(y_Min,y_Max)
      } else { # Orientation == "Horizontal"
        x_line = c(zoo::as.Date("2000-01-01"),zoo::as.Date("2000-12-01"))
        y_line = line_data$location
      }
      
      new_line = data.frame(
        ID = lID,
        label = line_data$label,
        x = x_line,
        y = y_line,
        lcolor = line_data$color,
        ltype = line_data$type,
        lwidth = 1,
        key_show = line_data$key_show
      )
      lines_data = rbind(lines_data,new_line)
      lID = lID+1
    }
  }
  
  # Add custom points to data:
  if (!is.null(points) && nrow(points) > 0) {
    
    for (i in 1:nrow(points)) {
      point_data <- points[i, ]
      
      new_point = data.frame(
        ID = pID,
        label = point_data$label,
        x = point_data$x_value,
        y = point_data$y_value,
        pcolor = point_data$color,
        pshape = point_data$shape,
        psize = point_data$size,
        key_show = FALSE
      )
      points_data = rbind(points_data,new_point)
      pID = pID+1
    }
  }
  
  # Add custom highlights to data:
  if (!is.null(highlights) && nrow(highlights) > 0) {
    
    if(any(highlights$type == "Fill")) {
      fills <- subset(highlights, type == "Fill")
      
      for (i in 1:nrow(fills)) {
        fill_data <- fills[i, ]
        
        new_fill = data.frame(
          ID = fID,
          label = fill_data$label,
          x1 = fill_data$x1,
          x2 = fill_data$x2,
          y1 = fill_data$y1,
          y2 = fill_data$y2,
          fill = fill_data$color,
          key_show = fill_data$key_show
        )
        fills_data = rbind(fills_data,new_fill)
        fID = fID+1
      }
    }
    
    if(any(highlights$type == "Box")) {
      boxes <- subset(highlights, type == "Box")
      
      for (i in 1:nrow(boxes)) {
        box_data <- boxes[i, ]
        
        new_box = data.frame(
          ID = bID,
          label = box_data$label,
          x1 = box_data$x1,
          x2 = box_data$x2,
          y1 = box_data$y1,
          y2 = box_data$y2,
          bcolor = box_data$color,
          key_show = box_data$key_show
        )
        boxes_data = rbind(boxes_data,new_box)
        bID = bID+1
      }
    }
    
  }
  
  # Plot data 
  p = ggplot()
  
  # Plot fills
  if (nrow(fills_data>0)){
    
    #Plot
    for (i in 1:(fID-1)){
      fill_data = subset(fills_data, ID == i)
      # Convert x values to dates
      fill_data$x1 = zoo::as.Date(fill_data$x1)
      fill_data$x2 = zoo::as.Date(fill_data$x2)
      
      if (fill_data$key_show){ # Show on legend
        p = p +  geom_rect(
          data = fill_data,
          aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = label),
          alpha = 0.5,
          inherit.aes = FALSE
        ) 
      } else { # Don't show on legend
        p = p +  geom_rect(
          data = fill_data,
          aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
          fill = fill_data$fill,
          alpha = 0.5,
          inherit.aes = FALSE
        )  
      }
    }
    
    # Add legend
    fills_legend = unique(subset(fills_data, key_show == TRUE)[,c("ID","label","fill")])
    if (!is.null(fills_legend) && nrow(fills_legend)>0){
      p = p + scale_fill_manual(values = setNames(fills_legend$fill,fills_legend$label))
    }
  }
  
  # Plot lines
  if (nrow(lines_data>0)){
    
    #Plot
    for (i in 1:(lID-1)){
      line_data = subset(lines_data, ID == i)
      
      if (line_data$key_show[1]){ # Show on legend
        p = p + geom_line(
          data = line_data,
          aes(x = x, y = y, linetype = label),
          linewidth = line_data$lwidth[1],
          color = line_data$lcolor[1]
        ) 
      } else { # Don't show on legend
        p = p + geom_line(
          data = line_data,
          aes(x = x, y = y),
          linetype = line_data$ltype[1],
          linewidth = line_data$lwidth[1],
          color = line_data$lcolor[1]
        ) 
      }
    }
    
    # Add legend
    lines_legend = unique(subset(lines_data, key_show == TRUE)[,c("ID","label","ltype")])
    if (!is.null(lines_legend) && nrow(lines_legend)>0){
      p = p + scale_linetype_manual(values = setNames(lines_legend$ltype,lines_legend$label))
    }
  }
  
  # Plot Boxes
  if (nrow(boxes_data>0)){
    
    #Plot
    for (i in 1:(bID-1)){
      box_data = subset(boxes_data, ID == i)
      # Convert x values to dates
      box_data$x1 = zoo::as.Date(box_data$x1)
      box_data$x2 = zoo::as.Date(box_data$x2)
      
      if (box_data$key_show){ # Show on legend
        p = p +  geom_rect(
          data = box_data,
          aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, color = label),
          fill = "white", alpha = 0, linewidth = 1,
          inherit.aes = FALSE
        ) 
      } else { # Don't show on legend
        p = p +  geom_rect(
          data = box_data,
          aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
          color = box_data$bcolor,
          fill = "white", alpha = 0, linewidth = 1,
          inherit.aes = FALSE
        )  
      }
    }
    
    # Add legend
    boxes_legend = unique(subset(boxes_data, key_show == TRUE)[,c("ID","label","bcolor")])
    if (!is.null(boxes_legend) && nrow(boxes_legend)>0){
      p = p + scale_color_manual(values = setNames(boxes_legend$bcolor,boxes_legend$label))
    }
  }
  
  # Plot points
  if (nrow(points_data)>0){
    
    for (i in 1:nrow(points_data)){
      point_data = points_data[i,]
      # Convert points_data x value to date
      point_data$x = zoo::as.Date(point_data$x)
      
      if (point_data$key_show){ # Show on legend
        p = p + geom_point(
          data = point_data,
          aes(x = x, y = y, shape = label),
          size = point_data$psize,
          color = point_data$pcolor
        ) 
      } else { # Don't show on legend
        p = p + geom_point(
          data = point_data,
          aes(x = x, y = y),
          shape = point_data$pshape,
          size = point_data$psize,
          color = point_data$pcolor
        )
      }  
      
      # Add labels
      p <- p + geom_text(
        data = point_data,
        aes(x = x, y = y, label = label),
        vjust = -1.5,   # vertical adjustment
        hjust = 0.5,  # horizontal adjustment
        size = 5,     # text size
        color = "black"  # or any color you want
      )
    }
    
    # Add legend
    points_legend = unique(subset(points_data, key_show == TRUE)[,c("ID","label","pshape")])
    if (!is.null(points_legend) && nrow(points_legend)>0){
      p = p + scale_shape_manual(values = setNames(points_legend$pshape,points_legend$label))
    }
  }
  
  # Add theme
  p <- p + theme_bw(base_size = 18)
  
  # Add title and subtitle if provided
  if (!is.null(titles)) {
    p <- p + labs(x = "Month", y = paste0(data$Variable[1]," [",data$Unit[1],"]"))
    
    if (titles$ts_title != " ") {
      p <- p + ggtitle(titles$ts_title)
    }
    if (!is.na(titles$ts_subtitle)) {
      p <- p + labs(subtitle = titles$ts_subtitle) 
    }
    p <- p + theme(
      plot.title = element_text(size = titles$ts_title_size, face = "bold"),
      plot.subtitle = element_text(size = titles$ts_title_size / 1.3, face = "plain"),
      axis.text=element_text(size = titles$ts_title_size / 1.6),
    )
  } 
  
  # Set legend groupings
  p <- p + guides(
    fill = guide_legend(title = NULL, order = 3),
    color = guide_legend(title = NULL, order = 4),
    linetype = guide_legend(title = "Legend", order = 1),
    shape = guide_legend(title = NULL, order = 2)
  )
  
  # Set legend visibility and position
  if (show_key) {
    p <- p + theme(legend.position = key_position)  # Apply the position from `key_position`
  } else {
    p <- p + theme(legend.position = "none")  # Remove the legend if `show_key == FALSE`
  }
  
  # Remove whitespace & label months
  p <- p +   scale_x_date(
    date_labels = "%b",    # Three-letter month labels
    date_breaks = "1 month",
    expand = c(0, 0)
  )
  p = p + scale_y_continuous(limits = c(y_Min,y_Max),expand = c(0, 0))
  
  # Return the final plot
  return(p)
}


#' (Annual Cycles) Generate Month Label
#'
#' Helper function to create a short string label representing a range of months.
#' Returns "Annual" if the full year is selected, otherwise a concatenation of month initials.
#'
#' @param range Integer vector of length 2. Start and end months (1 = January, 12 = December).
#'
#' @return A character string representing the month range (e.g., "JFM" or "Annual").

generate_month_label <- function(range) {
  if (range[1] == 1 && range[2] == 12) {
    "Annual"
  } else {
    month_letters <- c("D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
    paste(month_letters[(range[1]:range[2]) + 1], collapse = "")
  }
}


#' (Annual Cycles) Generate Metadata from User Inputs
#'
#' Collects all annual cycle plot UI inputs and compiles them into a single-row metadata table.  
#' Used for exporting or reloading customization settings. Does not include coordinate points.
#'
#' @param range_years5 Year(s) or year range selected for plotting.
#' @param custom_ts5 Logical indicating whether custom styling is enabled.
#' @param key_position_ts5 Position of the legend/key in the plot.
#' @param show_key_ts5 Logical indicating whether to display the legend/key.
#' @param title_mode_ts5 Title mode selection (e.g., automatic or custom).
#' @param title_size_input_ts5 Numeric value for the title font size.
#' @param title1_input_ts5 Custom title text input.
#' @param file_type_timeseries5 Selected file format for download (e.g., png, pdf).
#' @param dataset_selected5 Name of the selected dataset.
#' @param variable_selected5 Selected climate variable (e.g., Temperature, Precipitation).
#' @param range_latitude5 Latitude range selected for the subset.
#' @param range_longitude5 Longitude range selected for the subset.
#' @param ref_period5 Reference period used for anomaly calculations.
#' @param ref_period_sg5 Single year selected as reference (if applicable).
#' @param ref_single_year5 Logical indicating use of a single reference year.
#' @param mode_selected5 Mode of data display ("Absolute" or "Anomaly").
#' @param type_selected5 Type of output ("Average" or "Individual years").
#'
#' @return A single-row `data.frame` containing all relevant user customization inputs.


generate_metadata_annualcycle <- function(
    range_years5 = NA,
    custom_ts5 = NA,
    key_position_ts5 = NA,
    show_key_ts5 = NA,
    title_mode_ts5 = NA,
    title_size_input_ts5 = NA,
    title1_input_ts5 = NA,
    file_type_timeseries5 = NA,
    dataset_selected5 = NA,
    variable_selected5 = NA,
    range_latitude5 = NA,
    range_longitude5 = NA,
    ref_period5 = NA,
    ref_period_sg5 = NA,
    ref_single_year5 = NA,
    mode_selected5 = NA,
    type_selected5 = NA
) {
  collapse_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    paste(as.character(x), collapse = ", ")
  }
  
  meta <- data.frame(
    range_years5 = collapse_or_na(range_years5),
    custom_ts5 = custom_ts5,
    key_position_ts5 = key_position_ts5,
    show_key_ts5 = show_key_ts5,
    title_mode_ts5 = title_mode_ts5,
    title_size_input_ts5 = title_size_input_ts5,
    title1_input_ts5 = title1_input_ts5,
    file_type_timeseries5 = file_type_timeseries5,
    dataset_selected5 = dataset_selected5,
    variable_selected5 = variable_selected5,
    range_latitude5 = collapse_or_na(range_latitude5),
    range_longitude5 = collapse_or_na(range_longitude5),
    ref_period5 = collapse_or_na(ref_period5),
    ref_period_sg5 = ref_period_sg5,
    ref_single_year5 = ref_single_year5,
    mode_selected5 = mode_selected5,
    type_selected5 = type_selected5,
    
    stringsAsFactors = FALSE
  )
  
  return(meta)
}

#' (Annual Cycles) Process Uploaded Metadata for Annual Cycles
#'
#' Reads metadata and optional supporting data from an uploaded Excel file  
#' and updates all relevant UI inputs and reactive values for annual cycle visualization.
#'
#' @param file_path File path to the Excel file containing metadata and optional data sheets.
#' @param metadata_sheet Sheet name in the Excel file that contains the metadata (default is "custom_meta").
#' @param df_monthly_ts Optional sheet name for uploaded monthly timeseries data.
#' @param df_ts_points Optional sheet name for point annotations on the timeseries.
#' @param df_ts_highlights Optional sheet name for highlighted points/regions.
#' @param df_ts_lines Optional sheet name for line annotations on the timeseries.
#' @param monthly_ts_data Reactive value/function to store the uploaded monthly timeseries data.
#' @param ts_points_data Reactive value/function to store the uploaded point annotations.
#' @param ts_highlights_data Reactive value/function to store the uploaded highlight data.
#' @param ts_lines_data Reactive value/function to store the uploaded line annotations.
#'
#' @return None. This function updates UI elements and reactive values in a Shiny session.


process_uploaded_metadata_cycles <- function(
    file_path,
    metadata_sheet = "custom_meta",
    df_monthly_ts = NULL,
    df_ts_points = NULL,
    df_ts_highlights = NULL,
    df_ts_lines = NULL,
    monthly_ts_data = NULL,
    ts_points_data = NULL,
    ts_highlights_data = NULL,
    ts_lines_data = NULL
) {
  meta <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet)
  session <- getDefaultReactiveDomain()
  
  # === Helpers ===
  split_numeric_range <- function(value) {
    if (is.null(value) || is.na(value) || !is.character(value)) return(c(NA, NA))
    as.numeric(strsplit(value, ",\\s*")[[1]])
  }
  split_text_vector <- function(value) {
    if (is.null(value) || is.na(value)) return(character(0))
    strsplit(value, ",\\s*")[[1]]
  }
  
  # === UI Inputs ===
  updateTextInput(session, "range_years5", value = meta[1, "range_years5"])
  updateSelectInput(session, "dataset_selected5", selected = meta[1, "dataset_selected5"])
  updateSelectInput(session, "variable_selected5", selected = meta[1, "variable_selected5"])
  updateRadioButtons(session, "mode_selected5", selected = meta[1, "mode_selected5"])
  updateRadioButtons(session, "type_selected5", selected = meta[1, "type_selected5"])
  updateRadioButtons(session, "key_position_ts5", selected = meta[1, "key_position_ts5"])
  updateRadioButtons(session, "title_mode_ts5", selected = meta[1, "title_mode_ts5"])
  updateRadioButtons(session, "file_type_timeseries5", selected = meta[1, "file_type_timeseries5"])
  
  updateNumericRangeInput(session, "range_latitude5", value = split_numeric_range(meta[1, "range_latitude5"]))
  updateNumericRangeInput(session, "range_longitude5", value = split_numeric_range(meta[1, "range_longitude5"]))
  updateNumericRangeInput(session, "ref_period5", value = split_numeric_range(meta[1, "ref_period5"]))
  
  updateNumericInput(session, "ref_period_sg5", value = meta[1, "ref_period_sg5"])
  updateNumericInput(session, "title_size_input_ts5", value = meta[1, "title_size_input_ts5"])
  
  updateCheckboxInput(session, "custom_ts5", value = as.logical(meta[1, "custom_ts5"]))
  updateCheckboxInput(session, "ref_single_year5", value = as.logical(meta[1, "ref_single_year5"]))
  updateCheckboxInput(session, "show_key_ts5", value = as.logical(meta[1, "show_key_ts5"]))
  
  updateTextInput(session, "title1_input_ts5", value = meta[1, "title1_input_ts5"])
  
  # === Optional DataFrames ===
  if (!is.null(df_monthly_ts)) {
    df <- openxlsx::read.xlsx(file_path, sheet = df_monthly_ts)
    if (!is.null(df) && nrow(df) > 0) monthly_ts_data(df)
  }
  
  if (!is.null(df_ts_points)) {
    df <- openxlsx::read.xlsx(file_path, sheet = df_ts_points)
    if (!is.null(df) && nrow(df) > 0) ts_points_data(df)
  }
  
  if (!is.null(df_ts_highlights)) {
    df <- openxlsx::read.xlsx(file_path, sheet = df_ts_highlights)
    if (!is.null(df) && nrow(df) > 0) ts_highlights_data(df)
  }
  
  if (!is.null(df_ts_lines)) {
    df <- openxlsx::read.xlsx(file_path, sheet = df_ts_lines)
    if (!is.null(df) && nrow(df) > 0) ts_lines_data(df)
  }
}

#### SEA Functions ----


#' (SEA) Generate Metadata from Inputs for Plot Generation
#'
#' Flattens SEA UI inputs into a single-row metadata table for export or import.  
#' Captures all plot settings and optional reactive values.
#'
#' @param axis_input_6 Axis range for the y-axis in the SEA plot.
#' @param axis_mode_6 Axis scaling mode (e.g., "auto", "manual").
#' @param coordinates_type_6 Coordinate input mode ("latlon" or other).
#' @param custom_6 Logical; whether to enable custom visualization options.
#' @param dataset_selected_6 Selected dataset for SEA.
#' @param download_options_6 Logical; whether download options are enabled.
#' @param enable_custom_statistics_6 Logical; enable custom pre/post statistics.
#' @param enter_upload_6 Logical; indicates whether a file was uploaded.
#' @param event_years_6 Comma-separated string of event years.
#' @param file_type_timeseries6 Selected output file type (e.g., "pdf", "png").
#' @param lag_years_6 Numeric vector of lag years to include in SEA.
#' @param ME_statistic_6 Selected ModE-RA statistic (e.g., mean, median).
#' @param ME_variable_6 Selected ModE-RA variable (e.g., "Temperature").
#' @param range_latitude_6 Latitude range for spatial subset.
#' @param range_longitude_6 Longitude range for spatial subset.
#' @param range_months_6 Month range for seasonal aggregation.
#' @param ref_period_6 Numeric range for reference period baseline.
#' @param ref_period_sg_6 Single numeric value for reference alignment.
#' @param ref_single_year_6 Logical; if reference is a single year.
#' @param sample_size_6 Number of years used in composite statistics.
#' @param season_selected_6 Selected season or time aggregation (e.g., "DJF").
#' @param show_confidence_bands_6 Logical; whether to display confidence bands.
#' @param show_key_6 Logical; whether to display the legend/key.
#' @param show_means_6 Logical; whether to show mean lines.
#' @param show_observations_6 Logical; whether to show individual observations.
#' @param show_pvalues_6 Logical; whether to show p-values on plot.
#' @param show_ticks_6 Logical; whether to show axis ticks.
#' @param source_sea_6 Source of SEA data ("User data" or "ModE-RA").
#' @param title_mode_6 Title mode selection ("Default" or "Custom").
#' @param title1_input_6 Custom title text.
#' @param use_custom_post_6 Logical; use custom post-event year range.
#' @param use_custom_pre_6 Logical; use custom pre-event year range.
#' @param user_variable_6 Name of user-uploaded variable (if used).
#' @param y_label_6 Y-axis label.
#' @param lonlat_vals Optional vector of selected coordinates used in SEA.
#'
#' @return A single-row data.frame containing all SEA UI and reactive inputs.

generate_metadata_sea <- function(
    
  # SEA input controls
  axis_input_6               = NA,
  axis_mode_6                = NA,
  coordinates_type_6         = NA,
  custom_6                   = NA,
  dataset_selected_6         = NA,
  download_options_6         = NA,
  enable_custom_statistics_6 = NA,
  enter_upload_6             = NA,
  event_years_6              = NA,
  file_type_timeseries6      = NA,
  lag_years_6                = NA,
  ME_statistic_6             = NA,
  ME_variable_6              = NA,
  range_latitude_6           = NA,
  range_longitude_6          = NA,
  range_months_6             = NA,
  ref_period_6               = NA,
  ref_period_sg_6            = NA,
  ref_single_year_6          = NA,
  sample_size_6              = NA,
  season_selected_6          = NA,
  show_confidence_bands_6    = NA,
  show_key_6                 = NA,
  show_means_6               = NA,
  show_observations_6        = NA,
  show_pvalues_6             = NA,
  show_ticks_6               = NA,
  source_sea_6               = NA,
  title_mode_6               = NA,
  title1_input_6             = NA,
  use_custom_post_6          = NA,
  use_custom_pre_6           = NA,
  user_variable_6            = NA,
  y_label_6                  = NA,
  
  # Reactive values
  lonlat_vals = NA
) {
  collapse_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    paste(as.character(x), collapse = ", ")
  }
  
  meta <- data.frame(
    axis_input_6               = collapse_or_na(axis_input_6),
    axis_mode_6                = axis_mode_6,
    coordinates_type_6         = coordinates_type_6,
    custom_6                   = custom_6,
    dataset_selected_6         = dataset_selected_6,
    download_options_6         = download_options_6,
    enable_custom_statistics_6 = enable_custom_statistics_6,
    enter_upload_6             = enter_upload_6,
    event_years_6              = event_years_6,
    file_type_timeseries6      = file_type_timeseries6,
    lag_years_6                = collapse_or_na(lag_years_6),
    ME_statistic_6             = ME_statistic_6,
    ME_variable_6              = ME_variable_6,
    range_latitude_6           = collapse_or_na(range_latitude_6),
    range_longitude_6          = collapse_or_na(range_longitude_6),
    range_months_6             = collapse_or_na(range_months_6),
    ref_period_6               = collapse_or_na(ref_period_6),
    ref_period_sg_6            = ref_period_sg_6,
    ref_single_year_6          = ref_single_year_6,
    sample_size_6              = sample_size_6,
    season_selected_6          = season_selected_6,
    show_confidence_bands_6    = show_confidence_bands_6,
    show_key_6                 = show_key_6,
    show_means_6               = show_means_6,
    show_observations_6        = show_observations_6,
    show_pvalues_6             = show_pvalues_6,
    show_ticks_6               = show_ticks_6,
    source_sea_6               = source_sea_6,
    title_mode_6               = title_mode_6,
    title1_input_6             = title1_input_6,
    use_custom_post_6          = use_custom_post_6,
    use_custom_pre_6           = use_custom_pre_6,
    user_variable_6            = user_variable_6,
    y_label_6                  = y_label_6,
    
    # Reactive values
    lonlat_vals                = collapse_or_na(as.vector(lonlat_vals)),
    
    stringsAsFactors = FALSE
  )
  
  return(meta)
}


#' (SEA) Process Uploaded Metadata for SEA Visualization
#'
#' Loads and applies SEA-related settings from a metadata Excel file.  
#' Updates all relevant Shiny inputs and reactive values used in SEA plotting.
#'
#' @param file_path File path to the uploaded Excel metadata file.
#' @param metadata_sheet Name of the Excel sheet containing metadata (default is "custom_meta").
#' @param rv_lonlat_vals Optional reactive value object for selected coordinates.
#'
#' @return This function is called for its side effects. It updates Shiny UI inputs and reactive values.

process_uploaded_metadata_sea <- function(
    file_path,
    metadata_sheet = "custom_meta",
    rv_lonlat_vals = NULL
) {
  meta <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet)
  session <- getDefaultReactiveDomain()
  
  # Helpers
  split_numeric_range <- function(value) {
    if (is.null(value) || is.na(value) || !is.character(value)) return(c(NA, NA))
    as.numeric(strsplit(value, ",\\s*")[[1]])
  }
  split_text_vector <- function(value) {
    if (is.null(value) || is.na(value)) return(character(0))
    strsplit(value, ",\\s*")[[1]]
  }
  
  # === SEA UI Inputs ===
  updateSelectInput(session, "dataset_selected_6", selected = meta[1, "dataset_selected_6"])
  updateSelectInput(session, "ME_variable_6", selected = meta[1, "ME_variable_6"])
  updateSelectInput(session, "ME_statistic_6", selected = meta[1, "ME_statistic_6"])
  updateRadioButtons(session, "source_sea_6", selected = meta[1, "source_sea_6"])
  updateRadioButtons(session, "coordinates_type_6", selected = meta[1, "coordinates_type_6"])
  updateRadioButtons(session, "season_selected_6", selected = meta[1, "season_selected_6"])
  updateRadioButtons(session, "enter_upload_6", selected = meta[1, "enter_upload_6"])
  updateRadioButtons(session, "axis_mode_6", selected = meta[1, "axis_mode_6"])
  updateRadioButtons(session, "file_type_timeseries6", selected = meta[1, "file_type_timeseries6"])
  updateRadioButtons(session, "show_confidence_bands_6", selected = meta[1, "show_confidence_bands_6"])
  updateRadioButtons(session, "title_mode_6", selected = meta[1, "title_mode_6"])
  
  updateNumericRangeInput(session, "range_latitude_6", value = split_numeric_range(meta[1, "range_latitude_6"]))
  updateNumericRangeInput(session, "range_longitude_6", value = split_numeric_range(meta[1, "range_longitude_6"]))
  updateNumericRangeInput(session, "lag_years_6", value = split_numeric_range(meta[1, "lag_years_6"]))
  updateNumericRangeInput(session, "axis_input_6", value = split_numeric_range(meta[1, "axis_input_6"]))
  updateNumericRangeInput(session, "ref_period_6", value = split_numeric_range(meta[1, "ref_period_6"]))
  
  shinyWidgets::updateSliderTextInput(session, "range_months_6", selected = split_text_vector(meta[1, "range_months_6"]))
  
  updateTextInput(session, "event_years_6", value = meta[1, "event_years_6"])
  updateTextInput(session, "title1_input_6", value = meta[1, "title1_input_6"])
  updateTextInput(session, "y_label_6", value = meta[1, "y_label_6"])
  
  updateNumericInput(session, "ref_period_sg_6", value = meta[1, "ref_period_sg_6"])
  updateNumericInput(session, "sample_size_6", value = meta[1, "sample_size_6"])
  
  updateCheckboxInput(session, "custom_6", value = as.logical(meta[1, "custom_6"]))
  updateCheckboxInput(session, "ref_single_year_6", value = as.logical(meta[1, "ref_single_year_6"]))
  updateCheckboxInput(session, "enable_custom_statistics_6", value = as.logical(meta[1, "enable_custom_statistics_6"]))
  updateCheckboxInput(session, "download_options_6", value = as.logical(meta[1, "download_options_6"]))
  updateCheckboxInput(session, "show_key_6", value = as.logical(meta[1, "show_key_6"]))
  updateCheckboxInput(session, "show_means_6", value = as.logical(meta[1, "show_means_6"]))
  updateCheckboxInput(session, "show_observations_6", value = as.logical(meta[1, "show_observations_6"]))
  updateCheckboxInput(session, "show_pvalues_6", value = as.logical(meta[1, "show_pvalues_6"]))
  updateCheckboxInput(session, "show_ticks_6", value = as.logical(meta[1, "show_ticks_6"]))
  updateCheckboxInput(session, "use_custom_pre_6", value = as.logical(meta[1, "use_custom_pre_6"]))
  updateCheckboxInput(session, "use_custom_post_6", value = as.logical(meta[1, "use_custom_post_6"]))
  
  updateSelectInput(session, "user_variable_6", selected = meta[1, "user_variable_6"])
  
  # === ReactiveVal: lonlat_vals
  if (!is.null(rv_lonlat_vals)) {
    lon_vals <- suppressWarnings(as.numeric(strsplit(meta[1, "lonlat_vals"], ",\\s*")[[1]]))
    if (!anyNA(lon_vals)) rv_lonlat_vals(lon_vals)
  }
}


#' (SEA) Set Axis Values in Customization
#'
#' Calculates padded axis limits for SEA plots based on the data range.
#' Adds 5% padding on both sides and rounds to 3 significant digits.
#'
#' @param data_input Numeric vector or matrix input, typically from SEA_datatable().
#'
#' @return A numeric vector of length 2 representing the lower and upper axis bounds.

set_sea_axis_values <- function(data_input) {
  min_val <- min(data_input, na.rm = TRUE)
  max_val <- max(data_input, na.rm = TRUE)
  range_val <- max_val - min_val
  padded <- c(min_val - 0.05 * range_val, max_val + 0.05 * range_val)
  return(signif(padded, digits = 3))
}


#' (SEA) Read SEA Event Data
#'
#' Creates a standardized dataframe of SEA event years with optional pre- and post-event background years.
#' Supports manual entry or reading from a CSV/Excel file. Filters years if source is "ModE-RA".
#'
#' @param data_input_manual Character string of comma-separated years (e.g., "1815,1883,1783") if manual mode is selected.
#' @param data_input_filepath Character. File path to a `.csv` or Excel file containing event data.
#' @param year_input_mode Character. One of `"Manual"` or `"Upload"` to select the input mode.
#' @param data_source_sea Character. If `"ModE-"`, restricts event years to between 1422 and 2008.
#'
#' @return A data frame with columns `Event`, `Pre`, and `Post`, containing event years and optional background years.


read_sea_data <- function(data_input_manual,
                          data_input_filepath,
                          year_input_mode,
                          data_source_sea) {
  
  if (year_input_mode == "Manual") {
    year_vec <- as.integer(unlist(strsplit(data_input_manual, ",")))
    df <- data.frame(Event = year_vec, Pre = NA_integer_, Post = NA_integer_)
  } else if (is.null(data_input_filepath)) {
    df <- data.frame(Event = c(1815, 1816), Pre = NA_integer_, Post = NA_integer_)
  } else if (grepl(".csv", data_input_filepath, fixed = TRUE)) {
    df <- read.csv(data_input_filepath)
  } else {
    df <- readxl::read_excel(data_input_filepath)
  }
  
  # Ensure numeric types
  for (i in seq_len(min(3, ncol(df)))) {
    df[[i]] <- as.numeric(df[[i]])
  }
  
  # Filter to ModE-RA range
  if (data_source_sea == "ModE-") {
    df <- df[df[[1]] >= 1422 & df[[1]] <= 2008, , drop = FALSE]
  }
  
  # Pad to 3 columns
  while (ncol(df) < 3) {
    df[[ncol(df) + 1]] <- NA_integer_
  }
  
  # Rename columns for consistency
  colnames(df)[1:3] <- c("Event", "Pre", "Post")
  
  return(df)
}

