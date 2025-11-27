# Define server logic ----

server <- function(input, output, session) {
  # ClimeApp Desktop Download ----
  observeEvent(input$go_desktop, {
    updateTabsetPanel(session, "main_tabs", selected = "desktop_tab")
  })
  
  output$climeapp_desktop_download <- downloadHandler(
    filename = function() {"ClimeApp Desktop Installer.zip"},
    content = function(file) {
      file.copy("ClimeApp Desktop Installer.zip",file)
    }
  )
  
  coords_committed <- reactiveVal(list(
    lon = initial_lon_values,
    lat = initial_lat_values 
  ))
  
  pending_bbox <- reactiveVal(NULL)
  
  
  lonlat_vals <- reactive({
    cc <- coords_committed()
    c(cc$lon, cc$lat)
  })
  
  # Set up custom data, preprocessed data and SDratio reactive variables ----
  preprocessed_data_primary = reactiveVal()
  preprocessed_data_id_primary = reactiveVal(c(NA,NA,NA,NA)) # data_ID for current preprocessed data
  
  preprocessed_data_secondary = reactiveVal()
  preprocessed_data_id_secondary = reactiveVal(c(NA,NA,NA,NA)) # data_ID for secodnary preprocessed data
  
  custom_data_primary = reactiveVal()
  custom_data_id_primary = reactiveVal(c(NA,NA,NA,NA)) # data_ID for current custom data
  
  custom_data_secondary = reactiveVal()                 # custom data secondary is only used for variable 2 in correlation
  custom_data_id_secondary = reactiveVal(c(NA,NA,NA,NA)) 
  
  SDratio_data = reactiveVal()
  SDratio_data_id = reactiveVal(c(NA,NA,NA,NA)) # data_ID for current SD data
  
  ### Panel Visibility ----
  # Add logic to toggle the visibility of the specific tabPanel (Correlation Map) based on radio button values ("Timeseries")
  observe({
    if (input$type_v1 == "Timeseries" && input$type_v2 == "Timeseries") {
      shinyjs::runjs('
        // Get the tabPanel element by ID
        var tabPanelToHide = $("#tabset3 a[data-value=\'corr_map_tab\']").parent();
  
        // Hide the tabPanel
        tabPanelToHide.hide();
      ')
      shinyjs::runjs('
        // Get the tabPanel element by ID
        var tabPanelToHide = $("#tabset3 a[data-value=\'corr_map_data_tab\']").parent();
  
        // Hide the tabPanel
        tabPanelToHide.hide();
      ')
      
    } else if (input$type_v1 == "Field" && input$type_v2 == "Field") {
      # Get the range values
      range_lon_v1 <- input$range_longitude_v1
      range_lat_v1 <- input$range_latitude_v1
      range_lon_v2 <- input$range_longitude_v2
      range_lat_v2 <- input$range_latitude_v2
      
      # Check for overlap
      overlap_lon <- !(range_lon_v1[2] < range_lon_v2[1] || range_lon_v1[1] > range_lon_v2[2])
      overlap_lat <- !(range_lat_v1[2] < range_lat_v2[1] || range_lat_v1[1] > range_lat_v2[2])
      
      # Return the result
      if (overlap_lon && overlap_lat) {
        shinyjs::runjs('
      // Get the tabPanel element by ID
      var tabPanelToHide = $("#tabset3 a[data-value=\'corr_map_tab\']").parent();

      // Show the tabPanel
      tabPanelToHide.show();
    ')
        shinyjs::runjs('
      // Get the tabPanel element by ID
      var tabPanelToHide = $("#tabset3 a[data-value=\'corr_map_data_tab\']").parent();

      // Show the tabPanel
      tabPanelToHide.show();
    ')
        
      } else {
        shinyjs::runjs('
      // Get the tabPanel element by ID
      var tabPanelToHide = $("#tabset3 a[data-value=\'corr_map_tab\']").parent();

      // Hide the tabPanel
      tabPanelToHide.hide();
    ')
        
        shinyjs::runjs('
      // Get the tabPanel element by ID
      var tabPanelToHide = $("#tabset3 a[data-value=\'corr_map_data_tab\']").parent();

      // Hide the tabPanel
      tabPanelToHide.hide();
    ')
      }
    } else if ((input$type_v1 == "Field" && input$type_v2 == "Timeseries") || (input$type_v1 == "Timeseries" && input$type_v2 == "Field")) {
      shinyjs::runjs('
      // Get the tabPanel element by ID
      var tabPanelToShow = $("#tabset3 a[data-value=\'corr_map_tab\']").parent();

      // Show the tabPanel
      tabPanelToShow.show();
    ')
      shinyjs::runjs('
      // Get the tabPanel element by ID
      var tabPanelToShow = $("#tabset3 a[data-value=\'corr_map_data_tab\']").parent();

      // Show the tabPanel
      tabPanelToShow.show();
    ')
    }
  })
  
  # Add logic to toggle the visibility of a specific tabPanel (Correlation and Regression Map) based on radio button values ("Choose a data source:")
  observe({
    if (input$source_v1 == "User data" && input$source_v2 == "User data") {
      shinyjs::runjs('
        // Get the tabPanel element by ID
        var tabPanelToHide = $("#tabset3 a[data-value=\'corr_fad_tab\']").parent();
  
        // Hide the tabPanel
        tabPanelToHide.hide();
      ')
      
    } else 
      shinyjs::runjs('
        // Get the tabPanel element by ID
        var tabPanelToHide = $("#tabset3 a[data-value=\'corr_fad_tab\']").parent();
  
        // Show the tabPanel
        tabPanelToHide.show();
      ')
  })
  
  
  ### Hiding, showing, enabling/unabling certain inputs ----
  ####### Anomalies ----
  
  # Side Bar Panel
  observe({shinyjs::toggle(id = "season",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$season_selected == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_sec_map_download",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_map_mode != "None",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "range_years_sg",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$single_year == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "range_years",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$single_year == FALSE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_sg",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year == FALSE,
                           asis = FALSE)})
  
  # Customization
  ### Anomalies Maps
  
  observe({shinyjs::toggle(id = "hidden_custom_maps",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_map == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_features",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_features == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_points",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature == "Point",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_highlights",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature == "Highlight",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_geo_options",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_topo == TRUE,
                           asis = FALSE)})
  
  
  
  ### Anomalies TS
  
  observe({shinyjs::toggle(id = "hidden_custom_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_ts == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode_ts == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_key_position_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_key_ts == TRUE,
                           asis = FALSE)})

  observe({shinyjs::toggle(id = "hidden_custom_features_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_features_ts == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_points_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts == "Point",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_highlights_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts == "Highlight",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_line_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts == "Line",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options_ts == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "highlight_label_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_highlight_on_legend_ts == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "line_label_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_line_on_legend_ts == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode_ts == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_xaxis_interval_ts",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_ticks_ts == TRUE,
                           asis = FALSE)})
  
  
  ####### Composite ----
  
  # Side Bar
  observe({shinyjs::toggle(id = "optional2a",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected2 == "Fixed reference",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2b",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected2 == "X years prior",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2c",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enter_upload2 == "Manual",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2d",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enter_upload2 == "Upload",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2e",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = is.null(input$upload_file2),
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "season2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$season_selected2 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2f",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected2 == "Custom reference",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2g",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enter_upload2a == "Manual",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2h",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enter_upload2a == "Upload",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional2i",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = is.null(input$upload_file2a),
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_sg2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year2 == FALSE,
                           asis = FALSE)})
  
  # Main Panel
  observe({shinyjs::toggle(id = "hidden_sec_map_download2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_map_mode2 != "None",
                           asis = FALSE)})
  
  # Customization
  ### Composites Maps
  
  observe({shinyjs::toggle(id = "hidden_custom_maps2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_map2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode2 == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode2 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_features2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_features2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_points2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature2 == "Point",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_highlights2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature2 == "Highlight",
                           asis = FALSE)})

  observe({shinyjs::toggle(id = "custom_anomaly_years2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected2 == "Custom reference",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_geo_options2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_topo2 == TRUE,
                           asis = FALSE)})
  
  ### Composites TS
  
  observe({shinyjs::toggle(id = "hidden_custom_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_ts2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode_ts2 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_key_position_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_key_ts2 == TRUE,
                           asis = FALSE)})

  observe({shinyjs::toggle(id = "hidden_custom_features_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_features_ts2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_points_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts2 == "Point",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_highlights_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts2 == "Highlight",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_line_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts2 == "Line",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "custom_anomaly_years2b",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected2 == "Custom reference",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options_ts2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "highlight_label_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_highlight_on_legend_ts2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "line_label_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_line_on_legend_ts2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode_ts2 == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_xaxis_interval_ts2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_ticks_ts2 == TRUE,
                           asis = FALSE)})
  
  
  ####### Correlation ----
  ###Sidebar V1
  
  observe({shinyjs::toggle(id = "upload_forcings_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "User data",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "upload_example_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = is.null(input$user_file_v1),
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_user_variable_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "User data",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_me_dataset_variable_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_modera_variable_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "season_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$season_selected_v1 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected_v1 == "Anomaly",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_continents_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$coordinates_type_v1 == "Continents",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_sg_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year_v1 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_v1",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year_v1 == FALSE,
                           asis = FALSE)})
  
  
  
  ###Sidebar V2
  
  observe({shinyjs::toggle(id = "upload_forcings_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v2 == "User data",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "upload_example_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = is.null(input$user_file_v2),
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_user_variable_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v2 == "User data",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_me_dataset_variable_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v2 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_modera_variable_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v2 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "season_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$season_selected_v2 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$mode_selected_v2 == "Anomaly",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_continents_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$coordinates_type_v2 == "Continents",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_sg_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year_v2 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_v2",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year_v2 == FALSE,
                           asis = FALSE)})
  
  #Correlation (Main Panel)
  
  observe({shinyjs::toggle(id = "hidden_v1_fad_download",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_v1_fad",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_v2_fad_download",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v2 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_v2_fad",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v2 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_sec_map_download3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_map_mode3 != "None",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_ref_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_ref_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_score_ref_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$add_outliers_ref_ts3 == "z-score",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_trend_sd_ref_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$add_outliers_ref_ts3 == "Trend deviation",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_meta_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "ModE-" && input$source_v2 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_meta3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_v1 == "ModE-" && input$source_v2 == "ModE-",
                           asis = FALSE)})
  
  
  
  
  # Customization
  ### Correlation Maps
  
  observe({shinyjs::toggle(id = "hidden_custom_maps3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_map3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode3 == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode3 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_features3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_features3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_points3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature3 == "Point",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_highlights3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature3 == "Highlight",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_geo_options3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_topo3 == TRUE,
                           asis = FALSE)})
  
  ### Correlation TS
  
  observe({shinyjs::toggle(id = "hidden_custom_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode_ts3 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_key_position_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_key_ts3 == TRUE,
                           asis = FALSE)})

  observe({shinyjs::toggle(id = "hidden_moving_average_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_average_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_features_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_features_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_points_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts3 == "Point",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_highlights_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts3 == "Highlight",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_line_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$feature_ts3 == "Line",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "highlight_label_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_highlight_on_legend_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "line_label_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_line_on_legend_ts3 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode_ts3 == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_xaxis_interval_ts3",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$show_ticks_ts3 == TRUE,
                           asis = FALSE)})
  
  ####### SEA ----
  
  ###Sidebar Data
  observe({shinyjs::toggle(id = "upload_sea_data_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_sea_6 == "User data",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "upload_example_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = is.null(input$user_file_6),
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_user_data_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_sea_6 == "User data",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_me_dataset_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_sea_6 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_modera_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_sea_6 == "ModE-",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional6c",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enter_upload_6 == "Manual",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional6d",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enter_upload_6 == "Upload",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "optional6e",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = is.null(input$upload_file_6b),
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "season_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$season_selected_6 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_sg_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year_6 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "ref_period_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$ref_single_year_6 == FALSE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_continents_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$coordinates_type_6 == "Continents",
                           asis = FALSE)})
  
  #Customization and Downloads
  
  observe({shinyjs::toggle(id = "hidden_custom_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$custom_6 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_title_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$title_mode_6 == "Custom",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_statistics_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$enable_custom_statistics_6 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_download_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$download_options_6 == TRUE,
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_custom_axis_6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$axis_mode_6 == "Fixed",
                           asis = FALSE)})
  
  observe({shinyjs::toggle(id = "hidden_meta6",
                           anim = TRUE,
                           animType = "slide",
                           time = 0.5,
                           selector = NULL,
                           condition = input$source_sea_6 == "ModE-",
                           asis = FALSE)})

  ### ANOMALIES observe, update & interactive controls ----
  
  ####### Input updaters ----
  
  # Set iniital lon/lat values on startup
  lonlat_vals = reactiveVal(c(initial_lon_values,initial_lat_values))
  
  # Continent buttons - updates range inputs and lonlat_values
  observeEvent(input$button_global,{
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(-180,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(-90,90))
    
    lonlat_vals(c(-180,180,-90,90))
  }) 
  
  observeEvent(input$projection, { # also update to global if projection is changed
    if (input$projection != "UTM (default)") {
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_longitude",
        label = NULL,
        value = c(-180, 180))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_latitude",
        label = NULL,
        value = c(-90, 90))
      lonlat_vals(c(-180, 180, -90, 90))
    }
  })
  
  observeEvent(input$button_europe, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(-30,40))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(30,75))
    
    lonlat_vals(c(-30,40,30,75))
  })
  
  observeEvent(input$button_asia, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(25,170))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(5,80))
    
    lonlat_vals(c(25,170,5,80))
  })
  
  observeEvent(input$button_oceania, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(90,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(-55,20))
    
    lonlat_vals(c(90,180,-55,20))
  })
  
  observeEvent(input$button_africa, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(-25,55))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(-40,40))
    
    lonlat_vals(c(-25,55,-40,40))
  })
  
  observeEvent(input$button_n_america, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(-175,-10))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(5,85))
    
    lonlat_vals(c(-175,-10,5,85))
  })
  
  observeEvent(input$button_s_america, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude",
      label = NULL,
      value = c(-90,-30))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude",
      label = NULL,
      value = c(-60,15))
    
    lonlat_vals(c(-90,-30,-60,15))
  })
  
  observeEvent(input$button_coord, {
    lonlat_vals(c(input$range_longitude,input$range_latitude))        
  })
  
  #Make continental buttons stay highlighted
  observe({
    if (input$range_longitude[1] == -180 && input$range_longitude[2] == 180 &&
        input$range_latitude[1] == -90 && input$range_latitude[2] == 90) {
      shinyjs::addClass("button_global", "green-background")
    } else {
      shinyjs::removeClass("button_global", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude[1] == -30 && input$range_longitude[2] == 40 &&
        input$range_latitude[1] == 30 && input$range_latitude[2] == 75) {
      shinyjs::addClass("button_europe", "green-background")
    } else {
      shinyjs::removeClass("button_europe", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude[1] == 25 && input$range_longitude[2] == 170 &&
        input$range_latitude[1] == 5 && input$range_latitude[2] == 80) {
      shinyjs::addClass("button_asia", "green-background")
    } else {
      shinyjs::removeClass("button_asia", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude[1] == 90 && input$range_longitude[2] == 180 &&
        input$range_latitude[1] == -55 && input$range_latitude[2] == 20) {
      shinyjs::addClass("button_oceania", "green-background")
    } else {
      shinyjs::removeClass("button_oceania", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude[1] == -25 && input$range_longitude[2] == 55 &&
        input$range_latitude[1] == -40 && input$range_latitude[2] == 40) {
      shinyjs::addClass("button_africa", "green-background")
    } else {
      shinyjs::removeClass("button_africa", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude[1] == -175 && input$range_longitude[2] == -10 &&
        input$range_latitude[1] == 5 && input$range_latitude[2] == 85) {
      shinyjs::addClass("button_n_america", "green-background")
    } else {
      shinyjs::removeClass("button_n_america", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude[1] == -90 && input$range_longitude[2] == -30 &&
        input$range_latitude[1] == -60 && input$range_latitude[2] == 15) {
      shinyjs::addClass("button_s_america", "green-background")
    } else {
      shinyjs::removeClass("button_s_america", "green-background")
    }
  })
  
  #Month Range Updater
  observe({
    if (input$season_selected == "Annual"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months",
        label = NULL,
        selected = c("January", "December"))
    }
  })
  
  observe({
    if (input$season_selected == "DJF"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months",
        label = NULL,
        selected = c("December (prev.)", "February"))
    }
  })
  
  observe({
    if (input$season_selected == "MAM"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months",
        label = NULL,
        selected = c("March", "May"))
    }
  })
  
  observe({
    if (input$season_selected == "JJA"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months",
        label = NULL,
        selected = c("June", "August"))
    }
  })
  
  observe({
    if (input$season_selected == "SON"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months",
        label = NULL,
        selected = c("September", "November"))
    }
  })
  
  # Axis values updater MAP
  observe({
    if (input$axis_mode == "Automatic"){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input",
        value = c(NA,NA))
    }
  })
  
  observe({
    if (input$axis_mode == "Fixed" & is.null(input$axis_input)) {
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input",
        value = set_axis_values(data_input = final_map_data(), mode = "Anomaly")
      )
    }
  })
  
  # Axis values updater TS
  observe({
    if (input$axis_mode_ts == "Automatic"){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_ts",
        value = c(NA,NA))
    }
  })
  
  observe({
    if (input$axis_mode_ts == "Fixed" & is.null(input$axis_input_ts)){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_ts",
        value = set_ts_axis_values(data_input = timeseries_data()$Mean))
    }
  })
  
  #Set NetCDF Variable
  observeEvent(input$variable_selected, {
    
    choices  = c("Temperature", "Precipitation", "SLP", "Z500")
    
    shinyWidgets::updatePickerInput(
      session, "netcdf_variables",
      choices = choices,
      selected = input$variable_selected
    )
  })
  
  #Update Reference Map
  observe({
    if (input$dataset_selected == "ModE-RAclim"){
      updateRadioButtons(
        inputId = "ref_map_mode",
        label    = NULL,
        choices  = c("None", "Reference Values"),
        selected = "None" , inline = TRUE)
    } else if (input$dataset_selected == "ModE-Sim"){
      updateRadioButtons(
        inputId = "ref_map_mode",
        label    = NULL,
        choices  = c("None", "Absolute Values","Reference Values"),
        selected = "None" , inline = TRUE)
    } else {
      updateRadioButtons(
        inputId = "ref_map_mode",
        label    = NULL,
        choices  = c("None", "Absolute Values","Reference Values","SD ratio"),
        selected = "None" , inline = TRUE)
    }
  })
  
  #Show Absolute Warning 
  observe({
    if (input$ref_map_mode == "Absolute Values"){
      showModal(
        # Add modal dialog for warning message
        modalDialog(
          title = "Information",
          "Unrealistic values (such as negative precipitation) can occur if absolute values are used! Cf. “Usage Notes”",
          easyClose = TRUE,
          footer = tagList(modalButton("OK"))
        ))}
  })
  
  ####### Interactivity ----
  
  # Input geo-coded locations
  observeEvent(input$search, {
    location <- input$location
    if (!is.null(location) && nchar(location) > 0) {
      location_encoded <- URLencode(location)
      
      projection <- input$projection  # Make sure this input exists for anomalies
      result <- NULL
      
      if (projection == "UTM (default)") {
        result <- tmaptools::geocode_OSM(location_encoded)
      } else if (projection == "Robinson") {
        result <- tmaptools::geocode_OSM(location_encoded, projection = "+proj=robin")
      } else if (projection == "Orthographic") {
        result <- tmaptools::geocode_OSM(location_encoded,
                              projection = ortho_proj(input$center_lat, input$center_lon))
      } else if (projection == "LAEA") {
        result <- tmaptools::geocode_OSM(location_encoded, projection = laea_proj)
      }
      
      if (!is.null(result$coords)) {
        longitude <- result$coords[1]
        latitude <- result$coords[2]
        updateTextInput(session, "point_location_x", value = as.character(longitude))
        updateTextInput(session, "point_location_y", value = as.character(latitude))
        shinyjs::hide(id = "inv_location")  # Hide the "Invalid location" message
      } else {
        shinyjs::show(id = "inv_location")  # Show the "Invalid location" message
      }
    } else {
      shinyjs::hide(id = "inv_location")  # Hide the "Invalid location" message when no input
    }
  })
  
  # Map coordinates/highlights setter
  observeEvent(input$map_brush1,{
    
    x_brush_1 = input$map_brush1[[1]]
    x_brush_2 = input$map_brush1[[2]]
    
    if (input$custom_features == FALSE){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_longitude",
        label = NULL,
        value = round(c(x_brush_1,x_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_latitude",
        label = NULL,
        value = round(c(input$map_brush1[[3]], input$map_brush1[[4]]), digits = 2)
      )
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "feature",
        label = NULL,
        selected = "Highlight")
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_x_values",
        label = NULL,
        value = round(c(x_brush_1, x_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_y_values",
        label = NULL,
        value = round(c(input$map_brush1[[3]], input$map_brush1[[4]]), digits = 2)
      )
    }
  })
  
  # Map projection center hidden/show
  observeEvent(input$projection, {
    if (input$projection == "Orthographic") {
      shinyjs::show("hidden_map_center")
    } else {
      shinyjs::hide("hidden_map_center")
    }
  })
  
  observeEvent(input$projection2, {
    if (input$projection2 == "Orthographic") {
      shinyjs::show("hidden_map_center2")
    } else {
      shinyjs::hide("hidden_map_center2")
    }
  })
  
  observeEvent(input$projection3, {
    if (input$projection3 == "Orthographic") {
      shinyjs::show("hidden_map_center3")
    } else {
      shinyjs::hide("hidden_map_center3")
    }
  })
  
  
  # Map custom points selector
  observeEvent(input$map_dblclick1,{
    dblclick <- input$map_dblclick1
    
    updateCheckboxInput(
      session = getDefaultReactiveDomain(),
      inputId = "custom_features",
      label = NULL,
      value = TRUE)
    
    updateRadioButtons(
      session = getDefaultReactiveDomain(),
      inputId = "feature",
      label = NULL,
      selected = "Point")
    
    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "point_location_x",
      label = NULL,
      value = as.character(round(dblclick$x, digits = 2))
    )
    
    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "point_location_y",
      label = NULL,
      value = as.character(round(dblclick$y, digits = 2))
    )
  })
  
  # TS point/line setter
  observeEvent(input$ts_click1,{
    if (input$custom_features_ts == TRUE){
      if (input$feature_ts == "Point"){
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "point_location_x_ts",
          label = NULL,
          value = as.character(round(input$ts_click1$x, digits = 2))
        )
        
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "point_location_y_ts",
          label = NULL,
          value = as.character(round(input$ts_click1$y, digits = 2))
        )
      } 
      else if (input$feature_ts == "Line"){
        updateRadioButtons(
          session = getDefaultReactiveDomain(),
          inputId = "line_orientation_ts",
          label = NULL,
          selected = "Vertical")
        
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "line_position_ts",
          label = NULL,
          value = as.character(round(input$ts_click1$x, digits = 2))
        )
      }
    }
  })
  
  observeEvent(input$ts_dblclick1,{
    if (input$custom_features_ts == TRUE & input$feature_ts == "Line"){
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "line_orientation_ts",
        label = NULL,
        selected = "Horizontal")
      
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "line_position_ts",
        label = NULL,
        value = as.character(round(input$ts_dblclick1$y, digits = 2))
      )
    }
  })
  
  # TS highlight setter
  observeEvent(input$ts_brush1,{
    if (input$custom_features_ts == TRUE & input$feature_ts == "Highlight"){
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_x_values_ts",
        label = NULL,
        value = round(c(input$ts_brush1[[1]],input$ts_brush1[[2]]), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_y_values_ts",
        label = NULL,
        value = round(c(input$ts_brush1[[3]],input$ts_brush1[[4]]), digits = 2))
    }
  })
  
  ####### Initialise and update custom points lines highlights ----
  
  map_points_data = reactiveVal(data.frame())
  map_highlights_data = reactiveVal(data.frame())
  
  ts_points_data = reactiveVal(data.frame())
  ts_highlights_data = reactiveVal(data.frame())
  ts_lines_data = reactiveVal(data.frame())
  
  # Map Points
  observeEvent(
    input$add_point,
    {
      new_points <- create_new_points_data(
        point_x_values = input$point_location_x,
        point_y_values = input$point_location_y,
        point_label = input$point_label,
        point_shape = input$point_shape,
        point_color = input$point_colour,
        point_size = input$point_size
      )
      
      if (input$projection != "UTM (default)") {
        new_points <- transform_points_df(
          df = new_points,
          xcol = "x_value",
          ycol = "y_value",
          projection_from = switch(
            input$projection,
            "Robinson" = "+proj=robin",
            "Orthographic" = ortho_proj(input$center_lat, input$center_lon),
            "LAEA" = laea_proj
          ),
          projection_to = "+proj=longlat +datum=WGS84"
        )
      }
      
      map_points_data(rbind(map_points_data(), new_points))
    })
  
  observeEvent(input$remove_last_point, {
    map_points_data(map_points_data()[-nrow(map_points_data()), ])
  })
  
  observeEvent(input$remove_all_points, {
    map_points_data(data.frame())
  })
  
  # Map Highlights
  observeEvent(
    input$add_highlight,
    {
      new_highlight <- create_new_highlights_data(
        highlight_x_values = input$highlight_x_values,
        highlight_y_values = input$highlight_y_values,
        highlight_color = input$highlight_colour,
        highlight_type = input$highlight_type,
        show_highlight_on_key = NA,
        highlight_label = NA
      )
      
      if (input$projection != "UTM (default)") {
        new_highlight <- transform_box_df(
          df = new_highlight,
          x1col = "x1",
          x2col = "x2",
          y1col = "y1",
          y2col = "y2",
          projection_from = switch(
            input$projection,
            "Robinson" = "+proj=robin",
            "Orthographic" = ortho_proj(input$center_lat, input$center_lon),
            "LAEA" = laea_proj
          ),
          projection_to = "+proj=longlat +datum=WGS84"
        )
      }
      
      map_highlights_data(rbind(map_highlights_data(), new_highlight))
    })
  
  observeEvent(input$remove_last_highlight, {
    map_highlights_data(map_highlights_data()[-nrow(map_highlights_data()), ])
  })
  
  observeEvent(input$remove_all_highlights, {
    map_highlights_data(data.frame())
  })
  
  # timeseries Points
  observeEvent(input$add_point_ts, {
    ts_points_data(rbind(
      ts_points_data(),
      create_new_points_data(
        point_x_values = input$point_location_x_ts,
        point_y_values = input$point_location_y_ts,
        point_label = input$point_label_ts,
        point_shape = input$point_shape_ts,
        point_color = input$point_colour_ts,
        point_size = input$point_size_ts
      )
    ))
  })  
  
  observeEvent(input$remove_last_point_ts, {
    ts_points_data(ts_points_data()[-nrow(ts_points_data()),])
  })
  
  observeEvent(input$remove_all_points_ts, {
    ts_points_data(data.frame())
  })
  
  # timeseries Highlights
  observeEvent(input$add_highlight_ts, {
    ts_highlights_data(rbind(
      ts_highlights_data(),
      create_new_highlights_data(
        highlight_x_values = input$highlight_x_values_ts,
        highlight_y_values = input$highlight_y_values_ts,
        highlight_color = input$highlight_colour_ts,
        highlight_type = input$highlight_type_ts,
        show_highlight_on_key = input$show_highlight_on_legend_ts,
        highlight_label = input$highlight_label_ts
      )
    ))
  })  
  
  observeEvent(input$remove_last_highlight_ts, {
    ts_highlights_data(ts_highlights_data()[-nrow(ts_highlights_data()),])
  })
  
  observeEvent(input$remove_all_highlights_ts, {
    ts_highlights_data(data.frame())
  })
  
  # timeseries Lines
  observeEvent(input$add_line_ts, {
    ts_lines_data(rbind(
      ts_lines_data(),
      create_new_lines_data(
        line_orientation = input$line_orientation_ts,
        line_locations = input$line_position_ts,
        line_color = input$line_colour_ts,
        line_type = input$line_type_ts,
        show_line_on_key = input$show_line_on_legend_ts,
        line_label = input$line_label_ts
      )
    ))
  })  
  
  observeEvent(input$remove_last_line_ts, {
    ts_lines_data(ts_lines_data()[-nrow(ts_lines_data()),])
  })
  
  observeEvent(input$remove_all_lines_ts, {
    ts_lines_data(data.frame())
  })
  
  ####### Generate Metadata for map customization ----
  
  #Prepare Download MAP
  
  metadata_inputs <- reactive({
    generate_metadata_anomalies(
      
      # Common / input data
      range_years         = input$range_years,
      dataset_selected    = input$dataset_selected,
      range_latitude      = input$range_latitude,
      range_longitude     = input$range_longitude,
      range_months        = input$range_months,
      ref_period_sg       = input$ref_period_sg,
      ref_period          = input$ref_period,
      ref_single_year     = input$ref_single_year,
      season_selected     = input$season_selected,
      variable_selected   = input$variable_selected,
      single_year         = input$single_year,
      range_years_sg      = input$range_years_sg,
      axis_input          = input$axis_input,
      axis_mode           = input$axis_mode,
      
      # Map settings
      center_lat               = input$center_lat,
      center_lon               = input$center_lon,
      custom_map               = input$custom_map,
      custom_topo              = input$custom_topo,
      download_options         = input$download_options,
      file_type_map_sec        = input$file_type_map_sec,
      file_type_map            = input$file_type_map,
      file_type_timeseries     = input$file_type_timeseries,
      hide_axis                = input$hide_axis,
      hide_borders             = input$hide_borders,
      label_lakes              = input$label_lakes,
      label_mountains          = input$label_mountains,
      label_rivers             = input$label_rivers,
      projection               = input$projection,
      ref_map_mode             = input$ref_map_mode,
      show_lakes               = input$show_lakes,
      show_mountains           = input$show_mountains,
      show_rivers              = input$show_rivers,
      title_mode               = input$title_mode,
      title_size_input         = input$title_size_input,
      title1_input             = input$title1_input,
      title2_input             = input$title2_input,
      white_land               = input$white_land,
      white_ocean              = input$white_ocean,

      # Time series plot inputs
      axis_input_ts                = NA,
      axis_mode_ts                = NA,
      custom_ts                   = NA,
      download_options_ts         = NA,
      key_position_ts             = NA,
      show_key_ts                 = NA,
      show_ticks_ts               = NA,
      title_mode_ts               = NA,
      title_size_input_ts         = NA,
      title1_input_ts             = NA,
      xaxis_numeric_interval_ts   = NA,
      custom_percentile_ts        = NA,
      percentile_ts               = NA,
      show_ref_ts                 = NA,
      custom_average_ts           = NA,
      moving_percentile_ts        = NA,
      year_moving_ts              = NA,
      
      # Reactive Values
      plotOrder            = character(0),
      availableLayers      = character(0),
      lonlat_vals          = lonlat_vals()
    )
  })
  
  #Download MAP
  output$download_metadata <- downloadHandler(
    filename = function() {"metadata.xlsx"},
    
    content  = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "custom_meta")
      openxlsx::addWorksheet(wb, "custom_points")
      openxlsx::addWorksheet(wb, "custom_highlights")
      
      meta <- isolate(metadata_inputs())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      
      points <- map_points_data()
      if (!is.null(points) && nrow(points) > 0) openxlsx::writeData(wb, "custom_points", points)
      
      highlights <- map_highlights_data()
      if (!is.null(highlights) && nrow(highlights) > 0) openxlsx::writeData(wb, "custom_highlights", highlights)
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Upload MAP
  observeEvent(input$update_metadata, {
    req(input$upload_metadata)
    
    file_path <- input$upload_metadata$datapath
    file_name <- input$upload_metadata$name
    
    # Check that the uploaded file is named "metadata.xlsx"
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata") {
      
      process_uploaded_metadata(
        file_path           = file_path,
        mode                = "map",
        metadata_sheet      = "custom_meta",
        df_ts_points        = NULL,
        df_ts_highlights    = NULL,
        df_ts_lines         = NULL,
        df_map_points       = "custom_points",
        df_map_highlights   = "custom_highlights",
        rv_plotOrder        = plotOrder,
        rv_availableLayers  = availableLayers,
        rv_lonlat_vals      = lonlat_vals,
        map_points_data     = map_points_data,
        map_highlights_data = map_highlights_data,
        ts_points_data      = NULL,
        ts_highlights_data  = NULL,
        ts_lines_data       = NULL
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  #Prepare Download TS
  metadata_inputs_ts <- reactive({
    generate_metadata_anomalies(
      
      # Common / input data
      range_years         = input$range_years,
      dataset_selected    = input$dataset_selected,
      range_latitude      = input$range_latitude,
      range_longitude     = input$range_longitude,
      range_months        = input$range_months,
      ref_period_sg       = input$ref_period_sg,
      ref_period          = input$ref_period,
      ref_single_year     = input$ref_single_year,
      season_selected     = input$season_selected,
      variable_selected   = input$variable_selected,
      single_year         = input$single_year,
      range_years_sg      = input$range_years_sg,
      axis_input          = input$axis_input,
      axis_mode           = input$axis_mode,
      
      # Map settings
      center_lat               = NA,
      center_lon               = NA,
      custom_map               = NA,
      custom_topo              = NA,
      download_options         = NA,
      file_type_map_sec        = NA,
      file_type_map            = NA,
      hide_axis                = NA,
      hide_borders             = NA,
      label_lakes              = NA,
      label_mountains          = NA,
      label_rivers             = NA,
      projection               = NA,
      ref_map_mode             = NA,
      show_lakes               = NA,
      show_mountains           = NA,
      show_rivers              = NA,
      title_mode               = NA,
      title_size_input         = NA,
      title1_input             = NA,
      title2_input             = NA,
      white_land               = NA,
      white_ocean              = NA,

      # Time series plot inputs
      axis_input_ts              = input$axis_input_ts,
      axis_mode_ts               = input$axis_mode_ts,
      custom_ts                  = input$custom_ts,
      download_options_ts        = input$download_options_ts,
      file_type_timeseries       = input$file_type_timeseries,
      key_position_ts            = input$key_position_ts,
      show_key_ts                = input$show_key_ts,
      show_ticks_ts              = input$show_ticks_ts,
      title_mode_ts              = input$title_mode_ts,
      title_size_input_ts        = input$title_size_input_ts,
      title1_input_ts            = input$title1_input_ts,
      xaxis_numeric_interval_ts  = input$xaxis_numeric_interval_ts,
      custom_percentile_ts       = input$custom_percentile_ts,
      percentile_ts              = input$percentile_ts,
      show_ref_ts                = input$show_ref_ts,
      custom_average_ts          = input$custom_average_ts,
      moving_percentile_ts       = input$moving_percentile_ts,
      year_moving_ts             = input$year_moving_ts,
      
      # Reactive Values / DFs
      plotOrder            = NA,
      availableLayers      = NA,
      lonlat_vals          = lonlat_vals()
    )
  })
  
  # Download TS Anomalies Metadata
  output$download_metadata_ts <- downloadHandler(
    filename = function() {"metadata_ts.xlsx"},
    content  = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "custom_meta")
      openxlsx::addWorksheet(wb, "custom_points")
      openxlsx::addWorksheet(wb, "custom_highlights")
      openxlsx::addWorksheet(wb, "custom_lines")
      
      meta <- isolate(metadata_inputs_ts())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      
      points     <- ts_points_data()
      if (!is.null(points) && nrow(points) > 0) openxlsx::writeData(wb, "custom_points", points)
      
      highlights <- ts_highlights_data()
      if (!is.null(highlights) && nrow(highlights) > 0) openxlsx::writeData(wb, "custom_highlights", highlights)
      
      lines      <- ts_lines_data()
      if (!is.null(lines) && nrow(lines) > 0) openxlsx::writeData(wb, "custom_lines", lines)
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Upload TS Anomalies Metadata
  observeEvent(input$update_metadata_ts, {
    req(input$upload_metadata_ts)
    
    file_path <- input$upload_metadata_ts$datapath
    file_name <- input$upload_metadata_ts$name  # This gets the original uploaded file name
    
    # Check that the uploaded file name matches expectation
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata_ts") {
      
      # Proceed with processing
      process_uploaded_metadata(
        file_path           = file_path,
        mode                = "ts",
        metadata_sheet      = "custom_meta",
        df_ts_points        = "custom_points",
        df_ts_highlights    = "custom_highlights",
        df_ts_lines         = "custom_lines",
        df_map_points       = NULL,
        df_map_highlights   = NULL,
        rv_plotOrder        = NULL,
        rv_availableLayers  = NULL,
        rv_lonlat_vals      = lonlat_vals,
        map_points_data     = NULL,
        map_highlights_data = NULL,
        ts_points_data      = ts_points_data,
        ts_highlights_data  = ts_highlights_data,
        ts_lines_data       = ts_lines_data
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata_ts.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  ####### Generate Layer Options for customization ----
  
  ####### Reactive values
  plotOrder <- reactiveVal(character(0))        # full paths
  availableLayers <- reactiveVal(character(0))  # file names
  
  ####### Helper: extract and load shapefiles
  updatePlotOrder <- function(zipFile, plotOrder, availableLayers) {
    temp_dir <- tempfile(pattern = "anomaly_")
    dir.create(temp_dir)
    unzip(zipFile, exdir = temp_dir)
    
    shpFiles <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    layer_names <- tools::file_path_sans_ext(basename(shpFiles))
    
    plotOrder(shpFiles)
    availableLayers(layer_names)
  }
  
  ####### Trigger update on file upload
  observeEvent(input$shpFile, {
    req(input$shpFile)
    updatePlotOrder(
      zipFile = input$shpFile$datapath,
      plotOrder = plotOrder,
      availableLayers = availableLayers
    )
  })
  
  ####### Shape File Renderer
  output$shapefileSelector <- renderUI({
    req(availableLayers())
    shinyjqui::sortableCheckboxGroupInput(
      inputId = "shapes",
      label = "Select and order shapefiles (drag & drop)",
      choices = availableLayers()
    )
  })
  
  ####### Dynamic color pickers for selected shapefiles
  output$colorpickers <- renderUI({
    req(input$shapes, input$shapes_order, plotOrder())
    selected_ordered <- input$shapes_order[input$shapes_order %in% input$shapes]
    shp_files <- plotOrder()[match(selected_ordered, tools::file_path_sans_ext(basename(plotOrder())))]
    
    pickers <- lapply(shp_files, function(file) {
      file_name <- tools::file_path_sans_ext(basename(file))
      input_id <- paste0("shp_colour_", file_name)
      last_val <- isolate(input[[input_id]])  # <-- Get the current color if set, otherwise NULL
      colourpicker::colourInput(
        inputId = input_id,
        label   = paste("Border Color for", file_name),
        value   = if (!is.null(last_val)) last_val else "black",
        showColour = "background",
        palette = "limited",
        allowTransparent = FALSE
      )
    })
    do.call(tagList, pickers)
  })
  
  
  ### COMPOSITES observe, update & interactive controls ----
  
  ####### Input updaters ----
  
  # Set iniital lon/lat values on startup
  lonlat_vals2 = reactiveVal(c(initial_lon_values,initial_lat_values))
  
  # Continent buttons - updates range inputs and lonlat_values
  observeEvent(input$button_global2,{
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(-180,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(-90,90))
    
    lonlat_vals2(c(-180,180,-90,90))
  })
  
  observeEvent(input$projection2, { # also update to global if projection is changed
    if (input$projection2 != "UTM (default)") {
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_longitude2",
        label = NULL,
        value = c(-180, 180))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_latitude2",
        label = NULL,
        value = c(-90, 90))
      lonlat_vals2(c(-180, 180, -90, 90))
    }
  })
  
  observeEvent(input$button_europe2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(-30,40))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(30,75))
    
    lonlat_vals2(c(-30,40,30,75))
  })
  
  observeEvent(input$button_asia2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(25,170))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(5,80))
    
    lonlat_vals2(c(25,170,5,80))
  })
  
  observeEvent(input$button_oceania2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(90,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(-55,20))
    
    lonlat_vals2(c(90,180,-55,20))
  })
  
  observeEvent(input$button_africa2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(-25,55))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(-40,40))
    
    lonlat_vals2(c(-25,55,-40,40))
  })
  
  observeEvent(input$button_n_america2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(-175,-10))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(5,85))
    
    lonlat_vals2(c(-175,-10,5,85))
  })
  
  observeEvent(input$button_s_america2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude2",
      label = NULL,
      value = c(-90,-30))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude2",
      label = NULL,
      value = c(-60,15))
    
    lonlat_vals2(c(-90,-30,-60,15))
  })
  
  observeEvent(input$button_coord2, {
    lonlat_vals2(c(input$range_longitude2,input$range_latitude2))        
  })
  
  #Make continental buttons stay highlighted
  observe({
    if (input$range_longitude2[1] == -180 && input$range_longitude2[2] == 180 &&
        input$range_latitude2[1] == -90 && input$range_latitude2[2] == 90) {
      shinyjs::addClass("button_global2", "green-background")
    } else {
      shinyjs::removeClass("button_global2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude2[1] == -30 && input$range_longitude2[2] == 40 &&
        input$range_latitude2[1] == 30 && input$range_latitude2[2] == 75) {
      shinyjs::addClass("button_europe2", "green-background")
    } else {
      shinyjs::removeClass("button_europe2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude2[1] == 25 && input$range_longitude2[2] == 170 &&
        input$range_latitude2[1] == 5 && input$range_latitude2[2] == 80) {
      shinyjs::addClass("button_asia2", "green-background")
    } else {
      shinyjs::removeClass("button_asia2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude2[1] == 90 && input$range_longitude2[2] == 180 &&
        input$range_latitude2[1] == -55 && input$range_latitude2[2] == 20) {
      shinyjs::addClass("button_oceania2", "green-background")
    } else {
      shinyjs::removeClass("button_oceania2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude2[1] == -25 && input$range_longitude2[2] == 55 &&
        input$range_latitude2[1] == -40 && input$range_latitude2[2] == 40) {
      shinyjs::addClass("button_africa2", "green-background")
    } else {
      shinyjs::removeClass("button_africa2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude2[1] == -175 && input$range_longitude2[2] == -10 &&
        input$range_latitude2[1] == 5 && input$range_latitude2[2] == 85) {
      shinyjs::addClass("button_n_america2", "green-background")
    } else {
      shinyjs::removeClass("button_n_america2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude2[1] == -90 && input$range_longitude2[2] == -30 &&
        input$range_latitude2[1] == -60 && input$range_latitude2[2] == 15) {
      shinyjs::addClass("button_s_america2", "green-background")
    } else {
      shinyjs::removeClass("button_s_america2", "green-background")
    }
  })
  
  #Month Range Updater
  observe({
    if (input$season_selected2 == "Annual"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months2",
        label = NULL,
        selected = c("January", "December"))
    }
  })
  
  observe({
    if (input$season_selected2 == "DJF"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months2",
        label = NULL,
        selected = c("December (prev.)", "February"))
    }
  })
  
  observe({
    if (input$season_selected2 == "MAM"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months2",
        label = NULL,
        selected = c("March", "May"))
    }
  })
  
  observe({
    if (input$season_selected2 == "JJA"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months2",
        label = NULL,
        selected = c("June", "August"))
    }
  })
  
  observe({
    if (input$season_selected2 == "SON"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months2",
        label = NULL,
        selected = c("September", "November"))
    }
  })
  
  # Composite Axis values updater MAP
  observe({
    if (input$axis_mode2 == "Automatic"){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input2",
        value = c(NA,NA))
    }
  })
  
  observe({
    if (input$axis_mode2 == "Fixed" & is.null(input$axis_input2)){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input2",
        value = set_axis_values(data_input = map_data_2(), mode = input$mode_selected2))
    }
  })
  
  # Composite Axis values updater TS
  observe({
    if (input$axis_mode_ts2 == "Automatic"){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_ts2",
        value = c(NA,NA))
    }
  })
  
  observe({
    if (input$axis_mode_ts2 == "Fixed" & is.null(input$axis_input_ts2)){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_ts2",
        value = set_ts_axis_values(data_input = timeseries_data_2()$Mean))
    }
  })
  
  #Update Reference Map
  observe({
    if (input$dataset_selected2 == "ModE-RAclim"){
      updateRadioButtons(
        inputId = "ref_map_mode2",
        label    = NULL,
        choices  = c("None", "Reference Values"),
        selected = "None" , inline = TRUE)
    } else if (input$dataset_selected2 == "ModE-Sim"){
      updateRadioButtons(
        inputId = "ref_map_mode2",
        label    = NULL,
        choices  = c("None", "Absolute Values","Reference Values"),
        selected = "None" , inline = TRUE)
    } else {
      updateRadioButtons(
        inputId = "ref_map_mode2",
        label    = NULL,
        choices  = c("None", "Absolute Values","Reference Values", "SD ratio"),
        selected = "None" , inline = TRUE)
    }
  })
  
  observe({
    if(input$mode_selected2 == "X years prior" | input$mode_selected2 == "Custom reference"){
      updateRadioButtons(
        inputId = "ref_map_mode2",
        label    = NULL,
        choices  = c("None", "Absolute Values", "SD ratio"),
        selected = "None" , inline = TRUE)
    } else {
      updateRadioButtons(
        inputId = "ref_map_mode2",
        label    = NULL,
        choices  = c("None", "Absolute Values","Reference Values", "SD ratio"),
        selected = "None" , inline = TRUE)
    }
  })
  
  
  #Show Absolute Warning 
  observe({
    if (input$ref_map_mode2 == "Absolute Values"){
      showModal(
        # Add modal dialog for warning message
        modalDialog(
          title = "Information",
          "Unrealistic values (such as negative precipitation) can occur if absolute values are used! Cf. “Usage Notes”",
          easyClose = TRUE,
          footer = tagList(modalButton("OK"))
        ))}
  })
  
  
  ####### Interactivity ----
  
  # Input geo-coded locations
  observeEvent(input$search2, {
    location2 <- input$location2
    if (!is.null(location2) && nchar(location2) > 0) {
      location_encoded2 <- URLencode(location2)
      
      projection <- input$projection2
      result <- NULL
      
      if (projection == "UTM (default)") {
        result <- tmaptools::geocode_OSM(location_encoded2)
      } else if (projection == "Robinson") {
        result <- tmaptools::geocode_OSM(location_encoded2, projection = "+proj=robin")
      } else if (projection == "Orthographic") {
        result <- tmaptools::geocode_OSM(location_encoded2,
                              projection = ortho_proj(input$center_lat2, input$center_lon2))
      } else if (projection == "LAEA") {
        result <- tmaptools::geocode_OSM(location_encoded2, projection = laea_proj)
      }
      
      if (!is.null(result$coords)) {
        longitude2 <- result$coords[1]
        latitude2 <- result$coords[2]
        updateTextInput(session, "point_location_x2", value = as.character(longitude2))
        updateTextInput(session, "point_location_y2", value = as.character(latitude2))
        shinyjs::hide(id = "inv_location2")  # Hide the "Invalid location" message
      } else {
        shinyjs::show(id = "inv_location2")  # Show the "Invalid location" message
      }
    } else {
      shinyjs::hide(id = "inv_location2")  # Hide the "Invalid location" message when no input
    }
  })
  
  # Map coordinates/highlights setter
  observeEvent(input$map_brush2,{
    
    x_brush_1 = input$map_brush2[[1]]
    x_brush_2 = input$map_brush2[[2]]
    
    if (input$custom_features2 == FALSE){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_longitude2",
        label = NULL,
        value = round(c(x_brush_1,x_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_latitude2",
        label = NULL,
        value = round(c(input$map_brush2[[3]], input$map_brush2[[4]]), digits = 2)
      )
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "feature2",
        label = NULL,
        selected = "Highlight")
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_x_values2",
        label = NULL,
        value = round(c(x_brush_1, x_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_y_values2",
        label = NULL,
        value = round(c(input$map_brush2[[3]], input$map_brush2[[4]]), digits = 2)
      )
    }
  })
  
  # Map custom points selector
  observeEvent(input$map_dblclick2,{
    dblclick <- input$map_dblclick2
    
    updateCheckboxInput(
      session = getDefaultReactiveDomain(),
      inputId = "custom_features2",
      label = NULL,
      value = TRUE)
    
    updateRadioButtons(
      session = getDefaultReactiveDomain(),
      inputId = "feature2",
      label = NULL,
      selected = "Point")
    
    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "point_location_x2",
      label = NULL,
      value = as.character(round(dblclick$x, digits = 2))
    )
    
    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "point_location_y2",
      label = NULL,
      value = as.character(round(dblclick$y, digits = 2))
    )
  })
  
  # TS point/line setter
  observeEvent(input$ts_click2,{
    if (input$custom_features_ts2 == TRUE){
      if (input$feature_ts2 == "Point"){
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "point_location_x_ts2",
          label = NULL,
          value = as.character(round(input$ts_click2$x, digits = 2))
        )
        
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "point_location_y_ts2",
          label = NULL,
          value = as.character(round(input$ts_click2$y, digits = 2))
        )
      } 
      else if (input$feature_ts2 == "Line"){
        updateRadioButtons(
          session = getDefaultReactiveDomain(),
          inputId = "line_orientation_ts2",
          label = NULL,
          selected = "Vertical")
        
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "line_position_ts2",
          label = NULL,
          value = as.character(round(input$ts_click2$x, digits = 2))
        )
      }
    }
  })
  
  observeEvent(input$ts_dblclick2,{
    if (input$custom_features_ts2 == TRUE & input$feature_ts2 == "Line"){
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "line_orientation_ts2",
        label = NULL,
        selected = "Horizontal")
      
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "line_position_ts2",
        label = NULL,
        value = as.character(round(input$ts_dblclick2$y, digits = 2))
      )
    }
  })
  
  # TS highlight setter
  observeEvent(input$ts_brush2,{
    if (input$custom_features_ts2 == TRUE & input$feature_ts2 == "Highlight"){
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_x_values_ts2",
        label = NULL,
        value = round(c(input$ts_brush2[[1]],input$ts_brush2[[2]]), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_y_values_ts2",
        label = NULL,
        value = round(c(input$ts_brush2[[3]],input$ts_brush2[[4]]), digits = 2))
    }
  })
  
  
  ####### Initialise and update custom points lines highlights ----
  map_points_data2 = reactiveVal(data.frame())
  map_highlights_data2 = reactiveVal(data.frame())
  
  ts_points_data2 = reactiveVal(data.frame())
  ts_highlights_data2 = reactiveVal(data.frame())
  ts_lines_data2 = reactiveVal(data.frame())
  
  observeEvent(
    input$add_point2,
    {
      new_points <- create_new_points_data(
        point_x_values = input$point_location_x2,
        point_y_values = input$point_location_y2,
        point_label = input$point_label2,
        point_shape = input$point_shape2,
        point_color = input$point_colour2,
        point_size = input$point_size2
      )
      
      if (input$projection2 != "UTM (default)") {
        new_points <- transform_points_df(
          df = new_points,
          xcol = "x_value",
          ycol = "y_value",
          projection_from = switch(
            input$projection2,
            "Robinson" = "+proj=robin",
            "Orthographic" = ortho_proj(input$center_lat2, input$center_lon2),
            "LAEA" = laea_proj
          ),
          projection_to = "+proj=longlat +datum=WGS84"
        )
      }
      
      map_points_data2(rbind(map_points_data2(), new_points))
    })
  
  
  observeEvent(input$remove_last_point2, {
    map_points_data2(map_points_data2()[-nrow(map_points_data2()),])
  })
  
  observeEvent(input$remove_all_points2, {
    map_points_data2(data.frame())
  })
  
  # Map Highlights
  observeEvent(
    input$add_highlight2,
    {
      new_highlight <- create_new_highlights_data(
        highlight_x_values = input$highlight_x_values2,
        highlight_y_values = input$highlight_y_values2,
        highlight_color = input$highlight_colour2,
        highlight_type = input$highlight_type2,
        show_highlight_on_key = NA,
        highlight_label = NA
      )
      
      print(new_highlight)  # check what coords and columns look like
      
      if (input$projection2 != "UTM (default)") {
        new_highlight <- transform_box_df(
          df = new_highlight,
          x1col = "x1",
          x2col = "x2",
          y1col = "y1",
          y2col = "y2",
          projection_from = switch(
            input$projection2,
            "Robinson" = "+proj=robin",
            "Orthographic" = ortho_proj(input$center_lat2, input$center_lon2),
            "LAEA" = laea_proj
          ),
          projection_to = "+proj=longlat +datum=WGS84"
        )
      }
      
      map_highlights_data2(rbind(map_highlights_data2(), new_highlight))
    })
  
  
  observeEvent(input$remove_last_highlight2, {
    map_highlights_data2(map_highlights_data2()[-nrow(map_highlights_data2()),])
  })
  
  observeEvent(input$remove_all_highlights2, {
    map_highlights_data2(data.frame())
  })
  
  # timeseries Points
  observeEvent(input$add_point_ts2, {
    ts_points_data2(rbind(
      ts_points_data2(),
      create_new_points_data(
        point_x_values = input$point_location_x_ts2,
        point_y_values = input$point_location_y_ts2,
        point_label = input$point_label_ts2,
        point_shape = input$point_shape_ts2,
        point_color = input$point_colour_ts2,
        point_size = input$point_size_ts2
      )
    ))
  })  
  
  observeEvent(input$remove_last_point_ts2, {
    ts_points_data2(ts_points_data2()[-nrow(ts_points_data2()),])
  })
  
  observeEvent(input$remove_all_points_ts2, {
    ts_points_data2(data.frame())
  })
  
  # timeseries Highlights
  observeEvent(input$add_highlight_ts2, {
    ts_highlights_data2(rbind(
      ts_highlights_data2(),
      create_new_highlights_data(
        highlight_x_values = input$highlight_x_values_ts2,
        highlight_y_values = input$highlight_y_values_ts2,
        highlight_color = input$highlight_colour_ts2,
        highlight_type = input$highlight_type_ts2,
        show_highlight_on_key = input$show_highlight_on_legend_ts2,
        highlight_label = input$highlight_label_ts2
      )
    ))
  })  
  
  observeEvent(input$remove_last_highlight_ts2, {
    ts_highlights_data2(ts_highlights_data2()[-nrow(ts_highlights_data2()),])
  })
  
  observeEvent(input$remove_all_highlights_ts2, {
    ts_highlights_data2(data.frame())
  })
  
  # timeseries Lines
  observeEvent(input$add_line_ts2, {
    ts_lines_data2(rbind(
      ts_lines_data2(),
      create_new_lines_data(
        line_orientation = input$line_orientation_ts2,
        line_locations = input$line_position_ts2,
        line_color = input$line_colour_ts2,
        line_type = input$line_type_ts2,
        show_line_on_key = input$show_line_on_legend_ts2,
        line_label = input$line_label_ts2
      )
    ))
  })  
  
  observeEvent(input$remove_last_line_ts2, {
    ts_lines_data2(ts_lines_data2()[-nrow(ts_lines_data2()),])
  })
  
  observeEvent(input$remove_all_lines_ts2, {
    ts_lines_data2(data.frame())
  })
  
  ####### Generate Metadata for map customization ----
  
  #Prepare Download
  metadata_inputs_composite <- reactive({
    generate_metadata_composite(
      # Shared
      range_years2         = input$range_years2,
      range_years2a        = input$range_years2a,
      dataset_selected2    = input$dataset_selected2,
      range_latitude2      = input$range_latitude2,
      range_longitude2     = input$range_longitude2,
      range_months2        = input$range_months2,
      ref_period_sg2       = input$ref_period_sg2,
      ref_period2          = input$ref_period2,
      ref_single_year2     = input$ref_single_year2,
      season_selected2     = input$season_selected2,
      variable_selected2   = input$variable_selected2,
      enter_upload2        = input$enter_upload2,
      enter_upload2a       = input$enter_upload2a,
      mode_selected2       = input$mode_selected2,
      prior_years2         = input$prior_years2,
      
      # Map settings
      axis_input2               = input$axis_input2,
      axis_mode2                = input$axis_mode2,
      center_lat2               = input$center_lat2,
      center_lon2               = input$center_lon2,
      custom_map2               = input$custom_map2,
      custom_topo2              = input$custom_topo2,
      download_options2         = input$download_options2,
      enable_custom_statistics2 = input$enable_custom_statistics2,
      file_type_map_sec2        = input$file_type_map_sec2,
      file_type_map2            = input$file_type_map2,
      file_type_timeseries2     = input$file_type_timeseries2,
      hide_axis2                = input$hide_axis2,
      hide_borders2             = input$hide_borders2,
      label_lakes2              = input$label_lakes2,
      label_mountains2          = input$label_mountains2,
      label_rivers2             = input$label_rivers2,
      percentage_sign_match2    = input$percentage_sign_match2,
      projection2               = input$projection2,
      ref_map_mode2             = input$ref_map_mode2,
      sd_ratio2                 = input$sd_ratio2,
      show_lakes2               = input$show_lakes2,
      show_mountains2           = input$show_mountains2,
      show_rivers2              = input$show_rivers2,
      title_mode2               = input$title_mode2,
      title_size_input2         = input$title_size_input2,
      title1_input2             = input$title1_input2,
      title2_input2             = input$title2_input2,
      white_land2               = input$white_land2,
      white_ocean2              = input$white_ocean2,
      
      # TS section not needed here
      axis_input_ts2                = NA,
      axis_mode_ts2                = NA,
      custom_percentile_ts2        = NA,
      custom_ts2                   = NA,
      download_options_ts2         = NA,
      key_position_ts2             = NA,
      show_key_ts2                 = NA,
      show_ref_ts2                 = NA,
      show_ticks_ts2               = NA,
      title_mode_ts2               = NA,
      title_size_input_ts2         = NA,
      title1_input_ts2             = NA,
      xaxis_numeric_interval_ts2   = NA,
      
      # Reac values
      plotOrder            = character(0),
      availableLayers      = character(0),
      lonlat_vals          = lonlat_vals2()
    )
  })
  
  # Download Composite Map Metadata
  output$download_metadata2 <- downloadHandler(
    filename = function() {"metadata_composite.xlsx"},
    content  = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "custom_meta")
      openxlsx::addWorksheet(wb, "custom_points")
      openxlsx::addWorksheet(wb, "custom_highlights")
      
      meta <- isolate(metadata_inputs_composite())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      
      if (nrow(map_points_data2()) > 0) openxlsx::writeData(wb, "custom_points", map_points_data2())
      if (nrow(map_highlights_data2()) > 0) openxlsx::writeData(wb, "custom_highlights", map_highlights_data2())
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Upload Composite Map Metadata
  observeEvent(input$update_metadata2, {
    req(input$upload_metadata2)
    
    file_path <- input$upload_metadata2$datapath
    file_name <- input$upload_metadata2$name
    
    # Check that the uploaded file is named "metadata_composite.xlsx"
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata_composite") {
      
      process_uploaded_metadata_composite(
        file_path           = file_path,
        metadata_sheet      = "custom_meta",
        df_ts_points        = NULL,
        df_ts_highlights    = NULL,
        df_ts_lines         = NULL,
        df_map_points       = "custom_points",
        df_map_highlights   = "custom_highlights",
        rv_plotOrder        = plotOrder2,
        rv_availableLayers  = availableLayers2,
        rv_lonlat_vals      = lonlat_vals2,
        map_points_data     = map_points_data2,
        map_highlights_data = map_highlights_data2,
        ts_points_data      = NULL,
        ts_highlights_data  = NULL,
        ts_lines_data       = NULL
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata_composite.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  
  
  #Prepare TS Download
  metadata_inputs_composite_ts <- reactive({
    generate_metadata_composite(
      # Shared
      range_years2         = input$range_years2,
      range_years2a        = input$range_years2a,
      dataset_selected2    = input$dataset_selected2,
      range_latitude2      = input$range_latitude2,
      range_longitude2     = input$range_longitude2,
      range_months2        = input$range_months2,
      ref_period_sg2       = input$ref_period_sg2,
      ref_period2          = input$ref_period2,
      ref_single_year2     = input$ref_single_year2,
      season_selected2     = input$season_selected2,
      variable_selected2   = input$variable_selected2,
      enter_upload2        = input$enter_upload2,
      enter_upload2a       = input$enter_upload2a,
      mode_selected2       = input$mode_selected2,
      prior_years2         = input$prior_years2,
      
      # Map section not needed here
      axis_input2               = NA,
      axis_mode2                = NA,
      center_lat2               = NA,
      center_lon2               = NA,
      custom_map2               = NA,
      custom_topo2              = NA,
      download_options2         = NA,
      enable_custom_statistics2 = NA,
      file_type_map_sec2        = NA,
      file_type_map2            = NA,
      file_type_timeseries2     = NA,
      hide_axis2                = NA,
      hide_borders2             = NA,
      label_lakes2              = NA,
      label_mountains2          = NA,
      label_rivers2             = NA,
      percentage_sign_match2    = NA,
      projection2               = NA,
      ref_map_mode2             = NA,
      sd_ratio2                 = NA,
      show_lakes2               = NA,
      show_mountains2           = NA,
      show_rivers2              = NA,
      title_mode2               = NA,
      title_size_input2         = NA,
      title1_input2             = NA,
      title2_input2             = NA,
      white_land2               = NA,
      white_ocean2              = NA,
      
      # TS inputs
      axis_input_ts2                = input$axis_input_ts2,
      axis_mode_ts2                = input$axis_mode_ts2,
      custom_percentile_ts2        = input$custom_percentile_ts2,
      custom_ts2                   = input$custom_ts2,
      download_options_ts2         = input$download_options_ts2,
      key_position_ts2             = input$key_position_ts2,
      show_key_ts2                 = input$show_key_ts2,
      show_ref_ts2                 = input$show_ref_ts2,
      show_ticks_ts2               = input$show_ticks_ts2,
      title_mode_ts2               = input$title_mode_ts2,
      title_size_input_ts2         = input$title_size_input_ts2,
      title1_input_ts2             = input$title1_input_ts2,
      xaxis_numeric_interval_ts2   = input$xaxis_numeric_interval_ts2,
      
      # Reac values
      plotOrder           = NULL,
      availableLayers     = NULL,
      lonlat_vals         = lonlat_vals2()
    )
  })
  
  # Download Composite TS Metadata
  output$download_metadata_ts2 <- downloadHandler(
    filename = function() {"metadata_composite_ts.xlsx"},
    content  = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "custom_meta")
      openxlsx::addWorksheet(wb, "custom_points")
      openxlsx::addWorksheet(wb, "custom_highlights")
      openxlsx::addWorksheet(wb, "custom_lines")
      
      meta <- isolate(metadata_inputs_composite_ts())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      
      if (nrow(ts_points_data2()) > 0) openxlsx::writeData(wb, "custom_points", ts_points_data2())
      if (nrow(ts_highlights_data2()) > 0) openxlsx::writeData(wb, "custom_highlights", ts_highlights_data2())
      if (nrow(ts_lines_data2()) > 0) openxlsx::writeData(wb, "custom_lines", ts_lines_data2())
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Upload Composite TS Metadata
  observeEvent(input$update_metadata_ts2, {
    req(input$upload_metadata_ts2)
    
    file_path <- input$upload_metadata_ts2$datapath
    file_name <- input$upload_metadata_ts2$name
    
    # Check that the uploaded file is named "metadata_composite_ts.xlsx"
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata_composite_ts") {
      
      process_uploaded_metadata_composite(
        file_path           = file_path,
        metadata_sheet      = "custom_meta",
        df_ts_points        = "custom_points",
        df_ts_highlights    = "custom_highlights",
        df_ts_lines         = "custom_lines",
        df_map_points       = NULL,
        df_map_highlights   = NULL,
        rv_plotOrder        = NULL,
        rv_availableLayers  = NULL,
        rv_lonlat_vals      = lonlat_vals2,
        map_points_data     = NULL,
        map_highlights_data = NULL,
        ts_points_data      = ts_points_data2,
        ts_highlights_data  = ts_highlights_data2,
        ts_lines_data       = ts_lines_data2
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata_composite_ts.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  
  ####### Generate Layer Options for customization ----
  
  ### Reactive values
  plotOrder2 <- reactiveVal(character(0))        # full paths
  availableLayers2 <- reactiveVal(character(0))  # file names
  
  # Helper: extract and load shapefiles
  updatePlotOrder2 <- function(zipFile2, plotOrder2, availableLayers2) {
    temp_dir2 <- tempfile(pattern = "composite_")
    dir.create(temp_dir2)
    unzip(zipFile2, exdir = temp_dir2)
    
    shpFiles2 <- list.files(temp_dir2, pattern = "\\.shp$", full.names = TRUE)
    layer_names2 <- tools::file_path_sans_ext(basename(shpFiles2))
    
    plotOrder2(shpFiles2)
    availableLayers2(layer_names2)
  }
  
  # Trigger update on file upload
  observeEvent(input$shpFile2, {
    req(input$shpFile2)
    updatePlotOrder2(
      zipFile2 = input$shpFile2$datapath,
      plotOrder2 = plotOrder2,
      availableLayers2 = availableLayers2
    )
  })
  
  # Shape File Renderer
  output$shapefileSelector2 <- renderUI({
    req(availableLayers2())
    shinyjqui::sortableCheckboxGroupInput(
      inputId = "shapes2",
      label = "Select and order shapefiles (drag & drop)",
      choices = availableLayers2()
    )
  })
  
  # Dynamic color pickers for selected shapefiles
  output$colorpickers2 <- renderUI({
    req(input$shapes2, input$shapes2_order, plotOrder2())
    selected_ordered2 <- input$shapes2_order[input$shapes2_order %in% input$shapes2]
    shp_files2 <- plotOrder2()[match(selected_ordered2, tools::file_path_sans_ext(basename(plotOrder2())))]
    
    pickers2 <- lapply(shp_files2, function(file2) {
      file_name2 <- tools::file_path_sans_ext(basename(file2))
      input_id2 <- paste0("shp_colour2_", file_name2)
      last_val2 <- isolate(input[[input_id2]])
      colourpicker::colourInput(
        inputId = input_id2,
        label   = paste("Border Color for", file_name2),
        value   = if (!is.null(last_val2)) last_val2 else "black",
        showColour = "background",
        palette = "limited",
        allowTransparent = FALSE
      )
    })
    do.call(tagList, pickers2)
  })
  
  
  ### CORRELATION observe, update & interactive controls ----
  
  ####### Input updaters ----
  
  # Update variable selection
  observe({
    req(user_data_v1())
    
    if (input$source_v1 == "User data"){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "user_variable_v1",
        choices = names(user_data_v1())[-1])
    }
  })
  
  observe({
    req(user_data_v2())
    
    if (input$source_v2 == "User data"){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "user_variable_v2",
        choices = names(user_data_v2())[-1])
    }
  })
  
  # timeseries/Field updater
  observe({
    selected_type_v1 = input$type_v1
    
    # Check if source is user data OR map area is very small
    if ((input$source_v1 == "User data") |
        (((
          input$range_longitude_v1[2] - input$range_longitude_v1[1]
        ) < 4) &
        ((
          input$range_latitude_v1[2] - input$range_latitude_v1[1] < 4
        )))) {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "type_v1",
        label = NULL,
        choices = c("Timeseries"),
        selected =  "Timeseries"
      )
      
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "type_v1",
        label = NULL,
        choices  = c("Field", "Timeseries"),
        selected = selected_type_v1,
        inline = TRUE
      )
    }
  })    
  
  observe({
    selected_type_v2 = input$type_v2
    
    # Check if source is user data OR map area is very small
    if ((input$source_v2 == "User data") | (((input$range_longitude_v2[2]-input$range_longitude_v2[1])<4) & ((input$range_latitude_v2[2]-input$range_latitude_v2[1]<4)))){
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "type_v2",
        label = NULL,
        choices = c("Timeseries"),
        selected =  "Timeseries")
      
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "type_v2",
        label = NULL,
        choices  = c( "Field","Timeseries"),
        selected = selected_type_v2,
        inline = TRUE)
    }
  })     
  
  # Mode Updater (based on dataset0)
  observe({
    if (input$dataset_selected_v1 == "ModE-RAclim"){
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "mode_selected_v1",
        label = NULL,
        choices = c("Anomaly"),
        selected =  "Anomaly")
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "mode_selected_v1",
        label = NULL,
        choices = c("Anomaly","Absolute"))
    }
  })
  
  observe({
    if (input$dataset_selected_v2 == "ModE-RAclim"){
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "mode_selected_v2",
        label = NULL,
        choices = c("Anomaly"),
        selected =  "Anomaly")
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "mode_selected_v2",
        label = NULL,
        choices = c("Anomaly","Absolute"))
    }
  })
  
  #Month Range Updater
  observe({
    if (input$season_selected_v1 == "Annual"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v1",
        label = NULL,
        selected = c("January", "December"))
    }
  })
  
  observe({
    if (input$season_selected_v1 == "DJF"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v1",
        label = NULL,
        selected = c("December (prev.)", "February"))
    }
  })
  
  observe({
    if (input$season_selected_v1 == "MAM"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v1",
        label = NULL,
        selected = c("March", "May"))
    }
  })
  
  observe({
    if (input$season_selected_v1 == "JJA"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v1",
        label = NULL,
        selected = c("June", "August"))
    }
  })
  
  observe({
    if (input$season_selected_v1 == "SON"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v1",
        label = NULL,
        selected = c("September", "November"))
    }
  })
  
  observe({
    if (input$season_selected_v2 == "Annual"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v2",
        label = NULL,
        selected = c("January", "December"))
    }
  })
  
  observe({
    if (input$season_selected_v2 == "DJF"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v2",
        label = NULL,
        selected = c("December (prev.)", "February"))
    }
  })
  
  observe({
    if (input$season_selected_v2 == "MAM"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v2",
        label = NULL,
        selected = c("March", "May"))
    }
  })
  
  observe({
    if (input$season_selected_v2 == "JJA"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v2",
        label = NULL,
        selected = c("June", "August"))
    }
  })
  
  observe({
    if (input$season_selected_v2 == "SON"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_v2",
        label = NULL,
        selected = c("September", "November"))
    }
  })
  
  # Update correlation year range and check lag is still within limits
  observe({
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "lagyears_v1_cor",
      max = year_range_cor()[4]-input$range_years3[2],
      min = year_range_cor()[3]-input$range_years3[1]
    )
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "lagyears_v2_cor",
      max = year_range_cor()[6]-input$range_years3[2],
      min = year_range_cor()[5]-input$range_years3[1]
    )
  })
  
  # observeEvent({
  #   input$source_v1
  #   input$source_v2
  #   year_range_cor()
  # }, {
  #   req(year_range_cor(), length(year_range_cor()) >= 2)
  #   
  #   if (input$source_v1 == "User data" || input$source_v2 == "User data") {
  #     shinyWidgets::updateNumericRangeInput(
  #       session = getDefaultReactiveDomain(),
  #       inputId = "range_years3",
  #       label = paste("Select the range of years (", year_range_cor()[7], "-", year_range_cor()[8], ")"),
  #       value = year_range_cor()[1:2]
  #     )
  #   }
  # })
  
  observeEvent({
    input$source_v1
    input$source_v2
    year_range_cor()
  }, {
    req(year_range_cor(), length(year_range_cor()) >= 6)
    
    yr <- year_range_cor()
    
    if (input$source_v1 == "User data" && input$source_v2 == "User data") {
      # both user data → show shared range
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_years3",
        label = paste("Select the overlapping range of years (", yr[5], "-", yr[6], ")"),
        value = yr[5:6]
      )
    } else if (input$source_v1 == "User data") {
      # only variable 1 is user data
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_years3",
        label = paste("Select the range of years (", yr[7], "-", yr[8], ")"),
        value = yr[1:2]
      )
    } else if (input$source_v2 == "User data") {
      # only variable 2 is user data
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_years3",
        label = paste("Select the range of years (", yr[9], "-", yr[10], ")"),
        value = yr[3:4]
      )
    }
  })
  
  
  # Set iniital lon/lat values and update on button press
  lonlat_vals_v1 = reactiveVal(c(4,12,43,50))
  
  # Continent buttons - updates range inputs and lonlat_values
  observeEvent(input$button_global_v1,{
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(-180,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(-90,90))
    
    lonlat_vals_v1(c(-180,180,-90,90))
  }) 
  
  observeEvent(input$button_europe_v1, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(-30,40))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(30,75))
    
    lonlat_vals_v1(c(-30,40,30,75))
  })
  
  observeEvent(input$button_asia_v1, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(25,170))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(5,80))
    
    lonlat_vals_v1(c(25,170,5,80))
  })
  
  observeEvent(input$button_oceania_v1, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(90,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(-55,20))
    
    lonlat_vals_v1(c(90,180,-55,20))
  })
  
  observeEvent(input$button_africa_v1, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(-25,55))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(-40,40))
    
    lonlat_vals_v1(c(-25,55,-40,40))
  })
  
  observeEvent(input$button_n_america_v1, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(-175,-10))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(5,85))
    
    lonlat_vals_v1(c(-175,-10,5,85))
  })
  
  observeEvent(input$button_s_america_v1, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v1",
      label = NULL,
      value = c(-90,-30))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v1",
      label = NULL,
      value = c(-60,15))
    
    lonlat_vals_v1(c(-90,-30,-60,15))
  })
  
  observeEvent(input$button_coord_v1, {
    lonlat_vals_v1(c(input$range_longitude_v1,input$range_latitude_v1))        
  })
  
  #Make continental buttons stay highlighted
  observe({
    if (input$range_longitude_v1[1] == -180 && input$range_longitude_v1[2] == 180 &&
        input$range_latitude_v1[1] == -90 && input$range_latitude_v1[2] == 90) {
      shinyjs::addClass("button_global_v1", "green-background")
    } else {
      shinyjs::removeClass("button_global_v1", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v1[1] == -30 && input$range_longitude_v1[2] == 40 &&
        input$range_latitude_v1[1] == 30 && input$range_latitude_v1[2] == 75) {
      shinyjs::addClass("button_europe_v1", "green-background")
    } else {
      shinyjs::removeClass("button_europe_v1", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v1[1] == 25 && input$range_longitude_v1[2] == 170 &&
        input$range_latitude_v1[1] == 5 && input$range_latitude_v1[2] == 80) {
      shinyjs::addClass("button_asia_v1", "green-background")
    } else {
      shinyjs::removeClass("button_asia_v1", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v1[1] == 90 && input$range_longitude_v1[2] == 180 &&
        input$range_latitude_v1[1] == -55 && input$range_latitude_v1[2] == 20) {
      shinyjs::addClass("button_oceania_v1", "green-background")
    } else {
      shinyjs::removeClass("button_oceania_v1", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v1[1] == -25 && input$range_longitude_v1[2] == 55 &&
        input$range_latitude_v1[1] == -40 && input$range_latitude_v1[2] == 40) {
      shinyjs::addClass("button_africa_v1", "green-background")
    } else {
      shinyjs::removeClass("button_africa_v1", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v1[1] == -175 && input$range_longitude_v1[2] == -10 &&
        input$range_latitude_v1[1] == 5 && input$range_latitude_v1[2] == 85) {
      shinyjs::addClass("button_n_america_v1", "green-background")
    } else {
      shinyjs::removeClass("button_n_america_v1", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v1[1] == -90 && input$range_longitude_v1[2] == -30 &&
        input$range_latitude_v1[1] == -60 && input$range_latitude_v1[2] == 15) {
      shinyjs::addClass("button_s_america_v1", "green-background")
    } else {
      shinyjs::removeClass("button_s_america_v1", "green-background")
    }
  })
  
  # Update to global if projection is changed
  observeEvent(input$projection_v1, {
    if (input$projection_v1 != "UTM (default)" & (lonlat_vals_v1() != c(-180,180,-90,90) | lonlat_vals_v2 != c(-180,180,-90,90)) & (input$type_v1 == "Field" & input$type_v2 == "Field")) {
      # create a pop up message and with button selection
      showModal(modalDialog(
        title = "Action required",
        "Changing the projection will reset the map area to global. This requires a V1 or V2 to be global. Which do you want to change?",
        footer = tagList(
          actionButton("v1", "V1"),
          actionButton("v1", "V2")
        )
      ))
      
      
    }
  })
  
  # Set iniital lon/lat values and update on button press
  lonlat_vals_v2 = reactiveVal(c(initial_lon_values,initial_lat_values))
  
  # Continent buttons - updates range inputs and lonlat_values
  observeEvent(input$button_global_v2,{
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(-180,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(-90,90))
    
    lonlat_vals_v2(c(-180,180,-90,90))
  }) 
  
  observeEvent(input$button_europe_v2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(-30,40))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(30,75))
    
    lonlat_vals_v2(c(-30,40,30,75))
  })
  
  observeEvent(input$button_asia_v2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(25,170))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(5,80))
    
    lonlat_vals_v2(c(25,170,5,80))
  })
  
  observeEvent(input$button_oceania_v2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(90,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(-55,20))
    
    lonlat_vals_v2(c(90,180,-55,20))
  })
  
  observeEvent(input$button_africa_v2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(-25,55))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(-40,40))
    
    lonlat_vals_v2(c(-25,55,-40,40))
  })
  
  observeEvent(input$button_n_america_v2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(-175,-10))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(5,85))
    
    lonlat_vals_v2(c(-175,-10,5,85))
  })
  
  observeEvent(input$button_s_america_v2, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_v2",
      label = NULL,
      value = c(-90,-30))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_v2",
      label = NULL,
      value = c(-60,15))
    
    lonlat_vals_v2(c(-90,-30,-60,15))
  })
  
  observeEvent(input$button_coord_v2, {
    lonlat_vals_v2(c(input$range_longitude_v2,input$range_latitude_v2))        
  })
  
  #Make continental buttons stay highlighted
  observe({
    if (input$range_longitude_v2[1] == -180 && input$range_longitude_v2[2] == 180 &&
        input$range_latitude_v2[1] == -90 && input$range_latitude_v2[2] == 90) {
      shinyjs::addClass("button_global_v2", "green-background")
    } else {
      shinyjs::removeClass("button_global_v2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v2[1] == -30 && input$range_longitude_v2[2] == 40 &&
        input$range_latitude_v2[1] == 30 && input$range_latitude_v2[2] == 75) {
      shinyjs::addClass("button_europe_v2", "green-background")
    } else {
      shinyjs::removeClass("button_europe_v2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v2[1] == 25 && input$range_longitude_v2[2] == 170 &&
        input$range_latitude_v2[1] == 5 && input$range_latitude_v2[2] == 80) {
      shinyjs::addClass("button_asia_v2", "green-background")
    } else {
      shinyjs::removeClass("button_asia_v2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v2[1] == 90 && input$range_longitude_v2[2] == 180 &&
        input$range_latitude_v2[1] == -55 && input$range_latitude_v2[2] == 20) {
      shinyjs::addClass("button_oceania_v2", "green-background")
    } else {
      shinyjs::removeClass("button_oceania_v2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v2[1] == -25 && input$range_longitude_v2[2] == 55 &&
        input$range_latitude_v2[1] == -40 && input$range_latitude_v2[2] == 40) {
      shinyjs::addClass("button_africa_v2", "green-background")
    } else {
      shinyjs::removeClass("button_africa_v2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v2[1] == -175 && input$range_longitude_v2[2] == -10 &&
        input$range_latitude_v2[1] == 5 && input$range_latitude_v2[2] == 85) {
      shinyjs::addClass("button_n_america_v2", "green-background")
    } else {
      shinyjs::removeClass("button_n_america_v2", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_v2[1] == -90 && input$range_longitude_v2[2] == -30 &&
        input$range_latitude_v2[1] == -60 && input$range_latitude_v2[2] == 15) {
      shinyjs::addClass("button_s_america_v2", "green-background")
    } else {
      shinyjs::removeClass("button_s_america_v2", "green-background")
    }
  })
  
  # Correlation axis values updater Map
  observe({
    if (input$axis_mode3 == "Automatic"){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input3",
        value = c(NA,NA))
    }
  })
  
  observe({
    if (input$axis_mode3 == "Fixed" & is.null(input$axis_input3)){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input3",
        value = set_axis_values(data_input = correlation_map_data()[[3]], mode = "Anomaly"))
    }
  })
  
  # Correlation Axis values updater TS
  observe({
    if (input$axis_mode_ts3 == "Automatic"){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_ts3",
        value = c(NA,NA))
    }
  })
  
  observe({
    if (input$axis_mode_ts3 == "Fixed" & is.null(input$axis_input_ts3)){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_ts3",
        value = set_ts_axis_values(data_input = timeseries_data_v1()$Mean))
    }
  })
  
  # Update ts/map correlation method
  observeEvent(input$cor_method_ts, {
    updateRadioButtonsGroup(
      input$cor_method_ts,
      c("cor_method_ts", "cor_method_map", "cor_method_map_data")
    )
  })
  
  observeEvent(input$cor_method_map, {
    updateRadioButtonsGroup(
      input$cor_method_map,
      c("cor_method_ts", "cor_method_map", "cor_method_map_data")
    )
  })
  
  observeEvent(input$cor_method_map_data, {
    updateRadioButtonsGroup(
      input$cor_method_map_data,
      c("cor_method_ts", "cor_method_map", "cor_method_map_data")
    )
  })
  
  
  ####### Interactivity ----
  
  # Input geo-coded locations
  observeEvent(input$search3, {
    location3 <- input$location3
    if (!is.null(location3) && nchar(location3) > 0) {
      location_encoded3 <- URLencode(location3)
      
      projection3 <- input$projection3  # Ensure this exists in your UI
      result <- NULL
      
      if (projection3 == "UTM (default)") {
        result <- tmaptools::geocode_OSM(location_encoded3)
      } else if (projection3 == "Robinson") {
        result <- tmaptools::geocode_OSM(location_encoded3, projection = "+proj=robin")
      } else if (projection3 == "Orthographic") {
        result <- tmaptools::geocode_OSM(location_encoded3,
                              projection = ortho_proj(input$center_lat3, input$center_lon3))
      } else if (projection3 == "LAEA") {
        result <- tmaptools::geocode_OSM(location_encoded3, projection = laea_proj)
      }
      
      if (!is.null(result$coords)) {
        longitude3 <- result$coords[1]
        latitude3 <- result$coords[2]
        updateTextInput(session, "point_location_x3", value = as.character(longitude3))
        updateTextInput(session, "point_location_y3", value = as.character(latitude3))
        shinyjs::hide(id = "inv_location3")  # Hide the "Invalid location" message
      } else {
        shinyjs::show(id = "inv_location3")  # Show the "Invalid location" message
      }
    } else {
      shinyjs::hide(id = "inv_location3")  # Hide the "Invalid location" message when no input
    }
  })
  
  # Map coordinates/highlights setter
  observeEvent(input$map_brush3,{
    
    x_brush_1 = input$map_brush3[[1]]
    x_brush_2 = input$map_brush3[[2]]
    y_brush_1 = input$map_brush3[[3]]
    y_brush_2 = input$map_brush3[[4]]
    
    if (input$custom_features3 == FALSE){
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_longitude_v2",
        label = NULL,
        value = round(c(x_brush_1,x_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_latitude_v2",
        label = NULL,
        value = round(c(y_brush_1, y_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_longitude_v1",
        label = NULL,
        value = round(c(x_brush_1,x_brush_2), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_latitude_v1",
        label = NULL,
        value = round(c(y_brush_1, y_brush_2), digits = 2))
      
      
    } else {
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "feature3",
        label = NULL,
        selected = "Highlight")
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_x_values3",
        label = NULL,
        value = round(c(x_brush_1, x_brush_2), digits = 2))
      
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_y_values3",
        label = NULL,
        value = round(c(input$map_brush3[[3]],input$map_brush3[[4]]), digits = 2))
    }
  })
  
  # Map custom points selector
  observeEvent(input$map_dblclick3,{
    dblclick <- input$map_dblclick3
    
    
    updateCheckboxInput(
      session = getDefaultReactiveDomain(),
      inputId = "custom_features3",
      label = NULL,
      value = TRUE)
    
    updateRadioButtons(
      session = getDefaultReactiveDomain(),
      inputId = "feature3",
      label = NULL,
      selected = "Point")
    
    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "point_location_x3",
      label = NULL,
      value = as.character(round(dblclick$x, digits = 2))
    )
    
    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "point_location_y3",
      label = NULL,
      value = as.character(round(dblclick$y, digits = 2))
    )
  })
  
  # TS point/line setter
  observeEvent(input$ts_click3,{
    if (input$custom_features_ts3 == TRUE){
      if (input$feature_ts3 == "Point"){
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "point_location_x_ts3",
          label = NULL,
          value = as.character(round(input$ts_click3$x, digits = 2))
        )
        
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "point_location_y_ts3",
          label = NULL,
          value = as.character(round(input$ts_click3$y, digits = 2))
        )
      } 
      else if (input$feature_ts3 == "Line"){
        updateRadioButtons(
          session = getDefaultReactiveDomain(),
          inputId = "line_orientation_ts3",
          label = NULL,
          selected = "Vertical")
        
        updateTextInput(
          session = getDefaultReactiveDomain(),
          inputId = "line_position_ts3",
          label = NULL,
          value = as.character(round(input$ts_click3$x, digits = 2))
        )
      }
    }
  })
  
  observeEvent(input$ts_dblclick3,{
    if (input$custom_features_ts3 == TRUE & input$feature_ts3 == "Line"){
      updateRadioButtons(
        session = getDefaultReactiveDomain(),
        inputId = "line_orientation_ts3",
        label = NULL,
        selected = "Horizontal")
      
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "line_position_ts3",
        label = NULL,
        value = as.character(round(input$ts_dblclick3$y, digits = 2))
      )
    }
  })
  
  # TS highlight setter
  observeEvent(input$ts_brush3,{
    if (input$custom_features_ts3 == TRUE & input$feature_ts3 == "Highlight"){
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_x_values_ts3",
        label = NULL,
        value = round(c(input$ts_brush3[[1]],input$ts_brush3[[2]]), digits = 2))
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "highlight_y_values_ts3",
        label = NULL,
        value = round(c(input$ts_brush3[[3]],input$ts_brush3[[4]]), digits = 2))
    }
  })
  
  
  ####### Initialise and update custom points lines highlights ----
  
  map_points_data3 = reactiveVal(data.frame())
  map_highlights_data3 = reactiveVal(data.frame())
  
  ts_points_data3 = reactiveVal(data.frame())
  ts_highlights_data3 = reactiveVal(data.frame())
  ts_lines_data3 = reactiveVal(data.frame())
  
  # Map Points 3
  observeEvent(
    input$add_point3,
    {
      new_points <- create_new_points_data(
        point_x_values = input$point_location_x3,
        point_y_values = input$point_location_y3,
        point_label = input$point_label3,
        point_shape = input$point_shape3,
        point_color = input$point_colour3,
        point_size = input$point_size3
      )
      
      if (input$projection3 != "UTM (default)") {
        new_points <- transform_points_df(
          df = new_points,
          xcol = "x_value",
          ycol = "y_value",
          projection_from = switch(
            input$projection3,
            "Robinson" = "+proj=robin",
            "Orthographic" = ortho_proj(input$center_lat3, input$center_lon3),
            "LAEA" = laea_proj
          ),
          projection_to = "+proj=longlat +datum=WGS84"
        )
      }
      
      map_points_data3(rbind(map_points_data3(), new_points))
    })
  
  observeEvent(input$remove_last_point3, {
    map_points_data3(map_points_data3()[-nrow(map_points_data3()), ])
  })
  
  observeEvent(input$remove_all_points3, {
    map_points_data3(data.frame())
  })
  
  # Map Highlights 3
  observeEvent(
    input$add_highlight3,
    {
      new_highlight <- create_new_highlights_data(
        highlight_x_values = input$highlight_x_values3,
        highlight_y_values = input$highlight_y_values3,
        highlight_color = input$highlight_colour3,
        highlight_type = input$highlight_type3,
        show_highlight_on_key = NA,
        highlight_label = NA
      )
      
      if (input$projection3 != "UTM (default)") {
        new_highlight <- transform_box_df(
          df = new_highlight,
          x1col = "x1",
          x2col = "x2",
          y1col = "y1",
          y2col = "y2",
          projection_from = switch(
            input$projection3,
            "Robinson" = "+proj=robin",
            "Orthographic" = ortho_proj(input$center_lat3, input$center_lon3),
            "LAEA" = laea_proj
          ),
          projection_to = "+proj=longlat +datum=WGS84"
        )
      }
      
      map_highlights_data3(rbind(map_highlights_data3(), new_highlight))
    })
  
  observeEvent(input$remove_last_highlight3, {
    map_highlights_data3(map_highlights_data3()[-nrow(map_highlights_data3()), ])
  })
  
  observeEvent(input$remove_all_highlights3, {
    map_highlights_data3(data.frame())
  })
  
  # timeseries Points
  observeEvent(input$add_point_ts3, {
    ts_points_data3(rbind(
      ts_points_data3(),
      create_new_points_data(
        point_x_values = input$point_location_x_ts3,
        point_y_values = input$point_location_y_ts3,
        point_label = input$point_label_ts3,
        point_shape = input$point_shape_ts3,
        point_color = input$point_colour_ts3,
        point_size = input$point_size_ts3
      )
    ))
  })
  
  observeEvent(input$remove_last_point_ts3, {
    ts_points_data3(ts_points_data3()[-nrow(ts_points_data3()),])
  })
  
  observeEvent(input$remove_all_points_ts3, {
    ts_points_data3(data.frame())
  })
  
  # timeseries Highlights
  observeEvent(input$add_highlight_ts3, {
    ts_highlights_data3(rbind(
      ts_highlights_data3(),
      create_new_highlights_data(
        highlight_x_values = input$highlight_x_values_ts3,
        highlight_y_values = input$highlight_y_values_ts3,
        highlight_color = input$highlight_colour_ts3,
        highlight_type = input$highlight_type_ts3,
        show_highlight_on_key = input$show_highlight_on_legend_ts3,
        highlight_label = input$highlight_label_ts3
      )
    ))
  })  
  
  observeEvent(input$remove_last_highlight_ts3, {
    ts_highlights_data3(ts_highlights_data3()[-nrow(ts_highlights_data3()),])
  })
  
  observeEvent(input$remove_all_highlights_ts3, {
    ts_highlights_data3(data.frame())
  })
  
  # timeseries Lines
  observeEvent(input$add_line_ts3, {
    ts_lines_data3(rbind(
      ts_lines_data3(),
      create_new_lines_data(
        line_orientation = input$line_orientation_ts3,
        line_locations = input$line_position_ts3,
        line_color = input$line_colour_ts3,
        line_type = input$line_type_ts3,
        show_line_on_key = input$show_line_on_legend_ts3,
        line_label = input$line_label_ts3
      )
    ))
  })  
  
  observeEvent(input$remove_last_line_ts3, {
    ts_lines_data3(ts_lines_data3()[-nrow(ts_lines_data3()),])
  })
  
  observeEvent(input$remove_all_lines_ts3, {
    ts_lines_data3(data.frame())
  })
  
  ####### Generate Metadata for map customization ----
  
  # Reactive metadata collector for Correlation Timeseries
  metadata_inputs_correlation_ts <- reactive({
    generate_metadata_correlation(
      # Shared
      range_years3         = input$range_years3,
      dataset_selected_v1  = input$dataset_selected_v1,
      dataset_selected_v2  = input$dataset_selected_v2,
      ME_variable_v1       = input$ME_variable_v1,
      ME_variable_v2       = input$ME_variable_v2,
      coordinates_type_v1  = input$coordinates_type_v1,
      coordinates_type_v2  = input$coordinates_type_v2,
      mode_selected_v1     = input$mode_selected_v1,
      mode_selected_v2     = input$mode_selected_v2,
      season_selected_v1   = input$season_selected_v1,
      season_selected_v2   = input$season_selected_v2,
      range_months_v1      = input$range_months_v1,
      range_months_v2      = input$range_months_v2,
      range_latitude_v1    = input$range_latitude_v1,
      range_latitude_v2    = input$range_latitude_v2,
      range_longitude_v1   = input$range_longitude_v1,
      range_longitude_v2   = input$range_longitude_v2,
      ref_period_sg_v1     = input$ref_period_sg_v1,
      ref_period_sg_v2     = input$ref_period_sg_v2,
      ref_period_v1        = input$ref_period_v1,
      ref_period_v2        = input$ref_period_v2,
      ref_single_year_v1   = input$ref_single_year_v1,
      ref_single_year_v2   = input$ref_single_year_v2,
      source_v1            = input$source_v1,
      source_v2            = input$source_v2,
      type_v1              = input$type_v1,
      type_v2              = input$type_v2,
      lagyears_v1_cor      = input$lagyears_v1_cor,
      lagyears_v2_cor      = input$lagyears_v2_cor,
      
      # Map section NA
      axis_input3                = NA,
      axis_mode3                 = NA,
      center_lat3                = NA,
      center_lon3                = NA,
      custom_map3                = NA,
      custom_topo3               = NA,
      download_options3          = NA,
      file_type_map3             = NA,
      file_type_map_sec3         = NA,
      hide_axis3                 = NA,
      hide_borders3              = NA,
      label_lakes3               = NA,
      label_mountains3           = NA,
      label_rivers3              = NA,
      projection3                = NA,
      ref_map_mode3              = NA,
      cor_method_map             = NA,
      cor_method_map_data        = NA,
      show_lakes3                = NA,
      show_mountains3            = NA,
      show_rivers3               = NA,
      title_mode3                = NA,
      title_size_input3          = NA,
      title1_input3              = NA,
      title2_input3              = NA,
      white_land3                = NA,
      white_ocean3               = NA,
      
      # TS inputs
      axis_input_ts3             = input$axis_input_ts3,
      axis_mode_ts3             = input$axis_mode_ts3,
      cor_method_ts             = input$cor_method_ts,
      custom_ts3                = input$custom_ts3,
      custom_ref_ts3            = input$custom_ref_ts3,
      custom_average_ts3        = input$custom_average_ts3,
      download_options_ts3      = input$download_options_ts3,
      file_type_timeseries3     = input$file_type_timeseries3,
      key_position_ts3          = input$key_position_ts3,
      title_mode_ts3            = input$title_mode_ts3,
      title_size_input_ts3      = input$title_size_input_ts3,
      title1_input_ts3          = input$title1_input_ts3,
      xaxis_numeric_interval_ts3 = input$xaxis_numeric_interval_ts3,
      year_moving_ts3           = input$year_moving_ts3,
      add_outliers_ref_ts3      = input$add_outliers_ref_ts3,
      add_trend_ref_ts3         = input$add_trend_ref_ts3,
      show_key_ts3              = input$show_key_ts3,
      show_key_ref_ts3          = input$show_key_ref_ts3,
      show_ticks_ts3            = input$show_ticks_ts3,
      sd_input_ref_ts3          = input$sd_input_ref_ts3,
      trend_sd_input_ref_ts3    = input$trend_sd_input_ref_ts3,
      
      # Reactive values
      plotOrder       = NULL,
      availableLayers = NULL,
      lonlat_vals_v1 = lonlat_vals_v1(),
      lonlat_vals_v2 = lonlat_vals_v2()
    )
  })
  
  # Download Correlation TS Metadata
  output$download_metadata_ts3 <- downloadHandler(
    filename = function() { "metadata_correlation_ts.xlsx" },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "custom_meta")
      openxlsx::addWorksheet(wb, "custom_points")
      openxlsx::addWorksheet(wb, "custom_highlights")
      openxlsx::addWorksheet(wb, "custom_lines")
      
      meta <- isolate(metadata_inputs_correlation_ts())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      if (nrow(ts_points_data3()) > 0) openxlsx::writeData(wb, "custom_points", ts_points_data3())
      if (nrow(ts_highlights_data3()) > 0) openxlsx::writeData(wb, "custom_highlights", ts_highlights_data3())
      if (nrow(ts_lines_data3()) > 0) openxlsx::writeData(wb, "custom_lines", ts_lines_data3())
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Upload Correlation TS Metadata
  observeEvent(input$update_metadata_ts3, {
    req(input$upload_metadata_ts3)
    
    file_path <- input$upload_metadata_ts3$datapath
    file_name <- input$upload_metadata_ts3$name
    
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata_correlation_ts") {
      
      process_uploaded_metadata_correlation(
        file_path           = file_path,
        mode                = "ts",
        metadata_sheet      = "custom_meta",
        df_ts_points        = "custom_points",
        df_ts_highlights    = "custom_highlights",
        df_ts_lines         = "custom_lines",
        df_map_points       = NULL,
        df_map_highlights   = NULL,
        rv_plotOrder        = NULL,
        rv_availableLayers  = NULL,
        rv_lonlat_vals_v1   = lonlat_vals_v1,
        rv_lonlat_vals_v2   = lonlat_vals_v2,
        map_points_data     = NULL,
        map_highlights_data = NULL,
        ts_points_data      = ts_points_data3,
        ts_highlights_data  = ts_highlights_data3,
        ts_lines_data       = ts_lines_data3
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata_correlation_ts.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  
  # Reactive metadata collector for Correlation Map
  metadata_inputs_correlation <- reactive({
    generate_metadata_correlation(
      # Shared
      range_years3         = input$range_years3,
      dataset_selected_v1  = input$dataset_selected_v1,
      dataset_selected_v2  = input$dataset_selected_v2,
      ME_variable_v1       = input$ME_variable_v1,
      ME_variable_v2       = input$ME_variable_v2,
      coordinates_type_v1  = input$coordinates_type_v1,
      coordinates_type_v2  = input$coordinates_type_v2,
      mode_selected_v1     = input$mode_selected_v1,
      mode_selected_v2     = input$mode_selected_v2,
      season_selected_v1   = input$season_selected_v1,
      season_selected_v2   = input$season_selected_v2,
      range_months_v1      = input$range_months_v1,
      range_months_v2      = input$range_months_v2,
      range_latitude_v1    = input$range_latitude_v1,
      range_latitude_v2    = input$range_latitude_v2,
      range_longitude_v1   = input$range_longitude_v1,
      range_longitude_v2   = input$range_longitude_v2,
      ref_period_sg_v1     = input$ref_period_sg_v1,
      ref_period_sg_v2     = input$ref_period_sg_v2,
      ref_period_v1        = input$ref_period_v1,
      ref_period_v2        = input$ref_period_v2,
      ref_single_year_v1   = input$ref_single_year_v1,
      ref_single_year_v2   = input$ref_single_year_v2,
      source_v1            = input$source_v1,
      source_v2            = input$source_v2,
      type_v1              = input$type_v1,
      type_v2              = input$type_v2,
      lagyears_v1_cor      = input$lagyears_v1_cor,
      lagyears_v2_cor      = input$lagyears_v2_cor,
      
      # TS section NA
      axis_input_ts3              = NA,
      axis_mode_ts3               = NA,
      cor_method_ts               = NA,
      custom_ts3                  = NA,
      custom_ref_ts3              = NA,
      custom_average_ts3          = NA,
      download_options_ts3        = NA,
      file_type_timeseries3       = NA,
      key_position_ts3            = NA,
      title_mode_ts3              = NA,
      title_size_input_ts3        = NA,
      title1_input_ts3            = NA,
      xaxis_numeric_interval_ts3  = NA,
      add_outliers_ref_ts3        = NA,
      add_trend_ref_ts3           = NA,
      show_key_ts3                = NA,
      show_key_ref_ts3            = NA,
      show_ticks_ts3              = NA,
      sd_input_ref_ts3            = NA,
      trend_sd_input_ref_ts3      = NA,
      
      # Map inputs
      axis_input3                 = input$axis_input3,
      axis_mode3                  = input$axis_mode3,
      center_lat3                 = input$center_lat3,
      center_lon3                 = input$center_lon3,
      custom_map3                 = input$custom_map3,
      custom_topo3                = input$custom_topo3,
      download_options3           = input$download_options3,
      file_type_map3              = input$file_type_map3,
      file_type_map_sec3          = input$file_type_map_sec3,
      hide_axis3                  = input$hide_axis3,
      hide_borders3               = input$hide_borders3,
      label_lakes3                = input$label_lakes3,
      label_mountains3            = input$label_mountains3,
      label_rivers3               = input$label_rivers3,
      projection3                 = input$projection3,
      ref_map_mode3               = input$ref_map_mode3,
      cor_method_map              = input$cor_method_map,
      cor_method_map_data         = input$cor_method_map_data,
      show_lakes3                 = input$show_lakes3,
      show_mountains3             = input$show_mountains3,
      show_rivers3                = input$show_rivers3,
      title_mode3                 = input$title_mode3,
      title_size_input3           = input$title_size_input3,
      title1_input3               = input$title1_input3,
      title2_input3               = input$title2_input3,
      white_land3                 = input$white_land3,
      white_ocean3                = input$white_ocean3,
      
      # Reactive values
      plotOrder       = character(0),
      availableLayers = character(0),
      lonlat_vals_v1 = lonlat_vals_v1(),
      lonlat_vals_v2 = lonlat_vals_v2()
    )
  })
  
  # Download Correlation Map Metadata
  output$download_metadata3 <- downloadHandler(
    filename = function() { "metadata_correlation.xlsx" },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "custom_meta")
      openxlsx::addWorksheet(wb, "custom_points")
      openxlsx::addWorksheet(wb, "custom_highlights")
      
      meta <- isolate(metadata_inputs_correlation())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      if (nrow(map_points_data3()) > 0) openxlsx::writeData(wb, "custom_points", map_points_data3())
      if (nrow(map_highlights_data3()) > 0) openxlsx::writeData(wb, "custom_highlights", map_highlights_data3())
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Upload Correlation Map Metadata
  observeEvent(input$update_metadata3, {
    req(input$upload_metadata3)
    
    file_path <- input$upload_metadata3$datapath
    file_name <- input$upload_metadata3$name
    
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata_correlation") {
      
      process_uploaded_metadata_correlation(
        file_path           = file_path,
        mode                = "map",
        metadata_sheet      = "custom_meta",
        df_ts_points        = NULL,
        df_ts_highlights    = NULL,
        df_ts_lines         = NULL,
        df_map_points       = "custom_points",
        df_map_highlights   = "custom_highlights",
        rv_plotOrder        = plotOrder3,
        rv_availableLayers  = availableLayers3,
        rv_lonlat_vals_v1   = lonlat_vals_v1,
        rv_lonlat_vals_v2   = lonlat_vals_v2,
        map_points_data     = map_points_data3,
        map_highlights_data = map_highlights_data3,
        ts_points_data      = NULL,
        ts_highlights_data  = NULL,
        ts_lines_data       = NULL
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata_correlation.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  ####### Generate Layer Options for customization ----
  
  # Reactive values
  plotOrder3 <- reactiveVal(character(0))        # full paths
  availableLayers3 <- reactiveVal(character(0))  # file names
  
  # Helper: extract and load shapefiles
  updatePlotOrder3 <- function(zipFile3, plotOrder3, availableLayers3) {
    temp_dir3 <- tempfile(pattern = "correlation_")
    dir.create(temp_dir3)
    unzip(zipFile3, exdir = temp_dir3)
    
    shpFiles3 <- list.files(temp_dir3, pattern = "\\.shp$", full.names = TRUE)
    layer_names3 <- tools::file_path_sans_ext(basename(shpFiles3))
    
    plotOrder3(shpFiles3)
    availableLayers3(layer_names3)
  }
  
  # Trigger update on file upload
  observeEvent(input$shpFile3, {
    req(input$shpFile3)
    updatePlotOrder3(
      zipFile3 = input$shpFile3$datapath,
      plotOrder3 = plotOrder3,
      availableLayers3 = availableLayers3
    )
  })
  
  # Shape File Renderer for correlation set
  output$shapefileSelector3 <- renderUI({
    req(availableLayers3())
    shinyjqui::sortableCheckboxGroupInput(
      inputId = "shapes3",
      label = "Select and order shapefiles for Correlation (drag & drop)",
      choices = availableLayers3()
    )
  })
  
  # Dynamic color pickers for selected correlation shapefiles
  output$colorpickers3 <- renderUI({
    req(input$shapes3, input$shapes3_order, plotOrder3())
    selected_ordered3 <- input$shapes3_order[input$shapes3_order %in% input$shapes3]
    shp_files3 <- plotOrder3()[match(selected_ordered3, tools::file_path_sans_ext(basename(plotOrder3())))]
    
    pickers3 <- lapply(shp_files3, function(file3) {
      file_name3 <- tools::file_path_sans_ext(basename(file3))
      input_id3 <- paste0("shp_colour3_", file_name3)
      last_val3 <- isolate(input[[input_id3]])
      colourpicker::colourInput(
        inputId = input_id3,
        label   = paste("Border Color for", file_name3),
        value   = if (!is.null(last_val3)) last_val3 else "black",
        showColour = "background",
        palette = "limited",
        allowTransparent = FALSE
      )
    })
    do.call(tagList, pickers3)
  })
  
  
  ####### Initialise and update timeseries dataframe ----
  
  # Add in initial data
  monthly_ts_data = reactiveVal(monthly_ts_starter_data())
  # Set tracker to 1 (if tracker is 1, the first line, aka the starter data, gets replaced)
  monthly_ts_tracker = reactiveVal(1)
  
  # Add new data and update related inputs
  observeEvent(input$add_monthly_ts, {
    
    # Generate data ID
    monthly_ts_data_ID = generate_data_ID(
      dataset = input$dataset_selected5,
      variable = input$variable_selected5,
      month_range = c(NA, NA)
    )
    
    # Update custom_data if required
    if (!identical(custom_data_id_primary()[2:3], monthly_ts_data_ID[2:3])) {
      # ....i.e. changed variable or dataset
      custom_data_primary(load_ModE_data(dataset = input$dataset_selected5, variable = input$variable_selected5)) # load new custom data
      custom_data_id_primary(monthly_ts_data_ID) # update custom data ID
    }
    
    # Replace starter data if tracker = 1
    if (monthly_ts_tracker() == 1) {
      monthly_ts_data(
        create_monthly_TS_data(
          data_input = custom_data_primary(),
          dataset = input$dataset_selected5,
          variable = input$variable_selected5,
          years = input$range_years5,
          lon_range = input$range_longitude5,
          lat_range = input$range_latitude5,
          mode = input$mode_selected5,
          type = input$type_selected5,
          baseline_range = input$ref_period5
        )
      )
      
      # Limit variable choice to only that already chosen:
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId  = "variable_selected5",
        choices  = monthly_ts_data()[1,3], # Sets choices to only the Variable already selected
        selected = monthly_ts_data()[1,3])
      
      # update tracker
      monthly_ts_tracker(monthly_ts_tracker()+1)
    }
    # Otherwise, add to dataframe
    else {
      new_rows = create_monthly_TS_data(
        data_input = custom_data_primary(),
        dataset = input$dataset_selected5,
        variable = input$variable_selected5,
        years = input$range_years5,
        lon_range = input$range_longitude5,
        lat_range = input$range_latitude5,
        mode = input$mode_selected5,
        type = input$type_selected5,
        baseline_range = input$ref_period5
        
      )
      
      updated_monthly_ts_data = rbind(monthly_ts_data(),new_rows)
      
      monthly_ts_data(updated_monthly_ts_data)
    }
    
  })  
  
  # Remove last ts
  observeEvent(input$remove_last_monthly_ts, {
    monthly_ts_data(monthly_ts_data()[-nrow(monthly_ts_data()),])
    
    # If dataframe is empty, allow all variable choices and replot starter data
    if (dim(monthly_ts_data())[1] == 0){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId  = "variable_selected5",
        choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
        selected = "Temperature")
      
      monthly_ts_data(monthly_ts_starter_data())
      
      # update tracker
      monthly_ts_tracker(1)
    } 
  })
  
  # Remove all ts
  observeEvent(input$remove_all_monthly_ts, {
    monthly_ts_data(data.frame())
    
    # allow all variable choices and replot starter data
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId  = "variable_selected5",
      choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
      selected = "Temperature")
    
    monthly_ts_data(monthly_ts_starter_data())  
    
    # update tracker
    monthly_ts_tracker(1)
    
  })
  
  
  ####### Input updaters ----
  
 


  ### SEA observe, update & interactive controls----
  ####### Input updaters ----
  
  # Update variable selection
  observe({
    req(user_data_6())
    
    if (input$source_sea_6 == "User data"){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "user_variable_6",
        choices = names(user_data_6())[-1])
    }
  })
  
  # Set iniital lon/lat values on startup
  lonlat_vals6 = reactiveVal(c(initial_lon_values,initial_lat_values))
  
  # Continent buttons - updates range inputs and lonlat_values
  observeEvent(input$button_global_6,{
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(-180,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(-90,90))
    
    lonlat_vals6(c(-180,180,-90,90))
  }) 
  
  observeEvent(input$button_europe_6, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(-30,40))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(30,75))
    
    lonlat_vals6(c(-30,40,30,75))
  })
  
  observeEvent(input$button_asia_6, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(25,170))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(5,80))
    
    lonlat_vals6(c(25,170,5,80))
  })
  
  observeEvent(input$button_oceania_6, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(90,180))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(-55,20))
    
    lonlat_vals6(c(90,180,-55,20))
  })
  
  observeEvent(input$button_africa_6, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(-25,55))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(-40,40))
    
    lonlat_vals6(c(-25,55,-40,40))
  })
  
  observeEvent(input$button_n_america_6, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(-175,-10))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(5,85))
    
    lonlat_vals6(c(-175,-10,5,85))
  })
  
  observeEvent(input$button_s_america_6, {
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_longitude_6",
      label = NULL,
      value = c(-90,-30))
    
    shinyWidgets::updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "range_latitude_6",
      label = NULL,
      value = c(-60,15))
    
    lonlat_vals6(c(-90,-30,-60,15))
  })
  
  observeEvent(input$button_coord_6, {
    lonlat_vals(c(input$range_longitude_6,input$range_latitude_6))        
  })
  
  #Make continental buttons stay highlighted
  observe({
    if (input$range_longitude_6[1] == -180 && input$range_longitude_6[2] == 180 &&
        input$range_latitude_6[1] == -90 && input$range_latitude_6[2] == 90) {
      shinyjs::addClass("button_global_6", "green-background")
    } else {
      shinyjs::removeClass("button_global_6", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_6[1] == -30 && input$range_longitude_6[2] == 40 &&
        input$range_latitude_6[1] == 30 && input$range_latitude_6[2] == 75) {
      shinyjs::addClass("button_europe_6", "green-background")
    } else {
      shinyjs::removeClass("button_europe_6", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_6[1] == 25 && input$range_longitude_6[2] == 170 &&
        input$range_latitude_6[1] == 5 && input$range_latitude_6[2] == 80) {
      shinyjs::addClass("button_asia_6", "green-background")
    } else {
      shinyjs::removeClass("button_asia_6", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_6[1] == 90 && input$range_longitude_6[2] == 180 &&
        input$range_latitude_6[1] == -55 && input$range_latitude_6[2] == 20) {
      shinyjs::addClass("button_oceania_6", "green-background")
    } else {
      shinyjs::removeClass("button_oceania_6", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_6[1] == -25 && input$range_longitude_6[2] == 55 &&
        input$range_latitude_6[1] == -40 && input$range_latitude_6[2] == 40) {
      shinyjs::addClass("button_africa_6", "green-background")
    } else {
      shinyjs::removeClass("button_africa_6", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_6[1] == -175 && input$range_longitude_6[2] == -10 &&
        input$range_latitude_6[1] == 5 && input$range_latitude_6[2] == 85) {
      shinyjs::addClass("button_n_america_6", "green-background")
    } else {
      shinyjs::removeClass("button_n_america_6", "green-background")
    }
  })
  
  observe({
    if (input$range_longitude_6[1] == -90 && input$range_longitude_6[2] == -30 &&
        input$range_latitude_6[1] == -60 && input$range_latitude_6[2] == 15) {
      shinyjs::addClass("button_s_america_6", "green-background")
    } else {
      shinyjs::removeClass("button_s_america_6", "green-background")
    }
  })
  
  #Month Range Updater
  observe({
    if (input$season_selected_6 == "Annual"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_6",
        label = NULL,
        selected = c("January", "December"))
    }
  })
  
  observe({
    if (input$season_selected_6 == "DJF"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_6",
        label = NULL,
        selected = c("December (prev.)", "February"))
    }
  })
  
  observe({
    if (input$season_selected_6 == "MAM"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_6",
        label = NULL,
        selected = c("March", "May"))
    }
  })
  
  observe({
    if (input$season_selected_6 == "JJA"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_6",
        label = NULL,
        selected = c("June", "August"))
    }
  })
  
  observe({
    if (input$season_selected_6 == "SON"){
      shinyWidgets::updateSliderTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "range_months_6",
        label = NULL,
        selected = c("September", "November"))
    }
  })
  
  # Y-axis updater for SEA plot
  observe({
    if (input$axis_mode_6 == "Automatic") {
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_6",
        value = c(NA, NA)
      )
    }
  })
  
  observe({
    if (input$axis_mode_6 == "Fixed" &&
        (is.null(input$axis_input_6) || any(is.na(input$axis_input_6)))) {
      
      data <- SEA_datatable()
      relevant_cols <- data[, !names(data) %in% c("LAG_YEAR", "P")]
      
      padded_range <- set_sea_axis_values(data_input = relevant_cols)
      
      shinyWidgets::updateNumericRangeInput(
        session = getDefaultReactiveDomain(),
        inputId = "axis_input_6",
        value = padded_range
      )
    }
  })
  
  ####### Generate Metadata for SEA ----
  
  # === SEA Metadata Reactive ===
  metadata_inputs_sea <- reactive({
    generate_metadata_sea(
      axis_input_6               = input$axis_input_6,
      axis_mode_6                = input$axis_mode_6,
      coordinates_type_6         = input$coordinates_type_6,
      custom_6                   = input$custom_6,
      dataset_selected_6         = input$dataset_selected_6,
      download_options_6         = input$download_options_6,
      enable_custom_statistics_6 = input$enable_custom_statistics_6,
      enter_upload_6             = input$enter_upload_6,
      event_years_6              = input$event_years_6,
      file_type_timeseries6      = input$file_type_timeseries6,
      lag_years_6                = input$lag_years_6,
      ME_statistic_6             = input$ME_statistic_6,
      ME_variable_6              = input$ME_variable_6,
      range_latitude_6           = input$range_latitude_6,
      range_longitude_6          = input$range_longitude_6,
      range_months_6             = input$range_months_6,
      ref_period_6               = input$ref_period_6,
      ref_period_sg_6            = input$ref_period_sg_6,
      ref_single_year_6          = input$ref_single_year_6,
      sample_size_6              = input$sample_size_6,
      season_selected_6          = input$season_selected_6,
      show_confidence_bands_6    = input$show_confidence_bands_6,
      show_key_6                 = input$show_key_6,
      show_means_6               = input$show_means_6,
      show_observations_6        = input$show_observations_6,
      show_pvalues_6             = input$show_pvalues_6,
      show_ticks_6               = input$show_ticks_6,
      source_sea_6               = input$source_sea_6,
      title_mode_6               = input$title_mode_6,
      title1_input_6             = input$title1_input_6,
      use_custom_post_6          = input$use_custom_post_6,
      use_custom_pre_6           = input$use_custom_pre_6,
      user_variable_6            = input$user_variable_6,
      y_label_6                  = input$y_label_6,
      
      # Reactive val
      lonlat_vals = lonlat_vals6()
    )
  })
  
  # === SEA Metadata Download ===
  output$download_metadata_6 <- downloadHandler(
    filename = function() {"metadata_sea.xlsx"},
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "custom_meta")
      
      meta <- isolate(metadata_inputs_sea())
      if (nrow(meta) > 0) openxlsx::writeData(wb, "custom_meta", meta)
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # === SEA Metadata Upload ===
  observeEvent(input$update_metadata_6, {
    req(input$upload_metadata_6)
    
    file_path <- input$upload_metadata_6$datapath
    file_name <- input$upload_metadata_6$name
    
    if (!is.null(file_name) && tools::file_path_sans_ext(file_name) == "metadata_sea") {
      
      process_uploaded_metadata_sea(
        file_path        = file_path,
        metadata_sheet   = "custom_meta",
        rv_lonlat_vals   = lonlat_vals6
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please upload the correct file: 'metadata_sea.xlsx'.",
        easyClose = TRUE,
        size = "s"
      ))
    }
  })
  
  # Processing and Plotting ----
  ### DATA PROCESSING ----  
  # NOTE that "primary" refers to anomalies, composites, variable 1 and dependent
  # variable while "secondary" refers to variable 2 and independent variable
  
  ####### Month range ----
  
  month_range_primary <- reactive({
    #Creating Numeric Vector for Month Range between 0 and 12
    if (input$nav1 == "tab1") {
      # Anomalies
      create_month_range(month_names_vector = input$range_months)
    } else if (input$nav1 == "tab2") {
      # Composites
      create_month_range(month_names_vector = input$range_months2)
    } else if (input$nav1 == "tab3") {
      # Correlation
      create_month_range(month_names_vector = input$range_months_v1)
    } else if (input$nav1 == "tab6") {
      # SEA
      create_month_range(month_names_vector = input$range_months_6)
    }
  })
  
  month_range_secondary <- reactive({
    #Creating Numeric Vector for Month Range between 0 and 12
    if (input$nav1 == "tab3") {
      # Correlation
      create_month_range(month_names_vector = input$range_months_v2)
    }
  })
  
  ####### Subset lons & Subset lats ----
  
  subset_lons_primary <- reactive({
    if (input$nav1 == "tab1") {   # Anomalies
      create_subset_lon_IDs(lon_range = lonlat_vals()[1:2])       
    } else if (input$nav1 == "tab2") {   # Composites
      create_subset_lon_IDs(lon_range = lonlat_vals2()[1:2])
    } else if (input$nav1 == "tab3") {   # Correlation
      create_subset_lon_IDs(lon_range = lonlat_vals_v1()[1:2])
    } else if (input$nav1 == "tab6") {   # SEA
      create_subset_lon_IDs(lon_range = lonlat_vals6()[1:2])
    }
  })
  
  subset_lons_secondary <- reactive({
    if (input$nav1 == "tab3"){   # Correlation
      create_subset_lon_IDs(lon_range = lonlat_vals_v2()[1:2])
    } 
  })
  
  subset_lats_primary <- reactive({
    if (input$nav1 == "tab1"){   # Anomalies
      create_subset_lat_IDs(lonlat_vals()[3:4])       
    } else if (input$nav1 == "tab2"){   # Composites
      create_subset_lat_IDs(lonlat_vals2()[3:4])
    } else if (input$nav1 == "tab3"){   # Correlation
      create_subset_lat_IDs(lonlat_vals_v1()[3:4])
    } else if (input$nav1 == "tab6"){   # SEA
      create_subset_lat_IDs(lonlat_vals6()[3:4])
    }
  })
  
  subset_lats_secondary <- reactive({
    if (input$nav1 == "tab3"){   # Correlation
      create_subset_lat_IDs(lonlat_vals_v2()[3:4])
    }
  })
  
  ####### Data ID ----
  # Generating data ID - c(pre-processed data?,dataset,variable,season)
  
  data_id_primary <- reactive({
    if (input$nav1 == "tab1") {
      # Anomalies
      generate_data_ID(
        dataset = input$dataset_selected,
        variable = input$variable_selected,
        month_range = month_range_primary()
      )
    } else if (input$nav1 == "tab2") {
      # Composites
      generate_data_ID(
        dataset = input$dataset_selected2,
        variable = input$variable_selected2,
        month_range = month_range_primary()
      )
    } else if (input$nav1 == "tab3") {
      # Correlation
      generate_data_ID(
        dataset = input$dataset_selected_v1,
        variable = input$ME_variable_v1,
        month_range = month_range_primary()
      )
    } else if (input$nav1 == "tab6") {
      # SEA
      generate_data_ID(
        dataset = input$dataset_selected_6,
        variable = input$ME_variable_6,
        month_range = month_range_primary()
      )
    }
  })
  
  data_id_secondary <- reactive({
    if (input$nav1 == "tab3") {
      # Correlation
      generate_data_ID(
        dataset = input$dataset_selected_v2,
        variable = input$ME_variable_v2,
        month_range = month_range_secondary()
      )
    }
  })
  
  
  ####### Update custom_data ----
  
  # Update preprocessed and custom_data_primary (if required)
  
  observeEvent(data_id_primary(),{
    if (data_id_primary()[1] == 0){ # Only updates when new custom data is required...
      if (!identical(custom_data_id_primary()[2:3],data_id_primary()[2:3])){ # ....i.e. changed variable or dataset
        
        new_dataset = switch(data_id_primary()[2],
                             "1" = "ModE-RA",
                             "2" = "ModE-Sim",
                             "3" = "ModE-RAclim")
        new_variable = switch(data_id_primary()[3],
                              "1" = "Temperature",
                              "2" = "Precipitation",
                              "3" = "SLP",
                              "4" = "Z500")
        
        custom_data_primary(load_ModE_data(dataset = new_dataset, variable = new_variable)) # load new custom data
        custom_data_id_primary(data_id_primary()) # update custom data ID
      }
    } 
    
    # Update preprocessed data
    else if (data_id_primary()[1] == 2){ # Only updates when new pp data is required...
      if (!identical(preprocessed_data_id_primary()[2:4],data_id_primary()[2:4])){ # ....i.e. changed variable, dataset or month range
        preprocessed_data_primary(load_preprocessed_data(data_ID = data_id_primary()))# load new pp data
        preprocessed_data_id_primary(data_id_primary()) # update pp data ID
      }
    }
  })
  
  # Update custom_data_secondary (if required)
  observeEvent(data_id_secondary(),{
    if (data_id_secondary()[1] == 0){ # Only updates when new custom data is required...
      if (!identical(custom_data_id_secondary()[2:3],data_id_secondary()[2:3])){ # ....i.e. changed variable or dataset
        
        new_dataset = switch(data_id_secondary()[2],
                             "1" = "ModE-RA",
                             "2" = "ModE-Sim",
                             "3" = "ModE-RAclim")
        new_variable = switch(data_id_secondary()[3],
                              "1" = "Temperature",
                              "2" = "Precipitation",
                              "3" = "SLP",
                              "4" = "Z500")
        
        custom_data_secondary(load_ModE_data(dataset = new_dataset, variable = new_variable)) # load new custom data
        custom_data_id_secondary(data_id_secondary()) # update custom data ID
      }
    }
    
    # Update preprocessed data
    else if (data_id_secondary()[1] == 2){ # Only updates when new pp data is required...
      if (!identical(preprocessed_data_id_secondary()[2:4],data_id_secondary()[2:4])){ # ....i.e. changed variable, dataset or month range
        preprocessed_data_secondary(load_preprocessed_data(data_ID = data_id_secondary()))# load new pp data
        preprocessed_data_id_secondary(data_id_secondary()) # update pp data ID
      }
    }
  })
  
  ####### Geographic Subset ----
  
  data_output1_primary <- reactive({
    req(data_id_primary(),
        subset_lons_primary(),
        subset_lats_primary())
    if (data_id_primary()[1] != 2) {
      # i.e. preloaded or custom data
      create_latlon_subset(
        data_input = custom_data_primary(),
        data_ID = data_id_primary(),
        subset_lon_IDs = subset_lons_primary(),
        subset_lat_IDs = subset_lats_primary()
      )
    } else {
      # i.e. preloaded data
      create_latlon_subset(
        data_input = preprocessed_data_primary(),
        data_ID = data_id_primary(),
        subset_lon_IDs = subset_lons_primary(),
        subset_lat_IDs = subset_lats_primary()
        
      )
    }
  })
  
  data_output1_secondary <- reactive({
    req(data_id_secondary(),
        subset_lons_secondary(),
        subset_lats_secondary())
    if (data_id_secondary()[1] != 2) {
      # i.e. preloaded or custom data
      create_latlon_subset(
        data_input = custom_data_secondary(),
        data_ID = data_id_secondary(),
        subset_lon_IDs = subset_lons_secondary(),
        subset_lat_IDs = subset_lats_secondary()
      )
    } else {
      # i.e. preloaded data
      create_latlon_subset(
        data_input = preprocessed_data_secondary(),
        data_ID = data_id_secondary(),
        subset_lon_IDs = subset_lons_secondary(),
        subset_lat_IDs = subset_lats_secondary()
      )
    }
  })
  
  ####### Yearly subset ----
  
  data_output2_primary <- reactive({
    if (input$nav1 == "tab1") {
      # Anomalies
      create_yearly_subset(
        data_input = data_output1_primary(),
        data_ID = data_id_primary(),
        year_range = input$range_years,
        month_range = month_range_primary()
      )
    } else if (input$nav1 == "tab2") {
      # Composites
      create_yearly_subset_composite(
        data_input = data_output1_primary(),
        data_ID = data_id_primary(),
        year_set = year_set_comp(),
        month_range = month_range_primary()
      )
    } else if (input$nav1 == "tab3") {
      # Correlation
      adjusted_years = input$range_years3 + input$lagyears_v1_cor
      create_yearly_subset(
        data_input = data_output1_primary(),
        data_ID = data_id_primary(),
        year_range = adjusted_years,
        month_range = month_range_primary()
      )
    } else if (input$nav1 == "tab6") {
      # SEA
      create_yearly_subset(
        data_input = data_output1_primary(),
        data_ID = data_id_primary(),
        year_range = c(1422, 2008),
        month_range = month_range_primary()
      )
    }
  })
  
  data_output2_secondary <- reactive({
    if (input$nav1 == "tab3") {
      # Correlation
      adjusted_years = input$range_years3 + input$lagyears_v2_cor
      create_yearly_subset(
        data_input = data_output1_secondary(),
        data_ID = data_id_secondary(),
        year_range = adjusted_years,
        month_range = month_range_secondary()
      )
    }
  })
  
  ####### Reference subset ----
  # Create reference yearly subset & convert to mean
  data_output3_primary <- reactive({
    if (input$nav1 == "tab1") {
      # Anomalies
      apply(
        create_yearly_subset(
          data_input = data_output1_primary(),
          data_ID = data_id_primary(),
          year_range = input$ref_period,
          month_range = month_range_primary()
        ),
        c(1:2),
        mean
      )
    } else if (input$nav1 == "tab2") {
      # Composites
      if (input$mode_selected2 == "Fixed reference") {
        apply(
          create_yearly_subset(
            data_input = data_output1_primary(),
            data_ID = data_id_primary(),
            year_range = input$ref_period2,
            month_range = month_range_primary()
          ),
          
          c(1:2),
          mean
        )
      } else if (input$mode_selected2 == "Custom reference") {
        apply(
          create_yearly_subset_composite(
            data_input = data_output1_primary(),
            data_ID = data_id_primary(),
            year_set = year_set_comp_ref(),
            month_range = month_range_primary()
          ),
          c(1:2),
          mean
        )
      } else {
        NA
      }
    } else if (input$nav1 == "tab3") {
      # Correlation
      apply(
        create_yearly_subset(
          data_input = data_output1_primary(),
          data_ID = data_id_primary(),
          year_range = input$ref_period_v1,
          month_range = month_range_primary()
        ),
        c(1:2),
        mean
      )
    } else if (input$nav1 == "tab6") {
      # SEA
      apply(
        create_yearly_subset(
          data_input = data_output1_primary(),
          data_ID = data_id_primary(),
          year_range = input$ref_period_6,
          month_range = month_range_primary()
        ),
        c(1:2),
        mean
      )
    }
  })
  
  data_output3_secondary <- reactive({
    if (input$nav1 == "tab3") {
      # Correlation
      apply(
        create_yearly_subset(
          data_input = data_output1_secondary(),
          data_ID = data_id_secondary(),
          year_range = input$ref_period_v2,
          month_range = month_range_secondary()
        ),
        c(1:2),
        mean
      )
    }
  })    
  
  
  #Converting absolutes to anomalies
  data_output4 <- reactive({
    processed_data4 <- convert_subset_to_anomalies(data_input = data_output2_primary(), ref_data = data_output3_primary())
    
    return(processed_data4)
  })
  
  ####### Convert to anomalies ----
  
  data_output4_primary <- reactive({
    if (input$nav1 == "tab1") {
      # Anomalies
      convert_subset_to_anomalies(data_input = data_output2_primary(), ref_data = data_output3_primary())
    } else if (input$nav1 == "tab2") {
      # Composites
      if (input$mode_selected2 == "X years prior") {
        convert_composite_to_anomalies(
          data_input = data_output2_primary(),
          ref_data = data_output1_primary(),
          data_ID = data_id_primary(),
          year_set = year_set_comp(),
          month_range = month_range_primary(),
          baseline_year_before = input$prior_years2
        )
      } else {
        convert_subset_to_anomalies(data_input = data_output2_primary(), ref_data = data_output3_primary())
      }
    } else if (input$nav1 == "tab3") {
      # Correlation
      if (input$mode_selected_v1 == "Absolute") {
        data_output2_primary()
      } else {
        convert_subset_to_anomalies(data_input = data_output2_primary(), ref_data = data_output3_primary())
      }
    } else if (input$nav1 == "tab6") {
      # SEA
      convert_subset_to_anomalies(data_input = data_output2_primary(), ref_data = data_output3_primary())
    }
  })
  
  data_output4_secondary <- reactive({
    if (input$nav1 == "tab3"){   # Correlation
      if (input$mode_selected_v2 == "Absolute"){
        data_output2_secondary()
      } else {
        convert_subset_to_anomalies(data_input = data_output2_secondary(), ref_data = data_output3_secondary())
      }
    }
  })
  
  ### ANOMALIES Load SD ratio data, plotting & downloads ----  
  ####### SD ratio data ----
  
  # Update SD ratio data when required
  observe({
    if (input$ref_map_mode == "SD ratio") {
      if (input$nav1 == "tab1"){ # check current tab
        if (!identical(SDratio_data_id()[3:4], data_id_primary()[3:4])) { 
          # check to see if currently loaded variable & month range are the same
          if (data_id_primary()[1] != 0) { # check for preprocessed SD ratio data
            new_data_id <- data_id_primary()
            new_data_id[2] <- 4 # change data ID to SD ratio
            
            SDratio_data(load_preprocessed_data(data_ID = new_data_id)) # load new SD data
            SDratio_data_id(data_id_primary()) # update custom data ID
          } else {
            SDratio_data(load_ModE_data(dataset = "SD ratio", variable = input$variable_selected)) # load new SD data
            SDratio_data_id(data_id_primary()) # update custom data ID
          }
        } 
      }
    }
  })
  
  SDratio_subset <- reactive({
    req(input$nav1 == "tab1")             
    req(input$ref_map_mode == "SD ratio") 
    
    create_sdratio_data(
      data_input      = SDratio_data(),
      data_ID         = data_id_primary(),
      tab             = "general",  # oder "reference", falls du dafür einen eigenen Modus hast
      variable        = input$variable_selected,
      subset_lon_IDs  = subset_lons_primary(),
      subset_lat_IDs  = subset_lats_primary(),
      month_range     = month_range_primary(),
      year_range      = input$range_years
    )
  })
  
  
  
  
  ####### Plotting ----
  # Map customization (statistics and map titles)
  
  plot_titles <- reactive({
    req(input$nav1 == "tab1")
    req(input$range_years)
    
    # Validate year range
    if (length(input$range_years) < 2 ||
        any(is.na(input$range_years)) ||
        input$range_years[1] > input$range_years[2]) {
      return(NULL)
    }
    
    tryCatch({
      generate_titles(
        tab = "general",
        dataset = input$dataset_selected,
        variable = input$variable_selected,
        mode = "Anomaly",
        map_title_mode = input$title_mode,
        ts_title_mode = input$title_mode_ts,
        month_range = month_range_primary(),
        year_range = input$range_years,
        baseline_range = input$ref_period,
        baseline_years_before = NA,
        lon_range = lonlat_vals()[1:2],
        lat_range = lonlat_vals()[3:4],
        map_custom_title1 = input$title1_input,
        map_custom_title2 = input$title2_input,
        ts_custom_title1 = input$title1_input_ts,
        map_title_size = input$title_size_input,
        ts_title_size = input$title_size_input_ts,
        ts_data = timeseries_data()
      )
    }, error = function(e) {
      message("plot_titles() failed: ", e$message)
      return(NULL)
    })
  })
  
  
  # Add value to custom title
  # Clear inputs when switching to "Default"
  observeEvent(input$title_mode, {
    if (input$title_mode == "Default" ||
        input$title_mode_ts == "Default") {
      updateTextInput(session, "title1_input", value = "")
      updateTextInput(session, "title2_input", value = "")
      updateTextInput(session, "title1_input_ts", value = "")
    }
  })
  # Refill with updated defaults after clearing
  observeEvent({
    input$title_mode
    plot_titles()
  }, {
    req(input$title_mode == "Default" ||
          input$title_mode_ts == "Default")
    req(plot_titles())
    isolate({
      updateTextInput(session, "title1_input", value = plot_titles()$map_title)
      updateTextInput(session, "title2_input", value = plot_titles()$map_subtitle)
      updateTextInput(session, "title1_input_ts", value = plot_titles()$ts_title)
    })
  })
  
  #Plotting the Data (Maps)
  map_data <- function() {
    create_map_datatable(
      data_input = data_output4_primary(),
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary()
    )
  }
  
  final_map_data <- reactive({
    # req(input$value_type_map_data)  # Ensure input is available
    
    option <- input$value_type_map_data
    
    if (option == "Anomalies") {
      map_data()
    } else if (option == "Absolute") {
      create_map_datatable(data_input = data_output2_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (option == "Reference") {
      create_map_datatable(data_input = data_output3_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (option == "SD ratio") {
      req(SDratio_subset())
      create_map_datatable(data_input = SDratio_subset(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    }
  })
  
  output$data1 <- renderTable({final_map_data()}, rownames = TRUE)
  
  
  
  #Plotting the Map
  map_dimensions <- reactive({
    req(input$nav1 == "tab1") # Only run code if in the current tab
    req(!is.null(session$clientData$output_map_width))
    
    m_d = generate_map_dimensions(
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary(),
      output_width = session$clientData$output_map_width,
      output_height = input$dimension[2],
      hide_axis = input$hide_axis
    )
    return(m_d)
  })


  map_plot <- function() {
    validate(
      need(
        !is.null(input$range_years) &&
          length(input$range_years) == 2 &&
          !any(is.na(input$range_years)) &&
          input$range_years[1] >= 1422 &&
          input$range_years[2] <= 2008 &&
          input$range_years[1] <= input$range_years[2],
        "Please select a valid year range between 1422 and 2008."
      )
    )
    
    md <- map_data()
    ll <- lonlat_vals()
    gtf <- .create_geotiff_mem(md, ll)
    
    plot_map(
      data_input           = gtf,
      lon_lat_range        = ll,
      variable             = input$variable_selected,
      mode                 = "Anomaly",
      titles               = plot_titles(),
      axis_range           = input$axis_input,
      hide_axis            = input$hide_axis,
      
      points_data          = map_points_data(),
      highlights_data      = map_highlights_data(),

      c_borders            = input$hide_borders,
      white_ocean          = input$white_ocean,
      white_land           = input$white_land,
      
      plotOrder            = plotOrder(),
      shpOrder             = input$shapes_order[input$shapes_order %in% input$shapes],
      input                = input,
      plotType             = "shp_colour_",
      
      projection           = input$projection,
      center_lat           = input$center_lat,
      center_lon           = input$center_lon,
      
      show_rivers          = input$show_rivers,
      label_rivers         = input$label_rivers,
      show_lakes           = input$show_lakes,
      label_lakes          = input$label_lakes,
      show_mountains       = input$show_mountains,
      label_mountains      = input$label_mountains
    )
  }
 
  
   ###### Cached Plot 
  output$map <- renderCachedPlot({
    
    start_time <- Sys.time()
    
    req(map_dimensions()[1], map_dimensions()[2])
    
    p <- map_plot()
    
    end_time <- Sys.time()
    
    message(
      "Map generated in ",
      round(end_time - start_time, 3),
      " seconds"
    )
    
    p
    
  },
  
  # Cache key expression
  cacheKeyExpr = {
    points_key     <- tryCatch(overlay_key(map_points_data()),     error = function(e) "")
    highlights_key <- tryCatch(overlay_key(map_highlights_data()), error = function(e) "")

    shpfile_key <- tryCatch({
      f <- input$shpFile
      if (is.null(f)) "no-upload" else paste(
        paste(f$name, collapse = "|"),
        paste(f$size, collapse = "|"),
        paste(unname(tools::md5sum(f$datapath)), collapse = "|"),
        sep = "::"
      )
    }, error = function(e) "no-upload")
    
    shp_ids_key  <- tryCatch({
      ids <- input$shapes_order[input$shapes_order %in% input$shapes]
      ids <- ids[!duplicated(ids)]
      paste(ids, collapse = "|")
    }, error = function(e) "")
    
    shp_style_key <- tryCatch({
      ids <- input$shapes_order[input$shapes_order %in% input$shapes]
      ids <- ids[!duplicated(ids)]
      prefix <- "shp_colour_" 
      paste(vapply(ids, function(id) {
        val <- input[[paste0(prefix, id)]]
        col <- if (is.null(val) || is.na(val) || !nzchar(val)) "#000000" else as.character(val)[1]
        paste0(id, "=", col)
      }, character(1L)), collapse = "|")
    }, error = function(e) "")
    
    plotorder_key <- tryCatch(digest::digest(plotOrder()), error = function(e) "")
    shp_color_key <- tryCatch(digest::digest(shp_color_inputs()), error = function(e) "")
    
    months_key <- tryCatch({
      if (identical(input$season_selected, "Custom")) {
        paste(input$range_months %||% character(0), collapse = "->")
      } else {
        input$season_selected %||% "Annual"
      }
    }, error = function(e) "no-months")
    

    titles_key <- tryCatch({
      digest::digest(plot_titles())
    }, error = function(e) "")
    
    
    list(
      input$nav1,
      input$value_type_map_data,
      input$dataset_selected,
      input$variable_selected,
      input$range_years,
      input$ref_period,
      lonlat_vals(),
      subset_lons_primary(),
      subset_lats_primary(),
      input$axis_input,
      input$hide_axis,
      points_key,
      highlights_key,
      shpfile_key,
      plotorder_key,
      shp_ids_key,
      shp_style_key,
      shp_color_key,
      input$hide_borders,
      input$white_ocean,
      input$white_land,
      input$projection,
      input$center_lat,
      input$center_lon,
      input$show_rivers,
      input$label_rivers,
      input$show_lakes,
      input$label_lakes,
      input$show_mountains,
      input$label_mountains,
      input$shapes_order[input$shapes_order %in% input$shapes],
      input$title_mode,
      input$title_mode_ts,
      titles_key,
      input$title1_input_ts,
      input$title_size_input,
      input$title_size_input_ts,
      months_key
    )
  },
  width  = function() { map_dimensions()[1] },
  height = function() { map_dimensions()[2] })
  
  # Disable Grey land and Grey ocean for the Orthographic and LAEA projections
  allowed_projs <- c("UTM (default)", "Robinson")
  
  observeEvent(input$projection, {
    proj <- input$projection
    
    if (!is.null(proj) && proj %in% allowed_projs) {
      shinyjs::enable("white_ocean")
      shinyjs::enable("white_land")
    } else {
      
      try(updateCheckboxInput(session, "white_ocean", value = FALSE), silent = TRUE)
      try(updateCheckboxInput(session, "white_land", value = FALSE), silent = TRUE)
      
      shinyjs::disable("white_ocean")
      shinyjs::disable("white_land")
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  
  
  #Ref/Absolute/SD ratio Map
  ref_map_data <- function() {
    if (input$ref_map_mode == "Absolute Values") {
      create_map_datatable(data_input = data_output2_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (input$ref_map_mode == "Reference Values") {
      create_map_datatable(data_input = data_output3_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (input$ref_map_mode == "SD ratio") {
      req(SDratio_subset())
      create_map_datatable(data_input = SDratio_subset(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    }
  }    
  
  ref_map_titles = reactive({
    req(input$nav1 == "tab1") # Only run code if in the current tab
    
    active_tab <- ifelse(input$ref_map_mode == "SD ratio", "sdratio", "general")
    years_or_ref <- if (input$ref_map_mode == "Reference Values") {
      input$ref_period
    } else {
      input$range_years
    }
    
    rm_title <- generate_titles(
      tab = active_tab,
      dataset = input$dataset_selected,
      variable = input$variable_selected,
      mode = "Absolute",
      map_title_mode = input$title_mode,
      ts_title_mode = input$title_mode_ts,
      month_range = month_range_primary(),
      year_range = years_or_ref,
      lon_range = lonlat_vals()[1:2],
      lat_range = lonlat_vals()[3:4],
      map_custom_title1 = input$title1_input,
      map_custom_title2 = input$title2_input,
      ts_custom_title1 = input$title1_input_ts,
      map_title_size = input$title_size_input
    )
  })  
  
  ref_map_plot <- function(){
    if (input$ref_map_mode == "Absolute Values" | input$ref_map_mode == "Reference Values" ){
      v=input$variable_selected; m="Absolute"; axis_range=NULL
      
    } else if(input$ref_map_mode == "SD ratio"){
      v=NULL; m="SD ratio"; axis_range=c(0,1)
    }
    plot_map(
      data_input = create_geotiff(map_data = ref_map_data()),
      lon_lat_range = lonlat_vals(),
      variable = v,
      
      mode = m,
      
      titles = ref_map_titles(),
      axis_range = axis_range,
      
      c_borders = input$hide_borders,
      white_ocean = input$white_ocean,
      white_land = input$white_land,
      
      plotOrder = plotOrder(),
      shpOrder = input$shapes_order[input$shapes_order %in% input$shapes],
      input = input,
      plotType = "shp_colour_",
      
      projection = input$projection,
      center_lat = input$center_lat,
      center_lon = input$center_lon,
      
      show_rivers = input$show_rivers,
      label_rivers = input$label_rivers,
      show_lakes = input$show_lakes,
      label_lakes = input$label_lakes,
      show_mountains = input$show_mountains,
      label_mountains = input$label_mountains
    )
  }
  
  output$ref_map <- renderPlot({
    if (input$ref_map_mode == "None") {
      ref_map_plot_data <- NULL
    } else {
      ref_map_plot_data <- ref_map_plot()
    }
    ref_map_plot_data
  }, 
  width = function() {
    if (input$ref_map_mode == "None") {
      20
    } else {
      map_dimensions()[1]
    }
  }, 
  height = function() {
    if (input$ref_map_mode == "None") {
      10
    } else {
      map_dimensions()[2]
    }
  })
  
  
  # Plotting the data (timeseries)
  timeseries_data <- reactive({
    req(input$nav1 == "tab1") # Only run code if in the current tab
    
    # Plot normal timeseries if year range is > 1 year
    if (input$range_years[1] != input$range_years[2]) {
      ts_data <- create_timeseries_datatable(
        data_input      = data_output4_primary(),
        year_input      = input$range_years,
        year_input_type = "range",
        subset_lon_IDs  = subset_lons_primary(),
        subset_lat_IDs  = subset_lats_primary()
      )
      
    } else {
      # Plot monthly TS if year range = 1 year
      ts_raw <- load_ModE_data(
        dataset  = input$dataset_selected,
        variable = input$variable_selected
      )
      
      ts_data <- create_monthly_TS_data(
        data_input     = ts_raw,
        dataset        = input$dataset_selected,
        variable       = input$variable_selected,
        years          = input$range_years[1],
        lon_range      = input$range_longitude,
        lat_range      = input$range_latitude,
        mode           = "Anomaly",
        type           = "Individual years",
        baseline_range = input$ref_period
      )
    }
    
    ts_data
  })
  
  
  timeseries_data_output = reactive({
    req(input$nav1 == "tab1") # Only run code if in the current tab
    if (input$range_years[1] != input$range_years[2]) {
      output_ts_table = rewrite_tstable(tstable = timeseries_data(),
                                        variable = input$variable_selected)
    } else {
      output_ts_table = timeseries_data()
    }
    return(output_ts_table)
  })
  
  output$data2 <- DT::renderDataTable({timeseries_data_output()}, rownames = FALSE, options = list(
    autoWidth = TRUE, 
    searching = FALSE,
    paging = TRUE,
    pagingType = "numbers"
  ))
  
  timeseries_plot_anom<- function(){
    
    #Plot normal timeseries if year range is > 1 year
    if (input$range_years[1] != input$range_years[2]){
      # Generate NA or reference mean
      ref_ts = signif(mean(data_output3_primary()),3)
    } else {
      ref_ts = NA
    }
    
    # New 
    p <- plot_timeseries(
      type = "Anomaly",
      data = timeseries_data(),
      variable = input$variable_selected,
      ref = ref_ts,
      year_range = input$range_years,
      month_range_1 = month_range_primary(),
      titles = plot_titles(),
      show_key = input$show_key_ts,
      key_position = input$key_position_ts,
      show_ref = input$show_ref_ts,
      show_ticks = input$show_ticks_ts,
      tick_interval = input$xaxis_numeric_interval_ts,
      moving_ave = input$custom_average_ts,
      moving_ave_year = input$year_moving_ts,
      custom_percentile = input$custom_percentile_ts,
      percentiles = input$percentile_ts,
      highlights = ts_highlights_data(),
      lines = ts_lines_data(),
      points = ts_points_data(),
      axis_range = input$axis_input_ts
    )
    
    return(p)
  }
  
  output$timeseries <- renderPlot({timeseries_plot_anom()}, height = 400)
  
  ####### ModE-RA sources ----
  
  # Set up values and functions for plotting
  fad_zoom  <- reactiveVal(c(-180,180,-90,90)) # These are the min/max lon/lat for the zoomed plot
  
  season_fad_short = reactive({
    switch(input$fad_season,
           "April to September" = "summer",
           "October to March" = "winter")
  })
  
  # Load global data
  fad_global_data = reactive({
    load_modera_source_data(year = input$fad_year, season = season_fad_short())
  })
  
  # Plot map 
  fad_plot = function(base_size = 18) {
    plot_modera_sources(
      ME_source_data = fad_global_data(),
      year = input$fad_year,
      season = season_fad_short(),
      minmax_lonlat = fad_zoom(),
      base_size = base_size
    )
  }
  
  
  fad_dimensions <- reactive({
    req(input$nav1 == "tab1") # Only run code if in the current tab
    m_d_f = generate_map_dimensions(
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary(),
      output_width = session$clientData$output_fad_map_width,
      output_height = input$dimension[2],
      hide_axis = FALSE
    )
    return(m_d_f)
  })
  
  output$fad_map <- renderPlot({
    fad_plot()
  }, width = function() {
    fad_dimensions()[1]
  }, height = function() {
    fad_dimensions()[2]
  })
  
  # Set up data function
  fad_data <- function() {
    fad_base_data = download_feedback_data(
      global_data = fad_global_data(),
      lon_range = fad_zoom()[1:2],
      lat_range = fad_zoom()[3:4]
    )
    
    # Remove the last column
    fad_base_data = fad_base_data[, -ncol(fad_base_data)]
    
    return(fad_base_data)
  }
  
  observeEvent(lonlat_vals()|input$fad_reset_zoom,{
    fad_zoom(lonlat_vals())
  })
  
  observeEvent(input$brush_fad,{
    brush = input$brush_fad
    req(brush)  # ensure brush is not NULL
    fad_zoom(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
  })
  
  # Update fad_year 
  observeEvent(input$range_years[1], {
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "fad_year",
      value = input$range_years[1])
  })
  
  # Update fad_season
  observeEvent(month_range_primary()[1], {
    
    req(input$nav1 == "tab1") # Only run code if in the current tab
    
    if (month_range_primary()[1] >3 & month_range_primary()[1] <10){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "fad_season",
        selected = "April to September")
    } else {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "fad_season",
        selected = "October to March")
    }
  })
  
  
  ####### Downloads ----
  #Downloading General data
  output$download_map       <- downloadHandler(
    filename = function() {
      paste(plot_titles()$file_title,
            "-map.",
            input$file_type_map,
            sep = "")
    },
    content = function(file) {
      if (input$file_type_map == "png") {
        png(
          file,
          width = map_dimensions()[3],
          height = map_dimensions()[4],
          res = 200,
          bg = "transparent"
        )
      } else if (input$file_type_map == "jpeg") {
        jpeg(
          file,
          width = map_dimensions()[3],
          height = map_dimensions()[4],
          res = 200,
          bg = "white"
        )
      } else {
        pdf(
          file,
          width = map_dimensions()[3] / 200,
          height = map_dimensions()[4] / 200,
          bg = "transparent"
        )
      }
      print(map_plot())  # Use print to ensure the plot is fully rendered
      dev.off()
    }
  )
  
  output$download_map_sec   <- downloadHandler(filename = function() {paste(plot_titles()$file_title, "-sec_map.", input$file_type_map_sec, sep = "")},
                                               content = function(file) {
                                                 if (input$file_type_map_sec == "png") {
                                                   png(file, width = map_dimensions()[3], height = map_dimensions()[4], res = 200, bg = "transparent")
                                                 } else if (input$file_type_map_sec == "jpeg") {
                                                   jpeg(file, width = map_dimensions()[3], height = map_dimensions()[4], res = 200, bg = "white")
                                                 } else {
                                                   pdf(file, width = map_dimensions()[3] / 200, height = map_dimensions()[4] / 200, bg = "transparent")
                                                 }
                                                 print(ref_map_plot())
                                                 dev.off()}
  )
  
  output$download_timeseries      <- downloadHandler(filename = function(){paste(plot_titles()$file_title,"-ts.",input$file_type_timeseries, sep = "")},
                                                     content  = function(file) {
                                                       if (input$file_type_timeseries == "png"){
                                                         png(file, width = 3000, height = 1285, res = 200, bg = "transparent") 
                                                         
                                                       } else if (input$file_type_timeseries == "jpeg"){
                                                         jpeg(file, width = 3000, height = 1285, res = 200, bg = "white") 
                                                         
                                                       } else {
                                                         pdf(file, width = 14, height = 6, bg = "transparent") 
                                                       }
                                                       print(timeseries_plot_anom())
                                                       dev.off()
                                                     }) 
  
  output$download_map_data <- downloadHandler(
    filename = function() {
      paste(plot_titles()$file_title,
            "-mapdata.",
            input$file_type_map_data,
            sep = "")
    },
    content = function(file) {
      if (input$file_type_map_data == "csv") {
        map_data_new <- rewrite_maptable(
          maptable = final_map_data(),
          subset_lon_IDs = subset_lons_primary(),
          subset_lat_IDs = subset_lats_primary()
        )
        colnames(map_data_new) <- NULL
        
        write.csv(map_data_new, file, row.names = FALSE)
      } else if (input$file_type_map_data == "xlsx") {
        openxlsx::write.xlsx(
          rewrite_maptable(
            maptable = final_map_data(),
            subset_lon_IDs = subset_lons_primary(),
            subset_lat_IDs = subset_lats_primary()
          ),
          file,
          row.names = FALSE,
          col.names = FALSE
        )
      } else if (input$file_type_map_data == "GeoTIFF") {
        create_geotiff(map_data = final_map_data(), output_file = file)
      }
    }
  )
  
  output$download_timeseries_data  <- downloadHandler(filename = function(){paste(plot_titles()$file_title, "-tsdata.",input$file_type_timeseries_data, sep = "")},
                                                      content  = function(file) {
                                                        if (input$file_type_timeseries_data == "csv"){
                                                          write.csv(timeseries_data_output(), file,
                                                                    row.names = FALSE,
                                                                    fileEncoding = "latin1")
                                                        } else {
                                                          openxlsx::write.xlsx(timeseries_data_output(), file,
                                                                               row.names = FALSE,
                                                                               col.names = TRUE)
                                                        }})
  
  
  output$download_fad <- downloadHandler(
    filename = function()
    {paste("Assimilated Observations_", gsub(" ", "", input$fad_season), "_", input$fad_year, ".", input$file_type_fad, sep = "")},
    
    content = function(file) {
      mmd = generate_map_dimensions(
        subset_lon_IDs = subset_lons_primary(),
        subset_lat_IDs = subset_lats_primary(),
        output_width = session$clientData$output_fad_map_width,
        output_height = input$dimension[2],
        hide_axis = FALSE
      )
      if (input$file_type_fad == "png") {
        png(file, width = mmd[3], height = mmd[4], res = 400, bg = "transparent")
        print(fad_plot(base_size = 9))
        dev.off()
      } else if (input$file_type_fad == "jpeg") {
        jpeg(file, width = mmd[3], height = mmd[4], res = 400,bg = "white")
        print(fad_plot(base_size = 9))
        dev.off()
      } else {
        pdf(file, width = mmd[3] / 400, height = mmd[4] / 400, bg = "transparent")
        print(fad_plot(base_size = 9))
        dev.off()
      }
    }
  )
  
  
  output$download_fad_data       <- downloadHandler(filename = function(){paste("Assimilated Observations_",gsub(" ", "", input$fad_season),"_",input$fad_year,"_data.",input$data_file_type_fad, sep = "")},
                                                    content  = function(file) {
                                                      if (input$data_file_type_fad == "csv"){
                                                        write.csv(fad_data(), file,
                                                                  row.names = FALSE)
                                                      } else {
                                                        openxlsx::write.xlsx(fad_data(), file,
                                                                             col.names = TRUE,
                                                                             row.names = FALSE)
                                                      }})
  
  output$download_netcdf <- downloadHandler(
    filename = function() {
      paste(plot_titles()$netcdf_title, ".nc", sep = "")
    },
    content  = function(file) {
      netcdf_ID = sample(1:1000000, 1)
      generate_custom_netcdf (
        data_input = data_output4_primary(),
        tab = "general",
        dataset = input$dataset_selected,
        ncdf_ID = netcdf_ID,
        variable = input$variable_selected,
        user_nc_variables = input$netcdf_variables,
        mode = "Anomaly",
        subset_lon_IDs = subset_lons_primary(),
        subset_lat_IDs = subset_lats_primary(),
        month_range = month_range_primary(),
        year_range = input$range_years,
        baseline_range = input$ref_period,
        baseline_years_before = NA
      )
      file.copy(paste("user_ncdf/netcdf_", netcdf_ID, ".nc", sep = ""), file)
      file.remove(paste("user_ncdf/netcdf_", netcdf_ID, ".nc", sep = ""))
    }
  )
  
  ### COMPOSITE Year range,load SD ratio data, plotting & downloads ---- 
  
  ####### Year Range ----
  
  #Creating a year set for composite
  year_set_comp <- reactive({
    read_composite_data(
      data_input_manual = input$range_years2,
      data_input_filepath = input$upload_file2$datapath,
      year_input_mode = input$enter_upload2
    )
  })
  
  #List of custom anomaly years (from read Composite) as reference data
  year_set_comp_ref <- reactive({
    read_composite_data(
      data_input_manual = input$range_years2a,
      data_input_filepath = input$upload_file2a$datapath,
      year_input_mode = input$enter_upload2a
    )
  })
  
  ####### SD Ratio data ----
  
  ####### SD Ratio data ----
  
  # Update SD ratio data when required (Tab 2)
  observe({
    if ((input$ref_map_mode2 == "SD ratio") ||
        (input$value_type_map_data2 == "SD ratio")) {
      
      if (input$nav1 == "tab2") { # check current tab
        
        # check if currently loaded variable & month range are the same
        if (!identical(SDratio_data_id()[3:4], data_id_primary()[3:4])) {
          
          if (data_id_primary()[1] != 0) { # preprocessed SD ratio data vorhanden
            new_data_id <- data_id_primary()
            new_data_id[2] <- 4 # change data ID to SD ratio
            
            SDratio_data(load_preprocessed_data(data_ID = new_data_id)) # load new SD data
            SDratio_data_id(data_id_primary()) # update custom data ID
            
          } else {
            SDratio_data(load_ModE_data(dataset = "SD ratio",
                                        variable = input$variable_selected2)) # load new SD data
            SDratio_data_id(data_id_primary()) # update custom data ID
          }
        }
      }
    }
  })
  
  # Processed SD data for Tab 2
  SDratio_subset_2 <- reactive({
    req(input$nav1 == "tab2") # Only run code if in the current tab
    
    req((input$ref_map_mode2 == "SD ratio") ||
          (input$value_type_map_data2 == "SD ratio"))
    
    create_sdratio_data(
      data_input     = SDratio_data(),
      data_ID        = data_id_primary(),
      tab            = "composites",
      variable       = input$variable_selected2,
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary(),
      month_range    = month_range_primary(),
      year_range     = year_set_comp()
    )
  })
  
  
  ####### Plotting ----
  
  # Map customization (composites)
  
  plot_titles_composites <- reactive({
    req(input$nav1 == "tab2")
    req(input$ref_period2)
    
    # Validate year range
    if (length(input$ref_period2) < 2 || any(is.na(input$ref_period2)) || input$ref_period2[1] > input$ref_period2[2]) {
      return(NULL)
    }
    
    tryCatch({
      generate_titles(
        tab = "composites",
        dataset = input$dataset_selected2,
        variable = input$variable_selected2,
        mode = input$mode_selected2,
        map_title_mode = input$title_mode2,
        ts_title_mode = input$title_mode_ts2,
        month_range = month_range_primary(),
        year_range = input$range_years2,
        baseline_range = input$ref_period2,
        baseline_years_before = input$prior_years2,
        lon_range = lonlat_vals2()[1:2],
        lat_range = lonlat_vals2()[3:4],
        map_custom_title1 = input$title1_input2,
        map_custom_title2 = input$title2_input2,
        ts_custom_title1 = input$title1_input_ts2,
        map_title_size = input$title_size_input2,
        ts_title_size = input$title_size_input_ts2,
        ts_data = timeseries_data_2()
      )
    }, error = function(e) {
      message("plot_titles_composites() failed: ", e$message)
      return(NULL)
    })
  })
  
  # Add value to custom title (composites)
  # Clear inputs when switching to "Default"
  observeEvent(input$title_mode2, {
    if (input$title_mode2 == "Default" ||
        input$title_mode_ts2 == "Default") {
      updateTextInput(session, "title1_input2", value = "")
      updateTextInput(session, "title2_input2", value = "")
      updateTextInput(session, "title1_input_ts2", value = "")
    }
  })
  # Refill with updated defaults after clearing
  observeEvent({
    input$title_mode2
    plot_titles_composites()
  }, {
    req(input$title_mode2 == "Default" ||
          input$title_mode_ts2 == "Default")
    req(plot_titles_composites())
    isolate({
      updateTextInput(session, "title1_input2", value = plot_titles_composites()$map_title)
      updateTextInput(session, "title2_input2", value = plot_titles_composites()$map_subtitle)
      updateTextInput(session, "title1_input_ts2", value = plot_titles_composites()$ts_title)
    })
  })

  
  #Plotting the Data (Maps)
  map_data_2 <- function() {
    create_map_datatable(data_input = data_output4_primary(),
                         subset_lon_IDs = subset_lons_primary(),
                         subset_lat_IDs = subset_lats_primary())
  }
  
  final_map_data_2 <- reactive({
    req(input$value_type_map_data2)  # Ensure input is available
    
    option <- input$value_type_map_data2
    
    if (option == "Anomalies") {
      map_data()
    } else if (option == "Absolute") {
      create_map_datatable(data_input = data_output2_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (option == "Reference") {
      create_map_datatable(data_input = data_output3_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (option == "SD ratio") {
      req(SDratio_subset_2())
      create_map_datatable(data_input = SDratio_subset_2(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    }
  })
  
  output$data3 <- renderTable({final_map_data_2()},
                              rownames = TRUE)
  
  #Plotting the Map
  map_dimensions_2 <- reactive({
    
    req(input$nav1 == "tab2") # Only run code if in the current tab
    
    m_d_2 = generate_map_dimensions(
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary(),
      output_width = session$clientData$output_map2_width,
      output_height = input$dimension[2] * 0.85,
      hide_axis = input$hide_axis2
    )
    return(m_d_2)
  })
  
  map_plot_2 <- function() {
    validate(need(
      !is.null(input$ref_period2) &&
        length(input$ref_period2) == 2 &&
        !any(is.na(input$ref_period2)) &&
        input$ref_period2[1] >= 1422 &&
        input$ref_period2[2] <= 2008 &&
        input$ref_period2[1] <= input$ref_period2[2],
      "Please select a valid year range between 1422 and 2008."
    ))
    
    md  <- map_data_2()
    ll  <- lonlat_vals2()
    gtf <- .create_geotiff_mem(md, ll)
    
    plot_map(
      data_input           = gtf,
      lon_lat_range        = ll,
      variable             = input$variable_selected2,
      mode                 = input$mode_selected2,
      titles               = plot_titles_composites(),
      axis_range           = input$axis_input2,
      hide_axis            = input$hide_axis2,
      
      points_data          = map_points_data2(),
      highlights_data      = map_highlights_data2(),

      c_borders            = input$hide_borders2,
      white_ocean          = input$white_ocean2,
      white_land           = input$white_land2,
      
      plotOrder            = plotOrder2(),
      shpOrder             = input$shapes2_order[input$shapes2_order %in% input$shapes2],
      input                = input,
      plotType             = "shp_colour2_",
      
      projection           = input$projection2,
      center_lat           = input$center_lat2,
      center_lon           = input$center_lon2,
      
      show_rivers          = input$show_rivers2,
      label_rivers         = input$label_rivers2,
      show_lakes           = input$show_lakes2,
      label_lakes          = input$label_lakes2,
      show_mountains       = input$show_mountains2,
      label_mountains      = input$label_mountains2
    )
  }
  
  ###### Cached Plot
  output$map2 <- renderCachedPlot({
    req(map_dimensions_2()[1], map_dimensions_2()[2])
    map_plot_2()
  },
  cacheKeyExpr = {
    points_key2     <- tryCatch(overlay_key(map_points_data2()),     error = function(e) "")
    highlights_key2 <- tryCatch(overlay_key(map_highlights_data2()), error = function(e) "")
    
    shpfile_key2 <- tryCatch({
      f <- input$shpFile2
      if (is.null(f)) "no-upload" else paste(
        paste(f$name, collapse = "|"),
        paste(f$size, collapse = "|"),
        paste(unname(tools::md5sum(f$datapath)), collapse = "|"),
        sep = "::"
      )
    }, error = function(e) "no-upload")
    
    shp_ids_key2 <- tryCatch({
      ids <- input$shapes2_order[input$shapes2_order %in% input$shapes2]
      ids <- ids[!duplicated(ids)]
      paste(ids, collapse = "|")
    }, error = function(e) "")
    
    shp_style_key2 <- tryCatch({
      ids <- input$shapes2_order[input$shapes2_order %in% input$shapes2]
      ids <- ids[!duplicated(ids)]
      prefix <- "shp_colour2_"
      paste(vapply(ids, function(id) {
        val <- input[[paste0(prefix, id)]]
        col <- if (is.null(val) || is.na(val) || !nzchar(val)) "#000000" else as.character(val)[1]
        paste0(id, "=", col)
      }, character(1L)), collapse = "|")
    }, error = function(e) "")
    
    plotorder_key2 <- tryCatch(digest::digest(plotOrder2()), error = function(e) "")
    
    months_key2 <- tryCatch({
      if (identical(input$season_selected2, "Custom")) {
        paste(input$range_months2 %||% character(0), collapse = "->")
      } else {
        input$season_selected2 %||% "Annual"
      }
    }, error = function(e) "no-months")
    
    dim_key2 <- paste0(map_dimensions_2()[1], "x", map_dimensions_2()[2])
    
    list(
      input$nav1,
      input$dataset_selected2,
      input$variable_selected2,
      input$mode_selected2,
      input$range_years2,
      input$ref_period2,
      lonlat_vals2(),
      subset_lons_primary(),
      subset_lats_primary(),
      input$axis_input2,
      input$hide_axis2,
      points_key2,
      highlights_key2,
      shpfile_key2,
      plotorder_key2,
      shp_ids_key2,
      shp_style_key2,
      dim_key2,
      input$hide_borders2,
      input$white_ocean2,
      input$white_land2,
      input$projection2,
      input$center_lat2,
      input$center_lon2,
      input$show_rivers2,
      input$label_rivers2,
      input$show_lakes2,
      input$label_lakes2,
      input$show_mountains2,
      input$label_mountains2,
      plotOrder2(),
      input$shapes2_order[input$shapes2_order %in% input$shapes2],
      input$title_mode2,
      input$title_mode_ts2,
      input$title1_input2,
      input$title2_input2,
      input$title1_input_ts2,
      input$title_size_input2,
      input$title_size_input_ts2,
      months_key2,
      input$upload_file2,
      input$enter_upload2,
      input$enter_upload2a,
      input$upload_file2a
    )
  },
  width  = function() map_dimensions_2()[1],
  height = function() map_dimensions_2()[2]
  )
  
  
  # Disable Grey land and Grey ocean for the Orthographic and LAEA projections
  allowed_projs <- c("UTM (default)", "Robinson")
  
  observeEvent(input$projection2, {
    proj <- input$projection2
    
    if (!is.null(proj) && proj %in% allowed_projs) {
      shinyjs::enable("white_ocean2")
      shinyjs::enable("white_land2")
    } else {
      
      try(updateCheckboxInput(session, "white_ocean2", value = FALSE), silent = TRUE)
      try(updateCheckboxInput(session, "white_land2", value = FALSE), silent = TRUE)
      
      shinyjs::disable("white_ocean2")
      shinyjs::disable("white_land2")
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  
  
  # code line below sets height as a function of the ratio of lat/lon 
  
  
  #Ref/Absolute Map
  ref_map_data_2 <- function() {
    if (input$ref_map_mode2 == "Absolute Values") {
      create_map_datatable(data_input = data_output2_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (input$ref_map_mode2 == "Reference Values" &&
               input$mode_selected2 == "Fixed reference") {
      create_map_datatable(data_input = data_output3_primary(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    } else if (input$ref_map_mode2 == "SD ratio") {
      create_map_datatable(data_input = SDratio_subset_2(),
                           subset_lon_IDs = subset_lons_primary(),
                           subset_lat_IDs = subset_lats_primary())
    }
  }    
  
  ref_map_titles_2 = reactive({
    
    req(input$nav1 == "tab2") # Only run code if in the current tab
    
    # Define mode-specific parameters
    mode_params <- list(
      "Absolute Values" = list(type = "composites", years = year_set_comp()),
      "Reference Values" = list(type = "reference", years = input$ref_period2),
      "SD ratio" = list(type = "sdratio", years = c(NA, NA))
    )
    params <- mode_params[[input$ref_map_mode2]]
    
    rm_title2 <- generate_titles(
      tab = params$type,
      dataset = input$dataset_selected2,
      variable = input$variable_selected2,
      mode = "Absolute", 
      map_title_mode = input$title_mode2,
      ts_title_mode = input$title_mode_ts2, 
      month_range = month_range_primary(),
      year_range = params$years, 
      lon_range = lonlat_vals2()[1:2],
      lat_range = lonlat_vals2()[3:4], 
      map_custom_title1 = input$title1_input2,
      map_custom_title2 = input$title2_input2,
      ts_custom_title1 = input$title1_input_ts2, 
      map_title_size = input$title_size_input2
    )
  })  
  
  ref_map_plot_2 <- function(){
    if (input$ref_map_mode2 == "Absolute Values" | input$ref_map_mode2 == "Reference Values" ){
      v=input$variable_selected2; m="Absolute"; axis_range=NULL
      
    } else if (input$ref_map_mode2 == "SD ratio"){
      v=NULL; m="SD ratio"; axis_range=c(0,1)
    }
    plot_map(data_input = create_geotiff(map_data = ref_map_data_2()),
             lon_lat_range = lonlat_vals2(),
             variable = v,
             mode = m,
             titles = ref_map_titles_2(),
             axis_range,
             
             c_borders = input$hide_borders2,
             white_ocean = input$white_ocean2,
             white_land = input$white_land2,
             
             plotOrder = plotOrder2(),
             shpOrder = input$shapes2_order[input$shapes2_order %in% input$shapes2],
             input = input,
             plotType = "shp_colour2_", 
             
             projection = input$projection2,
             center_lat = input$center_lat2,
             center_lon = input$center_lon2,
             
             show_rivers = input$show_rivers2,
             label_rivers = input$label_rivers2,
             show_lakes = input$show_lakes2,
             label_lakes = input$label_lakes2,
             show_mountains = input$show_mountains2,
             label_mountains = input$label_mountains2)
  }
  
  output$ref_map2 <- renderPlot({
    if (input$ref_map_mode2 == "None") {
      ref_map_plot_data2 <- NULL
    } else {
      ref_map_plot_data2 <- ref_map_plot_2()
    }
    ref_map_plot_data2
  }, 
  width = function() {
    if (input$ref_map_mode2 == "None") {
      20
    } else {
      map_dimensions_2()[1]
    }
  }, 
  height = function() {
    if (input$ref_map_mode2 == "None") {
      10
    } else {
      map_dimensions_2()[2]
    }
  })
  
  
  #Plotting the data (timeseries)
  timeseries_data_2 <- reactive({
    
    req(input$nav1 == "tab2") # Only run code if in the current tab
    
    # Plot normal timeseries if year set is > 1 year
    if (length(year_set_comp()) > 1) {
      ts_data <- create_timeseries_datatable(
        data_input        = data_output4_primary(),
        year_input        = year_set_comp(),
        year_input_type   = "set",
        subset_lon_IDs    = subset_lons_primary(),
        subset_lat_IDs    = subset_lats_primary()
      )
      
    } else {
      # Plot monthly TS if year range = 1 year
      ts_raw <- load_ModE_data(
        dataset  = input$dataset_selected2,
        variable = input$variable_selected2
      )
      
      # Generate ref years
      if (input$mode_selected2 == "Fixed reference") {
        ref_years <- input$ref_period2
      } else if (input$mode_selected2 == "X years prior") {
        ref_years <- c(
          (year_set_comp() - input$prior_years2),
          year_set_comp() - 1
        )
      } else {
        ref_years <- year_set_comp_ref()
      }
      
      ts_data <- create_monthly_TS_data(
        data_input     = ts_raw,
        dataset        = input$dataset_selected2,
        variable       = input$variable_selected2,
        years          = year_set_comp(),
        lon_range      = input$range_longitude2,
        lat_range      = input$range_latitude2,
        mode           = "Anomaly",
        type           = "Individual years",
        baseline_range = ref_years
      )
    }
    
    ts_data
  })
  
  
  timeseries_data_output_2 = reactive({
    req(input$nav1 == "tab2") # Only run code if in the current tab
    
    if (length(year_set_comp()) > 1) {
      output_ts_table = rewrite_tstable(tstable = timeseries_data_2(),
                                        variable = input$variable_selected2)
    } else {
      output_ts_table = timeseries_data_2()
    }
    return(output_ts_table)
  })
  
  output$data4 <- DT::renderDataTable({timeseries_data_output_2()}, rownames = FALSE, options = list(
    autoWidth = TRUE, 
    searching = FALSE,
    paging = TRUE,
    pagingType = "numbers"
  ))
  
  #Plotting the timeseries
  timeseries_plot_comp <- function(){
    #Plot normal timeseries if year set is > 1 year
    #if (length(year_set_comp()) > 1){  
    # Generate NA or reference mean
    if(input$show_ref_ts2 == TRUE){
      ref_ts2 = signif(mean(data_output3_primary()),3)
    } else {
      ref_ts2 = NA
    }
    
    # New 
    p <- plot_timeseries(
      type = "Composites",
      data = timeseries_data_2(),
      variable = input$variable_selected2,
      ref = ref_ts2,
      year_range = year_set_comp(),
      month_range_1 = month_range_primary(),
      titles = plot_titles_composites(),
      #titles_mode=input$title_mode_ts2,
      show_key = input$show_key_ts2,
      key_position = input$key_position_ts2,
      show_ticks = input$show_ticks_ts2,
      tick_interval = input$xaxis_numeric_interval_ts2,
      show_ref = input$show_ref_ts2,
      custom_percentile = input$custom_percentile_ts2,
      percentiles = input$percentile_ts2,
      highlights = ts_highlights_data2(),
      lines = ts_lines_data2(),
      points = ts_points_data2(),
      axis_range = input$axis_input_ts2
    )
    
    return(p)
  }
  
  output$timeseries2 <- renderPlot({timeseries_plot_comp()}, height = 400)
  
  #List of chosen composite years (upload or manual) to plot
  output$text_years2 <- renderText("Chosen composite years:")
  output$years2 <- renderText({year_set_comp()})
  output$text_years2b <- renderText("Chosen composite years:")
  output$years2b <- renderText({year_set_comp()})
  
  output$text_custom_years2  <- renderText("Chosen reference years:")
  output$custom_years2       <- renderText({year_set_comp_ref()})
  output$text_custom_years2b <- renderText("Chosen reference years:")
  output$custom_years2b      <- renderText({year_set_comp_ref()})
  
  ####### ModE-RA sources ----
  
  # Set up values and functions for plotting
  fad_zoom2  <- reactiveVal(c(-180,180,-90,90)) # These are the min/max lon/lat for the zoomed plot
  
  season_fad_short2 = reactive({
    switch(input$fad_season2,
           "April to September" = "summer",
           "October to March" = "winter")
  })
  
  # Load global data
  fad_global_data2 = reactive({
    load_modera_source_data(year = input$fad_year2, season = season_fad_short2())
  })
  
  # Plot map 
  fad_plot2 = function(base_size = 18) {
    plot_modera_sources(
      ME_source_data = fad_global_data2(),
      year = input$fad_year2,
      season = season_fad_short2(),
      minmax_lonlat = fad_zoom2(),
      base_size = base_size
    )
  }
  
  fad_dimensions2 <- reactive({
    req(input$nav1 == "tab2") # Only run code if in the current tab
    m_d_f2 = generate_map_dimensions(
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary(),
      output_width = session$clientData$output_fad_map2_width,
      output_height = input$dimension[2],
      hide_axis = FALSE
    )
    return(m_d_f2)
  })
  
  output$fad_map2 <- renderPlot({
    fad_plot2()
  }, width = function() {
    fad_dimensions2()[1]
  }, height = function() {
    fad_dimensions2()[2]
  })
  
  # Set up data function
  fad_data2 <- function() {
    fad_base_data2 = download_feedback_data(
      global_data = fad_global_data2(),
      lon_range = fad_zoom2()[1:2],
      lat_range = fad_zoom2()[3:4]
    )
    
    # Remove the last column
    fad_base_data2 = fad_base_data2[, -ncol(fad_base_data2)]
    
    return(fad_base_data2)
  }
  
  observeEvent(lonlat_vals2()|input$fad_reset_zoom2,{
    fad_zoom2(lonlat_vals2())
  })
  
  observeEvent(input$brush_fad2,{
    brush = input$brush_fad2
    req(brush)  # ensure brush is not NULL
    fad_zoom2(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
  })
  
  # Update fad_year 
  observeEvent(year_set_comp()[1], {
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "fad_year2",
      value = year_set_comp()[1])
  })
  
  # Update fad_season
  observeEvent(month_range_primary()[1], {
    
    req(input$nav1 == "tab2") # Only run code if in the current tab
    
    if (month_range_primary()[1] >3 & month_range_primary()[1] <10){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "fad_season2",
        selected = "April to September")
    } else {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "fad_season2",
        selected = "October to March")
    }
  })
  
  ####### Downloads ----
  #Downloading General data
  output$download_map2            <- downloadHandler(filename = function() {paste(plot_titles_composites()$file_title, "-map.", input$file_type_map2, sep = "")},
                                                     content = function(file) {
                                                       if (input$file_type_map2 == "png") {
                                                         png(file, width = map_dimensions_2()[3], height = map_dimensions_2()[4], res = 200, bg = "transparent")
                                                       } else if (input$file_type_map2 == "jpeg") {
                                                         jpeg(file, width = map_dimensions_2()[3], height = map_dimensions_2()[4], res = 200, bg = "white")
                                                       } else {
                                                         pdf(file, width = map_dimensions_2()[3] / 200, height = map_dimensions_2()[4] / 200, bg = "transparent")
                                                       }
                                                       print(map_plot_2())
                                                       dev.off()}
  )
  
  output$download_map_sec2        <- downloadHandler(filename = function() {paste(plot_titles_composites()$file_title, "-sec_map.", input$file_type_map_sec2, sep = "")},
                                                     content = function(file) {
                                                       if (input$file_type_map_sec2 == "png") {
                                                         png(file, width = map_dimensions_2()[3], height = map_dimensions_2()[4], res = 200, bg = "transparent")
                                                       } else if (input$file_type_map_sec2 == "jpeg") {
                                                         jpeg(file, width = map_dimensions_2()[3], height = map_dimensions_2()[4], res = 200, bg = "white")
                                                       } else {
                                                         pdf(file, width = map_dimensions_2()[3] / 200, height = map_dimensions_2()[4] / 200, bg = "transparent")
                                                       }
                                                       print(ref_map_plot_2())
                                                       dev.off()}
  )
  
  output$download_timeseries2      <- downloadHandler(
    filename = function() {
      paste(plot_titles_composites()$file_title,
            "-ts.",
            input$file_type_timeseries2,
            sep = "")
    },
    content  = function(file) {
      if (input$file_type_timeseries2 == "png") {
        png(
          file,
          width = 3000,
          height = 1285,
          res = 200,
          bg = "transparent"
        )
      } else if (input$file_type_timeseries2 == "jpeg") {
        jpeg(
          file,
          width = 3000,
          height = 1285,
          res = 200,
          bg = "white"
        )
      } else {
        pdf(file,
            width = 14,
            height = 6,
            bg = "transparent")
      }
      print(timeseries_plot_comp())
      dev.off()
    }
  ) 
  
  output$download_map_data2 <- downloadHandler(
    filename = function() {
      paste(
        plot_titles_composites()$file_title,
        "-mapdata.",
        input$file_type_map_data2,
        sep = ""
      )
    },
    content  = function(file) {
      if (input$file_type_map_data2 == "csv") {
        map_data_new_2 <- rewrite_maptable(
          maptable = final_map_data_2(),
          subset_lon_IDs = subset_lons_primary(),
          subset_lat_IDs = subset_lats_primary()
        )
        colnames(map_data_new_2) <- NULL
        
        write.csv(map_data_new_2, file, row.names = FALSE)
      } else if (input$file_type_map_data2 == "xlsx") {
        openxlsx::write.xlsx(
          rewrite_maptable(
            maptable = final_map_data_2(),
            subset_lon_IDs = subset_lons_primary(),
            subset_lat_IDs = subset_lats_primary()
          ),
          file,
          row.names = FALSE,
          col.names = FALSE
        )
      } else if (input$file_type_map_data2 == "GeoTIFF") {
        create_geotiff(map_data = final_map_data_2(), output_file = file)
      }
    }
  )
  
  output$download_timeseries_data2  <- downloadHandler(filename = function(){paste(plot_titles_composites()$file_title, "-tsdata.",input$file_type_timeseries_data2, sep = "")},
                                                       content  = function(file) {
                                                         if (input$file_type_timeseries_data2 == "csv"){
                                                           write.csv(timeseries_data_output_2(), file,
                                                                     row.names = FALSE,
                                                                     fileEncoding = "latin1")
                                                         } else {
                                                           openxlsx::write.xlsx(timeseries_data_output_2(), file,
                                                                                row.names = FALSE,
                                                                                col.names = TRUE)
                                                         }})
  
  output$download_fad2 <- downloadHandler(
    filename = function()
    {paste("Assimilated Observations_", gsub(" ", "", input$fad_season2), "_", input$fad_year2, ".", input$file_type_fad2, sep = "")},
    
    content = function(file) {
      mmd = generate_map_dimensions(
        subset_lon_IDs = subset_lons_primary(),
        subset_lat_IDs = subset_lats_primary(),
        output_width = session$clientData$output_fad_map2_width,
        output_height = input$dimension[2],
        hide_axis = FALSE
      )
      if (input$file_type_fad2 == "png") {
        png(file, width = mmd[3], height = mmd[4], res = 400, bg = "transparent")
        print(fad_plot2(base_size = 9))
        dev.off()
      } else if (input$file_type_fad2 == "jpeg") {
        jpeg(file, width = mmd[3], height = mmd[4], res = 400,bg = "white")
        print(fad_plot2(base_size = 9))
        dev.off()
      } else {
        pdf(file, width = mmd[3] / 400, height = mmd[4] / 400, bg = "transparent")
        print(fad_plot2(base_size = 9))
        dev.off()
      }
    }
  )
  
  output$download_fad_data2       <- downloadHandler(filename = function(){paste("Assimilated Observations_",gsub(" ", "", input$fad_season2),"_",input$fad_year2,"_data.",input$data_file_type_fad2, sep = "")},
                                                     content  = function(file) {
                                                       if (input$data_file_type_fad2 == "csv"){
                                                         write.csv(fad_data2(), file,
                                                                   row.names = FALSE)
                                                       } else {
                                                         openxlsx::write.xlsx(fad_data2(), file,
                                                                              col.names = TRUE,
                                                                              row.names = FALSE)
                                                       }})
  
  ### CORRELATION shared lonlat/year range, user data, plotting & downloads ----
  
  ####### Shared lonlat/year_range ----
  
  # Find shared lonlat
  
  lonlat_vals3 = reactive({
    extract_shared_lonlat(
      variable1_type = input$type_v1,
      variable2_type = input$type_v2,
      variable1_lon_range = input$range_longitude_v1,
      variable1_lat_range = input$range_latitude_v1,
      variable2_lon_range = input$range_longitude_v2,
      variable2_lat_range = input$range_latitude_v2
    )
  })
  
  # Extract shared year range
  
  year_range_cor = reactive({
    result <- tryCatch({
      year_range <- extract_year_range(
        variable1_source = input$source_v1,
        variable2_source = input$source_v2,
        variable1_data_filepath = input$user_file_v1$datapath,
        variable2_data_filepath = input$user_file_v2$datapath,
        variable1_name = input$user_variable_v1,
        variable2_name = input$user_variable_v2,
        variable1_lag = input$lagyears_v1_cor,
        variable2_lag = input$lagyears_v2_cor
      )
      
      return(year_range)
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "There was an error in processing your uploaded data.\nPlease check if the file has the correct format.",
          easyClose = FALSE,
          footer = tagList(modalButton("OK"))
        )
      )
      return(NULL)
    })
    return(result)
  })
  
  
  
  ####### User data processing ----
  
  # Load in user data for variable 1
  user_data_v1 = reactive({
    
    req(input$user_file_v1)
    
    if (input$source_v1 == "User data"){
      new_data1 = read_regcomp_data(data_input_filepath = input$user_file_v1$datapath)
      
      return(new_data1)
    }
    else{
      return(NULL)
    }
  })
  
  # Load in user data for variable 2
  user_data_v2 = reactive({
    
    req(input$user_file_v2)
    
    if (input$source_v2 == "User data"){
      new_data2 = read_regcomp_data(data_input_filepath = input$user_file_v2$datapath)  
      return(new_data2)
    }
    else{
      return(NULL)
    }
  })
  
  # Subset v1 data to year_range and chosen variable
  user_subset_v1 = reactive({
    req(user_data_v1(), input$user_variable_v1)
    
    usr_ss1 = create_user_data_subset(
      data_input = user_data_v1(),
      variable = input$user_variable_v1,
      year_range = input$range_years3,
      lag = input$lagyears_v1_cor # pass the lag
    )
    
    return(usr_ss1)
  })
  
  # Subset v2 data to year_range and chosen variable
  user_subset_v2 = reactive({
    
    req(user_data_v2(),input$user_variable_v2)
    
    usr_ss2 = create_user_data_subset(
      data_input = user_data_v2(),
      variable = input$user_variable_v2,
      year_range = input$range_years3,
      lag = input$lagyears_v2_cor # pass the lag
    )
    
    return(usr_ss2)
  })
  
  ####### Generate plot data ---- 
  
  # for variable 1:
  #Map titles
  plot_titles_v1 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    my_title_v1 <- generate_titles(
      tab = "general",
      dataset = input$dataset_selected_v1,
      variable = input$ME_variable_v1,
      mode = input$mode_selected_v1,
      map_title_mode = "Default",
      ts_title_mode = "Default",
      month_range = month_range_primary(),
      year_range = input$range_years3,
      baseline_range = input$ref_period_v1,
      lon_range = lonlat_vals_v1()[1:2],
      lat_range = lonlat_vals_v1()[3:4]
    )
    return(my_title_v1)
  }) 
  
  # Generate Map data & plotting function
  map_data_v1 <- function() {
    create_map_datatable(data_input = data_output4_primary(),
                         subset_lon_IDs = subset_lons_primary(),
                         subset_lat_IDs = subset_lats_primary())
  }
  
  ME_map_plot_v1 <- function() {
    plot_map(
      data_input = create_geotiff(map_data = map_data_v1()),
      lon_lat_range = lonlat_vals_v1(),
      variable = input$ME_variable_v1,
      mode = input$mode_selected_v1,
      titles = plot_titles_v1(),
      shpOrder = NULL,
      plotOrder = NULL,
      input = NULL,
      plotType = "default"
    )
  }
  
  # Generate timeseries data & plotting function
  timeseries_data_v1 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    ts_data1_v1 <- create_timeseries_datatable(
      data_input = data_output4_primary(),
      year_input = input$range_years3,
      year_input_type = "range",
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary()
    )
    return(ts_data1_v1)
  })
  
  timeseries_plot_v1 = function() {
    p <- plot_timeseries(
      type = "Anomaly",
      data = timeseries_data_v1(),
      variable = input$ME_variable_v1,
      ref = NULL,
      year_range = input$range_years3,
      month_range_1 = month_range_primary(),
      titles = plot_titles_v1(),
      show_key = FALSE,
      show_ref = FALSE,
      moving_ave = FALSE,
      custom_percentile = FALSE,
      highlights = data.frame(),
      lines = data.frame(),
      points = data.frame()
    )
    
    return(p)
  }
  
  # for Variable 2:
  
  #Map titles
  plot_titles_v2 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    my_title_v2 <- generate_titles(
      tab = "general",
      dataset = input$dataset_selected_v2,
      variable = input$ME_variable_v2,
      mode = input$mode_selected_v2,
      map_title_mode = "Default",
      ts_title_mode = "Default",
      month_range = month_range_secondary(),
      year_range = input$range_years3,
      baseline_range = input$ref_period_v2,
      lon_range = lonlat_vals_v2()[1:2],
      lat_range = lonlat_vals_v2()[3:4]
    )
    return(my_title_v2)
  }) 
  
  # Generate Map data & plotting function
  map_data_v2 <- function() {
    req(data_output4_secondary(),
        subset_lons_secondary(),
        subset_lats_secondary())
    create_map_datatable(data_input = data_output4_secondary(),
                         subset_lon_IDs = subset_lons_secondary(),
                         subset_lat_IDs = subset_lats_secondary())
  }
  
  map_data_v2_tiff = reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    create_geotiff(map_data = map_data_v2())
  })
  
  ME_map_plot_v2 <- function() {
    plot_map(
      data_input = map_data_v2_tiff(),
      lon_lat_range = lonlat_vals_v2(),
      variable = input$ME_variable_v2,
      mode = input$mode_selected_v2,
      titles = plot_titles_v2(),
      shpOrder = NULL,
      plotOrder = NULL,
      input = NULL,
      plotType = "default"
    )
  }
  
  # Generate timeseries data & plotting function
  timeseries_data_v2 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    ts_data1_v2 <- create_timeseries_datatable(
      data_input = data_output4_secondary(),
      year_input = input$range_years3,
      year_input_type = "range",
      subset_lon_IDs = subset_lons_secondary(),
      subset_lat_IDs = subset_lats_secondary()
    )
    return(ts_data1_v2)
  })
  
  timeseries_plot_v2 = function(){
    
    p <- plot_timeseries(
      type = "Anomaly",
      data = timeseries_data_v2(),
      variable = input$ME_variable_v2,
      ref = NULL,
      year_range = input$range_years3,
      month_range_2 = month_range_secondary(),
      titles = plot_titles_v2(),
      show_key = FALSE,
      show_ref = FALSE,
      moving_ave = FALSE,
      custom_percentile = FALSE,
      highlights = data.frame(),
      lines = data.frame(),
      points = data.frame()
    )
    return(p)
  }
  
  ####### Plotting ----
  
  ######### Plot v1/v2 plots
  
  # Generate plot dimensions
  plot_dimensions_v1 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    if (input$type_v1 == "Timeseries") {
      map_dims_v1 = c(session$clientData$output_plot_v1_width, 400)
    } else {
      map_dims_v1 = generate_map_dimensions(
        subset_lon_IDs = subset_lons_primary(),
        subset_lat_IDs = subset_lats_primary(),
        output_width = session$clientData$output_plot_v1_width,
        output_height = (input$dimension[2]),
        hide_axis = FALSE
      )
    }
    return(map_dims_v1)
  })
  
  plot_dimensions_v2 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    if (input$type_v2 == "Timeseries") {
      map_dims_v2 = c(session$clientData$output_plot_v2_width, 400)
    } else {
      map_dims_v2 = generate_map_dimensions(
        subset_lons_secondary(),
        subset_lats_secondary(),
        session$clientData$output_plot_v2_width,
        (input$dimension[2]),
        FALSE
      )
    }
    return(map_dims_v2)
  })     
  
  # Plot
  output$plot_v1 <- renderPlot({
    if (input$source_v1 == "User data") {
      plot_user_timeseries(data_input = user_subset_v1(), color = "darkorange2")
    } else if (input$type_v1 == "Timeseries") {
      timeseries_plot_v1()
    } else{
      ME_map_plot_v1()
    }
  }, width = function() {
    plot_dimensions_v1()[1]
  }, height = function() {
    plot_dimensions_v1()[2]
  })  
  
  
  output$plot_v2 <- renderPlot({
    if (input$source_v2 == "User data") {
      plot_user_timeseries(data_input = user_subset_v2(), color = "saddlebrown")
    } else if (input$type_v2 == "Timeseries") {
      timeseries_plot_v2()
    } else{
      ME_map_plot_v2()
    }
  }, width = function() {
    plot_dimensions_v2()[1]
  }, height = function() {
    plot_dimensions_v2()[2]
  })  
  
  
  ######### Plot shared TS plot
  
  # Generate correlation titles
  plot_titles_cor = reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (input$source_v1 == "ModE-") {
      variable_v1 = input$ME_variable_v1
    } else {
      variable_v1 = input$user_variable_v1
    }
    
    if (input$source_v2 == "ModE-") {
      variable_v2 = input$ME_variable_v2
    } else {
      variable_v2 = input$user_variable_v2
    }
    
    ptc = generate_correlation_titles(
      variable1_source = input$source_v1,
      variable2_source = input$source_v2,
      variable1_dataset = input$dataset_selected_v1,
      variable2_dataset = input$dataset_selected_v2,
      variable1 = variable_v1,
      variable2 = variable_v2,
      variable1_type = input$type_v1,
      variable2_type = input$type_v2,
      variable1_mode = input$mode_selected_v1,
      variable2_mode = input$mode_selected_v2,
      variable1_month_range = month_range_primary(),
      variable2_month_range = month_range_secondary(),
      variable1_lon_range = lonlat_vals_v1()[1:2],
      variable2_lon_range = lonlat_vals_v2()[1:2],
      variable1_lat_range = lonlat_vals_v1()[3:4],
      variable2_lat_range = lonlat_vals_v2()[3:4],
      year_range = input$range_years3,
      method = input$cor_method_ts,
      map_title_mode = input$title_mode3,
      ts_title_mode = input$title_mode_ts3,
      map_custom_title = input$title1_input3,
      map_custom_subtitle = input$title2_input3,
      ts_custom_title = input$title1_input_ts3,
      map_title_size = input$title_size_input3,
      ts_title_size = input$title_size_input_ts3
    )
    return(ptc)
  }) 
  
  # Add value to custom title
  # Clear inputs when switching to "Default"
  observeEvent(input$title_mode3, {
    if (input$title_mode3 == "Default" ||
        input$title_mode_ts3 == "Default") {
      updateTextInput(session, "title1_input3", value = "")
      updateTextInput(session, "title2_input3", value = "")
      updateTextInput(session, "title1_input_ts3", value = "")
    }
  })
  # Refill with updated defaults after clearing
  observeEvent({
    input$title_mode3
    plot_titles_cor()
  }, {
    req(input$title_mode3 == "Default" ||
          input$title_mode_ts3 == "Default")
    req(plot_titles_cor())
    isolate({
      updateTextInput(session, "title1_input3", value = plot_titles_cor()$map_title)
      updateTextInput(session, "title2_input3", value = plot_titles_cor()$map_subtitle)
      updateTextInput(session, "title1_input_ts3", value = plot_titles_cor()$ts_title)
    })
  })
  
  
  # Select variable timeseries data
  ts_data_v1 = reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (input$source_v1 == "ModE-") {
      tsd_v1 = timeseries_data_v1()
    } else {
      tsd_v1 = user_subset_v1()
    }

    
    return(tsd_v1)
  })
  
  ts_data_v2 = reactive({
    
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (input$source_v2 == "ModE-"){
      tsd_v2 = timeseries_data_v2()
    } else {
      tsd_v2 = user_subset_v2()
    } 

    
    return(tsd_v2)
  })
  
  # Correlate timeseries
  correlation_stats = reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    c_st = correlate_timeseries(
      variable1_data = ts_data_v1(),
      variable2_data = ts_data_v2(),
      method = input$cor_method_ts
    )
    
    return(c_st)
  })
  
  # Plot
  output$correlation_r_value = renderText({
    paste(
      "Timeseries correlation coefficient: r =",
      signif(correlation_stats()$estimate, digits = 3),
      sep = ""
    )
  })
  output$correlation_p_value = renderText({
    paste(
      "Timeseries correlation p-value: p =",
      signif(correlation_stats()$p.value, digits = 3),
      sep = ""
    )
  })
  
  timeseries_plot_corr = function(){
    
    if (input$source_v1 == "ModE-"){
      variable_v1 = input$ME_variable_v1
    } else {
      variable_v1 = input$user_variable_v1
    }
    
    if (input$source_v2 == "ModE-"){
      variable_v2 = input$ME_variable_v2
    } else {
      variable_v2 = input$user_variable_v2
    }
    
    plot_timeseries(
      type = "Correlation",
      data_v1 = ts_data_v1(),
      data_v2 = ts_data_v2(),
      variable1 = variable_v1,
      variable2 = variable_v2,
      year_range = input$range_years3,
      month_range_1 = month_range_primary(),
      month_range_2 = month_range_secondary(),
      titles = plot_titles_cor(),
      show_ticks = input$show_ticks_ts3,
      tick_interval = input$xaxis_numeric_interval_ts3,
      show_key = input$show_key_ts3,
      key_position = input$key_position_ts3,
      moving_ave = input$custom_average_ts3,
      moving_ave_year = input$year_moving_ts3,
      highlights = ts_highlights_data3(),
      lines = ts_lines_data3(),
      points = ts_points_data3(),
      axis_range = input$axis_input_ts3
    )
    
  }
  
  output$correlation_ts = renderPlot({timeseries_plot_corr()}, height = 400)
  
  ######### Correlation Scatter Plot
  # Function
  scatter_plot_corr = function() {
    req(ts_data_v1(), ts_data_v2(), plot_titles_cor())
    
    #repare the data with year already included
    df_full <- data.frame(
      year = ts_data_v1()[, 1],
      v1 = ts_data_v1()[, 2],
      v2 = ts_data_v2()[, 2]
    )
    
    #Remove all rows with NAs
    df <- na.omit(df_full)
    
    #Guard: if nothing left, abort gracefully
    if (nrow(df) == 0) {
      showNotification("No valid data after filtering for scatter plot.", type = "error")
      return(NULL)
    }
    
    # Extract titles
    titles <- plot_titles_cor()
    title_text <- "Correlation scatter plot"
    x_label <- titles$V1_axis_label
    y_label <- titles$V2_axis_label
    
    # Base plot
    p <- ggplot(df, aes(x = v1, y = v2)) +
      geom_point(color = "#094030", alpha = 0.7, size = 4) +
      theme_minimal(base_size = 13) +
      labs(title = title_text, x = x_label, y = y_label) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.ticks = element_line(color = "black", linewidth = 0.5)
      ) +
      scale_x_continuous(minor_breaks = waiver()) +
      scale_y_continuous(minor_breaks = waiver())
    
    # Z-score outliers
    if (input$add_outliers_ref_ts3 == "z-score") {
      z_v1 <- (df$v1 - mean(df$v1)) / sd(df$v1)
      z_v2 <- (df$v2 - mean(df$v2)) / sd(df$v2)
      
      df$outlier <- factor(ifelse(abs(z_v1) > input$sd_input_ref_ts3 | abs(z_v2) > input$sd_input_ref_ts3,
                                  "Outlier", "Normal"))
      
      # Plot with outlier colors and year labels
      p <- ggplot(df, aes(x = v1, y = v2, color = outlier)) +
        geom_point(alpha = 0.7, size = 4) +
        geom_text(
          data = subset(df, outlier == "Outlier"),
          aes(label = year),
          vjust = -0.8, size = 3.5, color = "black"
        ) +
        scale_color_manual(values = c("Outlier" = "#FFC000", "Normal" = "#094030")) +
        theme_minimal(base_size = 13) +
        labs(title = title_text, x = x_label, y = y_label) +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        ) +
        scale_x_continuous(minor_breaks = waiver()) +
        scale_y_continuous(minor_breaks = waiver())
    }
    
    # Trend deviation outliers
    if (input$add_outliers_ref_ts3 == "Trend deviation") {
      model <- lm(v2 ~ v1, data = df)
      z_resid <- resid(model) / sd(resid(model))
      
      df$outlier <- factor(ifelse(abs(z_resid) > input$trend_sd_input_ref_ts3,
                                  "Outlier", "Normal"))
      
      p <- ggplot(df, aes(x = v1, y = v2, color = outlier)) +
        geom_point(alpha = 0.7, size = 4) +
        geom_text(
          data = subset(df, outlier == "Outlier"),
          aes(label = year),
          vjust = -0.8, size = 3.5, color = "black"
        ) +
        scale_color_manual(values = c("Outlier" = "#FFC000", "Normal" = "#094030")) +
        theme_minimal(base_size = 13) +
        labs(title = title_text, x = x_label, y = y_label) +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        ) +
        scale_x_continuous(minor_breaks = waiver()) +
        scale_y_continuous(minor_breaks = waiver())
    }
    
    # Add trendline if selected
    if (input$add_trend_ref_ts3) {
      p <- p + 
        geom_smooth(aes(linetype = "Trendline"),
                    method = "lm", se = FALSE, color = "black", size = 1) +
        scale_linetype_manual(name = "Legend", values = c("Trendline" = "dashed"))
    }
    
    # Toggle legend visibility
    if (input$show_key_ref_ts3) {
      p <- p +
        guides(
          color = guide_legend(title = "Statistics"),
          linetype = guide_legend(title = "Legend")
        )
    } else {
      p <- p +
        guides(
          color = "none",
          linetype = "none"
        )
    }
    
    return(p)
  }
  
  # Plotting
  output$ref_map3 <- renderPlot({
    scatter_plot_corr()
  })
  
  ######### Plot correlation map
  
  # Pick out relevant v1/v2 data:
  correlation_map_data_v1 = reactive({
    
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (input$type_v1 == "Field"){
      cmd_v1 = data_output4_primary()
    } else if (input$source_v1 == "User data"){
      cmd_v1 = user_subset_v1()
    } else {
      cmd_v1 = timeseries_data_v1()
    } 
  })
  
  correlation_map_data_v2 = reactive({
    
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (input$type_v2 == "Field"){
      cmd_v2 = data_output4_secondary()
    } else if (input$source_v2 == "User data"){
      cmd_v2 = user_subset_v2()
    } else {
      cmd_v2 = timeseries_data_v2()
    } 
  })
  
  # Generate correlation map data
  correlation_map_data = reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    corrmd = generate_correlation_map_data(
      variable1_data = correlation_map_data_v1(),
      variable2_data = correlation_map_data_v2(),
      method = input$cor_method_map,
      variable1_type = input$type_v1,
      variable2_type = input$type_v2,
      variable1_lon_range = lonlat_vals_v1()[1:2],
      variable2_lon_range = lonlat_vals_v2()[1:2],
      variable1_lat_range = lonlat_vals_v1()[3:4],
      variable2_lat_range = lonlat_vals_v2()[3:4]
    )
    return(corrmd)
  })
  
  # Generate plot dimensions
  correlation_map_dimensions <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    c_m_d = generate_map_dimensions(
      subset_lon_IDs = correlation_map_data()[[1]],
      subset_lat_IDs = correlation_map_data()[[2]],
      output_width = session$clientData$output_correlation_map_width,
      output_height = (input$dimension[2]),
      hide_axis = FALSE
    )
    
    return(c_m_d)
  })
  
  # Geotiff of correlation map data
  correlation_map_data_tiff = reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    create_geotiff(map_data = generate_correlation_map_datatable(data_input = correlation_map_data()))
  })
  
  
  # Get dynamically calculated axis values
  axis_range_dynamic <- reactive({
    vals <- terra::values(correlation_map_data_tiff())
    max_abs <- max(abs(vals), na.rm = TRUE)
    c(-max_abs, max_abs)
  })
  
  # Plot Correlation Map
  corr_m1 <- function() {
    if (any(input$type_v1 == "Field", input$type_v2 == "Field")) {
      if (input$type_v1 == "Field" && input$type_v2 == "Field") {
        v1 <- lonlat_vals_v1()
        v2 <- lonlat_vals_v2()
        lonlat_vals <- c(max(v1[1], v2[1]), min(v1[2], v2[2]), max(v1[3], v2[3]), min(v1[4], v2[4]))
      } else if (input$type_v1 == "Field") {
        lonlat_vals <- lonlat_vals_v1()
      } else {
        lonlat_vals <- lonlat_vals_v2()
      }
      
      # Dynamic axis range
      dynamic_axis <- axis_range_dynamic()
      
      # Use input axis or dynamic
      axis_input_empty <- is.null(input$axis_input3) ||
        any(is.na(input$axis_input3)) ||
        length(input$axis_input3) != 2
      
      axis_range_used <- if (axis_input_empty) {
        dynamic_axis
      } else {
        input$axis_input3
      }
      
      # Get correlation data
      corr_data <- correlation_map_data_tiff()
      
      # If dynamic axis is exactly [-1, 1], set all values to 1 (white map),
      # Ignore manual axis input
      if (all.equal(dynamic_axis, c(-1, 1)) == TRUE) {
        corr_data[] <- 1
      }
      
      titles <- plot_titles_cor()
      
      p <- plot_map(
        data_input    = corr_data,
        lon_lat_range = lonlat_vals,
        mode          = "Correlation",
        titles = plot_titles_cor(),
        axis_range = axis_range_used,
        hide_axis = input$hide_axis3,
        
        points_data = map_points_data3(),
        highlights_data = map_highlights_data3(),
        
        c_borders = input$hide_borders3,
        white_ocean = input$white_ocean3,
        white_land = input$white_land3,
        
        plotOrder = plotOrder3(),
        shpOrder = input$shapes3_order[input$shapes3_order %in% input$shapes3],
        input = input,
        plotType = "shp_colour3_",
        
        projection = input$projection3,
        center_lat = input$center_lat3,
        center_lon = input$center_lon3,
        
        show_rivers = input$show_rivers3,
        label_rivers = input$label_rivers3,
        show_lakes = input$show_lakes3,
        label_lakes = input$label_lakes3,
        show_mountains = input$show_mountains3,
        label_mountains = input$label_mountains3
      )
      
      # Adapt title for Europe–Asia combination
      if ((input$type_v1 == "Field") &&
          (input$type_v2 == "Field") &&
          # Europe
          (((
            input$range_longitude_v1[1] == -30 &&
            input$range_longitude_v1[2] == 40 &&
            input$range_latitude_v1[1] == 30 &&
            input$range_latitude_v1[2] == 75
          )
          &&
          # Asia
          (
            input$range_longitude_v2[1] == 25 &&
            input$range_longitude_v2[2] == 170 &&
            input$range_latitude_v2[1] == 5 &&
            input$range_latitude_v2[2] == 80
          )
          )
          ||
          # Europe
          ((
            input$range_longitude_v2[1] == -30 &&
            input$range_longitude_v2[2] == 40 &&
            input$range_latitude_v2[1] == 30 &&
            input$range_latitude_v2[2] == 75
          )
          &&
          # Asia
          (
            input$range_longitude_v1[1] == 25 &&
            input$range_longitude_v1[2] == 170 &&
            input$range_latitude_v1[1] == 5 &&
            input$range_latitude_v1[2] == 80
          )
          )
          )) {
        p <- p + labs(title = NA, subtitle = NA) +
          patchwork::plot_annotation(
            title = ifelse(titles$map_title != " ", titles$map_title, NULL),
            subtitle = ifelse(titles$map_subtitle != " ", titles$map_subtitle, NULL),
            theme = theme(
              plot.title = ggtext::element_textbox_simple(
                size = titles$map_title_size,
                face = "bold",
                margin = margin(0, 0, 5, 0)
              ),
              plot.subtitle = ggtext::element_textbox_simple(
                size = titles$map_title_size / 1.3,
                face = "plain",
                margin = margin(15, 0, 0, 0)
              ),
              axis.text = element_text(size = titles$map_title_size / 1.6)
            )
          )
      }
      
      return(p)
    }
  }
  
  
  ###### Cached Correlation Map
  output$correlation_map <- renderCachedPlot({
    req(input$nav1 == "tab3")
    req(correlation_map_dimensions()[1], correlation_map_dimensions()[2])
    
    corr_m1()
  },
  # Cache key expression
  cacheKeyExpr = {
    points_key     <- tryCatch(overlay_key(map_points_data3()),     error = function(e) "")
    highlights_key <- tryCatch(overlay_key(map_highlights_data3()), error = function(e) "")
    
    shpfile_key <- tryCatch({
      f <- input$shpFile3
      if (is.null(f)) "no-upload" else paste(
        paste(f$name, collapse = "|"),
        paste(f$size, collapse = "|"),
        paste(unname(tools::md5sum(f$datapath)), collapse = "|"),
        sep = "::"
      )
    }, error = function(e) "no-upload")
    
    shp_ids_key  <- tryCatch({
      ids <- input$shapes3_order[input$shapes3_order %in% input$shapes3]
      ids <- ids[!duplicated(ids)]
      paste(ids, collapse = "|")
    }, error = function(e) "")
    
    shp_style_key <- tryCatch({
      ids <- input$shapes3_order[input$shapes3_order %in% input$shapes3]
      ids <- ids[!duplicated(ids)]
      prefix <- "shp_colour3_"
      paste(vapply(ids, function(id) {
        val <- input[[paste0(prefix, id)]]
        col <- if (is.null(val) || is.na(val) || !nzchar(val)) "#000000" else as.character(val)[1]
        paste0(id, "=", col)
      }, character(1L)), collapse = "|")
    }, error = function(e) "")
    
    plotorder_key <- tryCatch(digest::digest(plotOrder3()), error = function(e) "")
    shp_color_key <- tryCatch(digest::digest(shp_color_inputs3()), error = function(e) "")
    
    corr_data_key <- tryCatch(digest::digest(correlation_map_data()), error = function(e) "")
    
    dim_key <- paste0(correlation_map_dimensions()[1], "x", correlation_map_dimensions()[2])
    
    axis_input_used <- tryCatch({
      a <- input$axis_input3
      if (is.null(a) || any(is.na(a)) || length(a) != 2) "dynamic" else paste(a, collapse = "_")
    }, error = function(e) "axis-error")
    
    list(
      input$nav1,
      input$type_v1,
      input$type_v2,
      input$source_v1,
      input$source_v2,
      corr_data_key,
      input$cor_method_map,
      lonlat_vals_v1()[1:4],
      lonlat_vals_v2()[1:4],
      axis_input_used,
      input$hide_axis3,
      points_key,
      highlights_key,
      shpfile_key,
      plotorder_key,
      shp_ids_key,
      shp_style_key,
      shp_color_key,
      dim_key,
      input$hide_borders3,
      input$white_ocean3,
      input$white_land3,
      input$projection3,
      input$center_lat3,
      input$center_lon3,
      input$show_rivers3,
      input$label_rivers3,
      input$show_lakes3,
      input$label_lakes3,
      input$show_mountains3,
      input$label_mountains3,
      plotOrder3(),
      input$shapes3_order[input$shapes3_order %in% input$shapes3],
      plot_titles_cor(),
      "Correlation_map"
    )
  },
  width  = function() { correlation_map_dimensions()[1] },
  height = function() { correlation_map_dimensions()[2] })
  
  
  
  
  # Disable Grey land and Grey ocean for the Orthographic and LAEA projections
  allowed_projs <- c("UTM (default)", "Robinson")
  
  observeEvent(input$projection3, {
    proj <- input$projection3
    
    if (!is.null(proj) && proj %in% allowed_projs) {
      shinyjs::enable("white_ocean3")
      shinyjs::enable("white_land3")
    } else {
      
      try(updateCheckboxInput(session, "white_ocean3", value = FALSE), silent = TRUE)
      try(updateCheckboxInput(session, "white_land3", value = FALSE), silent = TRUE)
      
      shinyjs::disable("white_ocean3")
      shinyjs::disable("white_land3")
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  
  
  
  ######### Data tables & Downloads 
  
  # Create output ts_data
  correlation_ts_datatable = reactive({
    
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (input$source_v1 == "ModE-"){
      variable_v1 = input$ME_variable_v1
    } else {
      variable_v1 = input$user_variable_v1
    }
    
    if (input$source_v2 == "ModE-"){
      variable_v2 = input$ME_variable_v2
    } else {
      variable_v2 = input$user_variable_v2
    }
    
    # Create v1/v2 datatables
    ctd_v1 = rewrite_tstable(tstable = ts_data_v1(), variable = variable_v1)
    ctd_v2 = rewrite_tstable(tstable = ts_data_v2(), variable = variable_v2)
    
    # Combine into dataframe
    ctd = data.frame(ctd_v1,ctd_v2[-1])
    
    # Add Var1/2 to names
    colnames(ctd) = c("Year",paste("Var1_",colnames(ctd_v1)[-1], sep = ""),paste("Var2_",colnames(ctd_v2)[-1], sep = ""))
    
    return(ctd)
  })
  
  output$correlation_ts_data = DT::renderDataTable({correlation_ts_datatable()}, rownames = FALSE, options = list(
    autoWidth = TRUE, 
    searching = FALSE,
    paging = TRUE,
    pagingType = "numbers"
  ))
  
  # Create output map data
  correlation_map_datatable = reactive({
    
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    corrmada = generate_correlation_map_datatable(data_input = correlation_map_data())
    
    return(corrmada)
  })
  
  output$correlation_map_data <- renderTable({correlation_map_datatable()}, rownames = TRUE)
  
  ####### ModE-RA sources ----
  
  # Set up values and functions for plotting
  fad_zoom3  <- reactiveVal(c(-180,180,-90,90)) # These are the min/max lon/lat for the zoomed plot
  
  season_fad_short3 = reactive({
    switch(input$fad_season3,
           "April to September" = "summer",
           "October to March" = "winter")
  })
  
  # Load global data
  fad_global_data3 = reactive({
    load_modera_source_data(year = input$fad_year3, season = season_fad_short3())
  })
  
  # Plot map 
  fad_plot3 = function(base_size = 18) {
    plot_modera_sources(
      ME_source_data = fad_global_data3(),
      year = input$fad_year3,
      season = season_fad_short3(),
      minmax_lonlat = fad_zoom3(),
      base_size = base_size
    )
  }
  
  fad_dimensions3 <- reactive({
    req(input$nav1 == "tab3") # Only run code if in the current tab
    m_d_f3 = generate_map_dimensions(
      subset_lon_IDs = subset_lons_secondary(),
      subset_lat_IDs = subset_lats_secondary(),
      output_width = session$clientData$output_fad_map3_width,
      output_height = input$dimension[2],
      hide_axis = FALSE
    )
    return(m_d_f3)
  })
  
  output$fad_map3 <- renderPlot({
    fad_plot3()
  }, width = function() {
    fad_dimensions3()[1]
  }, height = function() {
    fad_dimensions3()[2]
  })
  
  # Set up data function
  fad_data3 <- function() {
    fad_base_data3 = download_feedback_data(
      global_data = fad_global_data3(),
      lon_range = fad_zoom3()[1:2],
      lat_range = fad_zoom3()[3:4]
    )
    
    # Remove the last column
    fad_base_data3 = fad_base_data3[, -ncol(fad_base_data3)]
    
    return(fad_base_data3)
    
  }
  
  observeEvent(lonlat_vals_v2()|input$fad_reset_zoom3,{
    fad_zoom3(lonlat_vals_v2())
  })
  
  observeEvent(input$brush_fad3,{
    brush = input$brush_fad3
    req(brush)  # ensure brush is not NULL
    fad_zoom3(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
  })
  
  # Update fad_year 
  observeEvent(input$range_years3[1], {
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "fad_year3",
      value = input$range_years3[1])
  })
  
  # Update fad_season
  observeEvent(month_range_primary()[1], {
    
    req(input$nav1 == "tab3") # Only run code if in the current tab
    
    if (month_range_primary()[1] >3 & month_range_primary()[1] <10){
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "fad_season3",
        selected = "April to September")
    } else {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "fad_season3",
        selected = "October to March")
    }
  })
  
  ####### Downloads ----
  # Downloads
  
  output$download_timeseries3      <- downloadHandler(
    filename = function() {
      paste(plot_titles_cor()$file_title,
            "-ts.",
            input$file_type_timeseries3,
            sep = "")
    },
    content  = function(file) {
      if (input$file_type_timeseries3 == "png") {
        png(
          file,
          width = 3000,
          height = 1285,
          res = 200,
          bg = "transparent"
        )
      } else if (input$file_type_timeseries3 == "jpeg") {
        jpeg(
          file,
          width = 3000,
          height = 1285,
          res = 200,
          bg = "white"
        )
      } else {
        pdf(file,
            width = 14,
            height = 6,
            bg = "transparent")
      }
      print(timeseries_plot_corr())
      dev.off()
    }
  )
  
  output$download_map_sec3      <- downloadHandler(filename = function(){paste("Corr_Scatter_plot.",input$file_type_map_sec3, sep = "")},
                                                   content  = function(file) {
                                                     if (input$file_type_map_sec3 == "png"){
                                                       png(file, width = 3000, height = 1285, res = 200, bg = "transparent") 
                                                     } else if (input$file_type_map_sec3 == "jpeg"){
                                                       jpeg(file, width = 3000, height = 1285, res = 200, bg = "white") 
                                                     } else {
                                                       pdf(file, width = 14, height = 6, bg = "transparent") 
                                                     }
                                                     print(scatter_plot_corr())
                                                     dev.off()
                                                   }) 
  
  output$download_map3              <- downloadHandler(filename = function() {paste(plot_titles_cor()$file_title, "-map.", input$file_type_map3, sep = "")},
                                                       content = function(file) {
                                                         if (input$file_type_map3 == "png") {
                                                           png(file, width = correlation_map_dimensions()[3], height = correlation_map_dimensions()[4], res = 200, bg = "transparent")
                                                         } else if (input$file_type_map3 == "jpeg") {
                                                           jpeg(file, width = correlation_map_dimensions()[3], height = correlation_map_dimensions()[4], res = 200, bg = "white")
                                                         } else {
                                                           pdf(file, width = correlation_map_dimensions()[3] / 200, height = correlation_map_dimensions()[4] / 200, bg = "transparent")
                                                         }
                                                         print(corr_m1())
                                                         dev.off()}
  )
  
  output$download_timeseries_data3  <- downloadHandler(filename = function(){paste(plot_titles_cor()$file_title, "-tsdata.",input$file_type_timeseries_data3, sep = "")},
                                                       content  = function(file) {
                                                         if (input$file_type_timeseries_data3 == "csv"){
                                                           write.csv(correlation_ts_datatable(), file,
                                                                     row.names = FALSE,
                                                                     fileEncoding = "latin1")
                                                         } else {
                                                           openxlsx::write.xlsx(correlation_ts_datatable(), file,
                                                                                row.names = FALSE,
                                                                                col.names = TRUE)
                                                         }}) 
  
  output$download_map_data3 <- downloadHandler(
    filename = function() {
      paste(plot_titles_cor()$file_title,
            "-mapdata.",
            input$file_type_map_data3,
            sep = "")
    },
    content  = function(file) {
      if (input$file_type_map_data3 == "csv") {
        map_data_new_3 <- rewrite_maptable(
          maptable = correlation_map_datatable(),
          subset_lon_IDs = subset_lons_secondary(),
          subset_lat_IDs = subset_lats_secondary()
        )
        colnames(map_data_new_3) <- NULL
        
        write.csv(map_data_new_3, file, row.names = FALSE)
      } else if (input$file_type_map_data3 == "xlsx") {
        openxlsx::write.xlsx(
          rewrite_maptable(
            maptable = correlation_map_datatable(),
            subset_lon_IDs = subset_lons_secondary(),
            subset_lat_IDs = subset_lats_secondary()
          ),
          file,
          row.names = FALSE,
          col.names = FALSE
        )
      } else if (input$file_type_map_data3 == "GeoTIFF") {
        create_geotiff(map_data = correlation_map_datatable(), output_file = file)
      }
    }
  )
  
  output$download_fad3 <- downloadHandler(
    filename = function()
    {paste("Assimilated Observations_", gsub(" ", "", input$fad_season3), "_", input$fad_year3, ".", input$file_type_fad3, sep = "")},
    
    content = function(file) {
      mmd = generate_map_dimensions(
        subset_lon_IDs = subset_lons_secondary(),
        subset_lat_IDs = subset_lats_secondary(),
        output_width = session$clientData$output_fad_map3_width,
        output_height = input$dimension[2],
        hide_axis = FALSE
      )
      if (input$file_type_fad3 == "png") {
        png(file, width = mmd[3], height = mmd[4], res = 400, bg = "transparent")
        print(fad_plot3(base_size = 9))
        dev.off()
      } else if (input$file_type_fad3 == "jpeg") {
        jpeg(file, width = mmd[3], height = mmd[4], res = 400,bg = "white")
        print(fad_plot3(base_size = 9))
        dev.off()
      } else {
        pdf(file, width = mmd[3] / 400, height = mmd[4] / 400, bg = "transparent")
        print(fad_plot3(base_size = 9))
        dev.off()
      }
    }
  )
  
  output$download_fad_data3       <- downloadHandler(filename = function(){paste("Assimilated Observations_",gsub(" ", "", input$fad_season3),"_",input$fad_year3,"_data.",input$data_file_type_fad3, sep = "")},
                                                     content  = function(file) {
                                                       if (input$data_file_type_fad3 == "csv"){
                                                         write.csv(fad_data3(), file,
                                                                   row.names = FALSE)
                                                       } else {
                                                         openxlsx::write.xlsx(fad_data3(), file,
                                                                              col.names = TRUE,
                                                                              row.names = FALSE)
                                                       }})
  
  
  ### MODE-RA SOURCES data procession and plotting ----
  ####### Plotting (for download)----
  
  season_MES_short <- reactive({ c
    switch(input$season_MES,
           "April to September" = "summer",
           "October to March" = "winter")
  })
  
  # Load global data
  MES_global_data <- reactive({
    if (input$year_MES >= 1422 && input$year_MES <= 2008) {
      load_modera_source_data(year = input$year_MES, season = season_MES_short()) |>
        dplyr::select(LON, LAT, VARIABLE, TYPE, Name_Database, Paper_Database, Code_Proxy, Reference_Proxy, Reference_Proxy_Database, Omitted_Duplicates) |>
        sf::st_as_sf(coords = c('LON', 'LAT')) |>
        sf::st_set_crs(4326)
    } else {
      NULL
    }
  })
  
  ####### Leaflet Map ----
  
  output$MES_leaflet <- leaflet::renderLeaflet({
    data <- MES_global_data()
    
    leaflet::leaflet(data) |>
      # Base maps
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = "ESRI gray") |>
      leaflet::addTiles(group = "Open Street Map") |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellite") |>
      leaflet::setView(lng = 0, lat = 30, zoom = 1.6) |>
      
      # Add layers control for filtering by TYPE
      leaflet::addLayersControl(
        baseGroups = c("ESRI gray", "Open Street Map", "ESRI Satellite"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      
      # Add initial data points
      leaflet::addCircleMarkers(data = data,
                                radius = 5,
                                fillColor = ~pal_type(data$TYPE),
                                stroke = TRUE,
                                weight = 1,
                                color = "grey",
                                fillOpacity = 1,
                                opacity = 1,
                                group = named_types[data$TYPE],  # Use readable names
                                popup = paste(
                                  "<strong>Measurement type: </strong>", named_variables[data$VARIABLE],
                                  "<br><strong>Source type: </strong>", named_types[data$TYPE],
                                  "<br><strong>Name database: </strong>", "<a href='", data$Paper_Database, "' target='_blank'>", data$Name_Database, "</a>",
                                  "<br><strong>Proxy code: </strong>", data$Code_Proxy,
                                  "<br><strong>Proxy reference: </strong>", data$Reference_Proxy,
                                  "<br><strong>Proxy reference database: </strong>", data$Reference_Proxy_Database
                                ))
    
    
  })
  
  # Use a separate observer to show or hide the legend
  observe({
    data <- MES_global_data()
    
    proxy <- leaflet::leafletProxy("MES_leaflet")
    
    if (input$legend_MES == TRUE && !is.null(data)) {
      proxy |>
        leaflet::addLegend(pal = pal_type,
                           values = data$TYPE,  # use actual data
                           title = "Legend",
                           position = "bottomleft",
                           opacity = 1.0,
                           labFormat = function(type, values) {
                             named_types[values]  # display names instead of codes
                           }) |>
        leaflet::addControl(
          html = sprintf("<strong>Total global sources: %d</strong>", nrow(data)),
          position = "bottomleft"
        )
    } else {
      proxy |> leaflet::clearControls()
    }
  })
  
  # Use a separate observer to add the data points
  observe({
    
    data <- MES_global_data()
    
    if (!is.null(data)) {
      proxy <- leaflet::leafletProxy("MES_leaflet")
      
      proxy |> leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(data = data,
                                  radius = 5,
                                  fillColor = ~pal_type(data$TYPE),
                                  stroke = TRUE,
                                  weight = 1,
                                  color = "grey",
                                  fillOpacity = 1,
                                  opacity = 1,
                                  group = named_types[data$TYPE],  # Use readable names
                                  popup = paste(
                                    "<strong>Measurement type: </strong>", named_variables[data$VARIABLE],
                                    "<br><strong>Source type: </strong>", named_types[data$TYPE],
                                    "<br><strong>Name database: </strong>", "<a href='", data$Paper_Database, "' target='_blank'>", data$Name_Database, "</a>",
                                    "<br><strong>Proxy code: </strong>", data$Code_Proxy,
                                    "<br><strong>Proxy reference: </strong>", data$Reference_Proxy,
                                    "<br><strong>Proxy reference database: </strong>", data$Reference_Proxy_Database
                                  ))
    }
  })
  
  ### Download Preparation for Data (CSV/XLSX)
  # Set up values and functions for plotting
  fad_zoom_MES  <- reactiveVal(c(-180,180,-90,90)) # These are the min/max lon/lat for the zoomed plot
  
  MES_global_data_download = reactive({
    load_modera_source_data(year = input$year_MES, season = season_MES_short())
  })
  
  # Set up data function
  fad_data_MES <- function() {
    fad_base_data_MES = download_feedback_data(
      global_data = MES_global_data_download(),
      lon_range = fad_zoom_MES()[1:2],
      lat_range = fad_zoom_MES()[3:4]
    )
    
    # Remove the last column
    fad_base_data_MES = fad_base_data_MES[, -ncol(fad_base_data_MES)]
    
    return(fad_base_data_MES)
  }
  
  
  output$download_MES_data       <- downloadHandler(filename = function(){paste("Assimilated Observations_",gsub(" ", "", input$season_MES),"_",input$year_MES,"_data.",input$data_file_type_MES, sep = "")},
                                                    content  = function(file) {
                                                      if (input$data_file_type_MES == "csv"){
                                                        write.csv(fad_data_MES(), file,
                                                                  row.names = FALSE)
                                                      } else {
                                                        openxlsx::write.xlsx(fad_data_MES(), file,
                                                                             col.names = TRUE,
                                                                             row.names = FALSE)
                                                      }})
  
  ####### TS Sources and Observation Map ----
  ### Timeseries plot for ModE-ra sources and observations
  
  # File path and data parameters
  file_path_sources <- "data/feedback_archive_fin/Info/total_sources_observations.xlsx"
  sheet_name_sources <- "sources"
  year_column_sources <- "Year"
  value_columns_sources <- c("Total_global_sources_summer", 
                             "Total_global_sources_winter", 
                             "Total_global_sources") # List of columns to plot
  
  # Corresponding line titles for the legend
  line_titles_sources <- c("Total_global_sources_summer" = "Global sources (Apr. - Sept.)", 
                           "Total_global_sources_winter" = "Global sources (Oct. - Mar.)", 
                           "Total_global_sources" = "Global sources total")
  
  # Read data from Excel once and reuse it
  data_sources <- readxl::read_excel(file_path_sources, sheet = sheet_name_sources)
  data_sources[[year_column_sources]] <- as.numeric(data_sources[[year_column_sources]])
  
  # Render plot for selected lines using plotly
  output$time_series_plot <- plotly::renderPlotly({
    selected_columns <- c("Total_global_sources_summer",
                          "Total_global_sources_winter",
                          "Total_global_sources")
    year_range <- input$year_range_sources
    
    plot_ts_modera_sources(data_sources, year_column_sources, selected_columns, line_titles_sources,
                           title = "Total global sources",
                           x_label = "Year",
                           y_label = "Sources",
                           x_ticks_every = 20,
                           year_range = year_range)
  })
  
  ### SEA Superposed epoch analysis ----
  ####### User Data Processing ----
  
  # Load in user data for SEA
  user_data_6 = reactive({
    
    req(input$user_file_6)
    
    if (input$source_sea_6 == "User data"){
      new_data1 = read_regcomp_data(data_input_filepath = input$user_file_6$datapath)   
      return(new_data1)
    }
    else{
      return(NULL)
    }
  })
  
  # Subset user data to chosen variable
  user_subset_6 <- reactive({
    req(input$nav1 == "tab6")       # Only run if in the correct tab
    req(input$user_variable_6)      # Ensure input is available
    
    ts_data1 <- user_data_6()       # Get full timeseries data
    req(ncol(ts_data1) >= 2)        # Ensure at least two columns exist
    
    # Get first column name (e.g., "Jahr") and selected variable
    time_col <- colnames(ts_data1)[1]
    var_col <- input$user_variable_6
    
    # Subset to those two columns
    ts_us_sub <- ts_data1[, c(time_col, var_col), drop = FALSE]
    
    return(ts_us_sub)
  })
  
  #Creating a year set for sea
  year_set_sea <- reactive({
    read_sea_data(
      data_input_manual = input$event_years_6,
      data_input_filepath = input$upload_file_6b$datapath,
      year_input_mode = input$enter_upload_6,
      data_source_sea = input$source_sea_6
    )
  })
  
  ####### ModE-Data Processing ----
  
  #Using the time series data as input
  timeseries_data_sea <- reactive({
    req(input$nav1 == "tab6") # Only run code if in the current tab
    
    ts_data1 <- create_timeseries_datatable(
      data_input = data_output4_primary(),
      year_input = c(1422, 2008),
      year_input_type = "range",
      subset_lon_IDs = subset_lons_primary(),
      subset_lat_IDs = subset_lats_primary()
    )
    
    return(ts_data1)
  })
  
  #Choose the wished statistic
  timeseries_subdata_sea <- reactive({
    req(input$nav1 == "tab6") # Only run if in the correct tab
    req(input$ME_statistic_6) # Ensure input is available
    
    ts_data1 <- timeseries_data_sea() # Get full timeseries data
    
    # Create a dataframe with Year and the selected statistic
    ts_sub <- ts_data1[, c("Year", input$ME_statistic_6), drop = FALSE]
    
    return(ts_sub)
  })
  
  ####### SEA Processing ----
  
  # Extract years, title & y label based on selected data source
  years = reactive({
    if (input$source_sea_6 == "User data") {
      return(unlist(user_subset_6()[,1]))  # Extract years from user-uploaded data
    } else {
      return(unlist(timeseries_data_sea()[,1]))  # Extract years from ModE- data
    }
  })
  
  
  # Y Label and Plot Title
  ts_y_label = reactive({
    if (input$source_sea_6 == "User data") {
      if (input$y_label_6 != "" && input$title_mode_6 == "Custom"){
        return(input$y_label_6)
      } else {
        return(paste(colnames(user_subset_6())[2]))
      }
      
    } else {
      if (input$y_label_6 != "" && input$title_mode_6 == "Custom"){
        return(input$y_label_6)
      } else {
        return(paste(input$ME_statistic_6,input$season_selected_6,input$ME_variable_6))
      }
    }
  })
  
  ts_title = reactive({
    if (input$source_sea_6 == "User data") {
      if (input$title1_input_6 != "" && input$title_mode_6 == "Custom") {
        return(input$title1_input_6)
      } else {
        return(paste("SEA of", colnames(user_subset_6())[2]))
      }
    } else {
      if (input$title1_input_6 != "" && input$title_mode_6 == "Custom") {
        return(input$title1_input_6)
      } else {
        return(paste("SEA of", input$ME_statistic_6,input$season_selected_6,input$ME_variable_6))
      }
    }
  })
  
  
  # Add value to custom title
  # When switching to Default, reset the inputs to blank
  observeEvent(input$title_mode_6, {
    if (input$title_mode_6 != "Custom") {
      updateTextInput(session, "title1_input_6", value = "")
      updateTextInput(session, "y_label_6", value = "")
    }
  })
  # Fill the fields after they are blanked out
  observeEvent({
    input$title_mode_6
    input$ME_statistic_6
    input$ME_variable_6
    input$source_sea_6
  }, {
    req(input$title_mode_6 != "Custom")
    isolate({
      updateTextInput(session, "title1_input_6", value = ts_title())
      updateTextInput(session, "y_label_6", value = ts_y_label())
    })
  })
  
  
  
  # Turn "years" column into rownames
  ts_data = reactive({
    req(input$source_sea_6)  # Ensure source selection exists
    
    if (input$source_sea_6 == "User data") {
      ts_data_new = data.frame(user_subset_6()[, 2, drop = FALSE])  # Keep it as a dataframe
    } else {
      ts_data_new = data.frame(timeseries_subdata_sea()[, 2, drop = FALSE])  # Use selected statistic
    }
    
    row.names(ts_data_new) = years()  # Assign row names
    return(ts_data_new)
  })
  
  # Cut event years based on the data
  event_years_cut <- reactive({
    if (input$enter_upload_6 == "Manual") {
      v_years <- as.integer(unlist(strsplit(input$event_years_6, split = ",")))
    } else if (input$enter_upload_6 == "Upload") {
      df <- year_set_sea()
      v_years <- df$Event  # Use first column only
    } else {
      return(NULL)
    }
    
    v_years_cut <- subset(v_years, v_years > (min(years()) - input$lag_years_6[1]) &
                            v_years < (max(years()) - input$lag_years_6[2]))
    return(v_years_cut)
  })
  
  # Calculate SEA data
  SEA_data = reactive({
    ts <- ts_data()
    
    # Helper: fill NAs in ts_data for random resampling
    fill_na_random <- function(ts_input) {
      ts_copy <- ts_input
      if (sum(is.na(ts_copy)) > 0) {
        non_na_vals <- ts_copy[!is.na(ts_copy)]
        ts_copy[is.na(ts_copy)] <- sample(non_na_vals, sum(is.na(ts_copy)), replace = TRUE)
      }
      return(ts_copy)
    }
    
    # Case 1: Use event-specific windows from uploaded file
    if (input$enter_upload_6 == "Upload" &&
        (input$use_custom_pre_6 || input$use_custom_post_6)) {
      
      df_event_years <- year_set_sea()
      ts_range <- as.numeric(rownames(ts))
      
      # Prepare per-event lag-adjusted window
      lag_range <- seq(-abs(input$lag_years_6[1]), input$lag_years_6[2])
      obs_matrix <- matrix(NA, nrow = 0, ncol = length(lag_range))
      colnames(obs_matrix) <- lag_range
      used_events <- c()
      
      for (i in 1:nrow(df_event_years)) {
        event    <- as.integer(df_event_years[i, 1])
        pre_end  <- as.integer(df_event_years[i, 2])
        post_end <- as.integer(df_event_years[i, 3])
        
        lag_before <- abs(input$lag_years_6[1])
        lag_after  <- input$lag_years_6[2]
        
        # --- Pre-event years ---
        pre_years <- seq(event - 1, event - lag_before, by = -1)
        if (input$use_custom_pre_6 && !is.na(pre_end)) {
          pre_years <- pre_years[pre_years <= pre_end]
        }
        pre_years <- sort(pre_years) # Optional: ascending order
        
        # --- Post-event years ---
        post_years <- seq(event + 1, event + lag_after, by = 1)
        if (input$use_custom_post_6 && !is.na(post_end)) {
          post_years <- post_years[post_years <= post_end]
        }
        
        # Combine pre, event, post (include event if you wish)
        years_window <- c(pre_years, event, post_years)
        relative_lags <- years_window - event
        values <- ts[as.character(years_window), 1]
        
        row_vals <- rep(NA, length(lag_range))
        match_index <- match(relative_lags, lag_range)
        row_vals[match_index[!is.na(match_index)]] <- values[!is.na(match_index)]
        
        obs_matrix <- rbind(obs_matrix, row_vals)
        used_events <- c(used_events, event)
      }
      
      # Calculate observed mean
      obs_mean <- colMeans(obs_matrix, na.rm = TRUE)
      
      # Optional: fill NAs and recompute random sample for CI bands
      ts_filled <- fill_na_random(ts)
      rand_matrix <- matrix(NA, nrow = input$sample_size_6, ncol = length(lag_range))
      
      for (j in 1:input$sample_size_6) {
        rand_events <- sample(as.numeric(rownames(ts_filled)),
                              length(used_events), replace = TRUE)
        tmp_obs <- matrix(NA, nrow = length(rand_events), ncol = length(lag_range))
        
        for (k in seq_along(rand_events)) {
          event <- rand_events[k]
          years_window <- seq(event - abs(input$lag_years_6[1]), event + input$lag_years_6[2])
          relative_lags <- years_window - event
          values <- ts_filled[as.character(years_window), 1]
          
          row_vals <- rep(NA, length(lag_range))
          match_index <- match(relative_lags, lag_range)
          row_vals[match_index[!is.na(match_index)]] <- values[!is.na(match_index)]
          
          tmp_obs[k, ] <- row_vals
        }
        rand_matrix[j, ] <- colMeans(tmp_obs, na.rm = TRUE)
      }
      
      # Confidence intervals
      lower_95 <- apply(rand_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
      upper_95 <- apply(rand_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
      lower_99 <- apply(rand_matrix, 2, quantile, probs = 0.005, na.rm = TRUE)
      upper_99 <- apply(rand_matrix, 2, quantile, probs = 0.995, na.rm = TRUE)
      rand_mean <- colMeans(rand_matrix, na.rm = TRUE)
      
      return(list(
        actual = list(
          lag = lag_range,
          mean = obs_mean
        ),
        observed = obs_matrix,
        event_years = used_events,
        random = list(
          mean = rand_mean,
          lower_95 = lower_95,
          upper_95 = upper_95,
          lower_99 = lower_99,
          upper_99 = upper_99,
          lag = lag_range
        )
      ))
    }
    
    # Case 2: Standard SEA with global lags using burnr::sea()
    else {
      original_SEA <- burnr::sea(ts_data(), event_years_cut(),
                                 nbefore = abs(input$lag_years_6[1]),
                                 nafter  = input$lag_years_6[2],
                                 n_iter  = input$sample_size_6)
      
      # Optional NA random fill
      if (sum(is.na(ts_data())) > 0) {
        ts_filled <- fill_na_random(ts_data())
        new_SEA <- burnr::sea(ts_filled, event_years_cut(),
                              nbefore = abs(input$lag_years_6[1]),
                              nafter  = input$lag_years_6[2],
                              n_iter  = input$sample_size_6)
        
        original_SEA$random$mean <- new_SEA$random$mean
        original_SEA$random$lower_95 <- new_SEA$random$lower_95
        original_SEA$random$upper_95 <- new_SEA$random$upper_95
        original_SEA$random$lower_99 <- new_SEA$random$lower_99
        original_SEA$random$upper_99 <- new_SEA$random$upper_99
      }
      
      return(original_SEA)
    }
  })
  
  # Create SEA datatable
  SEA_datatable = reactive({
    
    LAG_YEAR = SEA_data()$random$lag
    OBSERVATIONS_MEAN =  SEA_data()$actual$mean
    SEAdatatable = data.frame(LAG_YEAR,OBSERVATIONS_MEAN)
    
    if (input$show_means_6){
      SAMPLE_MEAN = SEA_data()$random$mean
      SEAdatatable = data.frame(SEAdatatable,SAMPLE_MEAN)
    }
    
    if (input$show_confidence_bands_6 == "95%"){
      CI_95_LOWER = SEA_data()$random$lower_95
      CI_95_UPPER = SEA_data()$random$upper_95
      SEAdatatable = data.frame(SEAdatatable,CI_95_LOWER,CI_95_UPPER)
    }
    
    if (input$show_confidence_bands_6 == "99%"){
      CI_99_LOWER = SEA_data()$random$lower_99
      CI_99_UPPER = SEA_data()$random$upper_99
      SEAdatatable = data.frame(SEAdatatable,CI_99_LOWER,CI_99_UPPER)
    }
    
    if (input$show_pvalues_6){
      alternative_SEA = dplR::sea(ts_data(),event_years_cut(),lag = max(abs(input$lag_years_6)))
      P = alternative_SEA$p[which(alternative_SEA$lag %in% SEA_data()$actual$lag)]
      SEAdatatable = data.frame(SEAdatatable,P)
    }
    
    if (input$show_observations_6){
      
      Observations = SEA_data()$observed[1,]
      
      for (i in 2:dim(SEA_data()$observed)[1]){
        observation_data = SEA_data()$observed[i,]
        Observations = data.frame(Observations,observation_data)
      }
      
      colnames(Observations) = paste0("Observations_",SEA_data()$event_years)
      SEAdatatable = data.frame(SEAdatatable,Observations)
    }
    return(SEAdatatable)
  })
  
  # Create plot function:
  SEA_plotfunction <- function() {
    data <- SEA_datatable()
    
    # Set y-axis range
    y_data <- data[, !names(data) %in% c("LAG_YEAR", "P")]
    SEAmin <- min(y_data, na.rm = TRUE)
    SEAmax <- max(y_data, na.rm = TRUE)
    SEArange <- SEAmax - SEAmin
    
    if (!is.null(input$axis_input_6) && all(!is.na(input$axis_input_6))) {
      ymin <- input$axis_input_6[1]
      ymax <- input$axis_input_6[2]
    } else {
      ymin <- SEAmin - (0.05 * SEArange)
      ymax <- SEAmax + (0.05 * SEArange)
    }
    
    # Base plot
    p <- ggplot(data, aes(x = LAG_YEAR)) +
      geom_line(aes(y = OBSERVATIONS_MEAN, color = "Observation Means"), size = 1.2, 
                show.legend = input$show_key_6) +
      scale_color_manual(
        values = c(
          "Observation Means" = "black",
          "Individual Events" = "grey",
          "Random Sample Means" = "purple",
          "Upper 95% Confidence Band" = "gold",
          "Lower 95% Confidence Band" = "firebrick3",
          "Upper 99% Confidence Band" = "gold",
          "Lower 99% Confidence Band" = "firebrick3"
        ),
        breaks = c(
          "Observation Means",
          "Individual Events",
          "Random Sample Means",
          "Upper 95% Confidence Band",
          "Lower 95% Confidence Band",
          "Upper 99% Confidence Band",
          "Lower 99% Confidence Band"
        )
      ) +
      labs(color = "Legend:") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 1.2) +
      labs(x = "Lag Year", y = ts_y_label(), title = ts_title()) +
      theme_minimal(base_size = 16) +
      ylim(ymin, ymax) +
      theme(
        plot.margin = margin(6.7, 7.3, 4.1, 3.5, "pt"),
        legend.box = "vertical",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(0.4, "cm"),
        legend.key = element_blank(),
        legend.key.size = unit(1.2, "lines")
      )
    
    # Add individual event lines
    if (input$show_observations_6) {
      for (i in SEA_data()$event_years) {
        p <- p +
          geom_line(aes_string(y = paste0("Observations_", i), color = '"Individual Events"'),
                    size = 0.8, show.legend = input$show_key_6)
      }
    }
    
    # Add additional lines
    if (input$show_means_6) {
      p <- p + geom_line(aes(y = SAMPLE_MEAN, color = "Random Sample Means"), size = 1,
                         show.legend = input$show_key_6)
    }
    
    # Add Confidence Bands (if applicable)
    if (input$show_confidence_bands_6 == "95%") {
      p <- p + 
        geom_line(aes(y = CI_95_LOWER, color = "Lower 95% Confidence Band"), size = 1,
                  show.legend = input$show_key_6) +
        geom_line(aes(y = CI_95_UPPER, color = "Upper 95% Confidence Band"), size = 1,
                  show.legend = input$show_key_6)
    }
    
    if (input$show_confidence_bands_6 == "99%") {
      p <- p + 
        geom_line(aes(y = CI_99_LOWER, color = "Lower 99% Confidence Band"), size = 1,
                  show.legend = input$show_key_6) +
        geom_line(aes(y = CI_99_UPPER, color = "Upper 99% Confidence Band"), size = 1,
                  show.legend = input$show_key_6)
    }
    
    # Replot main line on top for visual reasons
    p <- p + geom_line(aes(y = OBSERVATIONS_MEAN), size = 1.2, color = "black")
    
    # Add p-values
    if (input$show_pvalues_6) {
      p <- p +
        geom_point(
          aes(
            y = OBSERVATIONS_MEAN,
            fill = factor(cut(P,
                              breaks = c(-Inf, 0.01, 0.05, Inf),
                              labels = c("p<0.01", "p<0.05", "NS"))
            )
          ),
          shape = 21, size = 6, stroke = 1.2, color = "black", show.legend = input$show_key_6
        ) +
        scale_fill_manual(
          values = c("p<0.01" = "#006D2C", "p<0.05" = "darkseagreen3", "NS" = "#EDF8E9"),
          labels = c("p<0.01", "p<0.05", "p>0.05"),
          breaks = c("p<0.01", "p<0.05", "NS"),
          drop = FALSE
        ) +
        labs(fill = "p-value:")
    }
    
    # Add tick marks
    if (input$show_ticks_6) {
      p <- p + scale_x_continuous(breaks = seq(input$lag_years_6[1], input$lag_years_6[2], by = 1))
    }
    
    # Separate legends for color and fill
    p <- p + guides(
      color = guide_legend(order = 1, override.aes = list(shape = NA, fill = NA)),
      fill = guide_legend(order = 2)
    )
    
    return(p)
  }
  
  #List of chosen event years (upload or manual) to plot
  output$text_years6 <- renderText("Chosen event years:")
  output$years6 <- renderText({
    sorted_years <- sort(as.numeric(event_years_cut()))
  })
  
  
  # Generate Plot
  output$SEA_plot_6 <- renderPlot({
    SEA_plotfunction()
  })
  
  ####### Downloads ----
  output$download_timeseries6      <- downloadHandler(filename = function(){paste(ts_title(),"-sea.",input$file_type_timeseries6, sep = "")},
                                                      content  = function(file) {
                                                        if (input$file_type_timeseries6 == "png"){
                                                          png(file, width = 3000, height = 1285, res = 200, bg = "transparent") 
                                                          
                                                        } else if (input$file_type_timeseries6 == "jpeg"){
                                                          jpeg(file, width = 3000, height = 1285, res = 200, bg = "white") 
                                                          
                                                        } else {
                                                          pdf(file, width = 14, height = 6, bg = "transparent") 
                                                        }
                                                        print(SEA_plotfunction())
                                                        dev.off()
                                                      })
  
  output$download_timeseries_data6  <- downloadHandler(filename = function(){paste(ts_title(),"-sea.",input$file_type_timeseries_data6, sep = "")},
                                                       content  = function(file) {
                                                         if (input$file_type_timeseries_data6 == "csv"){
                                                           write.csv(SEA_datatable(), file,
                                                                     row.names = FALSE,
                                                                     fileEncoding = "latin1")
                                                         } else {
                                                           openxlsx::write.xlsx(SEA_datatable(), file,
                                                                                col.names = TRUE,
                                                                                row.names = FALSE)
                                                         }})
  
  ### Concerning all modes (mainly updating Ui) ----
  
  # Jumps to Desktop Version Download Tab
  observeEvent(input$go_desktop, {
    updateTabsetPanel(session, "tabset0", selected = "desktop_tab")
  })
  
  observeEvent(input$go_desktop2, {
    updateTabsetPanel(session, "tabset0", selected = "desktop_tab")
  })
  
  #Updates Values outside of min / max (numericInput)
  
  updateNumericInputRange1 <- function(inputId, minValue, maxValue) {
    observe({
      input_values <- input[[inputId]]
      
      shinyjs::delay(3000, {
        if (is.null(input_values) || is.na(input_values)) {
        } else if (!is.numeric(input_values)) {
          updateNumericInput(inputId = inputId, value = minValue)
        } else {
          update_value <- function(val) {
            if (val < minValue) {
              updateNumericInput(inputId = inputId, value = minValue)
            } else if (val > maxValue) {
              updateNumericInput(inputId = inputId, value = maxValue)
            }
          }
          
          update_value(input_values)
        }
      })
    })
  }
  
  # Call the function for each input
  updateNumericInputRange1("point_size", 1, 10)
  updateNumericInputRange1("point_size2", 1, 10)
  updateNumericInputRange1("point_size3", 1, 10)
  
  updateNumericInputRange1("point_size_ts", 4, 20)
  updateNumericInputRange1("point_size_ts2", 4, 20)
  updateNumericInputRange1("point_size_ts3", 4, 20)

  updateNumericInputRange1("title_size_input", 1, 40)
  updateNumericInputRange1("title_size_input_ts", 1, 40)
  updateNumericInputRange1("title_size_input2", 1, 40)
  updateNumericInputRange1("title_size_input_ts2", 1, 40)
  updateNumericInputRange1("title_size_input3", 1, 40)
  updateNumericInputRange1("title_size_input_ts3", 1, 40)
  
  
  updateNumericInputRange1("prior_years2", 1, 50)
  updateNumericInputRange1("percentage_sign_match2", 1, 100)
  updateNumericInputRange1("sample_size_6", 100, 100000000000)
  
  updateNumericInputRange1("sd_ratio2", 0, 1)
  updateNumericInputRange1("sd_input_ref_ts3", 1, 10)
  updateNumericInputRange1("trend_sd_input_ref_ts3", 1, 10)
  
  updateNumericInputRange1("year_moving_ts", 3, 30)
  updateNumericInputRange1("year_moving_ts3", 3, 30)
  
  updateNumericInputRange1("xaxis_numeric_interval_ts", 1, 500)
  updateNumericInputRange1("xaxis_numeric_interval_ts2", 1, 500)
  updateNumericInputRange1("xaxis_numeric_interval_ts3", 1, 500)

  updateNumericInputRange1("center_lat", -90, 90)
  updateNumericInputRange1("center_lon", -180, 180)
  updateNumericInputRange1("center_lat2", -90, 90)
  updateNumericInputRange1("center_lon2", -180, 180)
  updateNumericInputRange1("center_lat3", -90, 90)
  updateNumericInputRange1("center_lon3", -180, 180)
  
  updateNumericInputRange1("lagyears_v1_cor", -100, 100)
  updateNumericInputRange1("lagyears_v2_cor", -100, 100)
  
  #Updates Values outside of min / max (numericRangeInput)
  updateNumericRangeInputSafe <- function(inputId, minValue, maxValue, skip_if = NULL) {
    observe({
      if (!is.null(skip_if) && skip_if()) return()
      
      range_values <- input[[inputId]]
      if (is.null(range_values) || length(range_values) != 2) return()
      
      left <- range_values[1]
      right <- range_values[2]
      
      new_left <- if (!is.numeric(left) || is.na(left) || left < minValue || left > maxValue) minValue else left
      new_right <- if (!is.numeric(right) || is.na(right) || right < minValue || right > maxValue) maxValue else right
      
      if (!identical(c(left, right), c(new_left, new_right))) {
        shinyWidgets::updateNumericRangeInput(inputId = inputId, value = c(new_left, new_right))
      }
    })
  }
  
  updateNumericRangeInputSafe("range_years3", 1422, 2008, skip_if = function() {
    input$source_v1 == "User data" && input$source_v2 == "User data"
  })
  
  
  updateNumericRangeInputSafe("range_years", 1422, 2008)
  updateNumericRangeInputSafe("ref_period", 1422, 2008)
  updateNumericRangeInputSafe("ref_period2", 1422, 2008)
  updateNumericRangeInputSafe("ref_period_v1", 1422, 2008)
  updateNumericRangeInputSafe("ref_period_v2", 1422, 2008)
  updateNumericRangeInputSafe("ref_period_6", 1422, 2008)
  
  updateNumericRangeInputSafe("range_longitude", -180, 180)
  updateNumericRangeInputSafe("range_longitude2", -180, 180)
  updateNumericRangeInputSafe("range_longitude_v1", -180, 180)
  updateNumericRangeInputSafe("range_longitude_v2", -180, 180)
  updateNumericRangeInputSafe("range_longitude_6", -180, 180)

  updateNumericRangeInputSafe("range_latitude", -90, 90)
  updateNumericRangeInputSafe("range_latitude2", -90, 90)
  updateNumericRangeInputSafe("range_latitude_v1", -90, 90)
  updateNumericRangeInputSafe("range_latitude_v2", -90, 90)
  updateNumericRangeInputSafe("range_latitude_6", -90, 90)

  updateNumericRangeInputSafe("lag_years_6", -100, 100)
  updateNumericRangeInputSafe("year_range_sources", 1421, 2009)
  
  #Single Year inputs update
  observe({
    if (!is.na(input$range_years_sg)) {
      shinyWidgets::updateNumericRangeInput(
        inputId = "range_years",
        value   = c(input$range_years_sg, input$range_years_sg)
      )
    }
  })
  
  observe({
    if (!is.na(input$ref_period_sg)) {
      shinyWidgets::updateNumericRangeInput(
        inputId = "ref_period",
        value   = c(input$ref_period_sg, input$ref_period_sg)
      )
    }
  })
  
  observe({
    if (!is.na(input$ref_period_sg2)) {
      shinyWidgets::updateNumericRangeInput(
        inputId = "ref_period2",
        value   = c(input$ref_period_sg2, input$ref_period_sg2)
      )
    }
  })
  
  observe({
    if (!is.na(input$ref_period_sg_6)) {
      shinyWidgets::updateNumericRangeInput(
        inputId = "ref_period_6",
        value   = c(input$ref_period_sg_6, input$ref_period_sg_6)
      )
    }
  })
  
  observe({
    if (!is.na(input$ref_period_sg_v1)) {
      shinyWidgets::updateNumericRangeInput(
        inputId = "ref_period_v1",
        value   = c(input$ref_period_sg_v1, input$ref_period_sg_v1)
      )
    }
  })
  
  observe({
    if (!is.na(input$ref_period_sg_v2)) {
      shinyWidgets::updateNumericRangeInput(
        inputId = "ref_period_v2",
        value   = c(input$ref_period_sg_v2, input$ref_period_sg_v2)
      )
    }
  })

  ### Stop App on end of session ----
  session$onSessionEnded(function() {
    stopApp()
  })
  # Close server function ----
}
