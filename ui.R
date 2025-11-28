### ClimeApp ###
# Source for helpers ----
source("helpers.R")
source("popovers.R")
source("setup.R")


# Define UI ----

ui <- navbarPage(
  id = "nav1",
  
  shinyjs::useShinyjs(),  # Enable shinyjs
  
  # shiny.tictoc to track performance
  tags$script(
    src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
  ),
  
  # --- Navbar title with logos and version ---
  title = div(
    style = "display:inline-flex; align-items:center; gap:10px;",
    tags$img(
      src    = logo_src,
      id     = logo_id,
      height = "75px",
      width  = logo_width,
      style  = "height:75px; width:auto; margin-right:5px; display:inline-block;"
    ),
    span("(light)")
  ),
  
  # Global CSS injection for hiding Shiny errors
  tags$head(
    tags$style(HTML("
      .shiny-output-error { visibility: hidden !important; }
      .shiny-output-error:before { visibility: hidden !important; }
    "))
  ),
  
  # JavaScript: screen dimension tracking
  tags$head(
    tags$script(HTML('
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
      });
    '))
  ),
  
  # --- Head content: Favicon, JS for resizing, JS for collapsing, CSS fixes ---
  # header = tagList(
  #   tags$head(
  #     # Set window icon
  #     tags$link(rel = "icon", type = "image/png", href = "pics/Logo_Favicon.png"),
  #     
  #     # JavaScript: navbar toggle tracking
  #     tags$script(HTML('
  #       $(document).ready(function() {
  #         $(".navbar-toggle").click(function() {
  #           $(".navbar").toggleClass("collapsed-menu");
  #         });
  #       });
  #     ')),
  #     
  #     # Offset for fixed-top navbar
  #     tags$style(HTML("body { padding-top: 90px; }"))
  #   )
  # ),
  
  header = tagList(
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "pics/Logo_Favicon.png"),
      tags$script(HTML('
      $(document).ready(function() {
        $(".navbar-toggle").click(function() {
          $(".navbar").toggleClass("collapsed-menu");
        });
      });
    ')),
      tags$style(HTML("
      body { padding-top: 90px; }
      .navbar-header {
        line-height: 0 !important;
      }
      .navbar-header .navbar-brand {
        line-height: 0 !important;
      }
    "))
    )
  ),
  
  footer = div(
    class = "navbar-footer",
    style = "display: inline;",
    # Oeschger Centre
    a(
      href = "https://www.oeschger.unibe.ch/",
      target = "_blank",
      style = "text-decoration: none; border: none;",
      img(
        src = 'pics/oeschger_logo_rgb.jpg',
        id = "ClimeApp3",
        height = "100px",
        width = "100px",
        style = "margin-top: 20px; margin-bottom: 20px;"
      )
    ),
    # ERC
    a(
      href = "https://erc.europa.eu/homepage",
      target = "_blank",
      style = "text-decoration: none; border: none;",
      img(
        src = 'pics/LOGO_ERC-FLAG_EU_.jpg',
        id = "ClimeApp4",
        height = "100px",
        width = "141px",
        style = "margin-top: 20px; margin-bottom: 20px;"
      )
    ),
    # Schweizerische Eidgenossenschaft
    a(
      href = "https://www.admin.ch/gov/de/start.html",
      target = "_blank",
      style = "text-decoration: none; border: none;",
      img(
        src = 'pics/WBF_SBFI_EU_Frameworkprogramme_E_RGB_pos_quer.jpg',
        id = "ClimeApp5",
        height = "100px",
        width = "349px",
        style = "margin-top: 20px; margin-bottom: 20px;"
      )
    ),
    # Schweizerischer Nationalfonds
    a(
      href = "https://www.snf.ch/de",
      target = "_blank",
      style = "text-decoration: none; border: none;",
      img(
        src = 'pics/SNF_Logo_Logo.png',
        id = "ClimeApp6",
        height = "75px",
        width = "560px",
        style = "margin-top: 20px; margin-bottom: 20px;"
      )
    ),
  ),
  
  
  
  # --- Navbar styling and properties ---
  theme = bslib::bs_theme(version = 5, bootswatch = "united", primary = "#094030", navbar_bg = "#094030"),
  position = "fixed-top",
  windowTitle = "ClimeApp",
  collapsible = TRUE,
  
  # Welcome START ----                             
  tabPanel("Welcome", value = "tab0",
           includeCSS("www/custom.css"),
           sidebarLayout(
             
             ## Sidebar Panels START ----          
             sidebarPanel(verticalLayout(
               ### First Side bar ----
               sidebarPanel(fluidRow(
                 
                 #Short description of the General Panel        
                 h1("Welcome to ClimeApp", style = "color: #094030;"),
                 br(),
                 h4(em(helpText("Created by Niklaus Bartlome & Richard Warren."))),
                 h4(em(helpText("Co-developed by No\u00E9mie Wellinger & Tanja Falasca."))),
                 br(),
                 h4("Data processing tool for the state-of-the-art ModE-RA global climate reanalysis", style = "color: #094030;"),
                 h5(helpText("V. Valler, J. Franke, Y. Brugnara, E. Samakinwa, R. Hand, E. Lundstad, A.-M. Burgdorf, L. Lipfert, A. R. Friedman, S. Br\u00F6nnimann, 2024")),
                 
                 
                 column(width = 12,
                        fluidRow(
                          column(width = 6, h5(strong("Date range:", style = "color: #094030;"))),
                          column(width = 6, h5(helpText("1422 to 2008 AD"))),
                          column(width = 6, h5(strong("Geographic range:", style = "color: #094030;"))),
                          column(width = 6, h5(helpText("Global"))),
                          column(width = 6, h5(strong("Resolution:", style = "color: #094030;"))),
                          column(width = 6, h5(helpText("Longitude = 1.875\u00B0")),
                                 h5(helpText("Latitude = 1.865\u00B0")))
                        )),
                 
                 column(width = 12, br()),
                 
                 # Note to ClimeApp Light
                 column(
                   width = 12,
                   style = "border: 5px solid #094030; padding: 10px; border-radius: 5px; color: #094030; background-color: white;",
                   h5(strong("Notice", style = "color: #094030;")),
                   h5(
                     "This is ", strong("ClimeApp Light."), "Initial plots might take longer to load. Download the full desktop version ",
                     actionLink("go_desktop2", tags$strong("here."))
                   )
                 ), 
                 
                 column(width = 12, br()),
                 
                 column(width = 12,

                        h5(strong("Variables:", style = "color: #094030;")),
                        h5(helpText("Temperature = air temperature at 2m [\u00B0C]")),
                        h5(helpText("Precipitation = total monthly precipitation [mm]")),
                        h5(helpText("SLP = sea level pressure [hPa]")),
                        h5(helpText("Z500 = 500 hPa geopotential height [m]"))
                 ),
                 
                 column(width = 12, br()),
                 
                 column(width = 12,
                        
                        h5(strong("User information:", style = "color: #094030;")),
                        h5(helpText("To receive additional information for certain options, click question mark icons of UI elements, e.g. Anomalies.")),
                        h5(helpText("Additional features and subordinate options are initially hidden but can be made visible by clicking or ticking the respective elements.")),
                        h5(helpText("ClimeApp updates instantly when inputs are changed or new values are selected. Customizations have to be added manually.")),
                        h5(helpText("Wait until the loading symbol is gone or the new plot is rendered before changing further values."))
                 )
                 
               ), width = 12),
               
               br(),
               
               ### Second side bar ----
               
               sidebarPanel(fluidRow(
                 h5("For feedback and suggestions on ClimeApp, please contact the ClimeApp team:"),
                 br(),
                 h5(tags$span(id = "email1", style = "color: #094030;")),
                 column(width = 12, br()),
                 h5("For queries relating to the ModE-RA data, please contact J\u00F6rg Franke:"),
                 h5(tags$span(id = "email2", style = "color: #094030;")),
                 column(width = 12, br()),
                 h5("To report technical issues, please contact the Linux support team:"),
                 h5(tags$span(id = "email3", style = "color: #094030;")),
                 
                 # JavaScript to obfuscate email addresses
                 tags$script(HTML('
                document.addEventListener("DOMContentLoaded", function() {
                  var email1 = ["climeapp.hist", "unibe.ch"];
                  var email2 = ["joerg.franke", "unibe.ch"];
                  var email3 = ["linux.support.giub", "unibe.ch"];
                  
                  var email1_link = "<a href=\'mailto:" + email1[0] + "@" + email1[1] + "\'>" + email1[0] + "@" + email1[1] + "</a>";
                  var email2_link = "<a href=\'mailto:" + email2[0] + "@" + email2[1] + "\'>" + email2[0] + "@" + email2[1] + "</a>";
                  var email3_link = "<a href=\'mailto:" + email3[0] + "@" + email3[1] + "\'>" + email3[0] + "@" + email3[1] + "</a>";
                  
                  document.getElementById("email1").innerHTML = email1_link;
                  document.getElementById("email2").innerHTML = email2_link;
                  document.getElementById("email3").innerHTML = email3_link;
                });
              '))
                 
               ), width = 12)
               
               ## Sidebar Panels END ---- 
             )),
             ## Main panel START ----
             mainPanel(
               ### Tabs Start ----
               tabsetPanel(id = "tabset0",
                           #### Tab Welcome ----
                           tabPanel("Welcome",
                                    br(),
                                    tags$video(
                                      id = "welcome_video",
                                      class = "responsive-video",
                                      controls = NA,
                                      poster = "pics/welcome_map.jpg",
                                      tags$source(src = "videos/intro.mp4", type = "video/mp4"),
                                      "Your browser does not support the video tag."
                                    ),
                                    br(),br(),
                                    
                                    fluidRow(
                                      tags$a(
                                        href   = "https://climeapp.github.io/ClimeApp_Tutorials/",
                                        target = "_blank",
                                        class  = "btn btn-primary",
                                        style  = "width:200px; margin-right:20px; margin-left:20px;",
                                        "Go to video tutorials"
                                      ),
                                      
                                      actionButton(
                                        "go_desktop", 
                                        "Download ClimeApp desktop version",
                                        class = "btn btn-primary",
                                        style = "width:300px;"
                                      )
                                    ),
                                    
                                    br(), br(), br(),
                                    
                                    h4("For more information on the ModE datasets and ClimeApp please see:", style = "color: #094030;"),
                                    h5(helpText(a("ModE-RA - a global monthly paleo-reanalysis of the modern era 1421 to 2008", href = "https://doi.org/10.1038/s41597-023-02733-8"), br(), a("ModE-RAclim - a version of the ModE-RA reanalysis with climatological prior for sensitivity studies", href = "https://www.wdc-climate.de/ui/entry?acronym=ModE-RAc"), br(),  a("ModE-Sim – a medium-sized atmospheric general circulation model (AGCM) ensemble to study climate variability during the modern era (1420 to 2009)", href = "https://gmd.copernicus.org/articles/16/4853/2023/"), br(), a("ClimeApp: data processing tool for monthly, global climate data from the ModE-RA palaeo-reanalysis, 1422 to 2008 CE", href = "https://doi.org/10.5194/cp-20-2645-2024"))),
                                    h4("To cite, please reference:", style = "color: #094030;"),
                                    h5(helpText("R. Warren, N. Bartlome, N. Wellinger, J. Franke, R. Hand, S. Br\u00F6nnimann, H. Huhtamaa: ClimeApp: data processing tool for monthly, global climate data from the ModE-RA palaeo-reanalysis, 1422 to 2008 CE. CP 20, 2645–2662 (2024).")),
                                    h5(helpText("V. Valler, J. Franke, Y. Brugnara, E. Samakinwa, R. Hand, E. Lundstad, A.-M. Burgdorf, L. Lipfert, A. R. Friedman, S. Br\u00F6nnimann: ModE-RA: a global monthly paleo-reanalysis of the modern era 1421 to 2008. Scientific Data 11 (2024).")),
                                    h5(helpText("R. Hand, E. Samakinwa, L. Lipfert, and S. Br\u00F6nnimann: ModE-Sim – a medium-sized atmospheric general circulation model (AGCM) ensemble to study climate variability during the modern era (1420 to 2009). GMD 16 (2023).")),
                                    h5("Funding:", style = "color: #094030;"),
                                    h6(helpText("PALAEO-RA: H2020/ERC grant number 787574")),
                                    h6(helpText("DEBTS: SNSF grant number PZ00P1_201953")),
                                    h6(helpText("VolCOPE: SERI contract number MB22.00030")),
                           ),
                           #### Tab ModE data ----
                           tabPanel("ModE data",
                                    br(), br(), 
                                    helpText(a("ModE-RA (ensemble members and statistics)", href = "https://doi.org/10.26050/WDCC/ModE-RA_s14203-18501"), "and", a("ModE-RAclim (ensemble statistics)", href = "https://doi.org/10.26050/WDCC/ModE-RAc_s14203-18501"), "are uploaded to NOAA and to the World Data Center for Climate (WDCC) at Deutsches Klimarechenzentrum in Hamburg, Germany. The two climate reconstructions are in NetCDF4 format; the NetCDF4 files cover the whole period per variable. Ensemble statistics include the mean (monthly anomalies with respect to the period 1901 to 2000), maximum, minimum and spread in terms of one standard deviation from the ensemble mean. The observation feedback archive files are available in tsv format (one file per 6 months), which contain all relevant information of the input data, how the input data were processed, and useful feedback information from the DA system.", a("A detailed list of all information", href = "https://www.wdc-climate.de/ui/entry?acronym=ModE-RA_info"), "stored in the feedback archive has been published with the dataset."),
                                    br(), br(), 
                                    helpText("The ModE-RA paleo-reanalysis is identical to the ModE-Sim simulations in areas far away from any assimilated observations, especially at the beginning of the reconstruction period. With time, more and more observations are available, suggesting that the reconstruction becomes more skillful. Therefore, the users first should ensure how reliable the paleo-reanalysis is for a given region and time period. This can be achieved by looking at the ensemble spread and the differences between ModE-Sim and ModE-RA. Among the reconstructed variables, the ones with observational input data are the most realistically estimated. We encourage the users to make use of the ensemble members and not only the ensemble mean."), 
                                    br(), br(), 
                                    helpText("ModE-Sim was generated in two phases (1420–1850 and 1850–2009) with different boundary conditions. In the earlier period, ModE-RA is based on ModE-Sim Set 1420-3, and in the later period on ModE-Sim Set 1850-1. ModE-RA is not split into the two periods of the ModE-Sim prior because the assimilated observational time series lead to a smooth transition between the two periods of the ModE-Sim sets."),
                                    br(), br(), 
                                    helpText("ModE-RA was generated by transforming both model simulations and observations to 71-year running anomalies. Hence, users should be aware that the centennial-scale variability is the model response to forcings. Therefore, we see great potential for future research, particularly in terms of intra-annual to multi-decadal variability. We provide monthly anomalies with respect to the 1901 to 2000 climatology and the model climatology for the 1901 to 2000 period. Be aware that the model climatology includes model biases. Therefore, we recommend using anomalies instead of absolute values."),
                                    br(), br(), 
                                    helpText("Furthermore, because of the employed setup, unrealistic values (such as negative precipitation) can occur if absolute values are generated by adding back a climatology. This is especially an issue in arid regions where monthly precipitation is not normally distributed. Precipitation is consistent in the periods of 1421–1800 and 1900–2009 when the observational network is quite stable, but in the 19th century, when many of the observation time series start, a trend is introduced in some arid land regions and tropical oceans. Hence, in the case of the reconstructed precipitation fields, the early and late period should be looked at separately."),
                                    br(), br(),
                                    helpText("ModE-RAclim should be seen as a sensitivity study and is only a side product of the project. ModE-RAclim does not contain centennial scale climate variability. For most users, the main product ModE-RA therefore should be used for regular studies on past climate. The main differences between ModE-RAclim and ModE-RA are on the model side: ModE-RAclim uses 100 randomly picked years from ModE-Sim as a priori state. Thereby, stationarity in the covariance structure is assumed, and the externally-forced signal in the model simulations is eliminated. In combination with ModE-Sim and ModE-RA it can be used to distinguish the forced and unforced parts of climate variability seen in ModE-RA."),
                                    br(), br(),
                                    helpText("ModE-RA makes use of several data compilations and assimilates various direct and indirect sources of past climate compared to 20CRv3. Hence, if monthly resolution is sufficient for the planned study, ModE-RA may have higher quality already from 1850 backwards to analyze past climate changes and can be viewed as the backward extension of 20CRv3."),
                                    br(), br(),
                                    h6(helpText("(Cf. ModE-RA paper", a("Usage notes.", href = "https://www.nature.com/articles/s41597-023-02733-8#Sec13"),")")),
                           ),
                           #### Tab ClimeApp functions ----
                           tabPanel("ClimeApp functions",
                                    
                                    br(), br(),
                                    h5(strong("Anomalies:", style = "color: #094030;")),
                                    helpText("The anomaly map function shows the spatial distribution of climate anomalies averaged over a user-selected year range and month range. For example, June, July, August (JJA), 1501 to 1600 if your focus is boreal summer in the 16th century. The anomalies are created from 3 data products:"),
                                    br(),
                                    tags$ol(
                                      tags$li(em("Annual Means"), "– a timeseries of annual means for each point on the map, created by averaging absolute ModE values across the selected month range."),
                                      tags$li(em("Reference Means"), "– a single reference mean for each point on the map, created by averaging annual means across a chosen reference year range."),
                                      tags$li(em("Annual Anomalies"), "– a timeseries of annual anomalies for each point on the map, created by subtracting the", em("reference means"), "from the", em("annual means."))
                                    ),
                                    helpText("The final anomalies shown are the time-averaged annual anomalies. These are plotted using the base R plotting functions along with the coastlines and borders from the", em("maps"), "package.  The anomaly timeseries is generated by averaging the annual anomalies for each year. See Appendix 2. for the specific calculations behind each data product."),
                                    br(),
                                    helpText("For reference, the calculations behind each data product are as follows:"),
                                    br(),
                                    helpText("The", em("annual mean"), "for a single year and single point on the map is given by the equation"),
                                    br(), withMathJax("$$Annual \\ Mean = \\overline{Absolute \\ Values \\ (M)}$$"), br(),
                                    helpText("where \\(\\ (M) \\) is the selected month range."),
                                    br(), br(),
                                    helpText("The", em("reference mean"), "for a single year and point is given by"),
                                    br(), withMathJax("$$ Reference \\ Mean = \\overline{Annual \\ Means \\ (Y_{\t{ref}})}$$"), br(),
                                    helpText("where \\(\\ Y_{\t{ref}} \\) is the selected reference year range."),
                                    br(), br(),
                                    helpText("The", em("annual anomaly"), "for a single year and point is given by:"),
                                    br(), withMathJax("$$ Annual \\ Anomaly = Annual \\ Mean - Reference \\ Mean$$"),
                                    br(), br(),
                                    helpText("Note that in the case of ModE-RAclim, the base data is already in anomaly format, so anomalies are merely calculated by subtracting time-averaged anomalies from each other."),
                                    br(), br(),
                                    helpText("The anomalies presented on the anomaly map and in the anomaly map data are given by"),
                                    br(), withMathJax("$$ Anomaly \\ (map) = \\overline{Annual \\ Means \\ (Y)} - \\ Reference \\ Mean = \\overline{Annual \\ Anomalies \\ (Y)}$$"), br(),
                                    helpText("where \\(Y \\) is the selected year range."),
                                    br(), br(),
                                    helpText("Anomalies presented on the timeseries map and timeseries data are given by"),
                                    br(), withMathJax("$$ Anomaly \\ (timeseries) = \\ (Annual \\ Means \\ (Lon, \\ Lat)) - \\ (Reference \\ Means \\ (Lon, \\ Lat)) = \\ (Annual \\ Anomalies \\ (Lon, \\ Lat))$$"), br(),
                                    helpText("where", em("Lon"), "and", em("Lat"), "are the selected longitude and latitude range."),
                                    
                                    tags$hr(),
                                    
                                    br(), br(), 
                                    h5(strong("Composites:", style = "color: #094030;")),
                                    helpText("ClimeApp’s composite maps show the time-averaged anomalies for a set of non-consecutive years, which can be entered or uploaded by the user. The anomaly reference period can be a fixed set of consecutive years, a custom set of non-consecutive years or an individual reference period generated for each year based on the X (a number of years chosen by the user) years prior. Calculations and plotting are performed in the same way as for anomalies, except for anomalies compared to X years prior (XYP):"),
                                    br(),
                                    tags$ol(
                                      tags$li(em("XYP Reference Means"), "– a set of reference means for each point on the map, one for each user-selected year. Calculated by averaging the X preceding", em("annual means.")),
                                      tags$li(em("XYP Annual Anomalies"), "– a set of annual anomalies for each point on the map. Created by subtracting the corresponding", em("reference mean"), "from each", em("annual mean.")),
                                    ),
                                    helpText("To give an indication of the consistency of anomalies over the set of years in the composite, ClimeApp contains a ‘% sign match’ statistical tool. This marks regions where the", em("annual anomalies"), "that form the composite agree in their sign more often than a user-defined threshold, given in percent. For example, for a composite of five years, with anomalies of -1°C, -5°C, 1°C, 15°C and -3°C, the displayed mean would be a positive 1.4°C, but only 40% of the years would match this, since 3 are in fact negative."),
                                    
                                    tags$hr(),
                                    
                                    br(), br(), 
                                    h5(strong("Correlation:", style = "color: #094030;")),
                                    helpText("The correlation function allows users to generate a map of correlation coefficients, comparing either ModE variables or user-uploaded timeseries. 
                              Using the", em("cor()"), "function from the", em("stats"), "R package (R Core Team, 2022), it can employ either the Pearson or Spearman’s Ranks correlation method. 
                              If both variables are in ‘field’ format, i.e. gridded map data, it performs a timeseries correlation of the", em("annual means"), " for each point on the map with the corresponding", em("annual means"), " for the second variable. 
                              If one variable is a timeseries however, it correlates each set of", em("annual means"), " with the same timeseries. 
                              In addition to the map, ClimeApp also produces a correlation timeseries, showing an annual timeseries of both variables (spatially averaged in the case of ModE variables) and a single correlation coefficient and p-value, calculated from those timeseries. 
                              The p-value shows the probability that the correlation was produced by random chance rather than an actual relationship between the variables. 
                              p < 0.05 is generally recommended for drawing legitimate conclusions."),
                                    
                                    tags$hr(),
                                    
                                    br(), br(), 
                                    h5(strong("Regression:", style = "color: #094030;")),
                                    helpText("In ClimeApp, regression operates in a similar way to correlation, performing a multiple linear regression analysis on a set of", em("annual means"), ". Using", em("lm()"), "from the", em("stats"), "R package, one or more independent variable timeseries are fitted to the dependent variable timeseries for each point on the map according to the model"),
                                    br(), withMathJax("$$ V_{\t{Dependent}} = \\beta_1 V_{\t{Independent \\ 1}} + \\beta_2 V_{\t{Independent \\ 2}} + ... + \\ Residual$$"), br(),
                                    helpText("where β is the coefficient and α is the intercept. ClimeApp then plots the spatial average of the dependent variable \\(\\ \\beta_1 V_{\t{Independent \\ 1}} + \\beta_2 V_{\t{Independent \\ 2}} + ... \\) and residual as a timeseries. Provided the dependent variable is a field, maps of the coefficients for each independent variable can be produced, as can maps of the p-values and residuals for each year."),
                                    tags$hr(),
                                    
                                    br(), br(), 
                                    h5(strong("Annual Cycles:", style = "color: #094030;")),
                                    helpText("This function shows the spatially averaged monthly ModE values over a given year or set of years. In the case of a set of years, these can be presented individually or as an average."),
                                    tags$hr(),
                                    
                                    br(), br(),
                                    h5(strong("Explore ModE-RA sources:", style = "color: #094030;")),
                                    helpText("The interactive map shows information on all the sources used to create ModE-RA and ModE-RAclim. Source data should include the study or database that observations were sourced from, along with supplementary information. A rounding algorithm was used to identfy each study based on source type and location, so there is a small chance that some data sources may have been mis-attributed. Please report any errors or omissions to the ClimeApp development team."),
                                    tags$hr(),
                                    
                                    br(), br(),
                                    h5(strong("Source Analysis and Further Statistical Functions:", style = "color: #094030;")),
                                    helpText("The accuracy of ModE-RA is dependent on the availability and reliability of observations to constrain the model ensemble of ModE-Sim. To capture this, ClimeApp includes tools for visualizing the sources used to create ModE-RA and ModE-RAclim and the standard deviation (SD) ratio of the ModE-RA and ModE-Sim ensembles. The ModE-RA sources are presented as a semi-annual map showing the data points assimilated for each half-year, grouped by type and variable. This allows the user to see where proxy, documentary or instrumental observations were integrated into the reconstruction and any gaps in the data. The SD ratio meanwhile, is the standard deviation of the ModE-Sim ensemble divided by the standard deviation of ModE-RA after the assimilation of observations:"),
                                    br(), withMathJax("$$ SD \\ ratio = \\frac{\\sigma_{ModE-RA \\ Ensemble}}{\\sigma_{ModE-SIM \\ Ensemble}}$$"), 
                                    helpText("This gives a value between 0 and 1 for each month and grid point, with 1 showing no constraint (i.e. the ModE-RA output is the same as that of ModE-Sim and entirely generated from the models) and lower values showing increasing constraint by observations, meaning there are either more observations or that they are more ‘trusted’ by the reconstruction. The temporal mean of the SD ratio can be presented in ClimeApp as a contour map or grid-point overlay on the anomaly maps."),
                                    br(), 
                                    helpText("On timeseries plots, users have the option to add percentiles and moving averages. The moving averages are calculated using a rolling mean of timeseries values over a number of years selected by the user (default 11). To generate the percentiles, a Shapiro-Wilk test (Shapiro and Wilk, 1965) is first conducted on the timeseries data. If the data is normally distributed, which is rare for ModE timeseries, then percentiles are calculated from the mean and standard deviation of the timeseries using the", em("qnorm()"), "function from the", em("stats"), "package. If the distribution is non-normal, ClimeApp instead finds the value corresponding to the quantile matching the users selection (i.e. for the 0.95 percentile, it returns values that 5% of all values are above/below), using the", em("quantile()"), "function from the stats package."),
                                    
                           ),
                           
                           #### Offline version ----
                           tabPanel("ClimeApp desktop", value = "desktop_tab",
                                    br(), br(),
                                    h5(strong("An offline version ClimeApp for Windows - CLIMEAPP DESKTOP - is available to download here:", style = "color: #094030;")),
                                    
                                    h5(helpText("To access all functionalities of ClimeApp, download the offline version. The offline version includes the following additional features:")),
                                    tags$ul(
                                      tags$li("Regression analysis"),
                                      tags$li("Annual cycle analysis"),
                                      tags$li("Custom statistics (SD ratio, % sign match)"),
                                      tags$li("Pre-loaded and pre-processed data")
                                    ),
                                    br(),
                                    downloadButton("climeapp_desktop_download",
                                                   label = "Download ClimeApp desktop"),
                                    br(), br(), br(),
                                    
                                    h5(strong("ClimeApp desktop user guide", style = "color: #094030;")),
                                    br(),
                                    htmltools::includeHTML("ClimeApp Desktop User Guide.html")
                                    
                           ),
                           
                           #### Tab Version History ----
                           tabPanel("Version history",
                                    br(), br(),
                                    h5(strong("ClimeApp (light) (28.11.2025)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Creation of a light and more stable version for online use"),
                                      tags$li("Relocation of regression, annual cycles and statistics features to desktop version"),
                                      tags$li("Removal of pre-loaded data"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_Light/tree/", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("v1.5 (28.11.2025)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Addition of welcome video and tutorials"),
                                      tags$li("Balanced library loading"),
                                      tags$li("Plot caching"),
                                      tags$li("Improved map plotting function"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_development/tree/ClimeApp_v1.5", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("v1.4 (18.07.2025)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("General overhaul of ClimeApp"),
                                      tags$li("Addition of Superposed Epoch Analysis (SEA)"),
                                      tags$li("New maps and timeseries design using ggplot2"),
                                      tags$li("Additional map customization options: font size, projection, greying out land or ocean"),
                                      tags$li("Map customization now also available for correlation and regression analysis"),
                                      tags$li("Export of map data as georeferenced TIFF"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_development/tree/ClimeApp_v1.4", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("v1.3 (19.07.2024)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Fixed the depiction of historical proxies on ModE-RA source plots"),
                                      tags$li("Changed ModE-RA source plots to only show the number of sources"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_development/tree/ClimeApp_v1.3", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("v1.2 (04.07.2024)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Preprocessed data for all datasets (Mode-Sim, Mode-R-Clim and Mode-RA) to speed loading time"),
                                      tags$li("Overhaul of Mode-RA source plots to allow exploration and access to detailed source data"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_development/tree/ClimeApp_v1.2", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("v1.1 (04.04.2024)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Option to add GeoPackage-Layers (shape files) on top of field plots"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_development/tree/ClimeApp_v1.1", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("v1.0 (11.03.2024)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Download / Upload option for metadata"),
                                      tags$li("Information panel for ClimeApp functions"),
                                      tags$li("Helptext as popovers for UI elements"),
                                      tags$li(a("View code on GitHub", 
                                                href = "https://github.com/ClimeApp/ClimeApp_development/tree/ClimeApp_v1.0", 
                                                target = "_blank")),
                                    ),
                                    br(),
                                    h5(strong("Beta v0.6 (15.02.2024)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Improved UI (i.e. Hide/Show country borders, Rearranged download sections"),
                                      tags$li("Switch to Annual Cycle when a single year is selected"),
                                      tags$li("Download ModE-RA source data as table"),
                                      tags$li("Loading symbols during plot generation"),
                                    ),
                                    br(),
                                    h5(strong("Beta v0.5 (22.12.2023)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Download NetCDF files"),
                                      tags$li("Version history"),
                                    ),
                                    br(),  
                                    h5(strong("Beta v0.4", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Select single years")
                                    ),
                                    br(), 
                                    h5(strong("Beta v0.3", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Timeseries customization"),
                                      tags$li("Percentiles, maps & statistics based on model constraint change"),
                                      tags$li("Reference line option in timeseries")
                                    ),
                                    br(), 
                                    h5(strong("Beta v0.2 (10.11.2023)", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("Use ModE-Sim and ModE-RAclim data"),
                                      tags$li("Create annual cycles method"),
                                      tags$li("View ModE-RA sources"),
                                      tags$li("Download ModE-RA sources maps as image"),
                                      tags$li("Upload User data for correlation and regression"),
                                      tags$li("Reference maps with absolute values, Reference values, and SD ratio for Anomalies and Composites")
                                    ),
                                    br(),  
                                    h5(strong("Beta", style = "color: #094030;")),
                                    tags$ul(
                                      tags$li("First running version online"),
                                      tags$li("Use ModE-RA data with four variables:  Temperature, Precipitation, Sea level pressure, Pressure at 500 hPa geopotential height"),
                                      tags$li("Calculate Anomalies, Composites, Correlations and Regressions (coefficien, p values residuals) as maps and timeseries"),
                                      tags$li("Customize maps and timeseries (title, labelling, add custom points and highlights, statistics)"),
                                      tags$li("Download maps and timeseries plots as images"),
                                      tags$li("Download map and timeseries data in xlsx or csv format")
                                    ),
                           ),
                           
                           ### Tabs END ----
               )            
               ## Main Panel END ----
             )
             # Welcome END ----  
           )),
  # Anomalies START ----                             
  tabPanel("Anomalies", value = "tab1",
           shinyjs::useShinyjs(),
           sidebarLayout(
             
             ## Sidebar Panels START ----          
             sidebarPanel(verticalLayout(
               
               ### First Sidebar panel (Variable and dataset selection) ----
               sidebarPanel(fluidRow(
                 #Method Title and Pop Over
                 anomalies_summary_popover("pop_anomalies"),
                 
                 br(),
                 
                 #Short description of the selection options        
                 h4("Select dataset and variable", style = "color: #094030;", dataset_variable_popover("pop_anomalies_datvar")),
                 
                 #Choose one of three datasets (Select)                
                 selectInput(inputId  = "dataset_selected",
                             label    = "Choose a dataset:",
                             choices  = c("ModE-RA", "ModE-Sim","ModE-RAclim"),
                             selected = "ModE-RA"),
                 
                 #Choose one of four variable (Select)                
                 selectInput(inputId  = "variable_selected",
                             label    = "Choose a variable to plot:",
                             choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
                             selected = "Temperature"),
                 
               ), width = 12), br(),
               
               ### Second Sidebar panel (Time selection) ----
               sidebarPanel(fluidRow(
                 
                 #Short description of the temporal selection        
                 h4("Select a year range, season and reference period", style = "color: #094030;",year_season_ref_popover("pop_anomalies_time")),
                 
                 #Choose your year of interest        
                 shinyjs::hidden(
                   shinyWidgets::numericRangeInput(inputId    = "range_years",
                                     label     = "Select the range of years (1422-2008):",
                                     value     = initial_year_values,
                                     separator = " to ",
                                     min       = 1422,
                                     max       = 2008)),
                 
                 #Choose single year
                 column(12,
                        checkboxInput(inputId = "single_year",
                                      label   = "Select single year",
                                      value   = FALSE)),
                 
                 
                 shinyjs::hidden(
                   numericInput(inputId   = "range_years_sg",
                                label     = "Select the single year:",
                                value     = NA,
                                min       = 1422,
                                max       = 2008,
                                updateOn = "blur")),
                 
                 #Choose Season, Year or Months
                 radioButtons(inputId  = "season_selected",
                              label    = "Select the range of months:",
                              choices  = c("Annual", "DJF", "MAM", "JJA", "SON", "Custom"),
                              selected = "Annual" , inline = TRUE),
                 
                 #Choose your range of months (Slider)
                 shinyjs::hidden(
                   div(id = "season",
                       shinyWidgets::sliderTextInput(inputId = "range_months",
                                       label = "Select custom months:",
                                       choices = c("December (prev.)", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                                       #Initially selected = 1 year (annual mean)
                                       selected = c("January", "December")),
                   )),      
                 
                 #Choose reference period
                 shinyjs::hidden(
                   shinyWidgets::numericRangeInput(inputId = "ref_period",
                                     label      = "Select the reference period:",
                                     value      = c(1961,1990),
                                     separator  = " to ",
                                     min        = 1422,
                                     max        = 2008)),
                 
                 #Choose single ref year
                 column(12,
                        checkboxInput(inputId = "ref_single_year",
                                      label   = "Select single year",
                                      value   = FALSE)),
                 
                 
                 shinyjs::hidden(
                   numericInput(inputId   = "ref_period_sg",
                                label     = "Select the reference year:",
                                value     = NA,
                                min       = 1422,
                                max       = 2008,
                                updateOn = "blur")),
                 
               ), width = 12), br(),
               
               ### Third sidebar panel (Location selection) ----
               sidebarPanel(fluidRow(
                 
                 #Short description of the Coord. Sidebar        
                 h4("Set geographical area", style = "color: #094030;"),
                 h5(helpText("Select a continent, enter coordinates manually or draw a box on the plot")),
                 
                 column(width = 12, fluidRow(
                   
                   br(), br(),   
                   
                   #Global Button
                   actionButton(inputId = "button_global",
                                label = "Global",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #Europe Button
                   actionButton(inputId = "button_europe",
                                label = "Europe",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #Asia Button
                   actionButton(inputId = "button_asia",
                                label = "Asia",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #Oceania Button
                   actionButton(inputId = "button_oceania",
                                label = "Oceania",
                                width = "110px"),
                   
                 )),
                 
                 column(width = 12, br()),
                 
                 column(width = 12, fluidRow(
                   
                   br(), br(),   
                   
                   #Africa Button
                   actionButton(inputId = "button_africa",
                                label = "Africa",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #North America Button
                   actionButton(inputId = "button_n_america",
                                label = "North America",
                                width = "150px"),
                   
                   br(), br(),
                   
                   #South America Button
                   actionButton(inputId = "button_s_america",
                                label = "South America",
                                width = "150px")
                 )),
                 
                 column(width = 12, br()),
                 
                 #Choose Longitude and Latitude Range          
                 shinyWidgets::numericRangeInput(inputId = "range_longitude",
                                   label = "Longitude range (-180 to 180):",
                                   value = initial_lon_values,
                                   separator = " to ",
                                   min = -180,
                                   max = 180),
                 
                 
                 #Choose Longitude and Latitude Range          
                 shinyWidgets::numericRangeInput(inputId = "range_latitude",
                                   label = "Latitude range (-90 to 90):",
                                   value = initial_lat_values,
                                   separator = " to ",
                                   min = -90,
                                   max = 90),
                 
                 #Enter Coordinates
                 column(12,
                        actionButton(inputId = "button_coord",
                                     label = "Update coordinates",
                                     width = "200px"),
                 ),
                 
               ), width = 12)
               ## Sidebar Panels END ----
             )),
             
             ## Main Panel START ----
             mainPanel(tabsetPanel(id = "tabset1",
                                   ### Map plot START ----   
                                   tabPanel("Map", br(),
                                            h4("Anomalies map", style = "color: #094030;"),
                                            
                                            shinycssloaders::withSpinner(ui_element = plotOutput("map", height = "750px", dblclick = "map_dblclick1", brush = brushOpts(id = "map_brush1",resetOnNew = TRUE)), 
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),
                                            
                                            conditionalPanel(
                                              condition = "input.location == 'VICES'",
                                              tags$img(
                                                src = "pics/no_image.jpg",
                                                id = "img_vices",
                                                height = "450",
                                                width = "600",
                                                style = "display:block; margin:0 auto;",
                                                loading = "lazy"
                                              )
                                            ),
                                            
                                            conditionalPanel(
                                              condition = "input.title1_input == 'ClimeApp'",
                                              tags$img(
                                                src = "pics/zero_image.jpg",
                                                id = "img_dev_team",
                                                height = "450",
                                                width = "600",
                                                style = "display:block; margin:0 auto;",
                                                loading = "lazy"
                                              )
                                            ),
                                            
                                            conditionalPanel(
                                              condition = "input.title2_input == 'Rüebli'",
                                              tags$img(
                                                src = "pics/zero_ruebli.jpg",
                                                id = "img_miau",
                                                height = "600",
                                                width = "338",
                                                style = "display:block; margin:0 auto;",
                                                loading = "lazy"
                                              )
                                            ),
                                            
                                            
                                            #### Customization panels START ----       
                                            fluidRow(
                                              #### Map customization ----       
                                              column(width = 4,
                                                     h4("Customize your map", style = "color: #094030;",map_customization_popover("pop_anomalies_cusmap")),  
                                                     
                                                     checkboxInput(inputId = "custom_map",
                                                                   label   = "Map customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_maps",
                                                           
                                                           selectInput(inputId = "projection",
                                                                       label = "Projection:",
                                                                       choices = c("UTM (default)", "Robinson", "Orthographic", "LAEA"),
                                                                       selected = "UTM (default)"),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_map_center",
                                                                 
                                                                 numericInput(inputId = "center_lat",
                                                                              label = "Center latitude:",
                                                                              value = 0,
                                                                              min = -90,
                                                                              max = 90,
                                                                              updateOn = "blur"),
                                                                 numericInput(inputId = "center_lon",
                                                                              label = "Center longitude:",
                                                                              value = 0,
                                                                              min = -180,
                                                                              max = 180,
                                                                              updateOn = "blur"
                                                                 ))),
                                                           
                                                           radioButtons(inputId  = "axis_mode",
                                                                        label    = "Axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input",
                                                                                   label      = "Set your axis values:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "hide_axis",
                                                                         label   = "Hide colorbar",
                                                                         value   = FALSE),
                                                           
                                                           br(),
                                                           
                                                           radioButtons(inputId  = "title_mode",
                                                                        label    = "Title customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_title",
                                                                 
                                                                 textInput(inputId     = "title1_input",
                                                                           label       = "Custom map title:", 
                                                                           width       = NULL,
                                                                           updateOn = "blur"),
                                                                 
                                                                 textInput(inputId     = "title2_input",
                                                                           label       = "Custom map subtitle (e.g. Ref-Period)",
                                                                           width       = NULL,
                                                                           updateOn = "blur"),
                                                                 
                                                                 numericInput(inputId = "title_size_input",
                                                                              label   = "Font size:",
                                                                              value   = 18,
                                                                              min     = 1,
                                                                              max     = 40,
                                                                              updateOn = "blur"))),
                                                           
                                                           br(), hr(),
                                                           
                                                           h4(helpText("Topography options and GIS upload (.shp)", map_customization_layers_popover("pop_anomalies_layers"))),
                                                           
                                                           checkboxInput(inputId = "custom_topo",
                                                                         label   = "Topographical customization",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_geo_options",
                                                                 
                                                                 checkboxInput(inputId = "hide_borders",
                                                                               label   = "Show country borders",
                                                                               value   = TRUE),
                                                                 
                                                                 checkboxInput(inputId = "white_ocean",
                                                                               label   = "Grey ocean",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "white_land",
                                                                               label   = "Grey land",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_rivers",
                                                                               label   = "Show rivers",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_rivers",
                                                                               label   = "Label rivers",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_lakes",
                                                                               label   = "Show lakes",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_lakes",
                                                                               label   = "Label lakes",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_mountains",
                                                                               label   = "Show mountains",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_mountains",
                                                                               label   = "Label mountains",
                                                                               value   = FALSE),
                                                                 
                                                             )),
                                                           
                                                           #Shape File Option
                                                           
                                                           fileInput(inputId = "shpFile", label = "Upload shapefile (ZIP)"),
                                                           
                                                           # Single sortable checkbox group input for selecting and ordering shapefiles
                                                           uiOutput("shapefileSelector"),
                                                           
                                                           uiOutput("colorpickers")
                                                           
                                                       )),
                                              ),
                                              
                                              #### Add Custom features (points and highlights) ----                        
                                              column(width = 4,
                                                     h4("Custom features", style = "color: #094030;",map_features_popover("pop_anomalies_mapfeat")),
                                                     
                                                     checkboxInput(inputId = "custom_features",
                                                                   label   = "Enable custom features",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_features",
                                                           radioButtons(inputId      = "feature",
                                                                        label        = "Select a feature type:",
                                                                        inline       = TRUE,
                                                                        choices      = c("Point", "Highlight")),
                                                           
                                                           #Custom Points
                                                           div(id = "hidden_custom_points",
                                                               h4(helpText("Add custom points",map_points_popover("pop_anomalies_mappoint"))),
                                                               
                                                               h6(helpText("Enter location/coordinates or double click on map")),
                                                               
                                                               textInput(inputId = "location", 
                                                                         label   = "Enter a location:",
                                                                         value   = NULL,
                                                                         placeholder = "e.g. Bern"),
                                                               
                                                               actionButton(inputId = "search",
                                                                            label   = "Search"),
                                                               
                                                               shinyjs::hidden(div(id = "inv_location",
                                                                                   h6(helpText("Invalid location"))
                                                               )),
                                                               
                                                               textInput(inputId = "point_label", 
                                                                         label   = "Point label:",
                                                                         value   = ""),
                                                               
                                                               column(width = 12, offset = 0,
                                                                      column(width = 6,
                                                                             textInput(inputId = "point_location_x", 
                                                                                       label   = "Point longitude:",
                                                                                       value   = "")
                                                                      ),
                                                                      column(width = 6,
                                                                             textInput(inputId = "point_location_y", 
                                                                                       label   = "Point latitude:",
                                                                                       value   = "")
                                                                      )),
                                                               
                                                               
                                                               radioButtons(inputId      = "point_shape",
                                                                            label        = "Point shape:",
                                                                            inline       = TRUE,
                                                                            choices      = c("\u25CF", "\u25B2", "\u25A0")),
                                                               
                                                               colourpicker::colourInput(inputId = "point_colour", 
                                                                                         label   = "Point colour:",
                                                                                         showColour = "background",
                                                                                         value = "#27408B",
                                                                                         palette = "limited"),  
                                                               
                                                               numericInput(inputId = "point_size",
                                                                            label   = "Point size:",
                                                                            value   = 1,
                                                                            min     = 1,
                                                                            max     = 10),
                                                               
                                                               column(width = 12,
                                                                      
                                                                      actionButton(inputId = "add_point",
                                                                                   label = "Add point"),
                                                                      br(), br(), br(),
                                                                      actionButton(inputId = "remove_last_point",
                                                                                   label = "Remove last point"),
                                                                      actionButton(inputId = "remove_all_points",
                                                                                   label = "Remove all points")),
                                                           ),
                                                           
                                                           #Custom Highlights
                                                           div(id = "hidden_custom_highlights",
                                                               h4(helpText("Add custom highlights",map_highlights_popover("pop_anomalies_maphl"))),
                                                               
                                                               h6(helpText("Enter coordinate or draw a box on map")),
                                                               
                                                               shinyWidgets::numericRangeInput(inputId = "highlight_x_values",
                                                                                 label  = "Longitude:",
                                                                                 value  = "",
                                                                                 min    = -180,
                                                                                 max    = 180),
                                                               
                                                               shinyWidgets::numericRangeInput(inputId = "highlight_y_values",
                                                                                 label  = "Latitude:",
                                                                                 value  = "",
                                                                                 min    = -90,
                                                                                 max    = 90),
                                                               
                                                               colourpicker::colourInput(inputId = "highlight_colour", 
                                                                                         label   = "Highlight colour:",
                                                                                         showColour = "background",
                                                                                         value = "#27408B",
                                                                                         palette = "limited"),
                                                               
                                                               radioButtons(inputId      = "highlight_type",
                                                                            label        = "Type for highlight:",
                                                                            inline       = TRUE,
                                                                            choiceNames  = c("Box \u2610", "Filled \u25A0","Hatched \u25A8"),
                                                                            choiceValues = c("Box","Filled","Hatched")),
                                                               
                                                               
                                                               column(width = 12,
                                                                      actionButton(inputId = "add_highlight",
                                                                                   label = "Add highlight"),
                                                                      br(), br(), br(),
                                                                      actionButton(inputId = "remove_last_highlight",
                                                                                   label = "Remove last highlight"),
                                                                      actionButton(inputId = "remove_all_highlights",
                                                                                   label = "Remove all highlights")),
                                                               
                                                           ))
                                                     )),
                                              
                                              
                                              #### Customization panels END ----
                                            ),
                                            
                                            #### Downloads ----
                                            h4("Downloads", style = "color: #094030;",downloads_popover("pop_anomalies_map_downloads")),
                                            checkboxInput(inputId = "download_options",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download",
                                                                # Download map 
                                                                h4(helpText("Map")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_map", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_map", label = "Download map")),
                                                                ),
                                                                
                                                                hr(),
                                                                
                                                                # Upload Meta data
                                                                h4(helpText("Metadata")),
                                                                fluidRow(
                                                                  column(3, downloadButton(outputId = "download_metadata", label = "Download metadata")),
                                                                  column(4, fileInput(inputId= "upload_metadata", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                  column(2, actionButton(inputId = "update_metadata", label = "Update upload inputs")),
                                                                ),
                                            )),
                                            
                                            #### Abs/Ref Map plot START ----
                                            h4("Reference map", style = "color: #094030;",reference_map_popover("pop_anomalies_refmap")), 
                                            
                                            radioButtons(
                                              inputId  = "ref_map_mode",
                                              label    = NULL,
                                              choices  = c("None", "Absolute Values", "Reference Values", "SD ratio"),
                                              selected = "None" ,
                                              inline = TRUE
                                            ),
                                            conditionalPanel(
                                              condition = "input.ref_map_mode && input.ref_map_mode !== 'None'",
                                              shinycssloaders::withSpinner(
                                                ui_element = plotOutput("ref_map", height = "auto"),
                                                image = spinner_image,
                                                image.width = spinner_width,
                                                image.height = spinner_height
                                              )
                                            ), 
                                            
                                            #### Download ref. map ----
                                            shinyjs::hidden(div(id ="hidden_sec_map_download",
                                                                h4("Download reference map", style = "color: #094030;"),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_map_sec", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_map_sec", label = "Download reference map"))
                                                                ),
                                            )),
                                            
                                            ### Map plot END ----
                                   ),
                                   
                                   ### TS plot START ----
                                   tabPanel("Timeseries", br(),
                                            h4("Anomalies timeseries", style = "color: #094030;"),
                                            
                                            shinycssloaders::withSpinner(ui_element = plotOutput("timeseries", click = "ts_click1",dblclick = "ts_dblclick1",brush = brushOpts(id = "ts_brush1",resetOnNew = TRUE)),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),
                                            #### Customization panels START ----       
                                            fluidRow(
                                              #### Timeseries customization ----
                                              column(width = 4,
                                                     h4("Customize your timeseries", style = "color: #094030;",timeseries_customization_popover("pop_anomalies_custime")),  
                                                     
                                                     checkboxInput(inputId = "custom_ts",
                                                                   label   = "Timeseries customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden( 
                                                       div(id = "hidden_custom_ts",
                                                           radioButtons(inputId  = "title_mode_ts",
                                                                        label    = "Title customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" ,
                                                                        inline = TRUE),
                                                           
                                                           shinyjs::hidden( 
                                                             div(id = "hidden_custom_title_ts",
                                                                 
                                                                 textInput(inputId     = "title1_input_ts",
                                                                           label       = "Custom plot title:", 
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 numericInput(inputId = "title_size_input_ts",
                                                                              label   = "Font size:",
                                                                              value   = 18,
                                                                              min     = 1,
                                                                              max     = 40,
                                                                              updateOn = "blur")
                                                             )),
                                                           
                                                           radioButtons(inputId  = "axis_mode_ts",
                                                                        label    = "Y-axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis_ts",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input_ts",
                                                                                   label      = "Set your y-axis values:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "show_ticks_ts",
                                                                         label   = "Customize year axis intervals",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_xaxis_interval_ts",
                                                                 
                                                                 numericInput(inputId = "xaxis_numeric_interval_ts",
                                                                              label   = "Year axis intervals:",
                                                                              value   = 50,
                                                                              min     = 1,
                                                                              max     = 500,
                                                                              updateOn = "blur"))),
                                                           
                                                           checkboxInput(inputId = "show_key_ts",
                                                                         label   = "Show key",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_key_position_ts",
                                                                 radioButtons(inputId  = "key_position_ts",
                                                                              label    = "Key position:",
                                                                              choiceNames  = c("right","bottom"),
                                                                              choiceValues = c("right","bottom"),
                                                                              selected = "right" ,
                                                                              inline = TRUE))),
                                                           
                                                           checkboxInput(inputId = "show_ref_ts",
                                                                         label   = "Show absolute mean for reference period",
                                                                         value   = FALSE),
                                                           
                                                       )),
                                                     
                                                     
                                                     
                                              ),
                                              
                                              #### Add Custom features (points, highlights, lines) ----                        
                                              column(width = 4,
                                                     h4("Custom features", style = "color: #094030;",timeseries_features_popover("pop_anomalies_timefeat")),
                                                     
                                                     checkboxInput(inputId = "custom_features_ts",
                                                                   label   = "Enable custom features",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_features_ts",
                                                           radioButtons(inputId      = "feature_ts",
                                                                        label        = "Select a feature type:",
                                                                        inline       = TRUE,
                                                                        choices      = c("Point", "Highlight", "Line")),
                                                           
                                                           #Custom Points
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_points_ts",
                                                                 h4(helpText("Add custom points",timeseries_points_popover("pop_anomalies_timepoint"))),
                                                                 h6(helpText("Enter position manually or click on plot")),
                                                                 
                                                                 textInput(inputId = "point_label_ts", 
                                                                           label   = "Point label:",
                                                                           value   = ""),
                                                                 
                                                                 column(width = 12, offset = 0,
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_x_ts", 
                                                                                         label   = "Point x position:",
                                                                                         value   = "")
                                                                        ),
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_y_ts", 
                                                                                         label   = "Point y position:",
                                                                                         value   = "")
                                                                        )),
                                                                 
                                                                 
                                                                 radioButtons(inputId      = "point_shape_ts",
                                                                              label        = "Point shape:",
                                                                              inline       = TRUE,
                                                                              choices      = c("\u25CF", "\u25B2", "\u25A0")),
                                                                 
                                                                 colourpicker::colourInput(inputId = "point_colour_ts", 
                                                                                           label   = "Point colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),                        
                                                                 
                                                                 
                                                                 numericInput(inputId = "point_size_ts",
                                                                              label   = "Point size",
                                                                              value   = 4,
                                                                              min     = 4,
                                                                              max     = 20),
                                                                 
                                                                 column(width = 12,
                                                                        
                                                                        actionButton(inputId = "add_point_ts",
                                                                                     label = "Add point"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_point_ts",
                                                                                     label = "Remove last point"),
                                                                        actionButton(inputId = "remove_all_points_ts",
                                                                                     label = "Remove all points")),
                                                             )),
                                                           
                                                           #Custom highlights
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_highlights_ts",
                                                                 h4(helpText("Add custom highlights", timeseries_highlights_popover("pop_anomalies_timehl"))),
                                                                 h6(helpText("Enter values manually or draw a box on plot")),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_x_values_ts",
                                                                                   label  = "X values:",
                                                                                   value  = ""),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_y_values_ts",
                                                                                   label  = "Y values:",
                                                                                   value  = ""),
                                                                 
                                                                 colourpicker::colourInput(inputId = "highlight_colour_ts", 
                                                                                           label   = "Highlight colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "highlight_type_ts",
                                                                              label        = "Type for highlight:",
                                                                              inline       = TRUE,
                                                                              choiceNames  = c("Fill \u25A0", "Box \u2610"),
                                                                              choiceValues = c("Fill","Box")),
                                                                 
                                                                 checkboxInput(inputId = "show_highlight_on_legend_ts",
                                                                               label   = "Show on key",
                                                                               value   = FALSE),
                                                                 
                                                                 shinyjs::hidden(
                                                                   textInput(inputId = "highlight_label_ts", 
                                                                             label   = "Label:",
                                                                             value   = "")),
                                                                 
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_highlight_ts",
                                                                                     label = "Add highlight"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_highlight_ts",
                                                                                     label = "Remove last highlight"),
                                                                        actionButton(inputId = "remove_all_highlights_ts",
                                                                                     label = "Remove all highlights")),
                                                                 
                                                             )),
                                                           
                                                           #Custom lines
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_line_ts",
                                                                 h4(helpText("Add custom lines",timeseries_lines_popover("pop_anomalies_timelines"))),
                                                                 h6(helpText("Enter position manually or click on plot, double click to change orientation")),
                                                                 
                                                                 radioButtons(inputId      = "line_orientation_ts",
                                                                              label        = "Orientation:",
                                                                              inline       = TRUE,
                                                                              choices      = c("Vertical", "Horizontal")),
                                                                 
                                                                 textInput(inputId = "line_position_ts", 
                                                                           label   = "Position:",
                                                                           value   = "",
                                                                           placeholder = "1830, 1832"),
                                                                 
                                                                 colourpicker::colourInput(inputId = "line_colour_ts", 
                                                                                           label   = "Line colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "line_type_ts",
                                                                              label        = "Type:",
                                                                              inline       = TRUE,
                                                                              choices = c("solid", "dashed")),
                                                                 
                                                                 checkboxInput(inputId = "show_line_on_legend_ts",
                                                                               label   = "Show on key",
                                                                               value   = FALSE),
                                                                 
                                                                 shinyjs::hidden(
                                                                   textInput(inputId = "line_label_ts", 
                                                                             label   = "Label:",
                                                                             value   = "")),
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_line_ts",
                                                                                     label = "Add line"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_line_ts",
                                                                                     label = "Remove last line"),
                                                                        actionButton(inputId = "remove_all_lines_ts",
                                                                                     label = "Remove all lines")
                                                                 )
                                                             ))
                                                       ))),
                                              

                                              #### Customization panels END ----
                                            ),
                                            #### Downloads TS ----
                                            h4("Downloads", style = "color: #094030;",downloads_popover("pop_anomalies_ts_downloads")),
                                            checkboxInput(inputId = "download_options_ts",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download_ts",
                                                                # Download TS
                                                                h4(helpText("Timeseries")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_timeseries", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_timeseries", label = "Download timeseries"))
                                                                ),
                                                                
                                                                hr(),
                                                                
                                                                # Upload Meta data 
                                                                h4(helpText("Metadata")),
                                                                fluidRow(
                                                                  column(3, downloadButton(outputId = "download_metadata_ts", label = "Download metadata")),
                                                                  column(4, fileInput(inputId= "upload_metadata_ts", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                  column(2, actionButton(inputId = "update_metadata_ts", label = "Update upload inputs")),
                                                                ),
                                            )),
                                            
                                            ### TS plot END ----       
                                   ),
                                   
                                   ### Other plots ----
                                   tabPanel("Map data",
                                            
                                            # Data values and Download
                                            br(), 
                                            fluidRow(
                                              column(width = 6,
                                                     h4("Data", style = "color: #094030;"),
                                                     fluidRow(
                                                       column(10, radioButtons(inputId = "value_type_map_data", label = "Choose data values:", 
                                                                               choices = c("Anomalies", "Absolute", "Reference", "SD ratio"), selected = "Anomalies", inline = TRUE))
                                                     )),
                                              column(width = 6,
                                                     h4("Download", style = "color: #094030;"),
                                                     fluidRow(
                                                       column(6, radioButtons(inputId = "file_type_map_data", label = "Choose file type:", 
                                                                              choices = c("csv", "xlsx", "GeoTIFF"), selected = "csv", inline = TRUE)),
                                                       column(6, downloadButton(outputId = "download_map_data", label = "Download map data"))
                                                     ))
                                            ),
                                            
                                            br(), 
                                            shinycssloaders::withSpinner(ui_element = tableOutput("data1"),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height)),
                                   
                                   tabPanel("Timeseries data",
                                            
                                            # Download
                                            br(),  h4("Download", style = "color: #094030;"),
                                            fluidRow(
                                              column(2, radioButtons(inputId = "file_type_timeseries_data", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                              column(3, downloadButton(outputId = "download_timeseries_data", label = "Download timeseries data"))
                                            ),
                                            
                                            br(), column(width = 3, 
                                                         shinycssloaders::withSpinner(ui_element = DT::dataTableOutput("data2"),
                                                                                       image = spinner_image,
                                                                                       image.width = spinner_width,
                                                                                       image.height = spinner_height))),                                      
                                   
                                   tabPanel("Download NETcdf data",
                                            br(), h4("Download NETcdf with one or more variable", style = "color: #094030;", netcdf_popover("pop_anomalies_netcdf")),
                                            #NETcdf download pickerInput checkboxGroupInput
                                            column(3, shinyWidgets::pickerInput(inputId = "netcdf_variables", label = "Choose one or multiple variables:", choices = NULL, selected = NULL, inline = TRUE, multiple = TRUE,)),
                                            column(3, downloadButton(outputId = "download_netcdf", label = "Download NETcdf"))),
                                   
                                   ### Feedback archive documentation (FAD) ----
                                   tabPanel("ModE-RA sources", br(),
                                            
                                            # Title & help pop up
                                            MEsource_popover("pop_anomalies_mesource"),
                                            
                                            fluidRow(
                                              
                                              # Year entry
                                              numericInput(
                                                inputId  = "fad_year",
                                                label   =  "Year",
                                                value = 1422,
                                                min = 1422,
                                                max = 2008,
                                                updateOn = "blur"),
                                              
                                              # Enter Season                
                                              selectInput(inputId  = "fad_season",
                                                          label    = "Months",
                                                          choices  = c("April to September","October to March"),
                                                          selected = "April to September"),
                                            ),
                                            
                                            h6("Use the Explore ModE-RA sources tab for more information", style = "color: #094030;"),
                                            
                                            shinycssloaders::withSpinner(
                                              ui_element = plotOutput(
                                                "fad_map",
                                                height = "auto",
                                                brush = brushOpts(
                                                  id = "brush_fad",
                                                  resetOnNew = TRUE,
                                                  delay = 10000,
                                                  # Delay in ms (adjust as needed)
                                                  delayType = "debounce"      # Triggers only after releasing the mouse
                                                )
                                              ),
                                              image = spinner_image,
                                              image.width = spinner_width,
                                              image.height = spinner_height),
                                            
                                            fluidRow(
                                              h6(helpText("Draw a box on the map to zoom in")),
                                              actionButton(inputId = "fad_reset_zoom",
                                                           label = "Reset zoom",
                                                           width = "200px"),
                                            ),
                                            
                                            br(),
                                            
                                            #Download
                                            h4("Downloads", style = "color: #094030;"),
                                            fluidRow(
                                              column(2,radioButtons(inputId = "file_type_fad", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                              column(3,downloadButton(outputId = "download_fad", label = "Download map")),
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              # Download data
                                              column(2,radioButtons(inputId = "data_file_type_fad", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                              column(3,downloadButton(outputId = "download_fad_data", label = "Download map data"))
                                            )
                                   )          
                                   ## Main Panel END ----
             ), width = 8),
             # Anomalies END ----  
           )),
  
  # Composites START----      
  tabPanel("Composites", value = "tab2",
           shinyjs::useShinyjs(),
           sidebarLayout(
             ## Sidebar Panels START ----
             sidebarPanel(verticalLayout(
               
               ### First Sidebar panel (Variable and dataset selection) ----
               sidebarPanel(fluidRow(
                 #Method Title and Pop Over
                 composites_summary_popover("pop_composites"),
                 
                 br(),
                 
                 #Short description of the Panel Composites        
                 h4("Select dataset and variable for composite anomalies", style = "color: #094030;",dataset_variable_popover("pop_composites_datvar")),
                 
                 #Choose one of three datasets (Select)                
                 selectInput(inputId  = "dataset_selected2",
                             label    = "Choose a dataset:",
                             choices  = c("ModE-RA", "ModE-Sim","ModE-RAclim"),
                             selected = "ModE-RA"),
                 
                 #Choose one of four variable (Select)                
                 selectInput(inputId  = "variable_selected2",
                             label    = "Choose a variable to plot:",
                             choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
                             selected = "Temperature"),
                 
               ), width = 12), br(),
               
               ### Second Sidebar panel (Time selection) ----
               sidebarPanel(fluidRow(
                 
                 #Short description of the temporal selection        
                 h4("Select years, season and reference period", style = "color: #094030;", year_season_ref_popover("pop_composites_time")),
                 
                 #Type in your year of interest OR upload a file
                 radioButtons(inputId  = "enter_upload2",
                              label    = "Choose how to enter composite years:",
                              choices  = c("Manual", "Upload"),
                              selected = "Manual" , inline = TRUE),
                 
                 shinyjs::hidden(div(id = "optional2c",
                                     textInput(inputId    = "range_years2",
                                               label     = "Enter your list of years, separated by commas:",
                                               value     = "1815, 1816",
                                               placeholder = "1815",
                                               updateOn = "blur"))),
                 
                 shinyjs::hidden(div(id = "optional2d",
                                     fileInput(inputId = "upload_file2",
                                               label = "Upload a list of years in .csv or .xlsx format:",
                                               multiple = FALSE,
                                               accept = c(".csv", ".xlsx", ".xls"),
                                               width = NULL,
                                               buttonLabel = "Browse your folders",
                                               placeholder = "No file selected"),
                                     
                                     shinyjs::hidden(div(id = "optional2e",
                                                         img(src = 'pics/composite_user_example.jpg', id = "comp_user_example", height = "300px", width = "150px"),
                                     ))
                 )
                 ),
                 
                 #Choose Season, Year or Months
                 radioButtons(inputId  = "season_selected2",
                              label    = "Select the range of months:",
                              choices  = c("Annual", "DJF", "MAM", "JJA", "SON", "Custom"),
                              selected = "Annual" , inline = TRUE),
                 
                 #Choose your range of months (Slider)
                 shinyjs::hidden(
                   div(id = "season2",
                       shinyWidgets::sliderTextInput(inputId = "range_months2",
                                       label = "Select custom months:",
                                       choices = c("December (prev.)", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                                       #Initially selected = 1 year (annual mean)
                                       selected = c("January", "December")),
                   )),      
                 
                 #Choose a Mode: Absolute, Fixed Anomaly or Anomalies compared to X years prior
                 radioButtons(inputId  = "mode_selected2",
                              label    = "Select type of reference period:",
                              choices  = c("Fixed reference","Custom reference", "X years prior"),
                              selected = "Fixed reference" , inline = TRUE),
                 
                 #Choose reference period for either Fixed Anomaly or enter X years (1-50) for Anomalies compared to X years prior (Hidden objects)      
                 shinyjs::hidden(
                   div(id = "optional2a",
                       shinyjs::hidden(
                         shinyWidgets::numericRangeInput(inputId = "ref_period2",
                                           label      = "Reference period:",
                                           value      = c(1961,1990),
                                           separator  = " to ",
                                           min        = 1422,
                                           max        = 2008)),
                       
                       #Choose single ref year
                       column(12,
                              checkboxInput(inputId = "ref_single_year2",
                                            label   = "Select single year",
                                            value   = FALSE)),
                       
                       
                       shinyjs::hidden(
                         numericInput(inputId   = "ref_period_sg2",
                                      label     = "Select the single year:",
                                      value     = NA,
                                      min       = 1422,
                                      max       = 2008,
                                      updateOn = "blur")),
                   )),
                 
                 shinyjs::hidden(
                   div(id = "optional2b",
                       numericInput(inputId = "prior_years2",
                                    label      = "X (number of years prior to composite years - max 50):",
                                    value      = 10,
                                    min        = 1,
                                    max        = 50,
                                    updateOn = "blur"))),
                 
                 #Reference period as list of years (manual or upload)
                 shinyjs::hidden(div(id = "optional2f",
                                     radioButtons(inputId  = "enter_upload2a",
                                                  label    = "Choose how to enter reference years:",
                                                  choices  = c("Manual", "Upload"),
                                                  selected = "Manual" , inline = TRUE),
                                     
                                     shinyjs::hidden(div(id = "optional2g",
                                                         textInput(inputId    = "range_years2a",
                                                                   label     = "Enter your list of years, separated by commas:",
                                                                   value     = "1641, 1642",
                                                                   placeholder = "1641",
                                                                   updateOn = "blur"))),
                                     
                                     shinyjs::hidden(div(id = "optional2h",
                                                         fileInput(inputId = "upload_file2a",
                                                                   label = "Upload a list of years in .csv or .xlsx format:",
                                                                   multiple = FALSE,
                                                                   accept = c(".csv", ".xlsx", ".xls"),
                                                                   width = NULL,
                                                                   buttonLabel = "Browse your folders",
                                                                   placeholder = "No file selected"),
                                                         
                                                         shinyjs::hidden(div(id = "optional2i",
                                                                             img(src = 'pics/composite_user_example.jpg', id = "comp_user_example_2", height = "300px", width = "150px"),
                                                         ))
                                     )),
                 )),
                 
               ), width = 12), br(),
               
               ### Third sidebar panel (Location selection) ----
               sidebarPanel(fluidRow(
                 
                 #Short description of the Coord. Sidebar        
                 h4("Set geographical area", style = "color: #094030;"),
                 h5(helpText("Select a continent, enter coordinates manually or draw a box on the plot")),
                 
                 column(width = 12, fluidRow(
                   
                   br(), br(),
                   
                   #Global Button
                   actionButton(inputId = "button_global2",
                                label = "Global",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #Europe Button
                   actionButton(inputId = "button_europe2",
                                label = "Europe",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #Asia Button
                   actionButton(inputId = "button_asia2",
                                label = "Asia",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #Oceania Button
                   actionButton(inputId = "button_oceania2",
                                label = "Oceania",
                                width = "110px"),
                   
                 )),
                 
                 column(width = 12, br()),
                 
                 column(width = 12, fluidRow(
                   
                   br(), br(),
                   
                   #Africa Button
                   actionButton(inputId = "button_africa2",
                                label = "Africa",
                                width = "100px"),
                   
                   br(), br(),
                   
                   #North America Button
                   actionButton(inputId = "button_n_america2",
                                label = "North America",
                                width = "150px"),
                   
                   br(), br(),
                   
                   #South America Button
                   actionButton(inputId = "button_s_america2",
                                label = "South America",
                                width = "150px")
                 )),
                 
                 column(width = 12, br()),
                 
                 #Choose Longitude and Latitude Range          
                 shinyWidgets::numericRangeInput(inputId = "range_longitude2",
                                   label = "Longitude range (-180 to 180):",
                                   value = initial_lon_values,
                                   separator = " to ",
                                   min = -180,
                                   max = 180),
                 
                 #Choose Longitude and Latitude Range          
                 shinyWidgets::numericRangeInput(inputId = "range_latitude2",
                                   label = "Latitude range (-90 to 90):",
                                   value = initial_lat_values,
                                   separator = " to ",
                                   min = -90,
                                   max = 90),
                 
                 br(), br(),
                 
                 #Enter Coordinates
                 column(12,
                        actionButton(inputId = "button_coord2",
                                     label = "Update coordinates",
                                     width = "200px"),
                 ),
                 
               ), width = 12)
               ## Sidebar Panels END ----
             )),
             
             ## Main Panel START ----
             mainPanel(tabsetPanel(id = "tabset2",
                                   ### Map plot START ----
                                   tabPanel("Map", br(),
                                            h4(textOutput("text_years2"), style = "color: #094030;"),
                                            textOutput("years2"),
                                            shinyjs::hidden(div(id = "custom_anomaly_years2",
                                                                h4(textOutput("text_custom_years2"), style = "color: #094030;"),
                                                                textOutput("custom_years2")
                                            )),
                                            
                                            shinycssloaders::withSpinner(ui_element = plotOutput("map2",height = "750px", dblclick = "map_dblclick2", brush = brushOpts(id = "map_brush2",resetOnNew = TRUE)),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),         
                                            
                                            #### Customization panels START ----       
                                            fluidRow(
                                              #### Map customization ----       
                                              column(width = 4,
                                                     
                                                     h4("Customize your map", style = "color: #094030;", map_customization_popover("pop_composites_cusmap")),  
                                                     
                                                     checkboxInput(inputId = "custom_map2",
                                                                   label   = "Map customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_maps2",
                                                           
                                                           selectInput(inputId = "projection2",
                                                                       label = "Projection:",
                                                                       choices = c("UTM (default)", "Robinson", "Orthographic", "LAEA"),
                                                                       selected = "UTM (default)"),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_map_center2",
                                                                 
                                                                 numericInput(inputId = "center_lat2",
                                                                              label = "Center latitude:",
                                                                              value = 0,
                                                                              min = -90,
                                                                              max = 90,
                                                                              updateOn = "blur"),
                                                                 numericInput(inputId = "center_lon2",
                                                                              label = "Center longitude:",
                                                                              value = 0,
                                                                              min = -180,
                                                                              max = 180,
                                                                              updateOn = "blur"))),
                                                           
                                                           radioButtons(inputId  = "axis_mode2",
                                                                        label    = "Axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis2",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input2",
                                                                                   label      = "Set your axis values:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "hide_axis2",
                                                                         label   = "Hide colorbar",
                                                                         value   = FALSE),
                                                           
                                                           br(),
                                                           
                                                           radioButtons(inputId  = "title_mode2",
                                                                        label    = "Title customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_title2",
                                                                 
                                                                 textInput(inputId     = "title1_input2",
                                                                           label       = "Custom map title:", 
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 textInput(inputId     = "title2_input2",
                                                                           label       = "Custom map subtitle (e.g. Ref-Period):",
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 numericInput(inputId = "title_size_input2",
                                                                              label   = "Font size:",
                                                                              value   = 18,
                                                                              min     = 1,
                                                                              max     = 40,
                                                                              updateOn = "blur"))),
                                                           
                                                           br(), hr(),
                                                           
                                                           h4(helpText("Topography options and GIS upload (.shp)", map_customization_layers_popover("pop_composites_layers"))),
                                                           
                                                           checkboxInput(inputId = "custom_topo2",
                                                                         label   = "Topographical customization",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_geo_options2",
                                                                 
                                                                 checkboxInput(inputId = "hide_borders2",
                                                                               label   = "Show country borders",
                                                                               value   = TRUE),
                                                                 
                                                                 checkboxInput(inputId = "white_ocean2",
                                                                               label   = "Grey ocean",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "white_land2",
                                                                               label   = "Grey land",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_rivers2",
                                                                               label   = "Show rivers",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_rivers2",
                                                                               label   = "Label rivers",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_lakes2",
                                                                               label   = "Show lakes",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_lakes2",
                                                                               label   = "Label lakes",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_mountains2",
                                                                               label   = "Show mountains",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_mountains2",
                                                                               label   = "Label mountains",
                                                                               value   = FALSE),
                                                                 
                                                             )),
                                                           
                                                           #Shape File Option
                                                           
                                                           fileInput(inputId = "shpFile2", label = "Upload shapefile (ZIP)"),
                                                           
                                                           # Single sortable checkbox group input for selecting and ordering shapefiles
                                                           uiOutput("shapefileSelector2"),
                                                           
                                                           uiOutput("colorpickers2")
                                                           
                                                       )),
                                              ),
                                              
                                              #### Add Custom features (points and highlights) ----                        
                                              column(width = 4,
                                                     
                                                     h4("Custom features", style = "color: #094030;",map_features_popover("pop_composites_mapfeat")),
                                                     
                                                     checkboxInput(inputId = "custom_features2",
                                                                   label   = "Enable custom features",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_features2",
                                                           radioButtons(inputId      = "feature2",
                                                                        label        = "Select a feature type:",
                                                                        inline       = TRUE,
                                                                        choices      = c("Point", "Highlight")),
                                                           
                                                           #Custom Points
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_points2",
                                                                 
                                                                 h4(helpText("Add custom points",map_points_popover("pop_composites_mappoint"))),
                                                                 
                                                                 h6(helpText("Enter location/coordinates or double click on map")),
                                                                 
                                                                 textInput(inputId = "location2", 
                                                                           label   = "Enter a location:",
                                                                           value   = NULL,
                                                                           placeholder = "e.g. Bern"),
                                                                 
                                                                 actionButton(inputId = "search2",
                                                                              label   = "Search"),
                                                                 
                                                                 
                                                                 shinyjs::hidden(div(id = "inv_location2",
                                                                                     h6(helpText("Invalid location"))
                                                                 )),
                                                                 
                                                                 textInput(inputId = "point_label2", 
                                                                           label   = "Point label:",
                                                                           value   = ""),
                                                                 
                                                                 column(width = 12, offset = 0,
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_x2", 
                                                                                         label   = "Point longitude:",
                                                                                         value   = "")
                                                                        ),
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_y2", 
                                                                                         label   = "Point latitude:",
                                                                                         value   = "")
                                                                        )),
                                                                 
                                                                 radioButtons(inputId      = "point_shape2",
                                                                              label        = "Point shape:",
                                                                              inline       = TRUE,
                                                                              choices      = c("\u25CF", "\u25B2", "\u25A0")),
                                                                 
                                                                 colourpicker::colourInput(inputId = "point_colour2", 
                                                                                           label   = "Point colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),                       
                                                                 
                                                                 
                                                                 numericInput(inputId = "point_size2",
                                                                              label   = "Point size:",
                                                                              value   = 1,
                                                                              min     = 1,
                                                                              max     = 10),
                                                                 
                                                                 column(width = 12,
                                                                        
                                                                        actionButton(inputId = "add_point2",
                                                                                     label = "Add point"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_point2",
                                                                                     label = "Remove last point"),
                                                                        actionButton(inputId = "remove_all_points2",
                                                                                     label = "Remove all points")),
                                                             )),
                                                           
                                                           #Custom Highlights
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_highlights2",
                                                                 
                                                                 h4(helpText("Add custom highlights",map_highlights_popover("pop_composites_maphl"))),
                                                                 h6(helpText("Enter coordinate or draw a box on map")),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_x_values2",
                                                                                   label  = "Longitude:",
                                                                                   value  = "",
                                                                                   min    = -180,
                                                                                   max    = 180),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_y_values2",
                                                                                   label  = "Latitude:",
                                                                                   value  = "",
                                                                                   min    = -90,
                                                                                   max    = 90),
                                                                 
                                                                 colourpicker::colourInput(inputId = "highlight_colour2", 
                                                                                           label   = "Highlight colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "highlight_type2",
                                                                              label        = "Type for highlight:",
                                                                              inline       = TRUE,
                                                                              choiceNames  = c("Box \u2610", "Filled \u25A0","Hatched \u25A8"),
                                                                              choiceValues = c("Box","Filled","Hatched")),
                                                                 
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_highlight2",
                                                                                     label = "Add highlight"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_highlight2",
                                                                                     label = "Remove last highlight"),
                                                                        actionButton(inputId = "remove_all_highlights2",
                                                                                     label = "Remove all highlights")),
                                                             )),
                                                       )),
                                              ),
                                              
                                             
                                              #### Customization panels END ----
                                            ),
                                            
                                            #### Downloads Map ----
                                            h4("Downloads", style = "color: #094030;",downloads_popover("pop_composites_map_downloads")),
                                            checkboxInput(inputId = "download_options2",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download2",
                                                                # Download map
                                                                h4(helpText("Map")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_map2", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_map2", label = "Download map"))
                                                                ),
                                                                
                                                                # Upload Meta data
                                                                h4(helpText("Metadata",)),
                                                                fluidRow(
                                                                  column(3, downloadButton(outputId = "download_metadata2", label = "Download metadata")),
                                                                  column(4, fileInput(inputId= "upload_metadata2", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                  column(2, actionButton(inputId = "update_metadata2", label = "Update upload inputs")),
                                                                ),
                                            )),
                                            
                                            #### Abs/Ref Map plot START ----
                                            h4("Reference map", style = "color: #094030;",reference_map_popover("pop_composites_refmap")), 
                                            
                                            radioButtons(
                                              inputId  = "ref_map_mode2",
                                              label    = NULL,
                                              choices  = c("None", "Absolute Values", "Reference Values", "SD ratio"),
                                              selected = "None" ,
                                              inline = TRUE
                                            ),
                                            conditionalPanel(
                                              condition = "input.ref_map_mode2 && input.ref_map_mode2 !== 'None'",
                                              shinycssloaders::withSpinner(
                                                ui_element = plotOutput("ref_map2", height = "auto"),
                                                image = spinner_image,
                                                image.width = spinner_width,
                                                image.height = spinner_height
                                              )
                                            ), 
                                            
                                            #### Download ref. map ----
                                            shinyjs::hidden(div(id ="hidden_sec_map_download2",
                                                                h4("Download reference map", style = "color: #094030;"),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_map_sec2", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_map_sec2", label = "Download reference map"))
                                                                ),
                                            )),
                                            
                                            ### Map plot END ----  
                                   ),
                                   
                                   ### Composite TS plot START ----
                                   tabPanel("Timeseries", br(),
                                            h4(textOutput("text_years2b"), style = "color: #094030;"),
                                            textOutput("years2b"),
                                            shinyjs::hidden(div(id = "custom_anomaly_years2b",
                                                                h4(textOutput("text_custom_years2b"), style = "color: #094030;"),
                                                                textOutput("custom_years2b")
                                            )),
                                            shinycssloaders::withSpinner(ui_element = plotOutput("timeseries2", click = "ts_click2", dblclick = "ts_dblclick2", brush = brushOpts(id = "ts_brush2",resetOnNew = TRUE)),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),
                                            
                                            #### Customization panels START ----       
                                            fluidRow(
                                              
                                              #### Timeseries customization ----
                                              column(width = 4,
                                                     h4("Customize your timeseries", style = "color: #094030;", timeseries_customization_popover("pop_composites_custime")),
                                                     
                                                     checkboxInput(inputId = "custom_ts2",
                                                                   label   = "Timeseries customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden( 
                                                       div(id = "hidden_custom_ts2",
                                                           radioButtons(inputId  = "title_mode_ts2",
                                                                        label    = "Title customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" ,
                                                                        inline = TRUE),
                                                           
                                                           shinyjs::hidden( 
                                                             div(id = "hidden_custom_title_ts2",
                                                                 
                                                                 textInput(inputId     = "title1_input_ts2",
                                                                           label       = "Custom plot title:", 
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 numericInput(inputId = "title_size_input_ts2",
                                                                              label   = "Font size:",
                                                                              value   = 18,
                                                                              min     = 1,
                                                                              max     = 40,
                                                                              updateOn = "blur"),
                                                             )),
                                                           
                                                           radioButtons(inputId  = "axis_mode_ts2",
                                                                        label    = "Y-axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis_ts2",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input_ts2",
                                                                                   label      = "Set your axis values:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "show_ticks_ts2",
                                                                         label   = "Customize year axis intervals",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_xaxis_interval_ts2",
                                                                 
                                                                 numericInput(inputId = "xaxis_numeric_interval_ts2",
                                                                              label   = "Year axis intervals:",
                                                                              value   = 50,
                                                                              min     = 1,
                                                                              max     = 500,
                                                                              updateOn = "blur"))),
                                                           
                                                           checkboxInput(inputId = "show_key_ts2",
                                                                         label   = "Show key",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_key_position_ts2",
                                                                 radioButtons(inputId  = "key_position_ts2",
                                                                              label    = "Key position:",
                                                                              choiceNames  = c("right","bottom"),
                                                                              choiceValues = c("right","bottom"),
                                                                              selected = "right" ,
                                                                              inline = TRUE))),
                                                           
                                                           checkboxInput(inputId = "show_ref_ts2",
                                                                         label   = "Show absolute mean for reference period",
                                                                         value   = FALSE),
                                                       )),    
                                              ),
                                              
                                              #### Add Custom features (points, highlights, lines) ----                        
                                              column(width = 4,
                                                     h4("Custom features", style = "color: #094030;", timeseries_features_popover("pop_composites_timefeat")),
                                                     
                                                     checkboxInput(inputId = "custom_features_ts2",
                                                                   label   = "Enable custom features",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_features_ts2",
                                                           radioButtons(inputId      = "feature_ts2",
                                                                        label        = "Select a feature type:",
                                                                        inline       = TRUE,
                                                                        choices      = c("Point", "Highlight", "Line")),
                                                           
                                                           #Custom Points
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_points_ts2",
                                                                 
                                                                 h4(helpText("Add custom points",timeseries_points_popover("pop_composites_timepoint"))),
                                                                 h6(helpText("Enter position manually or click on plot")),
                                                                 
                                                                 textInput(inputId = "point_label_ts2", 
                                                                           label   = "Point label:",
                                                                           value   = ""),
                                                                 
                                                                 column(width = 12, offset = 0,
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_x_ts2", 
                                                                                         label   = "Point x position:",
                                                                                         value   = "")
                                                                        ),
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_y_ts2", 
                                                                                         label   = "Point y position:",
                                                                                         value   = "")
                                                                        )),
                                                                 
                                                                 
                                                                 radioButtons(inputId      = "point_shape_ts2",
                                                                              label        = "Point shape:",
                                                                              inline       = TRUE,
                                                                              choices      = c("\u25CF", "\u25B2", "\u25A0")),
                                                                 
                                                                 colourpicker::colourInput(inputId = "point_colour_ts2", 
                                                                                           label   = "Point colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),                        
                                                                 
                                                                 
                                                                 numericInput(inputId = "point_size_ts2",
                                                                              label   = "Point size",
                                                                              value   = 4,
                                                                              min     = 4,
                                                                              max     = 20),
                                                                 
                                                                 column(width = 12,
                                                                        
                                                                        actionButton(inputId = "add_point_ts2",
                                                                                     label = "Add point"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_point_ts2",
                                                                                     label = "Remove last point"),
                                                                        actionButton(inputId = "remove_all_points_ts2",
                                                                                     label = "Remove all points")),
                                                             )),
                                                           
                                                           #Custom highlights
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_highlights_ts2",
                                                                 
                                                                 h4(helpText("Add custom highlights",timeseries_highlights_popover("pop_composites_timehl"))),
                                                                 h6(helpText("Enter values manually or draw a box on plot")),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_x_values_ts2",
                                                                                   label  = "X values:",
                                                                                   value  = ""),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_y_values_ts2",
                                                                                   label  = "Y values:",
                                                                                   value  = ""),
                                                                 
                                                                 colourpicker::colourInput(inputId = "highlight_colour_ts2", 
                                                                                           label   = "Highlight colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "highlight_type_ts2",
                                                                              label        = "Type for highlight:",
                                                                              inline       = TRUE,
                                                                              choiceNames  = c("Fill \u25A0", "Box \u2610"),
                                                                              choiceValues = c("Fill","Box")),
                                                                 
                                                                 checkboxInput(inputId = "show_highlight_on_legend_ts2",
                                                                               label   = "Show on key",
                                                                               value   = FALSE),
                                                                 
                                                                 shinyjs::hidden(
                                                                   textInput(inputId = "highlight_label_ts2", 
                                                                             label   = "Label:",
                                                                             value   = "")),
                                                                 
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_highlight_ts2",
                                                                                     label = "Add highlight"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_highlight_ts2",
                                                                                     label = "Remove last highlight"),
                                                                        actionButton(inputId = "remove_all_highlights_ts2",
                                                                                     label = "Remove all highlights")),
                                                                 
                                                             )),
                                                           
                                                           #Custom lines
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_line_ts2",
                                                                 
                                                                 h4(helpText("Add custom lines", timeseries_lines_popover("pop_composites_timelines"))),
                                                                 h6(helpText("Enter position manually or click on plot, double click to change orientation")),
                                                                 
                                                                 radioButtons(inputId      = "line_orientation_ts2",
                                                                              label        = "Orientation:",
                                                                              inline       = TRUE,
                                                                              choices      = c("Vertical", "Horizontal")),
                                                                 
                                                                 textInput(inputId = "line_position_ts2", 
                                                                           label   = "Position:",
                                                                           value   = "",
                                                                           placeholder = "1830, 1832"),
                                                                 
                                                                 colourpicker::colourInput(inputId = "line_colour_ts2", 
                                                                                           label   = "Line colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "line_type_ts2",
                                                                              label        = "Type:",
                                                                              inline       = TRUE,
                                                                              choices = c("solid", "dashed")),
                                                                 
                                                                 checkboxInput(inputId = "show_line_on_legend_ts2",
                                                                               label   = "Show on key",
                                                                               value   = FALSE),
                                                                 
                                                                 shinyjs::hidden(
                                                                   textInput(inputId = "line_label_ts2", 
                                                                             label   = "Label:",
                                                                             value   = "")),
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_line_ts2",
                                                                                     label = "Add line"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_line_ts2",
                                                                                     label = "Remove last line"),
                                                                        actionButton(inputId = "remove_all_lines_ts2",
                                                                                     label = "Remove all lines")
                                                                 )
                                                             ))
                                                       ))),

                                              #### Customization panels END ----
                                            ),
                                            
                                            #### Downloads TS ----
                                            h4("Downloads", style = "color: #094030;",downloads_popover("pop_composites_ts_downloads")),
                                            checkboxInput(inputId = "download_options_ts2",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download_ts2",
                                                                
                                                                # Downloads
                                                                h4(helpText("Timeseries")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_timeseries2", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_timeseries2", label = "Download timeseries"))
                                                                ),
                                                                
                                                                hr(),
                                                                
                                                                #Upload Meta data
                                                                h4(helpText("Metadata")),
                                                                fluidRow(
                                                                  column(3, downloadButton(outputId = "download_metadata_ts2", label = "Download metadata")),
                                                                  column(4, fileInput(inputId= "upload_metadata_ts2", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                  column(2, actionButton(inputId = "update_metadata_ts2", label = "Update upload inputs")),
                                                                ),
                                            )),
                                            ### Composite TS plot END ----
                                   ),
                                   
                                   ### Other plots ----
                                   tabPanel("Map data",
                                            
                                            # Data values and Download
                                            br(), 
                                            fluidRow(
                                              column(width = 6,
                                                     h4("Data", style = "color: #094030;"),
                                                     fluidRow(
                                                       column(10, radioButtons(inputId = "value_type_map_data2", label = "Choose data values:", 
                                                                               choices = c("Anomalies", "Absolute", "Reference", "SD ratio"), selected = "Anomalies", inline = TRUE))
                                                     )),
                                              column(width = 6,
                                                     h4("Download", style = "color: #094030;"),
                                                     fluidRow(
                                                       column(6, radioButtons(inputId = "file_type_map_data2", label = "Choose file type:", 
                                                                              choices = c("csv", "xlsx", "GeoTIFF"), selected = "csv", inline = TRUE)),
                                                       column(6, downloadButton(outputId = "download_map_data2", label = "Download map data"))
                                                     ))
                                            ),
                                            
                                            br(), shinycssloaders::withSpinner(ui_element = tableOutput("data3"),
                                                                                image = spinner_image,
                                                                                image.width = spinner_width,
                                                                                image.height = spinner_height)),
                                   tabPanel("Timeseries data",
                                            
                                            br(),  h4("Download", style = "color: #094030;"),
                                            fluidRow(
                                              column(2, radioButtons(inputId = "file_type_timeseries_data2", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                              column(3, downloadButton(outputId = "download_timeseries_data2", label = "Download timeseries data"))
                                            ),
                                            
                                            br(), column(width = 3, 
                                                         shinycssloaders::withSpinner(ui_element = DT::dataTableOutput("data4"),
                                                                                       image = spinner_image,
                                                                                       image.width = spinner_width,
                                                                                       image.height = spinner_height))),
                                   
                                   ### Feedback archive documentation (FAD) ----
                                   tabPanel("ModE-RA sources", br(),
                                            
                                            # Title & help pop up
                                            MEsource_popover("pop_composites_mesource"),
                                            
                                            fluidRow(
                                              
                                              # Year entry
                                              numericInput(
                                                inputId  = "fad_year2",
                                                label   =  "Year",
                                                value = 1422,
                                                min = 1422,
                                                max = 2008,
                                                updateOn = "blur"),
                                              
                                              # Enter Season                
                                              selectInput(inputId  = "fad_season2",
                                                          label    = "Months",
                                                          choices  = c("April to September","October to March"),
                                                          selected = "April to September"),
                                            ),
                                            
                                            h6("Use the Explore ModE-RA sources tab for more information", style = "color: #094030;"),
                                            
                                            shinycssloaders::withSpinner(
                                              ui_element = plotOutput(
                                                "fad_map2",
                                                height = "auto",
                                                brush = brushOpts(
                                                  id = "brush_fad2",
                                                  resetOnNew = TRUE,
                                                  delay = 10000,
                                                  # Delay in ms (adjust as needed)
                                                  delayType = "debounce"      # Triggers only after releasing the mouse
                                                )
                                              ),
                                              image = spinner_image,
                                              image.width = spinner_width,
                                              image.height = spinner_height),
                                            
                                            fluidRow(
                                              h6(helpText("Draw a box on the map to zoom in")),
                                              actionButton(inputId = "fad_reset_zoom2",
                                                           label = "Reset zoom",
                                                           width = "200px"),
                                            ),
                                            
                                            br(),
                                            
                                            #Download
                                            h4("Downloads", style = "color: #094030;"),
                                            fluidRow(
                                              column(2,radioButtons(inputId = "file_type_fad2", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                              column(3,downloadButton(outputId = "download_fad2", label = "Download map")),
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              # Download data
                                              column(2,radioButtons(inputId = "data_file_type_fad2", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                              column(3,downloadButton(outputId = "download_fad_data2", label = "Download map data"))
                                            )
                                   )          
                                   
                                   ## Main Panel END ----
             ), width = 8),
             # Composites END ----           
           )),
  
  # SEA START ----
  tabPanel("SEA", value = "tab6",
           shinyjs::useShinyjs(),
           sidebarLayout(
             ## Sidebar Panels START ----          
             sidebarPanel(verticalLayout(
               
               ### First Sidebar panel (Data) ----
               sidebarPanel(fluidRow(
                 #Method Title and Pop Over
                 sea_summary_popover("pop_sea"),
                 br(),
                 
                 h4("Select data", style = "color: #094030;", sea_data_popover("pop_sea_data")),
                 
                 #Choose a data source: ME or User 
                 radioButtons(inputId  = "source_sea_6",
                              label    = "Choose a data source:",
                              choices  = c("ModE-", "User data"),
                              selected = "ModE-" ,
                              inline = TRUE),
                 
                 # Upload user data
                 shinyjs::hidden(
                   div(id = "upload_sea_data_6",   
                       fileInput(inputId = "user_file_6",
                                 label = "Upload timeseries data in .csv or .xlsx format:",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx", ".xls"),
                                 width = NULL,
                                 buttonLabel = "Browse your folders",
                                 placeholder = "No file selected"),
                       
                       div(id = "upload_example_6",
                           img(src = 'pics/regcor_user_example.jpg', id = "sea_user_example_6", height = "300px", width = "300px"))
                   )),
                 
                 #Choose a variable (USER)
                 shinyjs::hidden(
                   div(id = "hidden_user_data_6",
                       selectInput(inputId  = "user_variable_6",
                                   label    = "Choose a variable:",
                                   choices  = NULL,
                                   selected = NULL),
                   )),
                 
                 #Choose one of three datasets (Select)
                 shinyjs::hidden(
                   div(id = "hidden_me_dataset_6",
                       fluidRow(
                         selectInput(inputId  = "dataset_selected_6",
                                     label    = "Choose a dataset:",
                                     choices  = c("ModE-RA", "ModE-Sim","ModE-RAclim"),
                                     selected = "ModE-RA"),
                         
                         #Choose a variable (Mod-ERA) 
                         selectInput(inputId  = "ME_variable_6",
                                     label    = "Choose a variable to plot:",
                                     choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
                                     selected = "Temperature"),
                         
                         #Choose a variable (Mod-ERA) 
                         selectInput(inputId  = "ME_statistic_6",
                                     label    = "Choose a statistic to plot:",
                                     choices  = c("Mean", "Min", "Max"),
                                     selected = "Mean")),
                   )),
                 
                 shinyjs::hidden(
                   div(id = "hidden_modera_6",
                       
                       #Choose Season, Year or Months
                       radioButtons(inputId  = "season_selected_6",
                                    label    = "Select the range of months:",
                                    choices  = c("Annual", "DJF", "MAM", "JJA", "SON", "Custom"),
                                    selected = "Annual" ,
                                    inline = TRUE),
                       
                       #Choose your range of months (Slider)
                       shinyjs::hidden(
                         div(id = "season_6",
                             shinyWidgets::sliderTextInput(inputId = "range_months_6",
                                             label = "Select custom months:",
                                             choices = c("December (prev.)", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                                             #Initially selected = 1 year (annual mean)
                                             selected = c("January", "December")),
                         )), 
                       
                       #Choose reference period      
                       shinyjs::hidden(
                         shinyWidgets::numericRangeInput(inputId = "ref_period_6",
                                           label      = "Reference period:",
                                           value      = c(1961,1990),
                                           separator  = " to ",
                                           min        = 1422,
                                           max        = 2008)),
                       
                       #Choose single ref year
                       column(12,
                              checkboxInput(inputId = "ref_single_year_6",
                                            label   = "Select single year",
                                            value   = FALSE)),
                       
                       
                       shinyjs::hidden(
                         numericInput(inputId   = "ref_period_sg_6",
                                      label     = "Select the single year:",
                                      value     = NA,
                                      min       = 1422,
                                      max       = 2008,
                                      updateOn = "blur")),
                       
                       #Choose Coordinates input 
                       radioButtons(inputId  = "coordinates_type_6",
                                    label    = "Choose input of coordinates:",
                                    choices  = c("Manual", "Continents"),
                                    selected = "Manual" ,
                                    inline = TRUE),
                       
                       shinyjs::hidden(
                         div(id = "hidden_continents_6",
                             column(width = 12, fluidRow(
                               
                               br(), br(),
                               
                               #Global Button
                               actionButton(inputId = "button_global_6",
                                            label = "Global",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Europe Button
                               actionButton(inputId = "button_europe_6",
                                            label = "Europe",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Asia Button
                               actionButton(inputId = "button_asia_6",
                                            label = "Asia",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Oceania Button
                               actionButton(inputId = "button_oceania_6",
                                            label = "Oceania",
                                            width = "110px"),
                               
                             )),
                             
                             column(width = 12, br()),
                             
                             column(width = 12, fluidRow(
                               
                               br(), br(),
                               
                               #Africa Button
                               actionButton(inputId = "button_africa_6",
                                            label = "Africa",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #North America Button
                               actionButton(inputId = "button_n_america_6",
                                            label = "North America",
                                            width = "150px"),
                               
                               br(), br(),
                               
                               #South America Button
                               actionButton(inputId = "button_s_america_6",
                                            label = "South America",
                                            width = "150px")
                             )),
                             
                             column(width = 12, br()),
                             
                         )),
                       
                       #Choose Longitude and Latitude Range          
                       shinyWidgets::numericRangeInput(inputId = "range_longitude_6",
                                         label = "Longitude range (-180 to 180):",
                                         value = c(4,12),
                                         separator = " to ",
                                         min = -180,
                                         max = 180),
                       
                       #Choose Longitude and Latitude Range          
                       shinyWidgets::numericRangeInput(inputId = "range_latitude_6",
                                         label = "Latitude range (-90 to 90):",
                                         value = c(43,50),
                                         separator = " to ",
                                         min = -90,
                                         max = 90),
                       
                       #Enter Coordinates
                       actionButton(inputId = "button_coord_6",
                                    label = "Update coordinates",
                                    width = "200px"),
                       
                   )),
                 
                 
               ), width = 12),
               
               br(),
               
               ### Second Sidebar panel (Inputs) ----
               
               sidebarPanel(fluidRow(
                 
                 #Short description of the option selection        
                 h4("SEA options", style = "color: #094030;", sea_options_popover("pop_sea_options")),
                 
                 #Choose your lag years
                 column(width = 8,
                        
                        shinyWidgets::numericRangeInput(inputId = "lag_years_6",
                                          label      = "Set lag years:",
                                          value      = c(-10,10),
                                          separator  = " to ",
                                          min        = -100,
                                          max        = 100),
                 ),
                 
                 #Type in your event years OR upload a file
                 radioButtons(inputId  = "enter_upload_6",
                              label    = "Choose how to enter event years:",
                              choices  = c("Manual", "Upload"),
                              selected = "Manual" , inline = TRUE),
                 
                 shinyjs::hidden(div(id = "optional6c",
                                     textInput(inputId    = "event_years_6",
                                               label     = "Set event years:",
                                               value     = "1452,1457,1585,1595,1600,1640,1695,1783,1809,1815,1831,1835,1883,1991",
                                               placeholder = "1452,1457,1585,1595,1600,1640,1695,1783,1809,1815,1831,1835,1883,1991"),
                                     
                                     helpText("Example: Years with major volcanic eruptions")
                                     
                 )),
                 
                 shinyjs::hidden(div(id = "optional6d",
                                     fileInput(inputId = "upload_file_6b",
                                               label = "Upload a list of years in .csv or .xlsx format:",
                                               multiple = FALSE,
                                               accept = c(".csv", ".xlsx", ".xls"),
                                               width = NULL,
                                               buttonLabel = "Browse your folders",
                                               placeholder = "No file selected"),
                                     
                                     shinyjs::hidden(div(id = "optional6e",
                                                         img(src = 'pics/sea_user_example.jpg', id = "sea_user_example_6b", height = "300px", width = "300px"),
                                     )),
                                     
                                     br(),
                                     
                                     h5("Optional SEA event background period", style = "color: #094030;", sea_background_popover("pop_sea_background")),
                                     
                                     checkboxInput(
                                       inputId = "use_custom_pre_6",
                                       label = "Use pre-event background period",
                                       value = FALSE),
                                     
                                     checkboxInput(
                                       inputId = "use_custom_post_6",
                                       label = "Use post-event background period",
                                       value = FALSE),
                                     
                 )),
                 
               ), width = 12),
               
               br(),
               
               ## Sidebar Panels END ----
             )),
             
             ## Main Panel START ----
             mainPanel(tabsetPanel(id = "tabset6",
                                   ### SEA START ----
                                   tabPanel("SEA", br(),
                                            
                                            h4("Superposed epoch analysis", style = "color: #094030;"),
                                            
                                            h6(textOutput("text_years6"), style = "color: #094030;"),
                                            textOutput("years6"),
                                            
                                            br(),
                                            
                                            shinycssloaders::withSpinner(ui_element = plotOutput("SEA_plot_6",click = "ts_click6", dblclick = "ts_dblclick6", brush = brushOpts(id = "ts_brush6", resetOnNew = TRUE)),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),
                                            
                                            #### Customization panels START ----       
                                            fluidRow(
                                              
                                              #### Timeseries customization ----
                                              column(width = 4,
                                                     h4("Customize your SEA", style = "color: #094030;", timeseries_customization_popover("pop_sea_custime")),  
                                                     
                                                     checkboxInput(inputId = "custom_6",
                                                                   label   = "SEA customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden( 
                                                       div(id = "hidden_custom_6",
                                                           
                                                           radioButtons(inputId  = "title_mode_6",
                                                                        label    = "Title and lable customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" ,
                                                                        inline = TRUE),
                                                           
                                                           shinyjs::hidden( 
                                                             div(id = "hidden_custom_title_6",
                                                                 
                                                                 textInput(inputId     = "title1_input_6",
                                                                           label       = "Custom plot title:", 
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 textInput(inputId    = "y_label_6",
                                                                           label     = "Custom y-axis label:",
                                                                           value     = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom lable",
                                                                           updateOn = "blur"),
                                                             )),
                                                           
                                                           radioButtons(inputId  = "axis_mode_6",
                                                                        label    = "Y-axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis_6",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input_6",
                                                                                   label      = "Set your axis values:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "show_observations_6",
                                                                         label   = "Show all observations",
                                                                         value   = FALSE),
                                                           
                                                           checkboxInput(inputId = "show_pvalues_6",
                                                                         label   = "Show p values",
                                                                         value   = FALSE),
                                                           
                                                           checkboxInput(inputId = "show_ticks_6",
                                                                         label   = "Show yearly ticks",
                                                                         value   = FALSE),
                                                           
                                                           checkboxInput(inputId = "show_key_6",
                                                                         label   = "Show key",
                                                                         value   = FALSE),
                                                       )),    
                                              ),
                                              
                                              #### No Custom Features ----                        
                                              column(width = 4
                                              ),
                                              
                                              #### Custom statistics ----
                                              column(width = 4,
                                                     
                                                     h4("Custom statistics", style = "color: #094030;", sea_statistics_popover("pop_sea_statistics")),
                                                     
                                                     checkboxInput(inputId = "enable_custom_statistics_6",
                                                                   label   = "Enable custom statistics",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_statistics_6",
                                                           
                                                           numericInput(inputId   = "sample_size_6",
                                                                        label     = "Size of random sample",
                                                                        value     = 1000,
                                                                        min       = 100,
                                                                        max       = 100000000000,
                                                                        updateOn = "blur"),
                                                           
                                                           checkboxInput(inputId = "show_means_6",
                                                                         label   = "Show sample means",
                                                                         value   = FALSE),
                                                           
                                                           radioButtons(inputId  = "show_confidence_bands_6",
                                                                        label    = "Confidence interval",
                                                                        choices  = c("None","95%", "99%"),
                                                                        selected = "None" , inline = TRUE)
                                                           
                                                       )),
                                              ),
                                              
                                              #### Customization panels END ----
                                            ),
                                            
                                            #### Downloads SEA ----
                                            h4("Downloads", style = "color: #094030;", downloads_popover("pop_sea_ts_downloads")),
                                            checkboxInput(inputId = "download_options_6",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download_6",
                                                                
                                                                # Downloads 
                                                                h4(helpText("SEA plot")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_timeseries6", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_timeseries6", label = "Download SEA"))
                                                                ),
                                                                
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_timeseries_data6", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_timeseries_data6", label = "Download SEA data"))
                                                                ),
                                                                
                                                                shinyjs::hidden(div(id = "hidden_meta6",
                                                                                    
                                                                                    hr(),
                                                                                    
                                                                                    # Upload Meta data 
                                                                                    h4(helpText("Metadata")),
                                                                                    fluidRow(
                                                                                      column(3, downloadButton(outputId = "download_metadata_6", label = "Download metadata")),
                                                                                      column(4, fileInput(inputId= "upload_metadata_6", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                                      column(2, actionButton(inputId = "update_metadata_6", label = "Update upload inputs")),
                                                                                    ),
                                                                                    
                                                                )),
                                            )),
                                            ### SEA END ----
                                   ),
                                   
                                   ## Main Panel END ----
             ), width = 8)
             
             # SEA END ----  
           )),
  
  # Correlation START ----
  tabPanel("Correlation", value = "tab3",
           shinyjs::useShinyjs(),
           
           sidebarLayout(
             ## Sidebar Panels START ----          
             sidebarPanel(verticalLayout(
               
               ### First Sidebar panel (Variable 1) ----
               sidebarPanel(fluidRow(
                 #Method Title and Pop Over
                 correlation_summary_popover("pop_correlation"),
                 br(),
                 
                 h4("Variable 1", style = "color: #094030;",correlation_variable_popover("pop_correlation_variable1")),
                 
                 #Choose a data source: ME or USer 
                 radioButtons(inputId  = "source_v1",
                              label    = "Choose a data source:",
                              choices  = c("ModE-", "User data"),
                              selected = "ModE-" ,
                              inline = TRUE),
                 
                 # Upload user data
                 shinyjs::hidden(
                   div(id = "upload_forcings_v1",   
                       fileInput(inputId = "user_file_v1",
                                 label = "Upload timeseries data in .csv or .xlsx format:",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx", ".xls"),
                                 width = NULL,
                                 buttonLabel = "Browse your folders",
                                 placeholder = "No file selected"),
                       
                       div(id = "upload_example_v1",
                           img(src = 'pics/regcor_user_example.jpg', id = "cor_user_example_v1", height = "300px", width = "300px"))
                   )),
                 
                 #Choose a variable (USER)
                 shinyjs::hidden(
                   div(id = "hidden_user_variable_v1",
                       selectInput(inputId  = "user_variable_v1",
                                   label    = "Choose a variable:",
                                   choices  = NULL,
                                   selected = NULL),
                   )),
                 
                 #Choose one of three datasets (Select)
                 shinyjs::hidden(
                   div(id = "hidden_me_dataset_variable_v1",
                       fluidRow(
                         selectInput(inputId  = "dataset_selected_v1",
                                     label    = "Choose a dataset:",
                                     choices  = c("ModE-RA", "ModE-Sim","ModE-RAclim"),
                                     selected = "ModE-RA"),
                         
                         #Choose a variable (Mod-ERA) 
                         selectInput(inputId  = "ME_variable_v1",
                                     label    = "Choose a variable to plot:",
                                     choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
                                     selected = "Temperature")),
                   )),
                 
                 shinyjs::hidden(
                   div(id = "hidden_modera_variable_v1",
                       #Choose how to use ME data: As a timeseries or field  
                       radioButtons(inputId  = "type_v1",
                                    label    = "Choose how to use ModE-RA data:",
                                    choices  = c("Field", "Timeseries"),
                                    selected = "Timeseries" ,
                                    inline = TRUE),
                       
                       #Choose a Mode: Absolute or Anomaly 
                       radioButtons(inputId  = "mode_selected_v1",
                                    label    = "Choose a mode:",
                                    choices  = c("Anomaly", "Absolute"),
                                    selected = "Anomaly" ,
                                    inline = TRUE),
                       
                       #Choose Season, Year or Months
                       radioButtons(inputId  = "season_selected_v1",
                                    label    = "Select the range of months:",
                                    choices  = c("Annual", "DJF", "MAM", "JJA", "SON", "Custom"),
                                    selected = "Annual" ,
                                    inline = TRUE),
                       
                       #Choose your range of months (Slider)
                       shinyjs::hidden(
                         div(id = "season_v1",
                             shinyWidgets::sliderTextInput(inputId = "range_months_v1",
                                             label = "Select custom months:",
                                             choices = c("December (prev.)", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                                             #Initially selected = 1 year (annual mean)
                                             selected = c("January", "December")),
                         )), 
                       
                       #Choose reference period if Anomaly values are chosen (Hidden object)      
                       shinyjs::hidden(
                         div(id = "optional_v1",
                             shinyjs::hidden(
                               shinyWidgets::numericRangeInput(inputId = "ref_period_v1",
                                                 label      = "Reference period:",
                                                 value      = c(1961,1990),
                                                 separator  = " to ",
                                                 min        = 1422,
                                                 max        = 2008)),
                             
                             #Choose single ref year
                             column(12,
                                    checkboxInput(inputId = "ref_single_year_v1",
                                                  label   = "Select single year",
                                                  value   = FALSE)),
                             
                             
                             shinyjs::hidden(
                               numericInput(inputId   = "ref_period_sg_v1",
                                            label     = "Select the single year:",
                                            value     = NA,
                                            min       = 1422,
                                            max       = 2008,
                                            updateOn = "blur")),
                             
                         )),
                       
                       #Choose Coordinates input 
                       radioButtons(inputId  = "coordinates_type_v1",
                                    label    = "Choose input of coordinates:",
                                    choices  = c("Manual", "Continents"),
                                    selected = "Manual" ,
                                    inline = TRUE),
                       
                       shinyjs::hidden(
                         div(id = "hidden_continents_v1",
                             column(width = 12, fluidRow(
                               
                               br(), br(),
                               
                               #Global Button
                               actionButton(inputId = "button_global_v1",
                                            label = "Global",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Europe Button
                               actionButton(inputId = "button_europe_v1",
                                            label = "Europe",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Asia Button
                               actionButton(inputId = "button_asia_v1",
                                            label = "Asia",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Oceania Button
                               actionButton(inputId = "button_oceania_v1",
                                            label = "Oceania",
                                            width = "110px"),
                               
                             )),
                             
                             column(width = 12, br()),
                             
                             column(width = 12, fluidRow(
                               
                               br(), br(),
                               
                               #Africa Button
                               actionButton(inputId = "button_africa_v1",
                                            label = "Africa",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #North America Button
                               actionButton(inputId = "button_n_america_v1",
                                            label = "North America",
                                            width = "150px"),
                               
                               br(), br(),
                               
                               #South America Button
                               actionButton(inputId = "button_s_america_v1",
                                            label = "South America",
                                            width = "150px")
                             )),
                             
                             column(width = 12, br()),
                             
                         )),
                       
                       #Choose Longitude and Latitude Range          
                       shinyWidgets::numericRangeInput(inputId = "range_longitude_v1",
                                         label = "Longitude range (-180 to 180):",
                                         value = c(4,12),
                                         separator = " to ",
                                         min = -180,
                                         max = 180),
                       
                       #Choose Longitude and Latitude Range          
                       shinyWidgets::numericRangeInput(inputId = "range_latitude_v1",
                                         label = "Latitude range (-90 to 90):",
                                         value = c(43,50),
                                         separator = " to ",
                                         min = -90,
                                         max = 90),
                       
                       #Enter Coordinates
                       actionButton(inputId = "button_coord_v1",
                                    label = "Update coordinates",
                                    width = "200px"),
                       
                   )),
                 
                 
               ), width = 12),
               
               br(),
               
               ### Second Sidebar panel (Year range) ----
               
               sidebarPanel(fluidRow(
                 
                 #Short description of the temporal selection        
                 h4("Year range", style = "color: #094030;",correlation_year_popover("pop_correlation_year")),
                 
                 #Choose your year of interest
                 column(width = 8,
                        
                        shinyWidgets::numericRangeInput(inputId    = "range_years3",
                                          label     = "Select the range of years (1422 - 2008):",
                                          value     = initial_year_values,
                                          separator = " to ",
                                          min       = -3000,
                                          max       = 3000)
                 ),
                 
                 # Set lag years
                 column(width = 8, fluidRow(
                   numericInput(inputId   = "lagyears_v1_cor",
                                label     = "Set variable 1 lag (in years):",
                                value     = 0,
                                min       = -100,
                                max       = 100),
                   
                   numericInput(inputId   = "lagyears_v2_cor",
                                label     = "Set variable 2 lag (in years):",
                                value     = 0,
                                min       = -100,
                                max       = 100),
                   
                 ))
                 
               ), width = 12),
               
               br(),
               
               ### Third Sidebar panel (Variable 2) ----
               
               sidebarPanel(fluidRow(
                 #Short description of the General Panel        
                 h4("Variable 2", style = "color: #094030;",correlation_variable_popover("pop_correlation_variable2")),
                 
                 #Choose a data source: ME or USer 
                 radioButtons(inputId  = "source_v2",
                              label    = "Choose a data source:",
                              choices  = c("ModE-", "User data"),
                              selected = "ModE-" ,
                              inline = TRUE),
                 
                 # Upload user data
                 shinyjs::hidden(
                   div(id = "upload_forcings_v2", 
                       fileInput(inputId = "user_file_v2",
                                 label = "Upload timeseries data in .csv or .xlsx format:",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx", ".xls"),
                                 width = NULL,
                                 buttonLabel = "Browse your folders",
                                 placeholder = "No file selected"),
                       
                       div(id = "upload_example_v2",
                           img(src = 'pics/regcor_user_example.jpg', id = "cor_user_example_v2", height = "300px", width = "300px")),
                   )),
                 
                 #Choose a variable (USER)
                 shinyjs::hidden(
                   div(id = "hidden_user_variable_v2",
                       selectInput(inputId  = "user_variable_v2",
                                   label    = "Choose a variable:",
                                   choices  = NULL,
                                   selected = NULL),
                   )),
                 
                 #Choose one of three datasets (Select)
                 shinyjs::hidden(
                   div(id = "hidden_me_dataset_variable_v2",
                       fluidRow(
                         selectInput(inputId  = "dataset_selected_v2",
                                     label    = "Choose a dataset:",
                                     choices  = c("ModE-RA", "ModE-Sim","ModE-RAclim"),
                                     selected = "ModE-RA"),
                         
                         #Choose a variable (Mod-ERA) 
                         selectInput(inputId  = "ME_variable_v2",
                                     label    = "Choose a variable to plot:",
                                     choices  = c("Temperature", "Precipitation", "SLP", "Z500"),
                                     selected = "Temperature")),
                   )),
                 
                 shinyjs::hidden(
                   div(id = "hidden_modera_variable_v2",
                       #Choose how to use ME data: As a timeseries or field  
                       radioButtons(inputId  = "type_v2",
                                    label    = "Choose how to use the ModE-RA data:",
                                    choices  = c("Field","Timeseries"),
                                    selected = "Field" ,
                                    inline = TRUE),
                       
                       #Choose a Mode: Absolute or Anomaly 
                       radioButtons(inputId  = "mode_selected_v2",
                                    label    = "Choose a mode:",
                                    choices  = c("Anomaly", "Absolute"),
                                    selected = "Anomaly" ,
                                    inline = TRUE),
                       
                       #Choose Season, Year or Months
                       radioButtons(inputId  = "season_selected_v2",
                                    label    = "Select the range of months:",
                                    choices  = c("Annual", "DJF", "MAM", "JJA", "SON", "Custom"),
                                    selected = "Annual" ,
                                    inline = TRUE),
                       
                       #Choose your range of months (Slider)
                       shinyjs::hidden(
                         div(id = "season_v2",
                             shinyWidgets::sliderTextInput(inputId = "range_months_v2",
                                             label = "Select custom months:",
                                             choices = c("December (prev.)", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                                             #Initially selected = 1 year (annual mean)
                                             selected = c("January", "December")),
                         )), 
                       
                       #Choose reference period if Anomaly values are chosen (Hidden object)      
                       shinyjs::hidden(
                         div(id = "optional_v2",
                             shinyjs::hidden(
                               shinyWidgets::numericRangeInput(inputId = "ref_period_v2",
                                                 label      = "Reference period:",
                                                 value      = c(1961,1990),
                                                 separator  = " to ",
                                                 min        = 1422,
                                                 max        = 2008)),
                             
                             #Choose single ref year
                             column(12,
                                    checkboxInput(inputId = "ref_single_year_v2",
                                                  label   = "Select single year",
                                                  value   = FALSE)),
                             
                             
                             shinyjs::hidden(
                               numericInput(inputId   = "ref_period_sg_v2",
                                            label     = "Select the single year:",
                                            value     = NA,
                                            min       = 1422,
                                            max       = 2008,
                                            updateOn = "blur")),
                             
                             
                         )),
                       
                       #Choose Coordinates input 
                       radioButtons(inputId  = "coordinates_type_v2",
                                    label    = "Choose input of coordinates:",
                                    choices  = c("Manual", "Continents"),
                                    selected = "Manual" ,
                                    inline = TRUE),
                       
                       shinyjs::hidden(
                         div(id = "hidden_continents_v2",
                             column(width = 12, fluidRow(
                               
                               br(), br(),
                               
                               #Global Button
                               actionButton(inputId = "button_global_v2",
                                            label = "Global",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Europe Button
                               actionButton(inputId = "button_europe_v2",
                                            label = "Europe",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Asia Button
                               actionButton(inputId = "button_asia_v2",
                                            label = "Asia",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #Oceania Button
                               actionButton(inputId = "button_oceania_v2",
                                            label = "Oceania",
                                            width = "110px"),
                               
                             )),
                             
                             column(width = 12, br()),
                             
                             column(width = 12, fluidRow(
                               
                               br(), br(),
                               
                               #Africa Button
                               actionButton(inputId = "button_africa_v2",
                                            label = "Africa",
                                            width = "100px"),
                               
                               br(), br(),
                               
                               #North America Button
                               actionButton(inputId = "button_n_america_v2",
                                            label = "North America",
                                            width = "150px"),
                               
                               br(), br(),
                               
                               #South America Button
                               actionButton(inputId = "button_s_america_v2",
                                            label = "South America",
                                            width = "150px")
                             )),
                             
                             column(width = 12, br()),
                             
                         )),
                       
                       #Choose Longitude and Latitude Range          
                       shinyWidgets::numericRangeInput(inputId = "range_longitude_v2",
                                         label = "Longitude range (-180 to 180):",
                                         value = initial_lon_values,
                                         separator = " to ",
                                         min = -180,
                                         max = 180),
                       
                       #Choose Longitude and Latitude Range          
                       shinyWidgets::numericRangeInput(inputId = "range_latitude_v2",
                                         label = "Latitude range (-90 to 90):",
                                         value = initial_lat_values,
                                         separator = " to ",
                                         min = -90,
                                         max = 90),
                       
                       #Enter Coordinates
                       actionButton(inputId = "button_coord_v2",
                                    label = "Update coordinates",
                                    width = "200px"),
                   )),
                 
               ), width = 12)
               
               ## Sidebar Panels END ----
             )),
             
             ## Main Panel START ----
             mainPanel(tabsetPanel(id = "tabset3",
                                   ### v1, v2 plot: ----
                                   tabPanel("Variables", br(),
                                            h4("Variable 1", style = "color: #094030;"),
                                            shinycssloaders::withSpinner(ui_element = plotOutput("plot_v1", height = "auto"),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),
                                            
                                            h4("Variable 2", style = "color: #094030;"),
                                            shinycssloaders::withSpinner(ui_element = plotOutput("plot_v2", height = "auto"),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height)),
                                   
                                   ### Shared TS plot: START ----
                                   tabPanel("Timeseries", br(),
                                            
                                            correlation_timeseries_popover("pop_correlation_timeseries"),
                                            
                                            br(),
                                            
                                            #Choose a correlation method
                                            #Button is synchronized with "cor_method_map" and "cor_method_map_data"
                                            radioButtons(inputId  = "cor_method_ts",
                                                         label    = "Choose a correlation method:",
                                                         choices  = c("pearson", "spearman"),
                                                         selected = "pearson" , inline = TRUE),
                                            
                                            
                                            textOutput("correlation_r_value"),
                                            textOutput("correlation_p_value"),
                                            br(),
                                            shinycssloaders::withSpinner(ui_element = plotOutput("correlation_ts",click = "ts_click3", dblclick = "ts_dblclick3", brush = brushOpts(id = "ts_brush3",resetOnNew = TRUE)),
                                                                          image = spinner_image,
                                                                          image.width = spinner_width,
                                                                          image.height = spinner_height),
                                            
                                            #### Customization panels START ----       
                                            fluidRow(
                                              
                                              #### Timeseries customization ----
                                              column(width = 4,
                                                     h4("Customize your timeseries", style = "color: #094030;",timeseries_customization_popover("pop_correlation_custime")),  
                                                     
                                                     checkboxInput(inputId = "custom_ts3",
                                                                   label   = "Timeseries customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden( 
                                                       div(id = "hidden_custom_ts3",
                                                           radioButtons(inputId  = "title_mode_ts3",
                                                                        label    = "Title customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" ,
                                                                        inline = TRUE),
                                                           
                                                           shinyjs::hidden( 
                                                             div(id = "hidden_custom_title_ts3",
                                                                 
                                                                 textInput(inputId     = "title1_input_ts3",
                                                                           label       = "Custom plot title:", 
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 numericInput(inputId = "title_size_input_ts3",
                                                                              label   = "Font size:",
                                                                              value   = 18,
                                                                              min     = 1,
                                                                              max     = 40,
                                                                              updateOn = "blur"),
                                                             )),
                                                           
                                                           radioButtons(inputId  = "axis_mode_ts3",
                                                                        label    = "Y-axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis_ts3",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input_ts3",
                                                                                   label      = "Set your axis values for V1:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "show_ticks_ts3",
                                                                         label   = "Customize year axis intervals",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_xaxis_interval_ts3",
                                                                 
                                                                 numericInput(inputId = "xaxis_numeric_interval_ts3",
                                                                              label   = "Year axis intervals:",
                                                                              value   = 50,
                                                                              min     = 1,
                                                                              max     = 500,
                                                                              updateOn = "blur"))),
                                                           
                                                           checkboxInput(inputId = "show_key_ts3",
                                                                         label   = "Show key",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_key_position_ts3",
                                                                 radioButtons(inputId  = "key_position_ts3",
                                                                              label    = "Key position:",
                                                                              choiceNames  = c("right","bottom"),
                                                                              choiceValues = c("right","bottom"),
                                                                              selected = "right" ,
                                                                              inline = TRUE))),
                                                       )),    
                                              ),
                                              
                                              #### Add Custom features (points, highlights, lines) ----                        
                                              column(width = 4,
                                                     h4("Custom features", style = "color: #094030;",timeseries_features_popover("pop_correlation_timefeat")),
                                                     
                                                     checkboxInput(inputId = "custom_features_ts3",
                                                                   label   = "Enable custom features",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_features_ts3",
                                                           radioButtons(inputId      = "feature_ts3",
                                                                        label        = "Select a feature type:",
                                                                        inline       = TRUE,
                                                                        choices      = c("Point", "Highlight", "Line")),
                                                           
                                                           #Custom Points
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_points_ts3",
                                                                 
                                                                 h4(helpText("Add custom points",timeseries_points_popover("pop_correlation_timepoint"))),
                                                                 h6(helpText("Enter position manually or click on plot")),
                                                                 
                                                                 textInput(inputId = "point_label_ts3", 
                                                                           label   = "Point label:",
                                                                           value   = ""),
                                                                 
                                                                 column(width = 12, offset = 0,
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_x_ts3", 
                                                                                         label   = "Point x position:",
                                                                                         value   = "")
                                                                        ),
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_y_ts3", 
                                                                                         label   = "Point y position:",
                                                                                         value   = "")
                                                                        )),
                                                                 
                                                                 
                                                                 radioButtons(inputId      = "point_shape_ts3",
                                                                              label        = "Point shape:",
                                                                              inline       = TRUE,
                                                                              choices      = c("\u25CF", "\u25B2", "\u25A0")),
                                                                 
                                                                 colourpicker::colourInput(inputId = "point_colour_ts3", 
                                                                                           label   = "Point colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),                        
                                                                 
                                                                 
                                                                 numericInput(inputId = "point_size_ts3",
                                                                              label   = "Point size",
                                                                              value   = 4,
                                                                              min     = 4,
                                                                              max     = 20),
                                                                 
                                                                 column(width = 12,
                                                                        
                                                                        actionButton(inputId = "add_point_ts3",
                                                                                     label = "Add point"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_point_ts3",
                                                                                     label = "Remove last point"),
                                                                        actionButton(inputId = "remove_all_points_ts3",
                                                                                     label = "Remove all points")),
                                                             )),
                                                           
                                                           #Custom highlights
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_highlights_ts3",
                                                                 
                                                                 h4(helpText("Add custom highlights",timeseries_highlights_popover("pop_correlation_timehl"))),
                                                                 h6(helpText("Enter values manually or draw a box on plot")),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_x_values_ts3",
                                                                                   label  = "X values:",
                                                                                   value  = ""),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_y_values_ts3",
                                                                                   label  = "Y values:",
                                                                                   value  = ""),
                                                                 
                                                                 colourpicker::colourInput(inputId = "highlight_colour_ts3", 
                                                                                           label   = "Highlight colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "highlight_type_ts3",
                                                                              label        = "Type for highlight:",
                                                                              inline       = TRUE,
                                                                              choiceNames  = c("Fill \u25A0", "Box \u2610"),
                                                                              choiceValues = c("Fill","Box")),
                                                                 
                                                                 checkboxInput(inputId = "show_highlight_on_legend_ts3",
                                                                               label   = "Show on key",
                                                                               value   = FALSE),
                                                                 
                                                                 shinyjs::hidden(
                                                                   textInput(inputId = "highlight_label_ts3", 
                                                                             label   = "Label:",
                                                                             value   = "")),
                                                                 
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_highlight_ts3",
                                                                                     label = "Add highlight"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_highlight_ts3",
                                                                                     label = "Remove last highlight"),
                                                                        actionButton(inputId = "remove_all_highlights_ts3",
                                                                                     label = "Remove all highlights")),
                                                                 
                                                             )),
                                                           
                                                           #Custom lines
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_line_ts3",
                                                                 
                                                                 h4(helpText("Add custom lines",timeseries_lines_popover("pop_correlation_timelines"))),
                                                                 h6(helpText("Enter position manually or click on plot, double click to change orientation")),
                                                                 
                                                                 radioButtons(inputId      = "line_orientation_ts3",
                                                                              label        = "Orientation:",
                                                                              inline       = TRUE,
                                                                              choices      = c("Vertical", "Horizontal")),
                                                                 
                                                                 textInput(inputId = "line_position_ts3", 
                                                                           label   = "Position:",
                                                                           value   = "",
                                                                           placeholder = "1830, 1832"),
                                                                 
                                                                 colourpicker::colourInput(inputId = "line_colour_ts3", 
                                                                                           label   = "Line colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "line_type_ts3",
                                                                              label        = "Type:",
                                                                              inline       = TRUE,
                                                                              choices = c("solid", "dashed")),
                                                                 
                                                                 checkboxInput(inputId = "show_line_on_legend_ts3",
                                                                               label   = "Show on key",
                                                                               value   = FALSE),
                                                                 
                                                                 shinyjs::hidden(
                                                                   textInput(inputId = "line_label_ts3", 
                                                                             label   = "Label:",
                                                                             value   = "")),
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_line_ts3",
                                                                                     label = "Add line"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_line_ts3",
                                                                                     label = "Remove last line"),
                                                                        actionButton(inputId = "remove_all_lines_ts3",
                                                                                     label = "Remove all lines")
                                                                 )
                                                             ))
                                                       ))),
                                              
                                              #### Customization panels END ----
                                            ),
                                            
                                            #### Downloads TS ----
                                            h4("Downloads", style = "color: #094030;",downloads_popover("pop_correlation_ts_downloads")),
                                            checkboxInput(inputId = "download_options_ts3",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download_ts3",
                                                                
                                                                # Downloads 
                                                                h4(helpText("Timeseries")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_timeseries3", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_timeseries3", label = "Download timeseries"))
                                                                ),
                                                                
                                                                shinyjs::hidden(div(id = "hidden_meta_ts3",
                                                                                    
                                                                                    hr(),
                                                                                    
                                                                                    # Upload Meta data 
                                                                                    h4(helpText("Metadata")),
                                                                                    fluidRow(
                                                                                      column(3, downloadButton(outputId = "download_metadata_ts3", label = "Download metadata")),
                                                                                      column(4, fileInput(inputId= "upload_metadata_ts3", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                                      column(2, actionButton(inputId = "update_metadata_ts3", label = "Update upload inputs")),
                                                                                    ),
                                                                                    
                                                                )),
                                            )),
                                            
                                            hr(),
                                            
                                            #### Scatter Plot START ----
                                            h4("Scatter plot", style = "color: #094030;", reference_map_popover("pop_correlation_refmap")), 
                                            
                                            radioButtons(inputId  = "ref_map_mode3",
                                                         label    = NULL,
                                                         choices  = c("None", "Scatter plot"),
                                                         selected = "None" , inline = TRUE),
                                            
                                            shinyjs::hidden(div(id ="hidden_sec_map_download3",
                                                                
                                                                shinycssloaders::withSpinner(ui_element = plotOutput("ref_map3"), 
                                                                                              image = spinner_image,
                                                                                              image.width = spinner_width,
                                                                                              image.height = spinner_height),
                                                                
                                                                #### Scatter plot customization ----
                                                                h4("Customize your scatter plot", style = "color: #094030;", timeseries_scatter_popover("pop_correlation_scatter")), 
                                                                fluidRow(column(width = 4,
                                                                                
                                                                                checkboxInput(inputId = "custom_ref_ts3",
                                                                                              label   = "Scatter plot customization",
                                                                                              value   = FALSE),
                                                                                
                                                                                shinyjs::hidden( 
                                                                                  div(id = "hidden_custom_ref_ts3",
                                                                                      
                                                                                      checkboxInput(inputId = "add_trend_ref_ts3",
                                                                                                    label   = "Add trendline:",
                                                                                                    value   = FALSE),
                                                                                      
                                                                                      radioButtons(inputId  = "add_outliers_ref_ts3"   ,
                                                                                                   label    = "Highlight statistical outliers:",
                                                                                                   choices  = c("None", "z-score", "Trend deviation"),
                                                                                                   selected = "None",
                                                                                                   inline = TRUE),
                                                                                      
                                                                                      shinyjs::hidden( 
                                                                                        div(id = "hidden_custom_score_ref_ts3",
                                                                                            
                                                                                            numericInput( inputId     = "sd_input_ref_ts3",
                                                                                                          label       = "Adjust SD for z-score:", 
                                                                                                          value       = 1,
                                                                                                          min         = 1,
                                                                                                          max         = 10,
                                                                                                          step        = 0.1,
                                                                                                          width       = "50%",
                                                                                                          updateOn = "blur")
                                                                                        )),
                                                                                      
                                                                                      shinyjs::hidden( 
                                                                                        div(id = "hidden_custom_trend_sd_ref_ts3",
                                                                                            
                                                                                            numericInput( inputId     = "trend_sd_input_ref_ts3",
                                                                                                          label       = "Adjust SD for trend deviation:", 
                                                                                                          value       = 1,
                                                                                                          min         = 1,
                                                                                                          max         = 10,
                                                                                                          step        = 0.1,
                                                                                                          width       = "50%",
                                                                                                          updateOn = "blur")
                                                                                        )),
                                                                                      
                                                                                      checkboxInput(inputId = "show_key_ref_ts3",
                                                                                                    label   = "Show key",
                                                                                                    value   = FALSE),
                                                                                      
                                                                                  )),    
                                                                )),
                                                                
                                                                #### Download scatter plot ----
                                                                h4("Download scatter plot", style = "color: #094030;"),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_map_sec3", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_map_sec3", label = "Download scatter plot"))
                                                                ),
                                                                #### Scatter Plot END ----
                                            )),
                                            
                                            #Easter Calm
                                            conditionalPanel(
                                              condition = "input.title1_input_ts3 == 'Keep Calm'",
                                              tags$img(
                                                src = "pics/KCAUCA.png",
                                                id = "img_britannia",
                                                height = "514",
                                                width = "488",
                                                style = "display:block; margin:0 auto;",
                                                loading = "lazy"
                                              )
                                            ),
                                            
                                            
                                            ### Shared TS plot: End ----          
                                   ),
                                   
                                   ### Map plot: START ----
                                   tabPanel("Correlation map", value = "corr_map_tab", br(),
                                            
                                            correlation_map_popover("pop_correlation_map"),
                                            
                                            br(),
                                            
                                            #Choose a correlation method 
                                            # This radio button is linked to radio buttons with inputId = "cor_method_map_data" and "cor_method_ts"
                                            radioButtons(
                                              inputId  = "cor_method_map",
                                              label    = "Choose a correlation method:",
                                              choices  = c("pearson", "spearman"),
                                              selected = "pearson" ,
                                              inline = TRUE
                                            ),
                                            shinycssloaders::withSpinner(
                                              ui_element = plotOutput(
                                                "correlation_map",
                                                height = "750px",
                                                dblclick = "map_dblclick3",
                                                brush = brushOpts(id = "map_brush3", resetOnNew = TRUE)
                                              ),
                                              image = spinner_image,
                                              image.width = spinner_width,
                                              image.height = spinner_height
                                            ), 
                                            
                                            #Easter Leaves
                                            conditionalPanel(
                                              condition = "input.title1_input3 == 'Leaves from the Vine'",
                                              tags$img(
                                                src = "pics/LeavesFromTheVine.jpg",
                                                id = "img_leaves",
                                                height = "450",
                                                width = "600",
                                                style = "display:block; margin:0 auto;",
                                                loading = "lazy"
                                              )
                                            ),
                                            
                                            #### Customization panels START ----       
                                            fluidRow(
                                              #### Map customization ----       
                                              column(width = 4,
                                                     
                                                     h4("Customize your map", style = "color: #094030;",map_customization_popover("pop_correlation_cusmap")),  
                                                     
                                                     checkboxInput(inputId = "custom_map3",
                                                                   label   = "Map customization",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_maps3",
                                                           
                                                           selectInput(inputId = "projection3",
                                                                       label = "Projection:",
                                                                       choices = c("UTM (default)", "Robinson", "Orthographic", "LAEA"),
                                                                       selected = "UTM (default)"),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_map_center3",
                                                                 
                                                                 numericInput(inputId = "center_lat3",
                                                                              label = "Center latitude:",
                                                                              value = 0,
                                                                              min = -90,
                                                                              max = 90,
                                                                              updateOn = "blur"),
                                                                 numericInput(inputId = "center_lon3",
                                                                              label = "Center longitude:",
                                                                              value = 0,
                                                                              min = -180,
                                                                              max = 180,
                                                                              updateOn = "blur"))),
                                                           
                                                           radioButtons(inputId  = "axis_mode3",
                                                                        label    = "Axis customization:",
                                                                        choices  = c("Automatic","Fixed"),
                                                                        selected = "Automatic" , inline = TRUE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_axis3",
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId    = "axis_input3",
                                                                                   label      = "Set your axis values:",
                                                                                   value      = c(NULL, NULL),
                                                                                   separator  = " to ",
                                                                                   min        = -Inf,
                                                                                   max        = Inf))),
                                                           
                                                           checkboxInput(inputId = "hide_axis3",
                                                                         label   = "Hide colorbar",
                                                                         value   = FALSE),
                                                           
                                                           br(),
                                                           
                                                           radioButtons(inputId  = "title_mode3",
                                                                        label    = "Title customization:",
                                                                        choices  = c("Default", "Custom"),
                                                                        selected = "Default" , inline = TRUE),
                                                           
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_title3",
                                                                 
                                                                 textInput(inputId     = "title1_input3",
                                                                           label       = "Custom map title:", 
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 textInput(inputId     = "title2_input3",
                                                                           label       = "Custom map subtitle (e.g. Ref-Period)",
                                                                           value       = NA,
                                                                           width       = NULL,
                                                                           placeholder = "Custom title",
                                                                           updateOn = "blur"),
                                                                 
                                                                 numericInput(inputId = "title_size_input3",
                                                                              label   = "Font size:",
                                                                              value   = 18,
                                                                              min     = 1,
                                                                              max     = 40,
                                                                              updateOn = "blur"))),
                                                           
                                                           br(), hr(),
                                                           
                                                           h4(helpText("Topography options and GIS upload (.shp)", map_customization_layers_popover("pop_correlation_layers"))),
                                                           
                                                           checkboxInput(inputId = "custom_topo3",
                                                                         label   = "Topographical customization",
                                                                         value   = FALSE),
                                                           
                                                           shinyjs::hidden(
                                                             div(id = "hidden_geo_options3",
                                                                 
                                                                 checkboxInput(inputId = "hide_borders3",
                                                                               label   = "Show country borders",
                                                                               value   = TRUE),
                                                                 
                                                                 checkboxInput(inputId = "white_ocean3",
                                                                               label   = "Grey ocean",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "white_land3",
                                                                               label   = "Grey land",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_rivers3",
                                                                               label   = "Show rivers",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_rivers3",
                                                                               label   = "Label rivers",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_lakes3",
                                                                               label   = "Show lakes",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_lakes3",
                                                                               label   = "Label lakes",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "show_mountains3",
                                                                               label   = "Show mountains",
                                                                               value   = FALSE),
                                                                 
                                                                 checkboxInput(inputId = "label_mountains3",
                                                                               label   = "Label mountains",
                                                                               value   = FALSE),
                                                                 
                                                             )),
                                                           
                                                           #Shape File Option
                                                           
                                                           fileInput(inputId = "shpFile3", label = "Upload shapefile (ZIP)"),
                                                           
                                                           # Single sortable checkbox group input for selecting and ordering shapefiles
                                                           uiOutput("shapefileSelector3"),
                                                           
                                                           uiOutput("colorpickers3")
                                                           
                                                       )),
                                              ),
                                              
                                              #### Add Custom features (points and highlights) ----                        
                                              column(width = 4,
                                                     
                                                     h4("Custom features", style = "color: #094030;",map_features_popover("pop_correlation_mapfeat")),
                                                     
                                                     checkboxInput(inputId = "custom_features3",
                                                                   label   = "Enable custom features",
                                                                   value   = FALSE),
                                                     
                                                     shinyjs::hidden(
                                                       div(id = "hidden_custom_features3",
                                                           radioButtons(inputId      = "feature3",
                                                                        label        = "Select a feature type:",
                                                                        inline       = TRUE,
                                                                        choices      = c("Point", "Highlight")),
                                                           
                                                           #Custom Points
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_points3",
                                                                 
                                                                 h4(helpText("Add custom points",map_points_popover("pop_correlation_mappoint"))),
                                                                 h6(helpText("Enter location/coordinates or double click on map")),
                                                                 
                                                                 textInput(inputId = "location3", 
                                                                           label   = "Enter a location:",
                                                                           value   = NULL,
                                                                           placeholder = "e.g. Bern"),
                                                                 
                                                                 actionButton(inputId = "search3",
                                                                              label   = "Search"),
                                                                 
                                                                 
                                                                 shinyjs::hidden(div(id = "inv_location3",
                                                                                     h6(helpText("Invalid location"))
                                                                 )),
                                                                 
                                                                 textInput(inputId = "point_label3", 
                                                                           label   = "Point label:",
                                                                           value   = ""),
                                                                 
                                                                 column(width = 12, offset = 0,
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_x3", 
                                                                                         label   = "Point longitude:",
                                                                                         value   = "")
                                                                        ),
                                                                        column(width = 6,
                                                                               textInput(inputId = "point_location_y3", 
                                                                                         label   = "Point latitude:",
                                                                                         value   = "")
                                                                        )),
                                                                 
                                                                 radioButtons(inputId      = "point_shape3",
                                                                              label        = "Point shape:",
                                                                              inline       = TRUE,
                                                                              choices      = c("\u25CF", "\u25B2", "\u25A0")),
                                                                 
                                                                 colourpicker::colourInput(inputId = "point_colour3", 
                                                                                           label   = "Point colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),                       
                                                                 
                                                                 
                                                                 numericInput(inputId = "point_size3",
                                                                              label   = "Point size:",
                                                                              value   = 1,
                                                                              min     = 1,
                                                                              max     = 10),
                                                                 
                                                                 column(width = 12,
                                                                        
                                                                        actionButton(inputId = "add_point3",
                                                                                     label = "Add point"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_point3",
                                                                                     label = "Remove last point"),
                                                                        actionButton(inputId = "remove_all_points3",
                                                                                     label = "Remove all points")),
                                                             )),
                                                           
                                                           #Custom Highlights
                                                           shinyjs::hidden(
                                                             div(id = "hidden_custom_highlights3",
                                                                 
                                                                 h4(helpText("Add custom highlights",map_highlights_popover("pop_correlation_maphl"))),
                                                                 h6(helpText("Enter coordinate or draw a box on map")),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_x_values3",
                                                                                   label  = "Longitude:",
                                                                                   value  = "",
                                                                                   min    = -180,
                                                                                   max    = 180),
                                                                 
                                                                 shinyWidgets::numericRangeInput(inputId = "highlight_y_values3",
                                                                                   label  = "Latitude:",
                                                                                   value  = "",
                                                                                   min    = -90,
                                                                                   max    = 90),
                                                                 
                                                                 colourpicker::colourInput(inputId = "highlight_colour3", 
                                                                                           label   = "Highlight colour:",
                                                                                           showColour = "background",
                                                                                           value = "#27408B",
                                                                                           palette = "limited"),
                                                                 
                                                                 radioButtons(inputId      = "highlight_type3",
                                                                              label        = "Type for highlight:",
                                                                              inline       = TRUE,
                                                                              choiceNames  = c("Box \u2610", "Filled \u25A0","Hatched \u25A8"),
                                                                              choiceValues = c("Box","Filled","Hatched")),
                                                                 
                                                                 
                                                                 column(width = 12,
                                                                        actionButton(inputId = "add_highlight3",
                                                                                     label = "Add highlight"),
                                                                        br(), br(), br(),
                                                                        actionButton(inputId = "remove_last_highlight3",
                                                                                     label = "Remove last highlight"),
                                                                        actionButton(inputId = "remove_all_highlights3",
                                                                                     label = "Remove all highlights")),
                                                             )),
                                                       )),
                                              ),
                                              
                                              #### Custom statistics ----
                                              column(width = 4
                                                     #[NOT IMPLEMENTED]
                                              ),
                                              #### Customization panels END ----
                                            ),
                                            
                                            #### Downloads Map ----
                                            h4("Downloads", style = "color: #094030;", downloads_popover("pop_correlation_map_downloads")),
                                            checkboxInput(inputId = "download_options3",
                                                          label   = "Enable download options",
                                                          value   = FALSE),
                                            
                                            shinyjs::hidden(div(id = "hidden_download3",
                                                                
                                                                # Download map
                                                                h4(helpText("Map")),
                                                                fluidRow(
                                                                  column(2, radioButtons(inputId = "file_type_map3", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                                                  column(3, downloadButton(outputId = "download_map3", label = "Download map"))
                                                                ),
                                                                
                                                                shinyjs::hidden(div(id = "hidden_meta3",
                                                                                    
                                                                                    hr(),
                                                                                    
                                                                                    # Upload Meta data 
                                                                                    h4(helpText("Metadata")),
                                                                                    fluidRow(
                                                                                      column(3, downloadButton(outputId = "download_metadata3", label = "Download metadata")),
                                                                                      column(4, fileInput(inputId= "upload_metadata3", label = NULL, buttonLabel = "Upload metadata", width = "300px", accept = ".xlsx")),
                                                                                      column(2, actionButton(inputId = "update_metadata3", label = "Update upload inputs")),
                                                                                    ),
                                                                                    
                                                                )),
                                            )),
                                            
                                            ### Map plot: END ----        
                                   ),
                                   
                                   ### Other plots ----
                                   tabPanel("Timeseries data", 
                                            
                                            #Download
                                            br(),  h4("Download", style = "color: #094030;"),
                                            fluidRow(
                                              column(2, radioButtons(inputId = "file_type_timeseries_data3", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                              column(3, downloadButton(outputId = "download_timeseries_data3", label = "Download timeseries data"))
                                            ),
                                            
                                            br(), column(width = 3, 
                                                         shinycssloaders::withSpinner(ui_element = DT::dataTableOutput("correlation_ts_data"),
                                                                                       image = spinner_image,
                                                                                       image.width = spinner_width,
                                                                                       image.height = spinner_height))),
                                   # Correlation map data
                                   tabPanel("Correlation map data", value = "corr_map_data_tab",
                                            
                                            # Data values and Download
                                            br(), 
                                            fluidRow(
                                              column(width = 6,
                                                     h4("Data", style = "color: #094030;"),
                                                     fluidRow(
                                                       # This radio button is linked to radio buttons with inputId = "cor_method_map" and "cor_method_ts"
                                                       column(10, radioButtons(inputId = "cor_method_map_data", label = "Choose correlation method:", 
                                                                               choices = c("pearson", "spearman"), selected = "pearson", inline = TRUE))
                                                     )),
                                              column(width = 6,
                                                     h4("Download", style = "color: #094030;"),
                                                     fluidRow(
                                                       column(6, radioButtons(inputId = "file_type_map_data3", label = "Choose file type:", 
                                                                              choices = c("csv", "xlsx", "GeoTIFF"), selected = "csv", inline = TRUE)),
                                                       column(6, downloadButton(outputId = "download_map_data3", label = "Download map data"))
                                                     ))
                                            ),
                                            
                                            br(), shinycssloaders::withSpinner(ui_element = tableOutput("correlation_map_data"),
                                                                                image = spinner_image,
                                                                                image.width = spinner_width,
                                                                                image.height = spinner_height)),
                                   
                                   ### Feedback archive documentation (FAD) ----
                                   tabPanel("ModE-RA sources", value = "corr_fad_tab", br(),
                                            
                                            # Title & help pop up
                                            MEsource_popover("pop_correlation_mesource"),
                                            
                                            fluidRow(
                                              
                                              # Year entry
                                              numericInput(
                                                inputId  = "fad_year3",
                                                label   =  "Year",
                                                value = 1422,
                                                min = 1422,
                                                max = 2008,
                                                updateOn = "blur"),
                                              
                                              # Enter Season                
                                              selectInput(inputId  = "fad_season3",
                                                          label    = "Months",
                                                          choices  = c("April to September","October to March"),
                                                          selected = "April to September"),
                                            ),
                                            
                                            h6("Use the Explore ModE-RA sources tab for more information", style = "color: #094030;"),
                                            
                                            shinycssloaders::withSpinner(
                                              ui_element = plotOutput(
                                                "fad_map3",
                                                height = "auto",
                                                brush = brushOpts(
                                                  id = "brush_fad3",
                                                  resetOnNew = TRUE,
                                                  delay = 10000,
                                                  # Delay in ms (adjust as needed)
                                                  delayType = "debounce"      # Triggers only after releasing the mouse
                                                )
                                              ),
                                              image = spinner_image,
                                              image.width = spinner_width,
                                              image.height = spinner_height),
                                            
                                            fluidRow(
                                              h6(helpText("Draw a box on the map to zoom in")),
                                              actionButton(inputId = "fad_reset_zoom3",
                                                           label = "Reset zoom",
                                                           width = "200px"),
                                            ),
                                            
                                            br(),
                                            
                                            #Download
                                            h4("Downloads", style = "color: #094030;"),
                                            fluidRow(
                                              column(2,radioButtons(inputId = "file_type_fad3", label = "Choose file type:", choices = c("png", "jpeg", "pdf"), selected = "png", inline = TRUE)),
                                              column(3,downloadButton(outputId = "download_fad3", label = "Download map")),
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              # Download data
                                              column(2,radioButtons(inputId = "data_file_type_fad3", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                                              column(3,downloadButton(outputId = "download_fad_data3", label = "Download map data"))
                                            )
                                   )       
                                   
                                   ## Main Panel END ----
             ), width = 8)
             
             # Correlation END ----  
           )),
  
  # ModE-RA Sources START ----                             
  tabPanel("Explore ModE-RA sources", value = "tab6",
           shinyjs::useShinyjs(),
           
           h4("Explore ModE-RA sources", style = "color: #094030;"),
           h6("This interactive map let's you explore the individual data sources that were used to create ModE-RA and ModE-RAclim.", style = "color: #094030;"),
           h6("Click on a point to get more information and access the database or publication behind it.", style = "color: #094030;"),
           
           fluidRow(
             column(2,
                    # Enter Year
                    numericInput(inputId   = "year_MES",
                                 label     = "Year",
                                 value     = 1422,
                                 min       = 1422,
                                 max       = 2008,
                                 updateOn = "blur"),
                    
                    # Enter Season                
                    selectInput(inputId  = "season_MES",
                                label    = "Months",
                                choices  = c("April to September","October to March"),
                                selected = "April to September"),
                    
                    # Add checkbox for legend
                    checkboxInput(inputId = "legend_MES", "Show legend", FALSE),
                    
                    br(),
                    # Download
                    h4("Download", style = "color: #094030;"),
                    radioButtons(inputId = "data_file_type_MES", label = "Choose file type:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE),
                    downloadButton(outputId = "download_MES_data", label = "Download map data"),
                    
                    br(), br(),
                    #Modera Time Series
                    h4("Total sources", style = "color: #094030;", sourcesandobservations_popover("pop_sourcesandobservation")),
                    
                    shinyWidgets::numericRangeInput(inputId = "year_range_sources",
                                      label = "Select year range:", 
                                      value = c(1421, 2009), 
                                      min = 1421, max = 2009, step = 1)
             ),
             
             column(10, div(id = "leaflet",
                            tags$style(type = "text/css", "#MES_leaflet {height: calc(80vh - 100px) !important;}"), # Adjust the height of the map
                            tags$style(type = "text/css", "div.leaflet-control {text-align: left;}"), # Makes sure that legend text is left-aligned
                            shinycssloaders::withSpinner(ui_element = leaflet::leafletOutput("MES_leaflet"), 
                                                          image = spinner_image,
                                                          image.width = spinner_width,
                                                          image.height = spinner_height)),
                    
                    br(), br(),
                    
                    plotly::plotlyOutput("time_series_plot")  # Use plotlyOutput instead of plotOutput
                    
             )),
           
           br(),
           
           # ModE-RA Sources END ----         
  )
  
  # END ----
)
