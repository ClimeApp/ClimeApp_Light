#### Popovers ####
# This file contains all the popover functions used in ClimeApp. Each function creates a popover with a specific text and style.


#### SIDE BARS ----

## DATASET & VARIABLE
## popover_IDs = pop_anomalies_datvar, pop_composites_datvar

dataset_variable_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Select between the three ModE datasets:", br(), 
    "- ModE-RA – the full reanalysis (recommended)", br(),
    "- ModE-Sim – an ensemble of bounded climate models", br(),
    "- ModE-RAclim – ModE-RA without centennial-scale variability or forcings",br(),br(),
    "And select a climate variable:",br(),
    "- Temperature - air temperature at 2m [°C]",br(),
    "- Precipitation - total monthly precipitation [mm/m]",br(),
    "- SLP - sea level pressure [hPa]",br(),
    "- Z500 - pressure at 500 hPa geopotential height [m]",br(),br(),
    "See",em("ModE data"),"tab on the Welcome page for more information.",
    id = popover_ID,
    placement = "right",
  )
}

## YEAR, SEASON & REFERENCE PERIOD
## popover_IDs = pop_anomalies_time, pop_composites_time

year_season_ref_popover = function(popover_ID){
  # Anomalies popover
  if (popover_ID == "pop_anomalies_time"){
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Enter a",em("range of years"),"and a",em("range of months."),"ClimeApp will then calculate an average over these months for each year in the year range. You can view the absolute values for your selection by selecting",em("Absolute Values"),"under the Reference map selection below the main map.",br(),br(),
      "Then choose the",em("reference period"),"you would like to use for your anomalies. ClimeApp will then subtract the average over this reference period from the average for each year in the year range. You can view the absolute means for your reference period by selecting",em("Reference Values"),"under the",em("Reference map"),"selection below the main map.",
      id = popover_ID,
      placement = "right",
    )     
  } else {
    # Composites popover
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Enter or upload a",em("list of years"),"and a",em("range of months."),"ClimeApp will then calculate an average over these months for each year in the year list. You can view the absolute values for your selection by selecting",em("Absolute Values"),"under the Reference map selection below the main map.",br(),br(),
      "Then choose the",em("reference period"),"you would like to use for your composite. You can select from three types of reference period: A",em("Fixed reference,"),"averaged over a range of consecutive years; a",em("Custom reference,"),"averaged over a set of non-consecutive years; or an",em("X years prior"),"reference period, which will generate a separate reference for each year in the composite, based on the",em("X"),"years preceding that year. ClimeApp will then subtract the average over this reference period (or periods) from the average for each year in the",em("list of years."),"You can view the absolute means for your reference period by selecting",em("Reference Values"),"under the",em("Reference map"),"selection below the main map.",
      id = popover_ID,
      placement = "right",
    )     
  }
}



#### CUSTOMIZATION ----

## MAP CUSTOMIZATION 
## popover_IDs = pop_anomalies_cusmap, pop_composites_cusmap, pop_correlation_cusmap, pop_regression_cusmap

map_customization_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Edit the titles, axes and borders displayed on your map.",br(),br(),
    "You can view and adjust the limits of the map colour axis by selecting",em("Fixed"),"under",em("Axis customization."),"These limits will stay fixed even after a plot has been changed. Select",em("Automatic"),"to allow the axis to adjust automatically again.",
    id = popover_ID,
    placement = "right",
  )
}

## MAP CUSTOMIZATION LAYERS
## popover_IDs = pop_anomalies_layers, pop_composites_layers, pop_correlation_layers

map_customization_layers_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Show or hide modern country border by ticking the checkbox or upload your own",em("GeoPackage-Layer"),"using the upload button.",br(),br(),
    "Layers can be selected individually, their outline separately coloured and even reordered.",br(),
    "You can upload one or several shape files, compressed in a zip file, to add your own",em("shapes, borders, rivers, cities"),"on top of the global map. The shape file(s) (.shp) must have all dependencies (.shx, .dbf, .prj etc.) inside the zip. Accepted shapes are",em("polygons, points and lines."),
    id = popover_ID,
    placement = "right",
  )
}

## CUSTOM MAP FEATURES
## popover_IDs = pop_anomalies_mapfeat, pop_composites_mapfeat, pop_correlation_mapfeat

map_features_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Add labelled points or rectangular highlights (a box or a hashed area) to your map.",
    id = popover_ID,
    placement = "right",
  )    
}

## CUSTOM MAP POINTS 
## popover_IDs = pop_anomalies_mappoint, pop_composites_mappoint, pop_correlation_mappoint

map_points_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "To add a point, double click on the map or search for location. This will automatically set the",em("Point longitude"),"and",em("Point latitude"),"(enter multiple values here, separated by commas if you want to plot multiple points simultaneously). Then type a",em("Point label"),"(if required) and choose a",em("Point shape, Point colour"),"and",em("Point size."),"Finally, click",em("Add point"),"to add your point to the map.",br(),br(), 
    "Points can be removed from the map using the",em("Remove last point"),"and",em("Remove all points"),"buttons.",
    id = popover_ID,
    placement = "right",
  )
}

## CUSTOM MAP HIGHLIGHTS
## popover_IDs = pop_anomalies_maphl, pop_composites_maphl, pop_correlation_maphl

map_highlights_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "To add a highlight, draw a box on the map or manually enter a",em("Longitude"),"and",em("Latitude"),"range. Then select a",em("Highlight colour"),"and",em("Highlight type"),"and click",em("Add highlight"),"to add your highlight to the map.",br(),br(),
    "Highlights can be removed from the map using the",em("Remove last highlight"),"and",em("Remove all highlights"),"buttons.",
    id = popover_ID,
    placement = "right",
  )  
}

## MAP STATISTICS
## popover_IDs = pop_anomalies_mapstat, pop_composites_mapstat

map_statistics_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Add stipples to the map to show where data points match a certain criteria.",
    id = popover_ID,
    placement = "right",
  )
}

## MAP CHOOSES STATISTIC 
## popover_IDs = pop_anomalies_choosestat, pop_composites_choosestat

map_choose_statistic_popover = function(popover_ID){
  # Anomalies popover
  if (popover_ID == "pop_anomalies_choosestat"){
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "The",em("SD ratio"),"option will show points on the map where the SD ratio (averaged over your selected years) is less than a chosen value. The SD ratio shows the extent to which the climate models used to construct ModE-RA were constrained by observations. An SD ratio of 1 shows no constraint (i.e. the ModE-RA output is entirely generated from the models) and lower values show increasing constraint, meaning there are either more observations or that they are more ‘trusted’ by the reconstruction.",br(),br(),
      "See",em("ClimeApp functions"),"tab on the Welcome page for more information.",
      id = popover_ID,
      placement = "right",
    ) 
  } else {
    # Composites popover
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "The",em("SD ratio"),"option will show points on the map where the SD ratio (averaged over your selected years) is less than a chosen value. The SD ratio shows the extent to which the climate models used to construct ModE-RA were constrained by observations. An SD ratio of 1 shows no constraint (i.e. the ModE-RA output is entirely generated from the models) and lower values show increasing constraint, meaning there are either more observations or that they are more ‘trusted’ by the reconstruction.",br(),br(),
      em("% sign match"),"will show points on the map where the yearly anomalies that form the composite agree in their sign more often than the selected threshold. This gives an indication of the consistency of anomalies over the composite. Example: A composite of five years, with anomalies of -1°C, -5°C, 1°C, 15°C and -3°C, would display positive average anomaly. However, there would only be a 40% sign match since three of the years in fact have a negative.", br(),br(),
      "See",em("ClimeApp functions"),"tab on the Welcome page for more information.",
      id = popover_ID,
      placement = "right",
    ) 
  }
  
}

#### TIMESERIES ----
## TIMESERIES CUSTOMIZATION
## popover_IDs = pop_anomalies_custime, pop_composites_custime, pop_correlation_custime, pop_annualcycles_custime, pop_sea_custime

timeseries_customization_popover = function(popover_ID){
  if (popover_ID == "pop_correlation_custime"|popover_ID == "pop_annualcycles_custime"){
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Edit the titles of your timeseries and add a features key.",
      id = popover_ID,
      placement = "right",
    )
  } else {
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Edit the titles of your timeseries and add a key or reference line.",
      br(),br(),
      "The",em("Show reference"),"option adds a line to the timeseries shows the mean for your selected reference period (i.e. the absolute value corresponding to an anomaly of 0).",
      id = popover_ID,
      placement = "right",
    )   
  }
}

## CUSTOM TIMESERIES FEATURES
## popover_IDs = pop_anomalies_timefeat, pop_composites_timefeat, pop_correlation_timefeat, pop_annualcycles_timefeat, pop_sea_timefeat

timeseries_features_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Add labelled points, rectangular highlights (a boxed, filled or hashed area) and horizontal/vertical lines to your timeseries.",
    id = popover_ID,
    placement = "right",
  )  
}

## TIMESERIES POINTS
## popover_IDs = pop_anomalies_timepoint, pop_composites_timepoint, pop_correlation_timepoint, pop_annualcycles_timepoint, pop_sea_timepoint

timeseries_points_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "To add a point, click on the timeseries or manually enter an",em("x"),"and",em("y"),"position. Clicking will automatically set the",em("x position"),"and",em("y position"),"(enter multiple values here, separated by commas if you want to plot multiple points simultaneously). Then type a",em("Point label"),"(if required) and choose a",em("Point shape, Point colour"),"and",em("Point size."),"Finally, click",em("Add point"),"to add your point to the timeseries.",br(),br(),
    "Points can be removed from the timeseries using the",em("Remove last point"),"and",em("Remove all points"),"buttons.",
    id = popover_ID,
    placement = "right",
  )  
}

## TIMESERIES HIGHLIGHTS
## popover_IDs = pop_anomalies_timehl, pop_composites_timehl, pop_correlation_timehl, pop_annualcycles_timehl, pop_sea_timehl

timeseries_highlights_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "To add a highlight, draw a box on the timeseries or manually enter",em("X"),"and",em("Y"),"values. Then select a",em("Highlight colour"),"and",em("Highlight type"),"and click",em("Add highlight"),"to add your highlight to the timeseries. Select",em("Show on key"),"and type a",em("Label"), "to add your highlight to the key. Remember to select the",em("Show key"),"option under",em("Customize your timeseries"),"to show the key on your plot",br(),br(),
    "Highlights can be removed from the timeseries using the",em("Remove last highlight"),"and",em("Remove all highlights"),"buttons.",
    id = popover_ID,
    placement = "right",
  )  
}

## TIMESERIES LINES
## popover_IDs = pop_anomalies_timelines, pop_composites_timelines, pop_correlation_timelines, pop_annualcycles_timelines, pop_sea_timelines

timeseries_lines_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "To add a line, click or double click on the timeseries to set the lines",em("Orientation"),"and",em("Position."),"Single click for a vertical line, double click for a horizontal. Multiple lines can be added simultaneously by entering multiple",em("Position"),"values, separated by commas. Then select a",em("Line colour"),"and",em("Type"),"and click",em("Add line"),"to add your line to the timeseries. Select",em("Show on key"),"and type a",em("Label"), "to add your line to the key. Remember to select the",em("Show key"),"option under",em("Customize your timeseries"),"to show the key on your plot",br(),br(),
    "Lines can be removed from the timeseries using the",em("Remove last line"),"and",em("Remove all lines"),"buttons.",
    id = popover_ID,
    placement = "right",
  )  
}

## TIMESERIES CUSTOM STATISTICS
## popover_IDs = pop_anomalies_timestats, pop_composites_timestats, pop_correlation_timestats

timeseries_statistics_popover = function(popover_ID){
  # Composites popover
  if (popover_ID == "pop_composites_timestats"){
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Add percentiles to your timeseries.",br(),br(),
      "Percentiles show where a percentage of points in the timeseries are above/below a given value. So, for the 0.95 percentile, 95% of points are between the two lines, 2.5% are above the upper line and 2.5% are below the lower line.",
      id = popover_ID,
      placement = "right",
    )  
  } else {
    # All other popovers
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Add moving averages and percentiles to your timeseries.",br(),br(),
      "A moving average shows the mean of a chosen number of years around each point on the timeseries. This is useful for ‘smoothing out’ short term variation.",br(),br(),
      "Percentiles show where a percentage of points in the timeseries are above/below a given value. So, for the 0.95 percentile, 95% of points are between the two lines, 2.5% are above the upper line and 2.5% are below the lower line.",
      id = popover_ID,
      placement = "right",
    )  
  }
}

#### ANOMALIES ----

## ANOMALIES SUMMARY
## popover_IDs = pop_anomalies

anomalies_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Anomalies <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "Anomalies show how a selected time period differs from a reference time period:",em("Anomalies = Absolute Values – Reference Values"),br(),br(),
    "The",em("Map"),"shows the average anomaly across all years in the range of years.",br(),br(),
    "The",em("Timeseries"),"shows the average anomaly across your selected geographic area for each year in the range of years.",br(),br(),  
    "See",em("ClimeApp functions"),"tab on the Welcome page for more information.",
    title = "What are anomalies?",
    id = popover_ID,
    placement = "right",
  ) 
}
## TIMESERIES CUSTOM SCATTER PLOT
## popover_IDs = pop_correlation_scatter

timeseries_scatter_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Add a trendline to your scatter plot or highlight potential outliers to identify unusual values.",br(),br(),
    "A trendline shows the general direction or pattern in the relationship between two variables, helping to visualize whether they tend to increase or decrease together.",br(),br(), 
    "A z-score (standard score) indicates how far a data point is from the mean, expressed in units of standard deviation. It helps identify unusually high or low values in the data.",
    id = popover_ID,
    placement = "right",
  ) 
}


#### ANOMALIES ----

## ANOMALIES SUMMARY
## popover_IDs = pop_anomalies

anomalies_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Anomalies <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "Anomalies show how a selected time period differs from a reference time period:",em("Anomalies = Absolute Values – Reference Values"),br(),br(),
    "The",em("Map"),"shows the average anomaly across all years in the range of years.",br(),br(),
    "The",em("Timeseries"),"shows the average anomaly across your selected geographic area for each year in the range of years.",br(),br(),  
    "See",em("ClimeApp functions"),"tab on the Welcome page for more information.",
    title = "What are anomalies?",
    id = popover_ID,
    placement = "right",
  ) 
}



#### COMPOSITES ----

## COMPOSITES SUMMARY
## popover_IDs = pop_composites

composites_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Composites <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "A composite is an average across multiple, non-consecutive years. The composite anomaly displayed is this average, minus the average over your selected",em("Reference period."),br(),br(),
    "The",em("Map"),"shows the average anomaly across all years in your list of years.",br(),br(),
    "The",em("Timeseries"),"shows the average anomaly across your selected geographic area for each year in the list of years.",br(),br(),
    "See",em("ClimeApp functions"),"tab on the Welcome page for more information.",
    title = "What are composites?",
    id = popover_ID,
    placement = "right",
  ) 
}


#### CORRELATION ----

## CORRELATION SUMMARY
## popover_IDs = pop_correlation

correlation_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Correlation <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "Correlation measures the statistical relationship (causal or non-causal) between two variables. This tab allows you to correlate two sets of ModE data, or upload your own data to correlate against the ModE data or correlate two sets of uploaded data.",br(),br(),
    "The", em("Timeseries"),"tab shows the timeseries and timeseries correlation for", em("Variable 1"),"and",em("Variable 2."),br(),br(),
    "The",em("Correlation map"),"shows the correlation between", em("Variable 1"),"and",em("Variable 2")," for each point on the map.",br(),br(),
    "See",em("ClimeApp functions"),"tab on the Welcome page for more information.",
    title = "What is correlation?",
    id = popover_ID,
    placement = "right",
  ) 
}

## CORRELATION VARIABLE
## popover_IDs = pop_correlation_variable1, pop_correlation_variable2

correlation_variable_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Select a ModE",em("dataset, variable, range of months"),"and",em("reference period"),"or upload your own annual user data.",br(),br(),
    "If uploading data, make sure it is yearly data. The variable name should be on the top row of each column and the first column should give the year of each entry, as shown.",br(),br(),
    "If using ModE data, select whether to use it as a",em("Field"),"or",em("Timeseries."),"ClimeApp will only correlate two sets of field data where their geographic areas overlap. Select",em("Timeseries"),"for one variable and",em("Field"),"for the other to have ClimeApp correlate a single timeseries with the timeseries for each point on the",em("Field"),"map.",br(),br(),
    "See",em("ModE data"),"and",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
    id = popover_ID,
    placement = "right",
  )  
}

## CORRELATION YEAR RANGE
## popover_IDs = pop_correlation_year

correlation_year_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Choose a",em("range of years"),"to correlate. This will be limited to the range of your own data or 1422 to 2008 for ModE-RA data.",br(),br(),
    "Use the",em("lag year"),"option to shift a variable forward or backward by that many years.",
    id = popover_ID,
    placement = "right",
  )   
}

## CORRELATION TIMESERIES
## popover_IDs = pop_correlation_timeseries

correlation_timeseries_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Timeseries Correlation <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 11px;"),
    "The plot shows your two selected variables plotted as timeseries.",br(),br(),
    "The",em("pearson"),"correlation method measures the linear relationship between the two timeseries, while the",em("spearman"),"correlation method measures the non-linear relationship.",br(),br(), 
    "The",em("r value"),"gives the strength of the correlation, with 1 being a perfect positive correlation, -1 being a perfect negative correlation and 0 showing no correlation.",br(),br(),
    "The",em("p value"),"shows the probability (as a decimal) of this correlation arising due to random chance rather than an actual statistical relationship.",
    id = popover_ID,
    placement = "right",
  ) 
}

## CORRELATION MAP
## popover_IDs = pop_correlation_map

correlation_map_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Correlation Map <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 11px;"),
    "The map shows the correlation between",em("Variable 1"),"and",em("Variable 2"),"for each point on the map. ",br(),br(),
    "The",em("pearson"),"correlation method measures the linear relationship between",em("Variable 1"),"and",em("Variable 2,"),"while the",em("spearman"),"correlation method measures the non-linear relationship.",br(),br(), 
    "The",em("r value"),"gives the strength of the correlation, with 1 being a perfect positive correlation, -1 being a perfect negative correlation and 0 showing no correlation.",
    id = popover_ID,
    placement = "right",
  ) 
}



#### RGRESSION ----

## REGRESSION SUMMARY
## popover_IDs = pop_regression

regression_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Regression <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "Regression takes one or more independent variables and calculates the linear equation that best links them to the dependent variable. This can be used ‘remove’ the effect of the independent variables, leaving the residual variation in the dependent variable.",br(),br(),
    "The",em("Regression timeseries"),"tab shows the",em("original, trend"),"and",em("residual"),"timeseries for the dependent variable, where",em("original = trend + residual."),"It also shows the statistics for the linear regression equation.",br(),br(),
    "The",em("Regression coefficient, pvalues"),"and",em("residuals"),"tabs show the",em("coefficients, p values"),"and",em("residuals"),"of the regression for each point on the map.",br(),br(),
    "See (?) on the individual tabs and the",em("ClimeApp functions"),"tab on the Welcome page for more information.",
    id = popover_ID,
    title = "What is regression?",
    placement = "right",
  ) 
}

## REGRESSION VARIABLE
## popover_IDs = pop_regression_independentvariable, pop_regression_dependentvariable

regression_variable_popover = function(popover_ID){
  if(popover_ID == "pop_regression_independentvariable"){
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Select a ModE",em("dataset, variable, range of months"),"and",em("reference period"),"or upload your own annual user data.",br(),br(),
      "If uploading data, make sure it is yearly data. The variable name should be on the top row of each column and the first column should give the year of each entry, as shown. You can select multiple variables to perform a multiple linear regression.",br(),br(),
      "ClimeApp will create a timeseries for the independent variable(s) and calculate the regression against a timeseries for the dependent variable. This is done for each point on the map if the dependent is a ModE variable.",br(),br(),
      "See",em("ModE data"),"and",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
      id = popover_ID,
      placement = "right",
    ) 
  } else {
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Select a ModE",em("dataset, variable, range of months"),"and",em("reference period"),"or upload your own annual user data.",br(),br(),
      "If uploading data, make sure it is yearly data. The variable name should be on the top row of each column and the first column should give the year of each entry, as shown.",br(),br(),
      "ClimeApp will create a timeseries for the independent variable(s) and calculate the regression against a timeseries for the dependent variable. This is done for each point on the map if the dependent is a ModE variable.",br(),br(),
      "See",em("ModE data"),"and",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
      id = popover_ID,
      placement = "right",
    )  
  }
}

## REGRESSION YEAR RANGE
## popover_IDs = pop_regression_year

regression_year_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Choose a",em("range of years"),"to perform the regression on. This will be limited to the range of your own data or 1422 to 2008 for ModE-RA data.",br(),br(),
    "Use the",em("lag year"),"option to shift a variable forward or backward by that many years.",
    id = popover_ID,
    placement = "right",
  )   
}

## REGRESSION TIMESERIES
## popover_IDs = pop_regression_timeseries

regression_timeseries_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Regression timeseries <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 11px;"),
    "Plot 1 shows the",em("original"),"data for the dependent variable and the",em("trend"),"in the dependent variable, as predicted from the linear regression and the independent variable(s).",br(),br(),
    "Plot 2 shows the",em("residual"),"values ( ",em("original – trend"),") for the dependent variable.",br(),br(),
    "The",em("Statistical summary"),"gives the",em("coefficients"),"and",em("intercept"),"for each independent variable as well as a summary of the",em("residuals"),"and further statistical information.",br(),br(),
    "See the",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
    id = popover_ID,
    placement = "right"
  )
}

## REGRESSION COEFFICIENT MAP
## popover_IDs = pop_regression_coefficients

regression_coefficient_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Regression coefficients map <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 11px;"),
    "The map shows the",em("coefficients"),"of the independent variable(s) in the linear regression equation, for each point on the map. Use",em("Choose a variable"),"to select which of the independent variables you would like to plot the coefficients for.",br(),br(),
    "The",em("coefficient"),"shows you what the independent variable is being multiplied by in the regression calculation.",br(),br(),
    "See the",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
    id = popover_ID,
    placement = "right"
  )
}

## REGRESSION P VALUES MAP
## popover_IDs = pop_regression_pvalues

regression_pvalue_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Regression P values map <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 11px;"),
    "The map shows the",em("p values"),"of the independent variable(s) in the linear regression equation, for each point on the map. Use",em("Choose a variable"),"to select which of the independent variables you would like to plot the p values for.",br(),br(),
    "The",em("p value"),"shows the probability (as a decimal) of the linear regression arising due to random chance rather than an actual statistical relationship.",br(),br(),
    "See the",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
    id = popover_ID,
    placement = "right"
  )
}

## REGRESSION RESIDUALS MAP
## popover_IDs = pop_regression_residuals

regression_residuals_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Regression residuals map <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 11px;"),
    "The map shows the",em("residual"),"values of the dependent variable, for the selected",em("Year,"),"for each point on the map.",br(),br(),
    "The residual values are the ‘leftover’ variation in the dependent variable, after the",em("trend"),"predicted from the independent variable(s) has been removed:",em("Residual = Original – Trend."),br(),br(),
    "See the",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
    id = popover_ID,
    placement = "right"
  )
}



#### ANNUAL CYLCES ----

## ANNUAL CYCLES SUMMARY
## popover_IDs = pop_annualcycles

annualcycles_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Annual Cycles <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "This tool can be used to plot monthly values of a variable (absolute or anomaly) as an",em("annual cycle"),"for a given year or set of years. Multiple years can be displayed on the same plot, allowing several",em("annual cycles"),"to be compared.",
    title = "What are annual cycles?",
    id = popover_ID,
    placement = "right",
  ) 
}

## ANNUAL CYCLES DATA
## popover_IDs = pop_annualcycles_data

annualcycles_data_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Select a ModE",em("dataset, variable, year/list of years/range of years"),"and",em("reference period."),br(),br(),
    "If entering a",em("list of years,"),"separate each year with a comma (e.g. 1815,1816). If entering a",em("range of years,"),"separate the two years with a dash (e.g. 1815-1820). Multiple years can be plotted as either",em("Individual years,"),"with all years in the list/range plotted as separate lines, or as an",em("Average,"),"which will plot a single line showing the monthly average for all years in the list/range.",br(),br(),
    "See",em("ModE data"),"and",em("ClimeApp functions"),"tabs on the Welcome page for more information.",
    id = popover_ID,
    placement = "right",
  )  
}

## ANNUAL CYCLES REGION
## popover_IDs = pop_annualcycles_region

annualcycles_region_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Select a geographical area and click",em("Add to graph"),"to add your current selection to the plot as a timeseries. The line will show the monthly average across this area.",br(),br(),
    "Select",em("Switch to point location input"),"to search for or enter a single point rather than an area.",br(),br(),
    "Timeseries can be removed using the",em("Remove last TS"),"and",em("Remove all TS buttons."),
    id = popover_ID,
    placement = "right",
  )  
}



#### REST ----

## DOWNLOADS
## popover_IDs = pop_anomalies_map_downloads,pop_anomalies_ts_downloads, pop_composites_map_downloads, pop_composites_ts_downloads,
##               pop_correlation_map_downloads, pop_correlation_ts_downloads, pop_sea_ts_downloads, pop_regression_ts_downloads, pop_annual_cycles_ts_downloads

downloads_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Download your plot as a PNG, JPEG or PDF file.",br(),br(),
    "Use",em("Download metadata"),"to download all currently selected options (data and customization) as metadata. This can be used for reference or to quickly regenerate your current plot in a future session.", br(),br(),
    em("Upload metadata"),"from a previous session and click",em("Update upload inputs"),"to restore all ClimeApp options to those from the metadata. Note that metadata must be restored to the same tab (i.e.",em("Anomalies"),").", br(),br(),
    "Metadata is downloaded in .xlsx format.", 
    id = popover_ID,
    placement = "right",
  ) 
}

## REFERENCE MAP
## popover_IDs = pop_anomalies_refmap, pop_composites_refmap, pop_correlation_refmap

reference_map_popover = function(popover_ID){
  # Anomalies popover
  if (popover_ID == "pop_anomalies_refmap"){
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Plot and download a reference map for your data.",br(),br(), 
      em("Absolute Values"),"shows the average for your selected month and year range, prior to subtracting the reference values. Note that the reanalysis method makes absolute values potentially unreliable – this is solved by using anomalies.",br(),br(),
      em("Reference Values"),"are the absolute means for your reference period.",br(),br(), 
      em("SD Ratio"),"shows the extent to which the climate models used to construct ModE-RA were constrained by observations. An SD ratio of 1 shows no constraint (i.e. the ModE-RA output is entirely generated from the models) and lower values show increasing constraint, meaning there are either more observations or that they are more ‘trusted’ by the reconstruction.",
      id = popover_ID,
      placement = "right",
    )
  } else if (popover_ID == "pop_composites_refmap") {
    # Composites popover
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Plot and download a reference map for your data.",br(),br(), 
      em("Absolute Values"),"shows the average for your selected month range and list of years, prior to subtracting the reference values. Note that the reanalysis method makes absolute values potentially unreliable – this is solved by using anomalies.",br(),br(),
      em("Reference Values"),"are the absolute means for your reference period.",br(),br(), 
      em("SD Ratio"),"shows the extent to which the climate models used to construct ModE-RA were constrained by observations. An SD ratio of 1 shows no constraint (i.e. the ModE-RA output is entirely generated from the models) and lower values show increasing constraint, meaning there are either more observations or that they are more ‘trusted’ by the reconstruction.",
      id = popover_ID,
      placement = "right",
    )
  } else {
    # Correlation popover
    bslib::popover(
      HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
      "Plot and download a scatter plot of your correlation.",br(),br(), 
      em("Scatter Plot"),"shows the relationship between the two selected variables. Each point represents a data pair from the two selected variables. You can add a trendline to visualize the overall relationship and highlight potential outliers to identify unusual values.",br(),br(),
      id = popover_ID,
      placement = "right",
    )
  }
}

## NETCDF
## popover_IDs = pop_anomalies_netcdf

netcdf_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Download the full dataset of yearly anomalies for your selected year, month and geographic range. You can include multiple variables in the same NetCDF file by selecting them on the dropdown menu.",
    id = popover_ID,
    placement = "right",
  )  
}

## MODE-RA SOURCES
## popover_IDs = pop_anomalies_mesource, pop_composites_mesource, pop_correlation_mesource, pop_regression_mesource, pop_anncyc_mesource

MEsource_popover = function(popover_ID){
  bslib::popover(
    h4(HTML("Plot ModE-RA sources <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "color: #094030; margin-left: 0px;"),
    "This plot shows location, type and variable measured for every source used to create ModE-RA and ModE-RAclim.",br(),br(), 
    "Note that each",em("source"),"may include one or more individual observations or measurements.", br(),br(),
    "The term", em("VARIABLE"),"refers to the value that was directly measured by each source. For example, a historical proxy might refer to a recorded tree flowering date, while a natural proxy, might refer to a measured tree ring width.",br(),br(), 
    "Use the",em("Explore ModE-RA Sources"),"tab or", em("Download Map Data"),"tool for more information on indiviudal sources",
    id = popover_ID,
    placement = "right",
  ) 
}

## TOTAL SOURCES AND OBSERVATIONS POPOVER
## popover_IDs = pop_sourcesandobservation
sourcesandobservations_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "The timeseries below the main map shows the total number of", em("sources"),"that were used to create ModE-RA/ModE-RAclim for each year/half-year.",br(),br(),
    "Note that each",em("source"),"may include one or more individual observations or measurements.", br(),br(),
    "Click names on the legend to add/remove lines from the plot.",
    id = popover_ID,
    placement = "right",
  )
}



#### SEA ----

## SEA SUMMARY
## popover_IDs = pop_sea

sea_summary_popover = function(popover_ID){
  bslib::popover(
    h3(HTML("Superposed epoch analysis <sup><i class='fas fa-question-circle fa-xs'></i></sup>"), style = "margin-left: 11px;"),
    "SEA is a statistical method used to detect and analyze patterns in time series data by aligning multiple events (epochs) to a common reference point (the zero time). It helps identify average signals or anomalies associated with specific types of events, such as volcanic eruptions, El Niño events, or other climatic or environmental triggers, by reducing noise and enhancing the signal across repeated instances.",
    id = popover_ID,
    title = "What is superposed epoch analysis?",
    placement = "right",
  ) 
}

## SEA DATA OPTIONS
## popover_IDs = pop_sea_data

sea_data_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "You can either upload your own dataset for analysis or explore the built-in ModE-datasets",
    id = popover_ID,
    placement = "right",
  ) 
}

## SEA OPTIONS
## popover_IDs = pop_sea_options

sea_options_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Customize your Superposed Epoch Analysis by specifying event years manually or uploading a file. You can also define the number of lag years before and after each event to shape the analysis window.",
    id = popover_ID,
    placement = "right",
  ) 
}

## SEA Background Period
## popover_IDs = pop_sea_background

sea_background_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "In your uploaded file, you can specify a custom pre-event and post-event background period for each event year.", br(),
    strong("Pre-event background:"), "In the second column, enter a year. Any years closer to the event - after this year - will be excluded from the analysis. This helps you avoid including years that may already be affected by nearby events.", br(),
    strong("Post-event background:"), "In the third column, enter a year. Any years after this year will be excluded from the analysis, helping you avoid the influence of other events that occur soon after.",
    id = popover_ID,
    placement = "right",
  ) 
}

## SEA Customization
## popover_IDs = pop_sea_custime

timeseries_customization_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Personalize your plot by setting a custom title, labeling the axes, and adjusting display options such as showing p-values, enabling yearly tick marks, or adding a legend (key) for clarity.",
    id = popover_ID,
    placement = "right",
  ) 
}

## SEA STATISTICS EXPLANATION
## popover_IDs = pop_sea_statistics

sea_statistics_popover = function(popover_ID){
  bslib::popover(
    HTML("<i class='fas fa-question-circle fa-2xs'></i></sup>"), style = "color: #094030; margin-left: 11px;",
    "Define the sample size used in the analysis and choose whether to display the sample mean and confidence bands. These options help assess the reliability and significance of the observed patterns.",
    id = popover_ID,
    placement = "right",
  ) 
}
