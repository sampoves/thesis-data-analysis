
# Helsinki Region Travel Time comparison application
# Helsinki Region Travel Time Matrix 2018 <--> My thesis survey results

# 10.6.2020
# Sampo Vesanen

# This interactive Travel time comparison application is dependent on ggiraph 
# 0.7.7 (only available through GitHub at the time of writing). With my setup
# ggiraph 0.7.0 would load the map an unreasonably long time. I strongly 
# suspected some fault in the tooltip generation.

# TODO: colouring of tooltip table cells to assist conclusion drawing
# TODO: Remember arbitrary selection of timeofday in thesisdata! Triplecheck it
# TODO: laskenko pct:n drivetimesta vai avg:sta?! decide!!
# TODO: legend name is better, but still incorrect in comparison, maybe elsewhere as well
# thesis drivetimes have negative values (this is a result in itself i think)


#### 1 Initialise --------------------------------------------------------------
rm(list = ls())
gc()

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(htmltools)
library(rgdal)
library(shinyjs)
library(ggiraph)
library(data.table)
library(rgeos)
library(sf)
library(shinyWidgets)
library(ggsn)
library(fst)
library(ggnewscale)


# App version
app_v <- "0038 (10.6.2020)"


# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Data directories
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018")
munspath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")
subdivpath <- file.path(wd, "python/suuralueet/PKS_suuralue.kml")

# Thesis' processed data
recordspath <- file.path(wd, "records_for_r.csv")
postal_path <- file.path(wd, "postal_for_r.csv")

# Directives
csspath <- file.path(wd, "python/thesis_data_traveltime_style.css")
jspath <- file.path(wd, "python/thesis_data_traveltime_script.js")
tooltip_path <- file.path(wd, "python/thesis_data_traveltime_tooltip.html")
info_path <- file.path(wd, "python/thesis_data_traveltime_info.html")

# Source functions and postal code variables
source(file.path(wd, "python/thesis_data_traveltime_funcs.R"))

# Convert Helsinki Region Travel Time Matrix 2018 into fst. This is a required
# step to be able to run the comparison application. It is only needed to be
# executed once. "fst_filepath" is the location of TTM18 converted to fst.
fst_filepath <- file.path(wd, "TTM18")
#source(file.path(wd, "python/thesis_data_traveltime_conv.R"))



#### 2 Import data layers ------------------------------------------------------

#### 2.1 Grid ------------------------------------------------------------------

# use this CRS information throughout the app
app_crs <- sp::CRS("+init=epsg:3067")

# Read an transform
grid <- rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

# Save grid as centroids for the spatial join of data
grid_point <- rgeos::gCentroid(grid, byid = TRUE)

# TTM18 Helsinki walking center polygon. Source: Henrikki Tenkanen. Get YKR_IDs
# which fit inside the walking center polygon.
walkingHki <- 
  data.frame(
    long = c(387678.024778, 387891.53396, 383453.380944, 383239.871737, 387678.024778),
    lat = c(6675360.99039, 6670403.35286, 6670212.21613, 6675169.85373, 6675360.99039)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = app_crs) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON")

# Fortified walking center for visualisation
walk_f <- 
  st_coordinates(walkingHki)[, 1:2] %>%
  sp::Polygon(.) %>%
  ggplot2::fortify(.) %>%
  dplyr::mutate(label = "walk")

walking_ids <- 
  sf::st_intersection(sf::st_as_sf(grid), walkingHki) %>%
  as(., "Spatial") %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      tibble::rownames_to_column(., var = "id"))} %>%
  dplyr::select(YKR_ID)
walking_ids <- walking_ids[, 1]



#### 2.2 Municipality borders --------------------------------------------------

# Get municipality borders. Fortify SP DataFrame for ggplot2. Remove unnecessary
# columns to save memory.
# Shapefile data is Regional population density 2012, Statistics Finland.
# http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226.
muns_f <-
  rgdal::readOGR(munspath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(namn, vaestontih, km2, vakiluku))



#### 2.3 Postal code areas -----------------------------------------------------

postal <- 
  read.csv(file = postal_path,
           header = TRUE, 
           sep = ",",
           colClasses = c(zipcode = "factor", kunta = "factor", 
                          geometry = "character"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(c(2, 3, 6, 108))

# "postal" geometries are in well-known text format. Some processing is needed 
# to utilise these polygons in R. readWKT() uses rgeos.
geometries <- lapply(postal[, "geometry"], "readWKT", p4s = app_crs)
sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(postal[, 1]))
row.names(postal) <- postal[, 1]

# Preserve SpatialPolygons version of "postal" for the spatial join
postal <- sp::SpatialPolygonsDataFrame(
  sp::SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)),
                      proj4string = app_crs), data = postal)

# Fortify and preserve Polygon attribute data
postal_f <- 
  postal %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(zipcode)),
                    by = "id")}



### 2.4 Subdivisions -----------------------------------------------------------

subdiv_f <- 
  rgdal::readOGR(subdivpath, 
                 use_iconv = TRUE, 
                 encoding = "UTF-8", 
                 stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  
  dplyr::select(-Description) %>%
  dplyr::mutate(Name = factor(Name, labels =
                                c("Vantaa Aviapolis", "Helsinki Southern", 
                                  "Vantaa Hakunila", "Helsinki Eastern", 
                                  "Helsinki Southeastern", "Kauniainen",
                                  "Helsinki Central", "Vantaa Kivistö", 
                                  "Helsinki Northeastern", "Vantaa Koivukylä", 
                                  "Vantaa Korso", "Helsinki Western",
                                  "Vantaa Myyrmäki", "Helsinki Northern", 
                                  "Espoo Pohjois-Espoo", "Espoo Suur-Espoonlahti", 
                                  "Espoo Suur-Kauklahti", "Espoo Suur-Leppävaara", 
                                  "Espoo Suur-Matinkylä", "Espoo Suur-Tapiola", 
                                  "Vantaa Tikkurila", "Espoo Vanha-Espoo",
                                  "Helsinki Östersundom"))) %>%
  
  dplyr::mutate(Name = factor(Name, levels = sort(levels(Name))))

subdiv_f <- subdiv_f[order(subdiv_f$Name), ]



# 2.5 Thesis survey data ------------------------------------------------------- 

# Import Python processed thesis survey data. This data is later (chapter 4.2) 
# joined with the currently fetched YKR ID Travel Time Matrix 2018 data.
thesisdata <- 
  read.csv(file = recordspath,
           header = TRUE, 
           sep = ",",
           colClasses = c(timestamp = "POSIXct", zipcode = "factor"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(zipcode, parktime, walktime, timeofday) %>%
  dplyr::filter(parktime <= 59,
                walktime <= 59) %>%
  dplyr::group_by(zipcode) %>%
  
  # Select timings for thesis_ columns
  # TODO: Double check equality with TTM data
  dplyr::mutate(r_sfp = case_when(timeofday == 1 ~ parktime, TRUE ~ NA_integer_),
                m_sfp = case_when(timeofday == 2 ~ parktime, TRUE ~ NA_integer_),
                sl_sfp = case_when(timeofday == 4 ~ parktime, TRUE ~ NA_integer_),
                r_wtd = case_when(timeofday == 1 ~ walktime, TRUE ~ NA_integer_),
                m_wtd = case_when(timeofday == 2 ~ walktime, TRUE ~ NA_integer_),
                sl_wtd = case_when(timeofday == 4 ~ walktime, TRUE ~ NA_integer_)) %>%
  
  # Calculate summaries by zipcode, then round to two decimals
  dplyr::summarise(thesis_r_sfp = mean(r_sfp, na.rm = TRUE),
                   thesis_m_sfp = mean(m_sfp, na.rm = TRUE),
                   thesis_sl_sfp = mean(sl_sfp, na.rm = TRUE),
                   thesis_r_wtd = mean(r_wtd, na.rm = TRUE),
                   thesis_m_wtd = mean(m_wtd, na.rm = TRUE),
                   thesis_sl_wtd = mean(sl_wtd, na.rm = TRUE),
                   vals_in_zip = length(zipcode)) %>%
  
  dplyr::mutate_if(is.numeric, round, 2)

# NaNs are introduced in calculation of mean. Change to NA. Do not apply changes
# to column zipcode
thesisdata[, -1] <- data.frame(
  sapply(thesisdata[, -1], function(x) ifelse(is.nan(x), NA, x)))



#### 2.6 Label comparison plot features ----------------------------------------

# Create labels for zipcodes
zipcode_lbl <- GetCentroids(postal_f, "zipcode", "zipcode")
muns_lbl <- GetCentroids(muns_f, "nimi", "nimi")
subdiv_lbl <- GetCentroids(subdiv_f, "Name", "Name")

# Finetune locations for certain labels in zipcode_lbl. GetCentroids saves the 
# second parameter as rownames. We can use that to reliably find correct rows
# to finetune.
zipcode_lbl["00250", 1] %+=% 500 # Taka-Töölö
zipcode_lbl["00980", 1] %-=% 850 # Etelä-Vuosaari
zipcode_lbl["01640", 2] %-=% 300 # Hämevaara 
zipcode_lbl["01730", 2] %-=% 500 # Vantaanpuisto
zipcode_lbl["02380", 1] %+=% 2400 # Suvisaaristo
zipcode_lbl["02820", 1] %+=% 1000 # Nupuri-Nuuksio

# Remove municipality names from subdivision annotations
subdiv_lbl$label <- gsub(".* ", "", unique(subdiv_f$Name)) 
rownames(subdiv_lbl) <- gsub(".* ", "", rownames(subdiv_lbl))

# Manually move labels several subdivision labels to better positions.
subdiv_lbl["Suur-Espoonlahti", "lat"] <- subdiv_lbl["Southern", "lat"]
subdiv_lbl["Suur-Espoonlahti", "long"] <- subdiv_lbl["Pohjois-Espoo", "long"]
subdiv_lbl["Southeastern", "lat"] <- subdiv_lbl["Southern", "lat"] + 1500
subdiv_lbl["Southeastern", "long"] <- subdiv_lbl["Korso", "long"]
subdiv_lbl["Southern", "lat"] %+=% 3000
subdiv_lbl["Östersundom", "lat"] %-=% 500
subdiv_lbl["Östersundom", "long"] %-=% 400
subdiv_lbl["Suur-Matinkylä", "lat"] %-=% 500

# In this named vector the first part is the name of the new, classified
# column. Second part is the original column name where the classification
# was calculated from.
vis_cols <- c("ttm18_r_t" = "car_r_t",
              "ttm18_m_t" = "car_m_t",
              "ttm18_sl_t" = "car_sl_t",
              "ttm18_r_avg" = "ttm_r_avg",
              "ttm18_m_avg" = "ttm_m_avg",
              "ttm18_sl_avg" = "ttm_sl_avg",
              "ttm18_r_drivetime" = "ttm_r_drivetime",
              "ttm18_m_drivetime" = "ttm_m_drivetime",
              "ttm18_sl_drivetime" = "ttm_sl_drivetime",
              "ttm18_r_pct" = "ttm_r_pct",
              "ttm18_m_pct" = "ttm_m_pct",
              "ttm18_sl_pct" = "ttm_sl_pct",
              
              "msc_r_sfp" = "thesis_r_sfp",
              "msc_m_sfp" = "thesis_m_sfp",
              "msc_sl_sfp" = "thesis_sl_sfp",
              "msc_r_wtd" = "thesis_r_wtd",
              "msc_m_wtd" = "thesis_m_wtd",
              "msc_sl_wtd" = "thesis_sl_wtd",
              "msc_r_drivetime" = "thesis_r_drivetime",
              "msc_m_drivetime" = "thesis_m_drivetime",
              "msc_sl_drivetime" = "thesis_sl_drivetime",
              "msc_r_pct" = "thesis_r_pct",
              "msc_m_pct" = "thesis_m_pct",
              "msc_sl_pct" = "thesis_sl_pct",
              
              "compare_r_sfp" = "comp_r_sfp",
              "compare_m_sfp" = "comp_m_sfp",
              "compare_sl_sfp" = "comp_sl_sfp",
              "compare_r_wtd" = "comp_r_wtd",
              "compare_m_wtd" = "comp_m_wtd",
              "compare_sl_wtd" = "comp_sl_wtd",
              "compare_r_drivetime" = "comp_r_drivetime",
              "compare_m_drivetime" = "comp_m_drivetime",
              "compare_sl_drivetime" = "comp_sl_drivetime",
              "compare_r_pct" = "comp_r_pct",
              "compare_m_pct" = "comp_m_pct",
              "compare_sl_pct" = "comp_sl_pct")



#### 3 Spatial join postal data to grid, fortify -------------------------------

# First, spatjoin postal data to grid centroids. Left = FALSE is inner join. Then,
# Join centroid data back to grid polygons and convert grid to SpatialPolygons.
# Finally, fortify for ggplot while keeping important columns.
grid_f <- 
  sf::st_join(sf::st_as_sf(grid_point), 
              sf::st_as_sf(postal),
              join = st_intersects,
              left = FALSE) %>%
  sf::st_join(sf::st_as_sf(grid), 
              ., 
              join = st_intersects) %>%
  as(., "Spatial") %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(x, y))



#### 4 Integrate TTM18 ---------------------------------------------------------

# NB! The execution of this code will fail at this point if TTM18 data is not 
# converted to fst. Please make sure you have run "thesis_data_traveltime_conv.R".



#### 4.1 Fetch TTM18 data ------------------------------------------------------

# Only specify an origin id (from_id in TTM18). to_id remains open because we 
# want to view all possible destinations.
origin_id <- "5985086"

# Get all of the fst filepaths
all_fst <- list.files(path = fst_filepath,
                      pattern = ".fst$",
                      recursive = TRUE,
                      full.names = TRUE)

# Get the position of the current origin. In TTM data, origin ykr_ids are always
# in the same position. We will use this to get the order to the ykr_ids once,
# and then, when needed, find out the location queried ykr_id from the vector
# of all the ykr_ids.
ykrid_source <- file.path(ttm_path, "/5785xxx/travel_times_to_ 5785640.txt")
ykr_ids <- read.csv(ykrid_source, sep = ";")[, 1]
pos <- match(as.numeric(origin_id), ykr_ids) # get index of current id

# Fetch fst format TTM18 and process the dataframe with the YKR_ID "origin_id"
# as the starting point.
result <- 
  lapply(all_fst, FUN = TTM18fst_fetch, pos) %>%
  data.table::rbindlist(., fill = TRUE) %>%
  data.table::merge.data.table(as.data.table(grid_f), .,
                               by.x = "YKR_ID", by.y = "to_id")



#### 4.2 Add thesis data values to the fortified data --------------------------

# Join "thesisdata", thesis survey results, to "result", the currently 
# calculated travel times dataframe
result <- dplyr::left_join(result, thesisdata, by = "zipcode") 



#### 4.3 Add new data columns --------------------------------------------------

# Get grouped means of TTM18 data for current starting point to all 
# destinations for each postal code area
result <- result %>%
  
  # Add Travel Time Matrix "searching for parking" and "walk to destination" data
  dplyr::mutate(ttm_sfp = 0.42,
                ttm_wtd = case_when(YKR_ID %in% walking_ids ~ 2.5, TRUE ~ 2)) %>%
  dplyr::group_by(zipcode) %>%
  dplyr::summarise(ttm_r_avg = mean(car_r_t),
                   ttm_m_avg = mean(car_m_t),
                   ttm_sl_avg = mean(car_sl_t),
                   ttm_sfp = mean(ttm_sfp),
                   ttm_wtd = mean(ttm_wtd)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  
  # Join summarised columns back to original dataframe "result"
  dplyr::inner_join(result, ., by = "zipcode") %>%
  
  # Generate drivetime (min) and pct (%) columns for TTM18 data
  dplyr::mutate(ttm_r_drivetime = ttm_r_avg - ttm_sfp - ttm_wtd,
                ttm_m_drivetime = ttm_m_avg - ttm_sfp - ttm_wtd,
                ttm_sl_drivetime = ttm_sl_avg - ttm_sfp - ttm_wtd,
                ttm_r_pct = (ttm_sfp + ttm_wtd) / ttm_r_drivetime,
                ttm_m_pct = (ttm_sfp + ttm_wtd) / ttm_m_drivetime,
                ttm_sl_pct = (ttm_sfp + ttm_wtd) / ttm_sl_drivetime) %>%
  dplyr::mutate_at(vars(ttm_r_pct, ttm_m_pct, ttm_sl_pct), ~round(., 2)) %>%

  # If zipcode is NA, then convert all calculated data to NA as well
  dplyr::mutate(ttm_r_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_avg),
                ttm_m_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_avg),
                ttm_sl_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sl_avg),
                ttm_sfp = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sfp),
                ttm_wtd = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_wtd),
                ttm_r_drivetime = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_drivetime),
                ttm_m_drivetime = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_drivetime),
                ttm_sl_drivetime = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sl_drivetime),
                ttm_r_pct = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_pct),
                ttm_m_pct = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_pct),
                ttm_sl_pct = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sl_pct)) %>%
  dplyr::mutate_at(vars(car_r_t, car_m_t, car_sl_t, ttm_r_avg, ttm_m_avg, 
                        ttm_sl_avg),
                   ~dplyr::na_if(., -1)) %>%
  
  # Add the rest of thesis_ columns. with if_else() change possible NA's to 
  # zeros so that calculations are not rendered NA
  dplyr::mutate(thesis_r_drivetime = ttm_r_avg - 
                  if_else(is.na(thesis_r_sfp), 0, thesis_r_sfp) - 
                  if_else(is.na(thesis_r_wtd), 0, thesis_r_wtd),
                thesis_m_drivetime = ttm_m_avg - 
                  if_else(is.na(thesis_m_sfp), 0, thesis_m_sfp) - 
                  if_else(is.na(thesis_m_wtd), 0, thesis_m_wtd),
                thesis_sl_drivetime = ttm_sl_avg - 
                  if_else(is.na(thesis_sl_sfp), 0, thesis_sl_sfp) - 
                  if_else(is.na(thesis_sl_wtd), 0, thesis_sl_wtd),
                thesis_r_pct = (
                  if_else(is.na(thesis_r_sfp), 0, thesis_r_sfp) + 
                  if_else(is.na(thesis_r_wtd), 0, thesis_r_wtd)) / ttm_r_avg,
                thesis_m_pct = (
                  if_else(is.na(thesis_m_sfp), 0, thesis_m_sfp) + 
                  if_else(is.na(thesis_m_wtd), 0, thesis_m_wtd)) / ttm_m_avg,
                thesis_sl_pct = (
                  if_else(is.na(thesis_sl_sfp), 0, thesis_sl_sfp) + 
                  if_else(is.na(thesis_sl_wtd), 0, thesis_sl_wtd)) / ttm_sl_avg) %>%
  dplyr::mutate_at(vars(thesis_r_pct, thesis_m_pct, thesis_sl_pct), 
                   ~round(., 2)) %>%
  
  # Add TTM18/thesis comparison columns
  dplyr::mutate(comp_r_sfp = thesis_r_sfp / ttm_sfp,
                comp_m_sfp = thesis_m_sfp / ttm_sfp,
                comp_sl_sfp = thesis_sl_sfp / ttm_sfp,
                comp_r_wtd = thesis_r_wtd / ttm_wtd,
                comp_m_wtd = thesis_m_wtd / ttm_wtd,
                comp_sl_wtd = thesis_sl_wtd / ttm_wtd,
                comp_r_drivetime = thesis_r_drivetime / ttm_r_drivetime,
                comp_m_drivetime = thesis_m_drivetime / ttm_m_drivetime,
                comp_sl_drivetime = thesis_sl_drivetime / ttm_sl_drivetime,
                comp_r_pct = thesis_r_pct / ttm_r_pct,
                comp_m_pct = thesis_m_pct / ttm_m_pct,
                comp_sl_pct = thesis_sl_pct / ttm_sl_pct) %>%
  dplyr::mutate_at(vars(comp_r_sfp, comp_m_sfp, comp_sl_sfp, comp_r_wtd,
                        comp_m_wtd, comp_sl_wtd, comp_r_drivetime, 
                        comp_m_drivetime, comp_sl_drivetime, comp_r_pct,
                        comp_m_pct, comp_sl_pct), 
                   ~round(., 2))



#### 6 Travel Time Comparison ShinyApp -----------------------------------------
server <- function(input, output, session) {

  #### 6.1 Reactive elements ---------------------------------------------------

  # Launch tooltip legend jQuery UI dialog
  observeEvent(input$info_dialog_btn, {
    
    # the div id='abbr-info' is loaded in "6.4 ShinyApp header", the div itself 
    # is the separate html file indicated in variable "info_path". Dialog 
    # window properties are located in .js
    runjs("$('#abbr-info').dialog('open');")
  })
  
  # Validate ykr-id in the numeric field
  validate_ykrid <- eventReactive(input$calcYkr, {
    
    # %then% allows only one error message at a time
    validate(
      need(is.numeric(input$ykrid), "Not an integer") %then%
      need(nchar(input$ykrid) == 7, "Seven digits pls") %then%
      need(input$ykrid <= max(result$YKR_ID), 
           paste("Value maximum is", max(result$YKR_ID))) %then%
      need(input$ykrid >= min(result$YKR_ID), 
           paste("Value minimum is", min(result$YKR_ID))) %then%
      need(input$ykrid %in% ykr_ids, "Value not a valid YKR_ID")
    )
    input$ykrid
  })
  
  # Print helpful text for the user
  helper_output_ykrid <- reactive({
    
    # if input from validate_ykrid() is numeric, display useful information
    # to the user
    if(is.numeric(validate_ykrid())) {
      
      thisVal <- result[result$YKR_ID == validate_ykrid(), ][1, ]
      help_output <- paste(
        "<p style='margin: 0 0 0px;'>",
        "<b>YKR_ID: ", validate_ykrid(), "</b>,<br>",
        thisVal[["zipcode"]], " ", thisVal[["nimi"]],
        "</p>", sep = "")
    }
    help_output
  })
  
  # This is the reactively built Helsinki Region Travel Time Matrix 2018 for the 
  # origin id inserted by the user. User's chosen YKR_ID value is 
  # validate_ykrid().
  thisTTM_df <- reactive({
    
    origin_id <- validate_ykrid()
    pos <- match(as.numeric(origin_id), ykr_ids)
    
    result <- 
      lapply(all_fst, FUN = TTM18fst_fetch, pos) %>%
      data.table::rbindlist(., fill = TRUE) %>%
      data.table::merge.data.table(dt_grid, ., by.x = "YKR_ID", by.y = "to_id")
    
    thesisdata <- 
      read.csv(file = recordspath,
               header = TRUE, 
               sep = ",",
               colClasses = c(timestamp = "POSIXct", zipcode = "factor"),
               stringsAsFactors = TRUE) %>%
      dplyr::select(zipcode, parktime, walktime) %>%
      dplyr::group_by(zipcode) %>%
      dplyr::summarise(res_park_avg = mean(parktime),
                       res_walk_avg = mean(walktime)) %>%
      dplyr::mutate_if(is.numeric, round, 2)
    
    # Join survey data to result
    result <- dplyr::inner_join(result, thesisdata, by = "zipcode") 
    
    result <- data.frame(result)
    car_cols <- c("car_r_t", "car_m_t", "car_sl_t", "ttm_r_t_avg", "ttm_m_t_avg", 
                  "ttm_sl_t_avg")
    
    # Get grouped means of TTM18 data for current starting point to all 
    # destinations for each postal code area
    result <- result %>%
      dplyr::group_by(zipcode) %>%
      dplyr::summarise(ttm_r_t_avg = mean(car_r_t),
                       ttm_m_t_avg = mean(car_m_t),
                       ttm_sl_t_avg = mean(car_sl_t)) %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::inner_join(result, ., by = "zipcode") %>%
      dplyr::mutate(ttm_r_t_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_t_avg),
                    ttm_m_t_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_t_avg),
                    ttm_sl_t_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sl_t_avg)) %>%
      dplyr::mutate_at(car_cols, ~dplyr::na_if(., -1))
    
    result
    
  })
  
  
  # equalBreaksColumn() calculates new class intervals when an input change is
  # detected on input$fill_column. Use the named vector "vis_cols".
  # TODO: could this be forgotten and just move the function to renderGirafe()?
  equalBreaksColumn <- reactive({
    
    res <- CreateJenksColumn_b(result, result, vis_cols[[input$fill_column]], 
                               input$fill_column, input$classIntervals_n)
    res
  })
  
  
  #### 6.2 ShinyApp outputs ----------------------------------------------------
  
  #### 6.2.1 Da plot -----------------------------------------------------------
  output$grid <- renderGirafe({
    
    # Reactive value: Insert equal breaks for mapping.
    #inputdata <- thisTTM_df()
    inputdata <- equalBreaksColumn()


    
    # Get an origin cell for mapping
    #origincell <- grid_f[grid_f["YKR_ID"] == as.numeric(validate_ykrid()), ]
    origincell <- grid_f[grid_f["YKR_ID"] == as.numeric(origin_id), ]
    
    
    # move this when finished
    GetLegendName <- function(val, origincell) {
      
      # Appropriately name the plot legend. Use parts of the input string to
      # figure out what to print. Use strwrap to automatically add newlines
      # to long strings.
      
      wherefrom <- paste0("origin ", origincell[, "YKR_ID"][1])
      
      if (grepl("ttm18_", val)) {
        datasource <- "TTM18 data"
        
      } else if (grepl("msc_", val)) {
        datasource <- "Thesis data"
        
      } else if (grepl("compare_", val)) {
        datasource <- "Comparison data"
      }
      
      if (grepl("_t", val)) {
        description <- 
          "The total travel time to YKR_ID (min)"
        
      } else if (grepl("_avg", val)) {
        description <- 
          "The mean total travel time to postal code area (min)"
        
      } else if (grepl("_drivetime", val)) {
        description <- 
          "The mean duration of the driving segment of the total travel time, to postal code area (min)"
        
      } else if (grepl("_pct", val)) {
        description <- 
          "The percentage of SFP and WTD in the total travel time (%)"
        
      } else if (grepl("_sfp", val)) {
        description <- 
          "The mean time consumed in searching for parking in the destination postal code area (min)"
        
      } else if (grepl("_wtd", val)) {
        description <- 
          "The mean duration to walk from one's parked car to the destination, in destination postal code area (min)"
      }
      # Automatically add newlines to the long description string
      description <- 
        strwrap(description, 22, prefix = "\n") %>%
        paste(., collapse = "")
      
      if (grepl("_m_", val)) {
        timeofday <- "during midday traffic"
        
      } else if (grepl("_r_", val)) {
        timeofday <- "during rush hour traffic"
        
      } else if (grepl("_sl_", val)) {
        timeofday <- "the route following speed limits without any additional impedances"
      }
      timeofday <- 
        strwrap(timeofday, 22, prefix = "\n") %>%
        paste(., collapse = "")
      
      result <- paste(datasource, ",\n", 
                      wherefrom, ":\n", 
                      description, ",\n",
                      timeofday, sep = "")
      return(result)
    }
    #GetLegendName("msc_r_drivetime", origincell)
    
    
    # Format legend labels (Equal breaks classes). Remove [, ], (, and ). Also 
    # add list dash. Create named vector for the origin cell legend entry
    l_labels <- gsub("(])|(\\()|(\\[)", "", levels(inputdata[, input$fill_column]))
    l_labels <- gsub(",", " \U2012 ", l_labels)
    
    legendname <- GetLegendName(input$fill_column, origincell)
    
    # if fillcolumn visualises postal code areas, allow counting of factor 
    # levels. Infix operator %notin% defined in funcs.R.
    if(input$fill_column %notin% c("ttm18_r_t", "ttm18_m_t", "ttm18_sl_t")) {
      l_labels <- AddLevelCounts(inputdata, vis_cols[[input$fill_column]], 
                                 input$fill_column, input$classIntervals_n, 
                                 l_labels)
    }
    
    # Origin id legend label
    o_label <- setNames("purple", 
                        origincell[, "nimi"] %>% 
                          unique() %>% 
                          as.character())
    
    # current_subdiv_lbl is created so that any values can be removed when 
    # necessary from the dataframe. By using a duplicate of subdiv_lbl, labels 
    # can also be returned into view.
    current_subdiv_lbl <- data.frame(subdiv_lbl)
    
    # Get the tooltip from a separate HTML file. Get rid of indentation and 
    # HTML comments in the function ReadAndClean().
    tooltip_content <- ReadAndClean(tooltip_path)

    
    
    #### 6.2.1.1 Define ggplot obligatory elements ----
    g <- ggplot(data = inputdata) + 
      geom_polygon_interactive(
        color = "black",
        size = 0.15,
        aes_string("long", "lat", 
                   group = "group",
                   fill = input$fill_column,
                   tooltip = substitute(
                     sprintf(tooltip_content, YKR_ID, 
                             zipcode, nimi,
                             
                             car_r_t, car_m_t, car_sl_t,
                             ttm_sfp, ttm_sfp, ttm_sfp,
                             ttm_wtd, ttm_wtd, ttm_wtd,
                             ttm_r_avg, ttm_m_avg, ttm_sl_avg,
                             ttm_r_drivetime, ttm_m_drivetime, ttm_sl_drivetime,
                             ttm_r_pct, ttm_m_pct, ttm_sl_pct,
                             
                             vals_in_zip,
                             thesis_r_sfp, thesis_m_sfp, thesis_sl_sfp,
                             thesis_r_wtd, thesis_m_wtd, thesis_sl_wtd,
                             thesis_r_drivetime, thesis_m_drivetime, thesis_sl_drivetime,
                             thesis_r_pct, thesis_m_pct, thesis_sl_pct,
                             
                             comp_r_sfp, comp_m_sfp, comp_sl_sfp,
                             comp_r_wtd, comp_m_wtd, comp_sl_wtd,
                             comp_r_drivetime, comp_m_drivetime, comp_sl_drivetime,
                             comp_r_pct, comp_m_pct, comp_sl_pct))
                   )) +
      
      # Jenks classes colouring and labels. drop = FALSE is very important in
      # most of the cases of input$fill_column. Levels may end up appearing zero
      # times in the data, and that would get them erased from the legend and
      # mix everything up.
      scale_fill_brewer(palette = "RdYlGn",
                        #name = paste("Distance from\n", validate_ykrid(), ", (min)",
                        #name = paste("Distance from\n", origin_id, ", (min)", 
                        #             sep = ""),
                        name = paste0(legendname, collapse = ""),
                        direction = -1,
                        labels = l_labels,
                        na.value = "darkgrey",
                        drop = FALSE) +
      
      # Define map extent manually
      coord_fixed(xlim = c(min(inputdata$lon) + 200, max(inputdata$lon) - 1200),
                  ylim = c(min(inputdata$lat) + 600, max(inputdata$lat) - 600)) +
      
      # ggnewscale makes it possible to map additional legends with same 
      # properties, in this case a new scale_fill. 
      ggnewscale::new_scale_fill() +
      
      # Plot origin YKR_ID, the starting position for TTM18
      geom_polygon(data = origincell,
                   aes(long, lat, fill = nimi),
                   linetype = "solid",
                   size = 0.6) +
      
      # Get a legend entry for origin ykr id
      scale_fill_manual(name = "Origin YKR ID", 
                        values = o_label,
                        labels = names(o_label)) +
    
      # Scale bar and north arrow
      ggsn::scalebar(inputdata, 
                     dist_unit = "km",
                     dist = 2,
                     st.dist = 0.01,
                     st.size = 4, 
                     height = 0.01, 
                     transform = FALSE) +
      
      ggsn::north(inputdata, 
                  location = "topright", 
                  scale = 0.04, 
                  symbol = 10) +
      
      # Legend settings
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14))
    
    
    
    #### 6.2.1.2 If statements for on-off switches ----
    
    # Plot municipality boundaries on the map
    if(input$show_muns == TRUE) {
      
      # Municipality boundaries
      g <- g + geom_polygon(data = muns_f,
                   aes(long, lat, group = group),   
                   color = alpha("black", 0.9), 
                   fill = "NA",
                   size = 1.0)
    }
    
    if(input$show_subdiv == TRUE) {
      
      # Municipality boundaries
      g <- g + geom_polygon(data = subdiv_f,
                            aes(long, lat, group = group),
                            color = alpha("black", 0.6), 
                            fill = "NA",
                            size = 0.6)
    }
    
    # Plot postal code area boundaries on the map
    if(input$show_postal == TRUE) {
      
      # Postal code area boundaries
      g <- g + geom_polygon(data = postal_f,
                   aes(long, lat, group = group),
                   color = "black", 
                   fill = "NA",
                   size = 0.2)
    }
    
    # Plot walking center boundaries
    if(input$show_walk == TRUE) {
      
      # New legend entry for walking center of Helsinki
      g <- g + ggnewscale::new_scale_color() +
        
        geom_polygon(data = walk_f,
                     aes(long, lat, color = label),
                     fill = NA,
                     size = 0.9) + 
        
        scale_color_manual(name = NULL,
                           values = setNames("#6b01ab", "walk"),
                           labels = "Helsinki walking\ncenter (TTM18)")
    }

    # Plot postal code area labels
    if(input$show_postal_labels == TRUE) {
      
      # Add zipcode labels
      g <- g + with(zipcode_lbl,
                    annotate(geom = "label", 
                             x = long, 
                             y = lat, 
                             label = label, 
                             label.size = NA,
                             fill = alpha("white", 0.5),
                             size = 4))
    }
    
    # Plot postal code area labels
    if(input$show_muns_labels == TRUE) {
      
      
      # Disable Kauniainen label on subdiv when muns labels visible
      if(input$show_subdiv_labels == TRUE) {
        current_subdiv_lbl["Kauniainen", "label"] <- NA
      }
      # Add zipcode labels
      g <- g + with(muns_lbl,
                    annotate(geom = "label", 
                             x = long, 
                             y = lat, 
                             label = label, 
                             label.size = NA,
                             fill = alpha("white", 0.5),
                             size = 6,
                             fontface = 2))
    }
    
    # Plot postal code area labels
    if(input$show_subdiv_labels == TRUE) {
      
      # Disable Kauniainen label on subdiv when muns labels visible
      if(input$show_muns_labels == TRUE) {
        current_subdiv_lbl["Kauniainen", "label"] <- NA
      }
      # Add zipcode labels
      g <- g + with(current_subdiv_lbl,
                    annotate(geom = "label", 
                             x = long, 
                             y = lat, 
                             label = label, 
                             label.size = NA,
                             fill = alpha("white", 0.5),
                             size = 5))
    }
    
    
    #### 6.2.1.3 Download and rendering ----
    
    # Prepare the downloadable interactive map. "interactive_out" is brought 
    # to global environment for download. Use larger fonts.
    compare_out <<- g + 
      theme(legend.title = element_text(size = 17),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
    
    
    # Render comparison map
    girafe(ggobj = g,
           width = 27,
           height = 18,
           options = list(opts_zoom(min = 1, max = 3),
                          opts_sizing(rescale = TRUE, width = 1),
                          opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  
  # Actual download functionality
  output$dl_compare <- downloadHandler(
    filename = paste("ttm18-thesis-compare_",
                     "mapfill-", input$fill_column, "_fromid-", input$ykrid, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".png",
                     sep = ""),
    
    content = function(file) {
      ggsave(plot = compare_out, file, width = 21, height = 14, dpi = 175)
    }
  )
  
  
  #### 6.2.2 Other outputs -----------------------------------------------------
  output$ykr_validator <- renderText({ 
    validate_ykrid()
  })
  
  # Helps user understand where their ykrid is located
  output$ykr_helper <- renderText({
    helper_output_ykrid()
  })
}


#### 6.3 ShinyApp UI -----------------------------------------------------------
ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("slate"),
    
    
    #### 6.4 ShinyApp header ---------------------------------------------------
    tags$head(tags$link(rel = "stylesheet", 
                        type = "text/css", 
                        href = "https://use.fontawesome.com/releases/v5.13.0/css/all.css"),
              tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
              tags$link(rel = "stylesheet", 
                        type = "text/css", 
                        href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
              htmltools::includeCSS(csspath)),
    htmltools::includeScript(path = jspath),
    
    # jQuery UI dialog content
    tags$html(HTML(ReadAndClean(info_path))),
    
    
    ### 6.5 Sidebar layout -----------------------------------------------------
    titlePanel(NULL, windowTitle = "Sampo Vesanen's thesis: Travel time comparison"),
    sidebarLayout(
      sidebarPanel(
        id = "sidebar",
        
        HTML("<label class='control-label'>Origin YKR ID</label>",
             "<div id='contents'>",
             "<div id='ykr-flash'>"),
        numericInput(
          inputId = "ykrid", 
          label = "Origin YKR ID",
          max = max(result$YKR_ID),
          min = min(result$YKR_ID),
          value = origin_id),
        HTML("</div>"),
                   
        actionButton(
          inputId = "calcYkr",
          label = HTML("<i class='icon calculator'></i>Calculate a new comparison with this YKR_ID")),
        
        HTML("<p id='smalltext'><b>Current origin YKR_ID</b></p>",
             "<div id='contents'>"),
        htmlOutput("ykr_helper"),
        HTML("</div>",
             "</div>"),
        
        HTML("<label class='control-label'>Symbology options</label>",
             "<div id='contents'>"),
        selectInput(
          inputId = "fill_column",
          label = "Visualise data (equal interval)",
          selected = "msc_sl_drivetime",
          choices = list(
            `Travel Time Matrix 2018` = 
              c("ttm18_r_t", "ttm18_m_t", "ttm18_sl_t", 
                "ttm18_r_avg", "ttm18_m_avg", "ttm18_sl_avg",
                "ttm18_r_drivetime", "ttm18_m_drivetime", "ttm18_sl_drivetime",
                "ttm18_r_pct", "ttm18_m_pct", "ttm18_sl_pct"),
            
            `Thesis survey results` = 
              c("msc_r_sfp", "msc_m_sfp", "msc_sl_sfp",
                "msc_r_wtd", "msc_m_wtd", "msc_sl_wtd",
                "msc_r_drivetime", "msc_m_drivetime", "msc_sl_drivetime",
                "msc_r_pct", "msc_m_pct", "msc_sl_pct"),
            
            `Compare TTM18 and thesis results` = 
              c("compare_r_sfp", "compare_m_sfp", "compare_sl_sfp",
                "compare_r_wtd", "compare_m_wtd", "compare_sl_wtd",
                "compare_r_drivetime", "compare_m_drivetime", "compare_sl_drivetime",
                "compare_r_pct", "compare_m_pct", "compare_sl_pct"))),
        
        sliderInput(
          inputId = "classIntervals_n",
          label = "Amount of classes",
          min = 2, 
          max = 11, 
          value = 11),
        HTML("</div>"),
        
        # Layer options: on-off switches
        # Postal code areas
        HTML("<label class='control-label'>Layer options</label>",
             "<div id='contents'>",
             "<div class='onoff-container'>",
             
             "<div class='onoff-div'><b>Postal code areas</b><br>",
             "<label class='control-label onoff-label' for='show_postal'>Boundaries</label>"),
        shinyWidgets::switchInput(
          inputId = "show_postal",
          size = "mini",
          value = TRUE),
        HTML("<label class='control-label onoff-label' for='show_postal_labels'>Labels</label>"),
        shinyWidgets::switchInput(
          inputId = "show_postal_labels", 
          size = "mini",
          value = FALSE),
        HTML("</div>"),
        
        # Walking center switch
        HTML("<div class='onoff-div'><b>Helsinki walking center (TTM18)</b><br>",
             "<label class='control-label onoff-label' for='show_walk'>Boundaries</label>"),
        shinyWidgets::switchInput(
          inputId = "show_walk",
          size = "mini",
          value = TRUE),
        HTML("</div>"),
        
        # Switches for muns
        HTML("<div class='onoff-div'><b>Municipalities</b><br>",
             "<label class='control-label onoff-label' for='show_muns'>Boundaries</label>"),
        shinyWidgets::switchInput(
          inputId = "show_muns", 
          size = "mini",
          value = TRUE),
                   
        HTML("<label class='control-label onoff-label' for='show_muns_labels'>Labels</label>"),
        shinyWidgets::switchInput(
          inputId = "show_muns_labels", 
          size = "mini",
          value = FALSE),
        HTML("</div>"),
        
        # Switches for subdivisions
        HTML("<div class='onoff-div'><b>Subdivisions</b><br>",
             "<label class='control-label onoff-label' for='show_subdiv'>Boundaries</label>"),
        shinyWidgets::switchInput(
          inputId = "show_subdiv", 
          size = "mini",
          value = FALSE),
        
        HTML("<label class='control-label onoff-label' for='show_subdiv_labels'>Labels</label>"),
        shinyWidgets::switchInput(
          inputId = "show_subdiv_labels", 
          size = "mini",
          value = FALSE),
        HTML("</div>",
             "</div>",
             "</div>"),
        
        HTML(paste("<p id='version-info'>Travel time comparison app version", 
                   app_v, "</p>")),
        width = 1),
      
      
      ### 6.6 Mainpanel layout -------------------------------------------------
      mainPanel(
        HTML("<div class='rightside-toolbar'>"),
        downloadLink(
          outputId = "dl_compare",
          label = HTML("<i class='icon file' title='Download hi-res version of this figure (png)'></i>")),
        actionLink(
          inputId = "info_dialog_btn",
          label = HTML("<i class='icon info' title='Open tooltip abbreviations legend dialog'></i>")),
        HTML("</div>"),
        girafeOutput("grid"), 
        
        width = 9
      )
    )
  )
)
shinyApp(ui = ui, server = server)