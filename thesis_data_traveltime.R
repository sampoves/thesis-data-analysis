
# Helsinki Region Travel Time comparison application
# Helsinki Region Travel Time Matrix 2018 <--> My thesis survey results

# 24.6.2020
# Sampo Vesanen


# Some decisions need decising:
# - Remember arbitrary selection of timeofday in thesisdata! Triplecheck it
# - Remember to be sure about calculation of pct: from drivetime or avg?
# - thesis drivetimes have negative values (this is a result in itself i think),
#   deal with this with colouring or something
# - verify download service products
# - add transparent zipcode boundaries
# - consider changing boundaries visibility to switch na-alpha0.45


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
app_v <- "0046.postal (24.6.2020)"

# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Data directories
munspath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
subdivpath <- file.path(wd, "python/suuralueet/PKS_suuralue.kml")
waterpath <- file.path(wd, "python/FI001L3_HELSINKI/ua2012_water.shp")
roadpath <- file.path(wd, "python/vayla/nopeusrajoitus_k_60.shp")

# Thesis' processed data
recordspath <- file.path(wd, "records_for_r.csv")
postal_path <- file.path(wd, "postal_for_r.csv")
gridzipcodes <- file.path(wd, "grid_for_r.csv")
fst_postal_fp <- file.path(wd, "TTM18_postal")

# Comparison app additional functionality
csspath <- file.path(wd, "python/thesis_data_traveltime_style.css")
jspath <- file.path(wd, "python/thesis_data_traveltime_script.js")
tooltip_path <- file.path(wd, "python/thesis_data_traveltime_tooltip.html")
info_path <- file.path(wd, "python/thesis_data_traveltime_info.html")

# Source functions and postal code variables
source(file.path(wd, "python/thesis_data_traveltime_funcs.R"))

# Convert Helsinki Region Travel Time Matrix 2018 into fst and then to dataset
# aggregated to postal code area resolution. The user of this script is not
# needed to run this script as the end-product of this script is provided in
# the GitHub repository.
#source(file.path(wd, "python/thesis_data_traveltime_conv.R"))



#### 2 Import data layers ------------------------------------------------------

#### 2.1 Walking center polygon ------------------------------------------------

# use this CRS information throughout the app
app_crs <- sp::CRS("+init=epsg:3067")

# TTM18 Helsinki walking center polygon. Use fortified version of the walking 
# center for visualisation
walkingHki <- 
  data.frame(
    long = c(387678.024778, 387891.53396, 383453.380944, 383239.871737, 387678.024778),
    lat = c(6675360.99039, 6670403.35286, 6670212.21613, 6675169.85373, 6675360.99039)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = app_crs) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON")

# Fortify
walk_f <- 
  sf::st_coordinates(walkingHki)[, 1:2] %>%
  sp::Polygon(.) %>%
  ggplot2::fortify(.) %>%
  dplyr::mutate(label = "walk")



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
  dplyr::select(-c(namn, vaestontih, km2, vakiluku, kuntakoodi))



#### 2.3 Postal code areas -----------------------------------------------------

postal <- 
  read.csv(file = postal_path,
           header = TRUE, 
           sep = ",",
           colClasses = c(zipcode = "factor", kunta = "factor", 
                          geometry = "character"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(c(2, 3, 108))

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
                    by = "id")} %>%
  dplyr::select(-geometry)

# This dataframe, produced in Python, helps find the zipcodes for each YKR_ID.
# 99999 is outside of research area.
ykrid_zipcodes <- 
  read.csv(file = gridzipcodes,
           header = TRUE, 
           sep = ",",
           colClasses = c(YKR_ID = "integer", zipcode = "character"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(-X)



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



#### 2.6 Water and roads -------------------------------------------------------

# # Digiroad
roads_f <-
  rgdal::readOGR(roadpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  ggplot2::fortify(.)

# UA2012 inland water
water_f <-
  rgdal::readOGR(waterpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  ggplot2::fortify(.)



#### 2.7 Label comparison plot features ----------------------------------------

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
vis_cols <- c("ttm18_r_avg" = "ttm_r_avg",
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



#### 4 Prepare reactive fetch of TTM18 data ----------------------------------

# NB! The execution of this code will fail at this point if TTM18 data is not 
# converted to fst. Please make sure you have run "thesis_data_traveltime_conv.R".

# Get all of the fst filepaths
# all_fst <- list.files(path = fst_filepath,
#                       pattern = ".fst$",
#                       recursive = TRUE,
#                       full.names = TRUE)

# Use "ykr_ids" as the location vector for each YKR_ID. This vector will be
# queried in the reactive fetching of TTM18 data.
# ykr_ids <- fst::read_fst(
#   file.path(fst_orig_fp, "5785xxx/travel_times_to_ 5785640.fst"))[, 1]

# Get all of the fst-format TTM18 aggregated to postal code area level
all_postal_fst <- list.files(path = fst_postal_fp,
                             pattern = ".fst$",
                             recursive = TRUE,
                             full.names = TRUE)



#### 6 Travel Time Comparison ShinyApp -----------------------------------------
server <- function(input, output, session) {

  #### 6.1 Reactive elements ---------------------------------------------------

  # Launch tooltip legend jQuery UI dialog
  shiny::observeEvent(input$info_dialog_btn, {
    
    # the div id='abbr-info' is loaded in "6.4 ShinyApp header", the div itself 
    # is the separate html file indicated in variable "info_path". Dialog 
    # window properties are located in .js
    shinyjs::runjs("$('#abbr-info').dialog('open');")
  })
  
  # Validate ykr-id in the numeric field
  validate_zipcode <- shiny::eventReactive(input$calcZip, {
    
    # %then% allows only one error message at a time
    shiny::validate(
      shiny::need(!is.na(as.numeric(input$zipcode)), "Can't contain letters") %then%
      shiny::need(nchar(input$zipcode) == 5, "Five digits pls") %then%
      shiny::need(input$zipcode %in% unique(ykrid_zipcodes$zipcode), 
           "Value not a valid postal code")
    )
    input$zipcode
  })
  
  # Print helpful text for the user
  helper_output_zip <- shiny::reactive({
    
    inputdata <- thisTTM()
    thisVal <- inputdata[inputdata$zipcode == validate_zipcode(), ][1, ]
    help_output <- paste(
      "<p style='margin: 0 0 0px;'>",
      "<b>Current origin postal code area:</b><br>",
      thisVal[["zipcode"]], " ", thisVal[["nimi"]],
      "</p>", sep = "")
    
    help_output
  })
  
  
  
  #### 6.1.1 Reactive fetch of TTM18 data --------------------------------------
  
  # This is the reactively built Helsinki Region Travel Time Matrix 2018 for the 
  # origin id inserted by the user. User's chosen YKR_ID value is 
  # validate_zipcode().
  thisTTM <- shiny::reactive({
    
    #### Fetch aggregated TTM18 data ---

    # Use 
    postal_loc <- grepl(validate_zipcode(), all_postal_fst, fixed = TRUE)
    this_fp <- all_postal_fst[postal_loc]
    
    result <- 
      fst::read_fst(this_fp, as.data.table = TRUE) %>%
      
      dplyr::left_join(., thesisdata, by = "zipcode") %>%
      dplyr::left_join(postal_f, ., by = "zipcode") %>%
      
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
      dplyr::mutate_at(vars(ttm_r_avg, ttm_m_avg, ttm_sl_avg),
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
    result
  })
  
  
  # equalBreaksColumn() calculates new class intervals when an input change is
  # detected on input$fill_column. Use the named vector "vis_cols".
  equalBreaksColumn <- reactive({
    
    inputdata <- thisTTM()
    res <- CreateJenksColumn_b(inputdata, inputdata, 
                               vis_cols[[input$fill_column]], 
                               input$fill_column, input$classIntervals_n)
    res
  })
  
  
  #### 6.2 ShinyApp outputs ----------------------------------------------------
  
  #### 6.2.1 Da plot -----------------------------------------------------------
  output$researcharea <- renderGirafe({
    
    # Reactive value: Insert equal breaks for mapping.
    inputdata <- equalBreaksColumn()
    
    # Get an origin cell for mapping
    origincell <- postal_f[postal_f["zipcode"] == validate_zipcode(), ]
    
    # Format legend labels (Equal breaks classes). Remove [, ], (, and ). Also 
    # add list dash. Create named vector for the origin cell legend entry
    l_labels <- 
      gsub("(])|(\\()|(\\[)", "", levels(inputdata[, input$fill_column])) %>%
      gsub(",", " \U2012 ", .)
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
        color = alpha("black", input$postal_vis),
        size = 0.3,
        aes_string("long", "lat", 
                   group = "group",
                   fill = input$fill_column,
                   tooltip = substitute(
                     sprintf(tooltip_content,
                             zipcode, nimi,
                             
                             "obs.", "obs.", "obs.",
                             ttm_sfp, ttm_sfp, ttm_sfp,
                             ttm_wtd, ttm_wtd, ttm_wtd,
                             ttm_r_avg, ttm_m_avg, ttm_sl_avg,
                             ttm_r_drivetime, ttm_m_drivetime, ttm_sl_drivetime,
                             ttm_r_pct, ttm_m_pct, ttm_sl_pct,
                             
                             zipcode, vals_in_zip,
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
                        name = paste0(legendname, collapse = ""),
                        direction = -1,
                        labels = l_labels,
                        na.value = "darkgrey",
                        drop = FALSE) +
      
      # Define map extent manually
      coord_fixed(xlim = c(min(inputdata$lon) + 200, max(inputdata$lon) - 1200),
                  ylim = c(min(inputdata$lat) + 600, max(inputdata$lat) - 600)) +
      
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
    
    
    # Roads and water in case we want them mapped
    if(input$show_water == TRUE) {
      g <- g + geom_polygon(data = water_f,
                   aes(long, lat, group = group),
                   color = alpha("blue", 0.9),
                   fill = "lightblue",
                   size = 0.4)
    }
    
    if(input$show_roads == TRUE) {
      g <- g + geom_polygon(data = roads_f,
                            aes(long, lat, group = group),
                            color = "#454545",
                            fill = "NA",
                            size = 0.7)
    }
    
    # Plot municipality boundaries on the map
    if(input$show_muns == TRUE) {
      
      # Municipality boundaries
      g <- g + geom_polygon(data = muns_f,
                            aes(long, lat, group = group),   
                            color = alpha("black", 0.9), 
                            fill = "NA",
                            size = 1.0)
    }
    
    # ggnewscale makes it possible to map additional legends with same 
    # properties, in this case a new scale_fill. 
    g <- g + ggnewscale::new_scale_color() +
      
      # Plot origin YKR_ID, the starting position for TTM18
      geom_polygon(data = origincell,
                   aes(long, lat, group = group, color = nimi),
                   fill = NA,
                   size = 1.2) +
      
      # Get a legend entry for origin ykr id
      scale_color_manual(name = "Origin postal\ncode area",
                         values = o_label,
                         labels = names(o_label))
    
    if(input$show_subdiv == TRUE) {
      
      # Municipality boundaries
      g <- g + geom_polygon(data = subdiv_f,
                            aes(long, lat, group = group),
                            color = alpha("black", 0.6), 
                            fill = "NA",
                            size = 0.6)
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
                     "mapfill-", input$fill_column, "_fromzip-", input$zipcode, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".png",
                     sep = ""),
    
    content = function(file) {
      ggsave(plot = compare_out, file, width = 21, height = 14, dpi = 175)
    }
  )
  
  
  #### 6.2.2 Other outputs -----------------------------------------------------
  
  # Helps user understand where their ykr_id is located
  output$zip_helper <- renderText({
    helper_output_zip()
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
        
        HTML("<label class='control-label'>Travel chain origin</label>",
             "<div id='contents'>",
             "<div id='zip-flash'>"),
        textInput(
          inputId = "zipcode",
          label = "Enter an origin postal code",
          value = "00100"), # Starting value Rautatientori, Ateneum
        HTML("</div>"),
                   
        actionButton(
          inputId = "calcZip",
          label = HTML("<i class='icon calculator'></i>Calculate a new comparison with this postal code")),
        
        HTML("<div id='contents'>"),
        htmlOutput("zip_helper"),
        HTML("</div>",
             "</div>"),
        
        HTML("<label class='control-label'>Symbology options</label>",
             "<div id='contents'>"),
        selectInput(
          inputId = "fill_column",
          label = "Visualise data (equal interval)",
          selected = "compare_r_sfp",
          choices = list(
            `Travel Time Matrix 2018 private car data` = 
              c("ttm18_r_avg", "ttm18_m_avg", "ttm18_sl_avg",
                "ttm18_r_drivetime", "ttm18_m_drivetime", "ttm18_sl_drivetime",
                "ttm18_r_pct", "ttm18_m_pct", "ttm18_sl_pct"),
            
            `Thesis survey results` = 
              c("msc_r_sfp", "msc_m_sfp", "msc_sl_sfp",
                "msc_r_wtd", "msc_m_wtd", "msc_sl_wtd",
                "msc_r_drivetime", "msc_m_drivetime", "msc_sl_drivetime",
                "msc_r_pct", "msc_m_pct", "msc_sl_pct"),
            
            `Compare TTM18 and thesis survey results` = 
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
        #### Postal code areas ---
        HTML("<label class='control-label'>Layer options</label>",
             "<div id='contents'>",
             "<div class='onoff-container'>"),
        
        HTML("<div class='onoff-div'><b>Postal code areas</b><br>"),
        HTML("<label class='control-label onoff-label' for='postal_vis'>Boundaries visibility</label>"),
        sliderInput(
          inputId = "postal_vis",
          label = NULL,
          min = 0, 
          max = 1, 
          value = 0.4,
          step = 0.1),
        
        HTML("<label class='control-label onoff-label' for='show_postal_labels'>Labels</label>"),
        shinyWidgets::switchInput(
          inputId = "show_postal_labels", 
          size = "mini",
          value = FALSE),
        HTML("</div>"),
        
        #### Walking center switch ---
        HTML("<div class='onoff-div'><b>Helsinki walking center (TTM18)</b><br>",
             "<label class='control-label onoff-label' for='show_walk'>Boundaries</label>"),
        shinyWidgets::switchInput(
          inputId = "show_walk",
          size = "mini",
          value = TRUE),
        HTML("</div>"),
        
        #### Switches for muns ---
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
        
        #### Switches for subdivisions ---
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
        HTML("</div>"),
        
        #### Switches for roads and water ---
        HTML("<div class='onoff-div'><b>Physical features</b><br>",
             "<label class='control-label onoff-label' for='show_roads'>Roads</label>"),
        shinyWidgets::switchInput(
          inputId = "show_roads",
          size = "mini",
          value = FALSE),
        
        HTML("<label class='control-label onoff-label' for='show_water'>Water</label>"),
        shinyWidgets::switchInput(
          inputId = "show_water", 
          size = "mini",
          value = FALSE),
        HTML("</div>",
             "</div>",
             "</div>"),
        
        HTML(paste("<p id='version-info'>Travel time comparison app version<br>", 
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
        
        girafeOutput("researcharea"), 
        
        width = 9
      )
    )
  )
)
shinyApp(ui = ui, server = server)