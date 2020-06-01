
# Travel time comparison test
# 31.5.2020
# Sampo Vesanen

# This interactive Travel time comparison test is dependent on ggiraph 0.7.7
# (only available through GitHub at the time of writing). With my setup ggiraph 
# 0.7.0 would load the map an unreasonably long time. I strongly suspected
# some fault in the tooltip generation.



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



# App version
app_v <- "0016 (31.5.2020)"


# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Data directories
postal_path <- file.path(wd, "postal_for_r.csv")
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018")
munspath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")
subdivpath <- file.path(wd, "python/suuralueet/PKS_suuralue.kml")

# Thesis data
recordspath <- file.path(wd, "records_for_r.csv")

# Directives
csspath <- file.path(wd, "python/thesis_data_traveltime_style.css")
jspath <- file.path(wd, "python/thesis_data_traveltime_script.js")

# Source functions and postal code variables
source(file.path(wd, "python/thesis_data_traveltime_funcs.R"))



#### 2 Import layers -----------------------------------------------------------

#### 2.1 Grid ------------------------------------------------------------------

# use this CRS information throughout the app
app_crs <- sp::CRS("+init=epsg:3067")

# Read an transform
grid <- rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

# Save grid as centroids for the spatial join of data
grid_point <- rgeos::gCentroid(grid, byid = TRUE)

# TTM18 Helsinki walking center polygon. Source: Henrikki Tenkanen.
walkingHki <- 
  data.frame(
    lon = c(387678.024778, 387891.53396, 383453.380944, 383239.871737, 387678.024778),
    lat = c(6675360.99039, 6670403.35286, 6670212.21613, 6675169.85373, 6675360.99039)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 3067) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON")





#### 2.2 Municipality borders --------------------------------------------------

# Get municipality borders. Fortify SP DataFrame for ggplot. Remove unnecessary
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
           colClasses = c(zipcode = "factor", kunta = "factor"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(c(2, 3, 6, 108))

# "postal" geometries are in well-known text format. Some processing is needed 
# to utilise these polygons in R. readWKT() uses rgeos
geometries <- lapply(postal[, "geometry"], "readWKT", p4s = app_crs)
sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(postal[, 1]))
row.names(postal) <- postal[, 1]

# Preserve SpatialPolygons format for spatial join
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



#### 3 Spatial join postal data to grid, fortify -----------------------------

# First, spatjoin postal data to grid centroids. Left FALSE is inner join. Then,
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

#### 4.1 Fetch TTM18 data ------------------------------------------------------

# Only specify origin id (from_id in TTM18). to_id remains open because we want
# to view all of the destinations.
origin_id <- "5985086"
origin_id_num <- as.numeric(origin_id)
col_range <- c(1, 2, 14, 16, 18)
col_range2 <- c("from_id", "to_id", "car_r_t", "car_m_t", "car_sl_t")
dt_grid <- as.data.table(grid_f)

# Get filepaths of all of the TTM18 data. Remove metadata textfile filepath.
all_files <- list.files(path = ttm_path, 
                        pattern = ".txt$", 
                        recursive = TRUE, 
                        full.names = TRUE)
all_files <- all_files[-length(all_files)]

# Fetch all the files of TTM18, select only relevant column and per file select
# relevant ykr_id. Use lapply() to avoid a for loop and save some time as this
# operation is supposed to be as fast as possible. The lapply output is a list
# of data.tables, so flatten that with rbindlist() and then merge with dt_grid,
# a data.table version of grid to get the basic TTM18 data we need for current
# calculation.

# NB! This runs for about 2.5 minutes, depending on hardware capabilities.
start.time <- Sys.time()
result <- 
  lapply(all_files, FUN = TTM18_fetch, col_range, origin_id_num) %>%
  data.table::rbindlist(., fill = TRUE) %>%
  data.table::merge.data.table(dt_grid, ., by.x = "YKR_ID", by.y = "to_id")

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)






#### PARQUET TEST ###
TTM18_to_parquet <- function(x) {
  
  # Generate parquet folder structure and filename
  splt <- unlist(strsplit(x, "/"))
  splt_len <- length(splt)
  filename <- paste(gsub(".txt", "", splt[splt_len]), ".parquet", sep = "")
  parquet_fp <- file.path(wd, "TTM18", splt[splt_len - 1])
  
  # Conditionally create folder
  if (!dir.exists(file.path(parquet_fp))) {
    dir.create(file.path(parquet_fp))
  }
  
  # data.table::fread current file, then write into parquet using compression.
  # Achieves about 33 % smaller file size.
  res <- fread(x, select = c(1, 2, 14, 16, 18))
  arrow::write_parquet(res, 
                       file.path(parquet_fp, filename), 
                       compression = "gzip", 
                       compression_level = 5)
}

# Conditionally create TTM18
if (!dir.exists(file.path(wd, "TTM18"))) {
  dir.create(file.path(wd, "TTM18"))
}
# This runs for 4-5 minutes
#lapply(all_files, FUN = TTM18_to_parquet)

all_parquet <- list.files(path = file.path(wd, "TTM18"), 
                          pattern = ".parquet$", 
                          recursive = TRUE, 
                          full.names = TRUE)

TTM18parquet_fetch <- function(x, origin_id) {
  res <- arrow::read_parquet(x)
  res <- subset(res, from_id == origin_id)
  return(res)
}
start.time <- Sys.time()
result <- 
  lapply(all_parquet, FUN = TTM18parquet_fetch, origin_id_num) %>%
  data.table::rbindlist(., fill = TRUE) %>%
  data.table::merge.data.table(dt_grid, ., by.x = "YKR_ID", by.y = "to_id")
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)








### FST TEST
#library(fst)

TTM18_to_fst <- function(x) {
  
  # Generate parquet folder structure and filename
  splt <- unlist(strsplit(x, "/"))
  splt_len <- length(splt)
  filename <- paste(gsub(".txt", "", splt[splt_len]), ".fst", sep = "")
  fst_fp <- file.path(wd, "TTM18", splt[splt_len - 1])
  
  # Conditionally create folder
  if (!dir.exists(file.path(fst_fp))) {
    dir.create(file.path(fst_fp))
  }
  
  # data.table::fread current file, then write into fst
  res <- data.table::fread(x, select = c(1, 2, 14, 16, 18))
  fst::write_fst(res, file.path(fst_fp, filename), compress = 100)
}

# Conditionally create folder "TTM18"
if (!dir.exists(file.path(wd, "TTM18"))) {
  dir.create(file.path(wd, "TTM18"))
}
# This runs for 5-10 minutes because of maximum compression
#lapply(all_files, FUN = TTM18_to_fst)

# read all fst files
all_fst <- list.files(path = file.path(wd, "TTM18"), 
                      pattern = ".fst$", 
                      recursive = TRUE, 
                      full.names = TRUE)

TTM18fst_fetch <- function(x, pos) {
  fst::read_fst(x, from = pos, to = pos, as.data.table = TRUE)
}

# get the position of current origin
ykr_ids <- read.csv(all_files[1], sep = ";")[, 1]
pos <- match(origin_id_num, ykr_ids) # get index of current id

start.time <- Sys.time()
result <- 
  lapply(all_fst, FUN = TTM18fst_fetch, pos) %>%
  data.table::rbindlist(., fill = TRUE) %>%
  data.table::merge.data.table(dt_grid, ., by.x = "YKR_ID", by.y = "to_id")
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)














# ARE THEY ALWAYS AT SAME POSITION??
# THIS WORKS IN SHINY, AT LEAST WITH  ORIGIN_ID DEFAULT!!
# try with parquet or vroom later!!
ykr_ids <- read.csv(all_files[1], sep = ";")[, 1]
pos <- match(origin_id_num, ykr_ids) # get index of current id

# read only specific columns and one row, our row
TTM18_fetch33 <- function(x, pos) {
  data.table::fread(x, select = c(1, 2, 14, 16, 18), nrows = 1, skip = pos)
}

# reading only one row per file removes colnames. Seems to work pretty good?? 1.2 min??
start.time <- Sys.time()
result <- 
  lapply(all_files, FUN = TTM18_fetch33, pos) %>%
  data.table::rbindlist(., fill = TRUE) %>%
  data.table::merge.data.table(dt_grid, ., by.x = "YKR_ID", by.y = "V2") #V2 = "to_id"

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# joined rename columns
colnames(result)[colnames(result) == "V1"] <- "from_id"
colnames(result)[colnames(result) == "V14"] <- "car_r_t"
colnames(result)[colnames(result) == "V16"] <- "car_m_t"
colnames(result)[colnames(result) == "V18"] <- "car_sl_t"








#### 4.2 Add thesis data values to the fortified data --------------------------

# TODO: Take into account all times of parking just like in Python

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



#### 4.4 Add averaged columns --------------------------------------------------

# backup to for not having to run that long forloop all the time
result2 <- data.frame(result)
car_cols <- c("car_r_t", "car_m_t", "car_sl_t", "car_r_t_avg", "car_m_t_avg", 
              "car_sl_t_avg")

# Get grouped means of TTM18 data for current starting point to all 
# destinations for each postal code area
result2 <- result2 %>%
  dplyr::group_by(zipcode) %>%
  dplyr::summarise(car_r_t_avg = mean(car_r_t),
                   car_m_t_avg = mean(car_m_t),
                   car_sl_t_avg = mean(car_sl_t)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  dplyr::inner_join(result2, ., by = "zipcode") %>%
  dplyr::mutate(car_r_t_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ car_r_t_avg),
                car_m_t_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ car_m_t_avg),
                car_sl_t_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ car_sl_t_avg)) %>%
  dplyr::mutate_at(car_cols, ~dplyr::na_if(., -1))



#### 5 Labeling plot features --------------------------------------------------

# Create labels for zipcodes
zipcode_lbl <- GetCentroids(postal_f, "zipcode", "zipcode")
muns_lbl <- GetCentroids(muns_f, "nimi", "nimi")
subdiv_lbl <- GetCentroids(subdiv_f, "Name", "Name")

# Get an origin cell for mapping
origincell <- grid_f[grid_f["YKR_ID"] == as.numeric(origin_id), ]

# Get all unique ykr_id values
unique_ykr <- unique(result2$YKR_ID)



#### 6 Travel Time Comparison ShinyApp -----------------------------------------
server <- function(input, output, session) {

  #### 6.1 Reactive elements ---------------------------------------------------

  # Validate ykr-id in the numeric field
  validate_ykrid <- eventReactive(input$calcYkr, {
    
    # %then% allows only one error message at a time
    validate(
      need(is.numeric(input$ykrid), "Not an integer") %then%
      need(nchar(input$ykrid) == 7, "Seven digits pls") %then%
      need(input$ykrid <= max(result2$YKR_ID), 
           paste("Value maximum is", max(result2$YKR_ID))) %then%
      need(input$ykrid >= min(result2$YKR_ID), 
           paste("Value minimum is", min(result2$YKR_ID))) %then%
      need(input$ykrid %in% unique_ykr, "Value not a valid YKR_ID")
    )
    input$ykrid
  })
  
  # Print helpful text for the user
  helper_output_ykrid <- reactive({
    
    # if input from validate_ykrid() is numeric, display useful information
    # to the user
    if(is.numeric(validate_ykrid())) {
      
      thisVal <- result2[result2$YKR_ID == validate_ykrid(), ][1, ]
      help_output <- paste(
        "<p style='margin: 0 0 0px;'>",
        "<b>YKR_ID: ", validate_ykrid(), "</b>,<br>",
        thisVal[["zipcode"]], " ", thisVal[["nimi"]], 
        "</p>", sep = "")
    }
    help_output
  })
  
  # currentinput() calculates new class intervals when input change detected
  currentinput <- reactive({
    res <- CreateJenksColumn2(result2, result2, "car_r_t_avg", "carrt_equal", input$classIntervals_n)
    res <- CreateJenksColumn2(res, res, "car_m_t_avg", "carmt_equal", input$classIntervals_n)
    res <- CreateJenksColumn2(res, res, "car_sl_t_avg", "carslt_equal", input$classIntervals_n)
    res
  })
  
  
  #### 6.2 ShinyApp outputs ----------------------------------------------------
  output$grid <- renderggiraph({
    
    # Reactive value: Insert equal breaks for mapping.
    inputdata <- currentinput()
    
    # Format map labels (Equal breaks classes). Remove [, ], (, and ). Also add 
    # list dash
    labels <- gsub("(])|(\\()|(\\[)", "", levels(inputdata[, input$fill_column]))
    labels <- gsub(",", " \U2012 ", labels)
    
    # current_subdiv is created so that values can be removed when necessary
    # but they can also be returned into view.
    current_subdiv_lbl <- data.frame(subdiv_lbl)
    
    tooltip_content <- paste0("<div id='app-tooltip'>",
                              "<div><b>id: %s</b></br>",
                              "%s, %s</div>",
                              "<hr id='tooltip-hr'>",
                              "<div>Park: %s, walk: %s</div>",
                              "<hr id='tooltip-hr'>",
                              "<div>r_t_avg: %s min</br>",
                              "m_t_avg: %s min</br>",
                              "sl_t_avg: %s min</div>",
                              "<hr id='tooltip-hr'>",
                              "<div>r_t: %s min</br>",
                              "m_t: %s min</br>",
                              "sl_t: %s min</div>",
                              "</div>")
    
    g <- ggplot(data = inputdata) + 
      geom_polygon_interactive(
        color = "black",
        size = 0.2,
        aes_string("long", "lat", 
                   group = "group",
                   tooltip = substitute(sprintf(tooltip_content, YKR_ID, 
                                                zipcode, nimi, res_park_avg, 
                                                res_walk_avg, car_r_t_avg, 
                                                car_m_t_avg, car_sl_t_avg,
                                                car_r_t, car_m_t, car_sl_t)),
                   fill = input$fill_column)) +
      
      # Jenks classes colouring and labels
      scale_fill_brewer(palette = "RdYlGn",
                         direction = -1,
                         name = paste("Distance from\n", origin_id, ", (min)", 
                                      sep = ""),
                         labels = labels,
                         na.value = "darkgrey") +
      
      # Define map extent manually
      coord_fixed(xlim = c(min(inputdata$lon) + 200, max(inputdata$lon) - 1200),
                  ylim = c(min(inputdata$lat) + 600, max(inputdata$lat) - 600)) +
      
      # Map starting position
      geom_polygon(data = origincell,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("purple", 0.5), 
                   fill = "purple",
                   size = 0.6) +
      
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
    
    
    #### On-off switch if statements 
    
    # Plot municipality boundaries on the map
    if(input$show_muns == TRUE) {
      
      # Municipality boundaries
      g <- g + geom_polygon(data = muns_f,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("black", 0.9), 
                   fill = "NA",
                   size = 1.0)
    }
    
    if(input$show_subdiv == TRUE) {
      
      # Municipality boundaries
      g <- g + geom_polygon(data = subdiv_f,
                            aes(long, lat, group = group),
                            linetype = "solid",
                            color = alpha("black", 0.6), 
                            fill = "NA",
                            size = 0.6)
    }
    
    # Plot postal code area boundaries on the map
    if(input$show_postal == TRUE) {
      
      # Postal code area boundaries
      g <- g + geom_polygon(data = postal_f,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = "black", 
                   fill = "NA",
                   size = 0.2)
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


    # Render interactive map
    ggiraph(code = print(g),
            width_svg = 26,
            height_svg = 19)
  })
  
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
              htmltools::includeCSS(csspath)),
    htmltools::includeScript(path = jspath),
    htmltools::htmlDependency(name = "svg.min.js", 
                              version ="3.0.15", 
                              src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/svg.js/3.0.15/"), 
                              script = "svg.min.js"),
        
    ### 6.5 Sidebar layout -----------------------------------------------------
    titlePanel(NULL, windowTitle = "Travel time comparison ShinyApp"),
    sidebarLayout(
      sidebarPanel(id = "sidebar",
                   
                   HTML("<div id='contents'>",
                        "<div id='ykr-flash'>"),
                   numericInput(
                     "ykrid", 
                     label = "Origin YKR ID",
                     max = max(result2$YKR_ID),
                     min = min(result2$YKR_ID),
                     value = origin_id),
                   HTML("</div>"),
                   
                   actionButton(
                     "calcYkr",
                     HTML("<i class='icon calculator'></i>Calculate a new comparison with this YKR_ID")),
                   
                   HTML("<p id='smalltext'><b>Current origin YKR_ID</b></p>",
                        "<div id='contents'>"),
                   htmlOutput("ykr_helper"),
                   HTML("</div>"),
                   HTML("</div>"),
                   
                   HTML("<div id='contents'>"),
                   selectInput(
                     "fill_column", 
                     HTML("Select map fill"),
                     c("carrt_equal", "carmt_equal", "carslt_equal"),
                     # TODO: add originals and averages above!
                   ),
                   
                   sliderInput(
                     "classIntervals_n",
                     "Amount of classes",
                     min = 2, 
                     max = 11, 
                     value = 11),
                   HTML("</div>"),
                   
                   # Layer options: on-off switches
                   HTML("<label class='control-label'>Layer options</label>",
                        "<div id='contents'>",
                        "<div class='onoff-container'>",
                        "<div class='onoff-div'><b>Postal code areas</b><br>"),
                   # Switch for interactive map labels
                   HTML("<label class='control-label onoff-label' for='show_postal'>Boundaries</label>"),
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
        ggiraphOutput("grid"),
      )
    )
  )
)
shinyApp(ui = ui, server = server)