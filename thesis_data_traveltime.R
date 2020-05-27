
# Travel time comparison test
# 27.5.2020
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



# App version
app_v <- "0007 (27.5.2020)"


# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Data directories
postal_path <- file.path(wd, "postal_for_r.csv")
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018")
munspath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")

# Directives
csspath <- file.path(wd, "python/thesis_data_traveltime_style.css")

# Source functions and postal code variables
source(file.path(wd, "python/thesis_data_traveltime_funcs.R"))



#### 2 Import layers -----------------------------------------------------------

#### 2.1 Grid ------------------------------------------------------------------

# use this CRS information throughout the app
app_crs <- sp::CRS("+init=epsg:3067")

# Read an transform
grid <- rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

# Save grid as centroids for spatial join
grid_point <- rgeos::gCentroid(grid, byid = TRUE)

# Fortify for ggplot OBSOLETE, DOES NOT CONTAIN POSTAL DATA
#grid_f <- grid %>%
#  {dplyr::left_join(ggplot2::fortify(.),
#                    as.data.frame(.) %>%
#                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
#  dplyr::select(-c(x, y))



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



#### 2.4 Spatial join postal data to grid ----------------------------------------
# spat join test. Left FALSE is inner join
grid_f <- 
  sf::st_join(st_as_sf(grid_point), 
              st_as_sf(postal), 
              join = st_intersects,
              left = FALSE) %>%
  sf::st_join(st_as_sf(grid), 
              .,
              join = st_intersects) %>%
  as(., "Spatial") %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(x, y))



#jeje <- sf::st_join(st_as_sf(grid), grid_join_inner, join = st_intersects)
#jeje <- as(jeje, "Spatial")

# Fortify for ggplot  
#jeje_f <- jeje %>%
#  {dplyr::left_join(ggplot2::fortify(.),
#                    as.data.frame(.) %>%
#                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
#  dplyr::select(-c(x, y))



#### 3 Integrate TTM18 ---------------------------------------------------------

# Only specify origin id (from_id in TTM18). to_id remains open because we want
# to view all of the destinations.
origin_id <- "5985086"
col_range <- c(1, 2, 14, 16, 18)

# Get filepaths of all of the TTM18 data. Remove metadata textfile filepath.
all_files <- list.files(path = ttm_path, pattern = ".txt$", recursive = TRUE)
all_files <- all_files[-length(all_files)]

# create dt_grid so that we don't need to recalculate grid all the time. 
# Establish an empty data.table result for the data appending.
dt_grid <- as.data.table(grid_f)
result <- data.table(YKR_ID = integer(), long = numeric(), lat = numeric(), 
                     order = integer(), hole = logical(), piece = factor(), 
                     id = character(), group = factor(), to_id = integer(), 
                     car_r_t = integer(), car_m_t = integer(), 
                     car_sl_t = integer())

start.time <- Sys.time()

# Iterate through all files in "all_files". From each file, import only relevant
# private car columns. Then select only the column with the id of our origin.
# Then, merge the newly found row of data with grid ykr_id, which for each
# iteration finds the rows signifying one grid cell. Then append this data to
# a new data.table which has all the private car columns we wanted.
# NB! This loop runs for 4-5 minutes.
for(thispath in all_files) {
  this_ttm <- file.path(ttm_path, thispath)
  thisTable <- data.table::fread(this_ttm, select = col_range)
  slice <- subset(thisTable, from_id == as.numeric(origin_id))

  if (nrow(slice) != 0) {
    thisMerge <- data.table::merge.data.table(dt_grid, slice, by.x = "YKR_ID", 
                                              by.y = "to_id")
    result <- rbindlist(list(result, thisMerge), fill = TRUE)
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)





#### 3.1 Add thesis data values to the fortified data --------------------------




#### 3.2 Add classIntervals columns for graduated colouring --------------------

# backup to for not having to run that long forloop all the time
result2 <- data.frame(result)

# -1 to NA
result2$car_r_t <- dplyr::na_if(result2$car_r_t, -1)
result2$car_m_t <- dplyr::na_if(result2$car_m_t, -1)
result2$car_sl_t <- dplyr::na_if(result2$car_sl_t, -1)

# Insert equal breaks for mapping
result2 <- CreateJenksColumn2(result2, result2, "car_r_t", "carrt_equal", 11)
result2 <- CreateJenksColumn2(result2, result2, "car_m_t", "carmt_equal", 11)
result2 <- CreateJenksColumn2(result2, result2, "car_sl_t", "carslt_equal", 11)



#### 3.2 Labeling features -----------------------------------------------------

# Create labels for zipcodes
zipcode_lbl <- GetCentroids(postal_f, "zipcode", "zipcode")

# Get an origin cell for mapping
origincell <- grid_f[grid_f["YKR_ID"] == as.numeric(origin_id), ]




#### 4 Travel Time Comparison ShinyApp -----------------------------------------
server <- function(input, output, session) {

  output$gridi <- renderggiraph({
    
    # Format map labels. Remove [, ], (, and ). Also add list dash
    labels <- gsub("(])|(\\()|(\\[)", "", levels(result2[, input$fill_column]))
    labels <- gsub(",", " \U2012 ", labels)
    
    tooltip_content <- paste0("<div><b>id: %s</b></br>",
                              "r_t: %s min</br>",
                              "m_t: %s min</br>",
                              "sl_t: %s min</div>")
    
    g <- ggplot(data = result2) + 
      geom_polygon_interactive(
        color = "black",
        size = 0.2,
        aes_string("long", "lat", 
                   group = "group",
                   tooltip = substitute(sprintf(tooltip_content, YKR_ID,
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
      coord_fixed(xlim = c(min(result2$lon) + 200, max(result2$lon) - 1200),
                  ylim = c(min(result2$lat) + 600, max(result2$lat) - 600)) +
      
      # Municipality boundaries
      geom_polygon(data = muns_f,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("black", 0.9), 
                   fill = "NA",
                   size = 1.0) +
      
      # Postal code area boundaries
      geom_polygon(data = postal_f,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = "black", 
                   fill = "NA",
                   size = 0.2) +
      
      # Map starting position
      geom_polygon(data = origincell,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("purple", 0.5), 
                   fill = "purple",
                   size = 0.6) +
      
      # Add zipcode labels
      with(zipcode_lbl,
           annotate(geom = "label", 
                    x = long, 
                    y = lat, 
                    label = label, 
                    label.size = NA,
                    fill = alpha("white", 0.5),
                    size = 4)) +
      
      # Legend settings
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14))
    
    
    # Render interactive map
    ggiraph(code = print(g),
            width_svg = 26,
            height_svg = 19)
  })
}

#### 4.1 ShinyApp UI elements --------------------------------------------------
ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("slate"),
    
    #### 4.2 ShinyApp header ---------------------------------------------------
    tags$head(htmltools::includeCSS(csspath)),
    
    titlePanel(NULL, windowTitle = "Travel time comparison ShinyApp"),
    sidebarLayout(
      sidebarPanel(id = "sidebar",
                   
                   selectInput(
                     "fill_column", 
                     HTML("Select map fill"),
                     c("carrt_equal", "carmt_equal", "carslt_equal"),
                   ),
                   HTML(paste("<p id='version-info'>Travel time comparison app version", 
                               app_v, "</p>")),
                   width = 1),
    
      mainPanel(
        ggiraphOutput("gridi") %>% shinycssloaders::withSpinner()
      )
    )
  )
)
shinyApp(ui = ui, server = server)