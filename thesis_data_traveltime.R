
# Travel time comparison test
# 30.5.2020
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
app_v <- "0012 (30.5.2020)"


# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Data directories
postal_path <- file.path(wd, "postal_for_r.csv")
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018")
munspath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")
subdivpath <- file.path(wd, "python/suuralueet/PKS_suuralue.kml")

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

# Save grid as centroids for spatial join
grid_point <- rgeos::gCentroid(grid, byid = TRUE)



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



#### 2.4 Spatial join postal data to grid, fortify -----------------------------

# TODO: maybe add buffer to postal so that gray border cells get recognised
# as part of zipcodes

# First, spatjoin postal data to grid centroids. Left FALSE is inner join. Then,
# Join centroid data back to grid polygons and convert grid to SpatialPolygons.
# Finally, fortify for ggplot while keeping important columns.
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

# TODO



#### 3.2 Add classIntervals columns for graduated colouring --------------------

# backup to for not having to run that long forloop all the time
result2 <- data.frame(result)
car_cols <- c("car_r_t", "car_m_t", "car_sl_t", "car_r_t_avg", "car_m_t_avg", 
              "car_sl_t_avg")

# Get grouped means for TTM18 data for current starting point to all 
# destinations
result2 <- result2 %>%
  #dplyr::mutate(zipcode = forcats::fct_explicit_na(zipcode)) %>%
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


# -1 to NA
# result2$car_r_t <- dplyr::na_if(result2$car_r_t, -1)
# result2$car_m_t <- dplyr::na_if(result2$car_m_t, -1)
# result2$car_sl_t <- dplyr::na_if(result2$car_sl_t, -1)
# result2$car_r_t_avg <- dplyr::na_if(result2$car_r_t_avg, -1)
# result2$car_m_t_avg <- dplyr::na_if(result2$car_m_t_avg, -1)
# result2$car_sl_t_avg <- dplyr::na_if(result2$car_sl_t_avg, -1)

# Insert equal breaks for mapping
#result2 <- CreateJenksColumn2(result2, result2, "car_r_t_avg", "carrt_equal", 11)
#result2 <- CreateJenksColumn2(result2, result2, "car_m_t_avg", "carmt_equal", 11)
#result2 <- CreateJenksColumn2(result2, result2, "car_sl_t_avg", "carslt_equal", 11)



#### 3.2 Labeling features -----------------------------------------------------

# Create labels for zipcodes
zipcode_lbl <- GetCentroids(postal_f, "zipcode", "zipcode")
muns_lbl <- GetCentroids(muns_f, "nimi", "nimi")
subdiv_lbl <- GetCentroids(subdiv_f, "Name", "Name")

# Get an origin cell for mapping
origincell <- grid_f[grid_f["YKR_ID"] == as.numeric(origin_id), ]

# Get all unique ykr_id values
unique_ykr <- unique(result2$YKR_ID)



#### 4 Travel Time Comparison ShinyApp -----------------------------------------
server <- function(input, output, session) {

  #### 4.1 Reactive elements ---------------------------------------------------

  # reactive expression
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
  
  
  #### 4.2 ShinyApp outputs ----------------------------------------------------
  output$grid <- renderggiraph({
    
    # Insert equal breaks for mapping
    result2 <- CreateJenksColumn2(result2, result2, "car_r_t_avg", "carrt_equal", input$classIntervals_n)
    result2 <- CreateJenksColumn2(result2, result2, "car_m_t_avg", "carmt_equal", input$classIntervals_n)
    result2 <- CreateJenksColumn2(result2, result2, "car_sl_t_avg", "carslt_equal", input$classIntervals_n)
    
    # Format map labels (Equal breaks classes). Remove [, ], (, and ). Also add 
    # list dash
    labels <- gsub("(])|(\\()|(\\[)", "", levels(result2[, input$fill_column]))
    labels <- gsub(",", " \U2012 ", labels)
    
    # current_subdiv is created so that values can be removed when necessary
    # but they can also be returned into view.
    current_subdiv_lbl <- data.frame(subdiv_lbl)
    
    tooltip_content <- paste0("<div id='app-tooltip'>",
                              "<div><b>id: %s</b></br>",
                              "%s, %s</div>",
                              "<hr id='tooltip-hr'>",
                              "<div>r_t_avg: %s min</br>",
                              "m_t_avg: %s min</br>",
                              "sl_t_avg: %s min</div>",
                              "<hr id='tooltip-hr'>",
                              "<div>r_t: %s min</br>",
                              "m_t: %s min</br>",
                              "sl_t: %s min</div>",
                              "</div>")
    
    g <- ggplot(data = result2) + 
      geom_polygon_interactive(
        color = "black",
        size = 0.2,
        aes_string("long", "lat", 
                   group = "group",
                   tooltip = substitute(sprintf(tooltip_content, YKR_ID, 
                                                zipcode, nimi, car_r_t_avg, 
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
      coord_fixed(xlim = c(min(result2$lon) + 200, max(result2$lon) - 1200),
                  ylim = c(min(result2$lat) + 600, max(result2$lat) - 600)) +
      
      # Map starting position
      geom_polygon(data = origincell,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("purple", 0.5), 
                   fill = "purple",
                   size = 0.6) +
      
      # Scale bar and north arrow
      ggsn::scalebar(result2, 
                     dist_unit = "km",
                     dist = 2,
                     st.dist = 0.01,
                     st.size = 4, 
                     height = 0.01, 
                     transform = FALSE) +
      ggsn::north(result2, 
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


#### 4.1 ShinyApp UI -----------------------------------------------------------
ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("slate"),
    
    
    #### 4.2 ShinyApp header ---------------------------------------------------
    tags$head(tags$link(rel = "stylesheet", 
                        type = "text/css", 
                        href = "https://use.fontawesome.com/releases/v5.13.0/css/all.css"),
              htmltools::includeCSS(csspath)),
    htmltools::includeScript(path = jspath),

        
    ### 4.3 Sidebar layout -----------------------------------------------------
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
    
      
      ### 4.4 Mainpanel layout -------------------------------------------------
      mainPanel(
        ggiraphOutput("grid"),
      )
    )
  )
)
shinyApp(ui = ui, server = server)