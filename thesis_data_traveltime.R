
# Travel time comparison test
# 26.5.2020
# Sampo Vesanen

# This interactive Travel time comparison test is dependent on ggiraph 0.7.7.
# With my setup ggiraph 0.7.0 would load the map an unreasonably long time.

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(htmltools)
library(rgdal)
library(shinyjs)
library(ggiraph)
library(data.table)

# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Data directories
#datapath <- file.path(wd, "records_for_r.csv")
#visitorpath <- file.path(wd, "leaflet_survey_results/visitors.csv")
#postal_path <- file.path(wd, "postal_for_r.csv")
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018")
munspath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")



#### FUNCS -----

CreateJenksColumn2 <- function(fortified, postal, datacol, newcolname, classes_n = 5) {
  
  # Use this function to create a column in fortified dataframe that can be
  # used to portray Jenks breaks colouring in a ggplot map. Dplyr note: to
  # enable parameters as column names in dplyr, apply !! and := for the left
  # side and for the right side !!rlang::sym().
  #
  # Adapted from:
  # https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
  
  # Suppress n jenks warnings, problem probably handled
  classes <- suppressWarnings(
    classInt::classIntervals(postal[, datacol], n = classes_n, style = "equal"))
  
  # classes$brk has to be wrapped with unique(), otherwise we can't get more
  # than six classes for parktime_median or walktime_median
  result <- fortified %>%
    dplyr::mutate(!!newcolname := cut(!!rlang::sym(datacol), 
                                      unique(classes$brks), 
                                      include.lowest = T))
  
  # Reverse column values to enable rising values from bottom to top in ggplot.
  # In ggplot, use scale_fill_brewer(direction = -1) with this operation to flip
  # the legend.
  #result[, newcolname] = factor(result[, newcolname], 
  #                              levels = rev(levels(result[, newcolname])))
  
  return(result)
}



#### Import grid ---------------------------------------------------------------
app_crs <- sp::CRS("+init=epsg:3067")

grid <-
  rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(x, y))



#### 3.2 Municipality borders --------------------------------------------------

# Get municipality borders. Fortify SP DataFrame for ggplot. Remove unnecessary
# columns to save memory.
# Shapefile data is Regional population density 2012, Statistics Finland.
# http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226.
muns_f <-
  rgdal::readOGR(munsclippedpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(namn, vaestontih, km2, vakiluku))



#### Prepare the map -----------------------------------------------------------

# Only specify origin id (from_id in TTM18). to_id remains open because we want
# to view all of the destinations.
origin_id <- "5985086"
col_range <- c(1, 2, 14, 16, 18)

# Get filepaths of all of the TTM18 data. Remove metadata textfile filepath.
all_files <- list.files(path = ttm_path2, pattern = ".txt$", recursive = TRUE)
all_files <- all_files[-length(all_files)]

# create dt_grid so that we don't need to recalculate grid all the time. 
# Establish an empty data.table result for the data appending.
dt_grid <- as.data.table(grid)
result <- data.table(YKR_ID = integer(), long = numeric(), lat = numeric(), 
                     order = integer(), hole = logical(), piece = factor(), 
                     id = character(), group = factor(), to_id = integer(), 
                     car_r_t = integer(), car_m_t = integer(), 
                     car_sl_t = integer())

start.time <- Sys.time()

# get origin cell for mapping
origincell <- grid[grid["YKR_ID"] == as.numeric(origin_id), ]

# NB! This runs for 4-5 minutes
for(thispath in all_files) {
  this_ttm <- file.path(ttm_path, thispath)
  thisTable <- data.table::fread(this_ttm, select = col_range)
  slice <- subset(thisTable, from_id == as.numeric(origin_id))

  if (nrow(slice) != 0) {
    ddd <- data.table::merge.data.table(dt_grid, slice, by.x = "YKR_ID", by.y = "to_id")
    result <- rbindlist(list(result, ddd), fill = TRUE)
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Did it work?
#ggplot(data = result) + geom_polygon(aes(long, lat, group = group, fill = car_r_t))

result2 <- data.frame(result)
result2$car_r_t <- dplyr::na_if(result2$car_r_t, -1)
result2$car_m_t <- dplyr::na_if(result2$car_m_t, -1)
result2$car_sl_t <- dplyr::na_if(result2$car_sl_t, -1)
result2 <- CreateJenksColumn2(result2, result2, "car_r_t", "carrt_jenks", 11)
result2 <- CreateJenksColumn2(result2, result2, "car_m_t", "carmt_jenks", 11)
result2 <- CreateJenksColumn2(result2, result2, "car_sl_t", "carslt_jenks", 11)



#### Travel Time Comparison ShinyApp -------------------------------------------
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
      
      coord_fixed() +
      
      # Municipality boundaries
      geom_polygon(data = muns_f,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("black", 0.9), 
                   fill = "NA",
                   size = 1.0) +
      
      # Map starting position
      geom_polygon(data = origincell,
                   aes(long, lat, group = group),
                   linetype = "solid",
                   color = alpha("purple", 0.5), 
                   fill = "purple",
                   size = 0.6) +
      
      # Legend settings
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14))
    
    
    # Render interactive map
    ggiraph(code = print(g),
            width_svg = 21,
            height_svg = 16)
  })
}

#### ShinyApp UI elements ------------------------------------------------------
ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("slate"),
    titlePanel(NULL, windowTitle = "Travel time comparison ShinyApp"),
    sidebarLayout(
      sidebarPanel(id = "sidebar",
                   
                   selectInput(
                     "fill_column", 
                     HTML("Select map fill"),
                     c("carrt_jenks", "carmt_jenks", "carslt_jenks"),
                     
                   ),
                   width = 1),
    
      mainPanel(
        ggiraphOutput("gridi") %>% shinycssloaders::withSpinner()
      )
    )
  )
)
shinyApp(ui = ui, server = server)