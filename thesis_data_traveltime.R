
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
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")



#### Import grid ---------------------------------------------------------------
app_crs <- sp::CRS("+init=epsg:3067")

grid <-
  rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(x, y))



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

# THIS RUNS FOR 4-5 MINUTES
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
ggplot(data = result) + geom_polygon(aes(long, lat, group = group, fill = car_r_t))







#### Travel Time Comparison ShinyApp -------------------------------------------
server <- function(input, output, session) {

  output$gridi <- renderggiraph({
    
    tooltip_content <- paste0("<div>%s</div>")
    
    g <- ggplot(data = result) + 
      geom_polygon_interactive(
        color = "black",
        size = 0.2,
        aes_string("long", "lat", 
                   group = "group",
                   tooltip = substitute(sprintf(tooltip_content, car_r_t)),
                   fill = input$fill_column)) +
      coord_fixed()
    
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
                   HTML("wow mapp"),
                   selectInput(
                     "fill_column", 
                     HTML("Select map fill"),
                     c("car_r_t", "car_m_t", "car_sl_t"),
                   ),
                   width = 1),
    
      mainPanel(
        ggiraphOutput("gridi") %>% shinycssloaders::withSpinner()
      )
    )
  )
)
shinyApp(ui = ui, server = server)