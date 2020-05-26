
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

# Python prepared data directories
datapath <- file.path(wd, "records_for_r.csv")
visitorpath <- file.path(wd, "leaflet_survey_results/visitors.csv")
postal_path <- file.path(wd, "postal_for_r.csv")
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018\\%sxxx\\travel_times_to_ %s.txt")
ttm_path2 <- file.path(wd, "HelsinkiTravelTimeMatrix2018")


# data path
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")

# Import grid
app_crs <- sp::CRS("+init=epsg:3067")

grid <-
  rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(x, y))




## TEST 3
# try to merge spatialdataframe and TTM18 before fortifying
test3grid <-
  rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

kaga <- data.table(YKR_ID = integer(), long = numeric(), lat = numeric(), 
                   order = integer(), hole = logical(), piece = factor(), 
                   id = character(), group = factor(), to_id = integer(), 
                   car_r_t = integer(), car_m_t = integer(), 
                   car_sl_t = integer())

all_files <- list.files(path = ttm_path2, pattern = ".txt$", recursive = TRUE)
all_files <- all_files[-length(all_files)] #remove metadata

fromy_id <- "5985086" #originId
to_id <- "5866836" #destinationId
col_range <- c(1, 2, 14, 16, 18)

for(thispath in all_files) {
  this_ttm <- file.path(ttm_path2, thispath)
  thisTable <- data.table::fread(this_ttm, select = col_range)
  slice <- subset(thisTable, from_id == as.numeric(fromy_id))
  
  if (nrow(slice) != 0) {
    ddd <- merge(test3grid, slice, by.x = "YKR_ID", by.y = "from_id")
    kaga <- rbindlist(list(kaga, ddd))
  }
}

#dt <- data.table::fread(this_ttm, select = col_range)
#subb <- subset(dt, from_id == as.numeric(fromy_id))

#kaga <- merge(grid, subb, by.x = "YKR_ID", by.y = "from_id")
kaga <- kaga %>% 
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(x, y))

ggplot(data = kaga) + geom_polygon(aes(long, lat, group = group, fill = car_r_t))







# TEST111111 GRID POPULATE TEST
fromy_id <- "5985086" #originId
to_id <- "5866836" #destinationId
this_ttm <- sprintf(ttm_path, substr(to_id, 1, 4), to_id)
col_range <- c(1, 2, 14, 16, 18)
dt <- data.table::fread(this_ttm, select = col_range)
subb <- subset(dt, from_id == as.numeric(fromy_id))





# test2
# Use forloop to find currently specified values from all TTM18 files
fromy_id <- "5985086" #originId
to_id <- "5866836" #destinationId

all_files <- list.files(path = ttm_path2, pattern = ".txt$", recursive = TRUE)
all_files <- all_files[-length(all_files)] #remove metadata

dt_grid <- as.data.table(grid)
result <- data.table(YKR_ID = integer(), long = numeric(), lat = numeric(), 
                     order = integer(), hole = logical(), piece = factor(), 
                     id = character(), group = factor(), to_id = integer(), 
                     car_r_t = integer(), car_m_t = integer(), 
                     car_sl_t = integer())

start.time <- Sys.time()
for(thispath in all_files) {
  this_ttm <- file.path(ttm_path2, thispath)
  thisTable <- data.table::fread(this_ttm, select = col_range)
  slice <- subset(thisTable, from_id == as.numeric(fromy_id))
  
  if (nrow(slice) != 0) {
    ddd <- data.table::merge.data.table(dt_grid, slice, by.x = "YKR_ID", by.y = "from_id")
    result <- rbindlist(list(result, ddd))
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ggplot(data = result) + geom_polygon(aes(long, lat, group = group, fill = car_r_t))
#ddd <- data.table::merge.data.table(grid, slice, by.x = "YKR_ID", by.y = "from_id")







# TEST4
# To_id are ALL OF THE IDS! CANT PLACE A DESTINATION BECAUSE WE WANT ALL
fromy_id <- "5985086" #originId

all_files <- list.files(path = ttm_path2, pattern = ".txt$", recursive = TRUE)
all_files <- all_files[-length(all_files)] #remove metadata

dt_grid <- as.data.table(grid)
result <- data.table(YKR_ID = integer(), long = numeric(), lat = numeric(), 
                     order = integer(), hole = logical(), piece = factor(), 
                     id = character(), group = factor(), to_id = integer(), 
                     car_r_t = integer(), car_m_t = integer(), 
                     car_sl_t = integer())

start.time <- Sys.time()

for(thispath in all_files) {
  this_ttm <- file.path(ttm_path2, thispath)
  thisTable <- data.table::fread(this_ttm, select = col_range)
  slice <- subset(thisTable, from_id == as.numeric(fromy_id))

  if (nrow(slice) != 0) {
    ddd <- data.table::merge.data.table(dt_grid, slice, by.x = "YKR_ID", by.y = "to_id")
    result <- rbindlist(list(result, ddd), fill = TRUE)
  }
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


ggplot(data = result) + geom_polygon(aes(long, lat, group = group, fill = car_r_t))




















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
                   fill = result$car_r_t)) +
      coord_fixed()
    
    # Render interactive map
    ggiraph(code = print(g),
            width_svg = 21,
            height_svg = 16)
  })
}

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("slate"),
    titlePanel(NULL, windowTitle = "Test"),
    sidebarLayout(
      sidebarPanel(id = "sidebar",
                   HTML("Test"),
                   width = 5),
    
      mainPanel(
        HTML("Test"),
        ggiraphOutput("gridi") %>% shinycssloaders::withSpinner()
      )
    )
  )
)
shinyApp(ui = ui, server = server)