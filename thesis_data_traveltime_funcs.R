
# Sampo Vesanen's Master's thesis statistical tests and visualisation
# Travel time comparison functions

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 29.5.2020



# infix operator
`%then%` <- shiny:::`%OR%`



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



GetCentroids <- function(fortified, unique_id, nominator) {
  
  # Annotate desired feature in ggplot. Adapted from: 
  # https://stackoverflow.com/a/28963405/9455395
  
  # Insert a fortified Spatial object and the column name you want to use as 
  # the label. With parameters "unique_id" and "nominator" a few functionalities 
  # can be attained: 
  
  # Unique_id tells what column to use as the unique identifier. This can be 
  # for example "kunta": four rows with coordinates and labels are created. 
  # If used "zipcode", 167 rows are created with coordinates and labels. 
  # "nominator" allocates the labels. "nominator" must contain the same amount 
  # of unique values, or more, than "unique_id", for example combination
  # unique_id = "kunta" and nominator = "zipcode" will create broken results.
  
  # unique_id will be stored as rowname for possible later use when row 
  # identification is needed.
  
  # Examples:
  # unique_id = "kunta" and nominator = "kunta":
  # --- 4 rows, centroids in the middle of municipalities, labels by "kunta"
  
  # unique_id = "zipcode" and nominator = "parktime_median":
  # --- 167 rows, centroids in the middle of zipcodes, labels by "parktime_median"
  
  # Change R options, otherwise as.numeric() loses some important digits
  options(digits = 15)
  
  result <- 
    do.call("rbind.data.frame",
            by(fortified,
               fortified[, unique_id],
               function(x) {c(sp::Polygon(x[c("long", "lat")])@labpt,
                              x %>% 
                                dplyr::group_by(!!rlang::sym(nominator)) %>%
                                dplyr::summarise() %>%
                                as.vector())
               })) %>%
    setNames(., c("long", "lat", "label"))
  
  # Change long and lat to numeric vectors, if they already aren't
  if(is.factor(result$long) == TRUE) {
    result$long <- as.numeric(levels(result$long))[result$long]
  } 
  if (is.factor(result$lat) == TRUE) {
    result$lat <- as.numeric(levels(result$lat))[result$lat]
  }
  
  return(result)
}