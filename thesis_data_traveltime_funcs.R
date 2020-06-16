
# Sampo Vesanen's Master's thesis
# Helsinki Region Travel Time comparison application functions

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 17.6.2020



# infix operator for sequential validation of input$ykrid
`%then%` <- shiny:::`%OR%`

# These custom infix operators work in the manner of += and ++ in C++/C# and
# Java. Save some space and increase readability.
`%+=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 + e2))
`%-=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 - e2))
`%notin%` <- Negate(`%in%`)



# Fetch function for fst format Travel Time Matrix 2018 data
TTM18fst_fetch <- function(x, pos) {
  fst::read_fst(x, from = pos, to = pos, as.data.table = TRUE)
}



# Fetch a fst format Travel Time Matrix data file, then subset that file with
# the character vector zipcode_ids, which represents ids of one postal code area.
TTM18fst_fetch2 <- function(x, zipcode_ids) {
  df <- fst::read_fst(x, as.data.table = TRUE)
  df <- df[from_id %in% zipcode_ids, ]
  return(df)
}


ReadAndClean <- function(fp) {
  
  # Use this function to read a html file into R and clean it of tabulator 
  # spaces and HTML comments.
  
  result <- 
    paste(readLines(fp, encoding = "UTF-8"), collapse = "") %>%
    gsub("[\t]", "", .) %>%
    gsub(" <!--(.*?)-->", "", .)
  
  return(result)
}



CreateJenksColumn_b <- function(fortified, inputDf, datacol, newcolname, 
                                classes_n = 11) {
  
  # Function name _b refers to this function being the variant B of the function
  # CreateJenksColumn in thesis_data_vis.R.
  #
  # Use this function to create a column in fortified dataframe that can be
  # used to portray Jenks breaks colouring in a ggplot map. Dplyr note: to
  # enable parameters as column names in dplyr, apply !! and := for the left
  # side and for the right side !!rlang::sym().
  #
  # Adapted from:
  # https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
  
  # Suppress n jenks warnings, problem probably handled
  classes <- suppressWarnings(
    classInt::classIntervals(inputDf[, datacol], n = classes_n, style = "equal"))
  
  # classes$brk has to be wrapped with unique(), otherwise we can't get more
  # than six classes for parktime_median or walktime_median
  result <- fortified %>%
    dplyr::mutate(!!newcolname := cut(!!rlang::sym(datacol), 
                                      unique(classes$brks), 
                                      include.lowest = T))
  return(result)
}



AddLevelCounts <- function(thisDf, datacol, newcolname, classes_n,
                           labels_to_input) {
  
  # Use this function to calculate how many times a legend key color appears
  # in the comparison app and add those values to the ggplot legend labels.
  # This function is non-optimal as half of this functionality is carried out
  # in CreateJenksColumn_b(), but for the sake of clarity keep them apart.
  
  intervals <- suppressWarnings(
    classInt::classIntervals(thisDf[, datacol], n = classes_n, style = "equal"))
  
  interv_codes <- cut(thisDf[, datacol], unique(intervals$brks),
                     include.lowest = T)
  input_levels <- levels(thisDf[, newcolname])
  
  # Create level appearance count in this clunky way.
  # In left_join NAs can be preserved if the new dataframe has NA present
  # like this: data.frame(brks = c(input_levels, NA))
  levels_n <-
    dplyr::select(thisDf, zipcode) %>%
    dplyr::mutate(brks = interv_codes) %>%
    dplyr::group_by(brks) %>%
    dplyr::summarise(n = dplyr::n_distinct(zipcode)) %>%
    dplyr::left_join(data.frame(brks = input_levels), .)
  levels_n$n[is.na(levels_n$n)] <- 0
  
  result <- paste0(labels_to_input, " [", levels_n$n, "]")
  
  return(result)
}



GetLegendName <- function(val, origincell) {
  
  # Appropriately name the plot legend. Use parts of the input string to
  # figure out what to print. Use strwrap() to automatically add newlines
  # to long strings. Use origincell to add information about origin ykr_id.
  
  wherefrom <- paste0("origin ", origincell[, "zipcode"][1])
  #wherefrom <- paste0("origin ", origincell)
  
  if (grepl("ttm18_", val)) {
    datasource <- "TTM18 data"
    
  } else if (grepl("msc_", val)) {
    datasource <- "Thesis data"
    
  } else if (grepl("compare_", val)) {
    datasource <- "Compare data sources (thesis data / TTM18 data)"
  }
  # Automatically add newlines to the long datasource string
  datasource <- 
    strwrap(datasource, 22, prefix = "\n") %>%
    paste(., collapse = "")
  
  # thisUnit is minutes, except when datasource is "compare" or 
  # description "_pct"
  if (grepl("compare_", val) || grepl("_pct", val)) {
    thisUnit <- "(unit %)"
  } else {
    thisUnit <- "(unit minutes)"
  }
  
  if (grepl("_t", val)) {
    description <- 
      "The total travel time to YKR_ID"
    
  } else if (grepl("_avg", val)) {
    description <- 
      "The mean total travel time to postal code area"
    
  } else if (grepl("_drivetime", val)) {
    description <- 
      "The mean duration of the driving segment of the total travel time, to a postal code area"
    
  } else if (grepl("_pct", val)) {
    description <- 
      "The percentage of SFP and WTD in the total travel time"
    
  } else if (grepl("_sfp", val)) {
    description <- 
      "The mean time consumed in searching for parking in the destination postal code area"
    
  } else if (grepl("_wtd", val)) {
    description <- 
      "The mean duration to walk from one's parked car to the destination, in the destination postal code area"
  }
  description <- 
    strwrap(description, 22, prefix = "\n") %>%
    paste(., collapse = "")
  
  if (grepl("_m_", val)) {
    timeofday <- paste("during midday traffic", thisUnit)
    
  } else if (grepl("_r_", val)) {
    timeofday <- paste("during rush hour traffic", thisUnit)
    
  } else if (grepl("_sl_", val)) {
    timeofday <- paste(
      "the route following speed limits without any additional impedances", 
      thisUnit)
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