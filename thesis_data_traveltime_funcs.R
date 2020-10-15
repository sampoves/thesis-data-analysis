
# Sampo Vesanen's Master's thesis
# Helsinki Region Travel Time comparison application functions

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 15.10.2020



# infix operator for sequential validation of input$ykrid
`%then%` <- shiny:::`%OR%`

# These custom infix operators work in the manner of += and ++ in C++/C# and
# Java. Save some space and increase readability.
`%+=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 + e2))
`%-=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 - e2))



ReadAndClean <- function(fp) {
  
  # Use this function to read a html file into R and clean it of tabulator 
  # spaces and HTML comments.
  
  result <- 
    paste(readLines(fp, encoding = "UTF-8"), collapse = "") %>%
    gsub("[\t]", "", .) %>%
    gsub(" <!--(.*?)-->", "", .)
  
  return(result)
}



CreateEqualColumn <- function(inputDf, datacol, newcolname, classes_n = 11, 
                              lockedclasses) {
  
  # This function is a variant of CreateJenksColumn() in thesis_data_vis.R.
  #
  # Use this function to create a column in fortified dataframe that can be
  # used to portray equal intervals colouring in a ggplot map. Dplyr note: to
  # enable parameters as column names in dplyr, apply !! and := for the left
  # side and for the right side !!rlang::sym().
  #
  # Provide possibility to select how classes breaks are created: create class
  # breaks from a column in parameter dataframe "inputDf", or from an external
  # vector of type double.
  #
  # Adapted from:
  # https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
  
  if(missing(lockedclasses)) {
    # Normal function behaviour
    
    # Suppress n classes warnings, problem probably handled
    classes <- suppressWarnings(
      classInt::classIntervals(inputDf[, datacol], n = classes_n, style = "equal"))
    
  } else {
    # This condition is met when the parameter "lockedclasses" is detected. This 
    # condition works with the values outputted by locked_class_breaks_params() 
    # and locked_class_breaks_all().
    classes <- suppressWarnings(
      classInt::classIntervals(lockedclasses, n = classes_n, style = "equal"))
  }
  
  # Make note that breaks are rounded to two decimal places. In some cases the 
  # rounding would create non-unique breaks and this breaks the map view for the 
  # active symbology. If two decimals is not enough, use three.
  result <- 
    inputDf %>%
    dplyr::mutate(!!newcolname := cut(!!rlang::sym(datacol), 
                                      unique(round(classes$brks, 5)), 
                                      include.lowest = TRUE))
  return(result)
}



AddLevelCounts <- function(thisDf, datacol, newcolname, classes_n,
                           labels_to_input, lockedclasses) {
  
  # Use this function to calculate how many times a legend key color appears
  # in the comparison app and add those values to the ggplot legend labels.
  # This function is non-optimal as half of this functionality is carried out
  # in CreateEqualColumn(), but for the sake of clarity keep them apart. For
  # example, interval calculation with classInt could be moved to its own
  # function, as it is identical with the one in CreateEqualColumn()
  #
  # We use an if statement to detect the state of locked classes breaks.
  
  if(missing(lockedclasses)) {
    # Normal function behaviour
      
    intervals <- suppressWarnings(
      classInt::classIntervals(thisDf[, datacol], n = classes_n, style = "equal"))
    
  } else {
    # This condition is met when the parameter "lockedclasses" is detected. This 
    # condition works with the values outputted by locked_class_breaks_params() 
    # and locked_class_breaks_all().
    intervals <- suppressWarnings(
      classInt::classIntervals(lockedclasses, n = classes_n, style = "equal"))
  }
  
  # Breaks are rounded to two decimal places
  interv_codes <- cut(thisDf[, datacol],
                      round(unique(intervals$brks), 5),
                      include.lowest = TRUE)
  input_levels <- levels(thisDf[, newcolname])
  
  # Create level occurrence count in this clunky way.
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



GetLegendName <- function(val, originzip) {
  
  # Appropriately name the plot legend. Use parts of the input string to
  # figure out what to print. Use strwrap() to automatically add newlines
  # to long strings. Use originzip to add information about origin ykr_id.
  
  wherefrom <- paste0("origin ", originzip[, "zipcode"][1])
  
  if (grepl("ttm18_", val)) {
    datasource <- "TTM18 data"
    
  } else if (grepl("msc_", val)) {
    datasource <- "Thesis data"
    
  } else if (grepl("compare_", val)) {
    datasource <- "Comparison of the data sources (thesis data / TTM18 data)"
  }
  # Automatically add newlines to the long datasource string
  datasource <- 
    strwrap(datasource, 22, prefix = "\n") %>%
    paste(., collapse = "")
  
  # thisUnit is minutes, except when datasource is "compare" or 
  # description "_pct"
  if (grepl("compare_", val) || grepl("_pct", val)) {
    thisUnit <- "(unit percent, where 1.00 = 100 %)"
  } else {
    thisUnit <- "(unit minutes)"
  }
  
  if (grepl("_avg", val)) {
    description <- 
      "The mean total travel time to the destination postal code area"
    
  } else if (grepl("_drivetime", val)) {
    description <- 
      "The duration of the driving segment in the total travel chain, to the destination postal code area"
    
  } else if (grepl("_pct", val)) {
    description <- 
      "The percentage of SFP and WTD durations in the mean total travel time to the destination postal code area"
    
  } else if (grepl("_sfp", val)) {
    description <- 
      "The mean time consumed in searching for parking in the destination postal code area"
    
  } else if (grepl("_wtd", val)) {
    description <- 
      "The mean duration to walk from one's parked car to the final destination of the travel chain in the destination postal code area"
  }
  description <- 
    strwrap(description, 22, prefix = "\n") %>%
    paste(., collapse = "")
  
  if (grepl("_m_", val)) {
    timeofday <- paste("during midday traffic", thisUnit)
    
  } else if (grepl("_r_", val)) {
    timeofday <- paste("during rush hour traffic", thisUnit)
    
  } else if (grepl("_all_", val)) {
    timeofday <- paste(
      "using an average of all temporal choices", 
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



GetSymbologyHelp <- function(val) {
  
  # Get helpful text for the meaning of the cryptically named columns. 
  # Functionally close to GetLegendName().
  
  if (grepl("ttm18_", val)) {
    datasource <- "<i class='icon database'></i>TTM18 data"
    
  } else if (grepl("msc_", val)) {
    datasource <- "<i class='icon poll'></i>Thesis data"
    
  } else if (grepl("compare_", val)) {
    datasource <- "<i class='icon exchange-alt'></i>Comparison of the data sources (thesis data / TTM18 data)"
  }
  
  # thisUnit is minutes, except when datasource is "compare" or 
  # description "_pct"
  if (grepl("compare_", val) || grepl("_pct", val)) {
    thisUnit <- "(unit percent, where 1 = 100 %)"
  } else {
    thisUnit <- "(unit minutes)"
  }
  
  # Describe the data. The p tag which defines a vertical bar in the final 
  # product needs to be defined here. The tag opening is pasted together with 
  # the tag closure other outputs below.
  if (grepl("_avg", val)) {
    description <- 
      "The mean total travel time to the destination postal code area"
    ptag <- "<p class='line lred'>"
    
  } else if (grepl("_drivetime", val)) {
    description <- 
      "The duration of the driving segment in the total travel chain to the destination postal code area"
    ptag <- "<p class='line lcyan'>"
    
  } else if (grepl("_pct", val)) {
    description <- 
      "The percentage of <i>searching for parking</i> and <i>walking to the destination</i> durations in the mean total travel time"
    ptag <- "<p class='line lcyan'>"
    
  } else if (grepl("_sfp", val)) {
    description <- 
      "The mean time consumed in <i>searching for parking</i> in the destination postal code area"
    ptag <- "<p class='line lyellow'>"
    
  } else if (grepl("_wtd", val)) {
    description <- 
      "The mean duration to <i>walk from one's parked car to the final destination</i> of the travel chain in the destination postal code area"
    ptag <- "<p class='line lyellow'>"
  }
  
  # Time of day
  if (grepl("_m_", val)) {
    timeofday <- paste("during midday traffic", thisUnit)
    
  } else if (grepl("_r_", val)) {
    timeofday <- paste("during rush hour traffic", thisUnit)
    
  } else if (grepl("_all_", val)) {
    timeofday <- paste(
      "using an average of all temporal choices", 
      thisUnit)
  }
  
  result <- paste(ptag, datasource, ":<br>", description, ",<br>", timeofday, 
                  "</p>", sep = "")
  
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



str_start <- function (string, n) {
  
  # Get an amount of n characters from the beginning of a string.
  
  substr(string, 1, n)
}

str_end <- function (string, n) {
  
  # Get an amount of n characters from the end of a string.
  
  substr(string, nchar(string) - (n - 1), nchar(string))
}

columnFinder <- function(columnname, compare_vector) {
  
  # Insert a column name that exists in an output of the reactive object of 
  # thisTTM() and receive all three columns of the same type. To be used with 
  # locked class breaks option "params". In this script, "compare_vector" is 
  # names(vis_cols).
  
  bool <- 
    grepl(str_start(columnname, 3), compare_vector) & 
    grepl(str_end(columnname, 3), compare_vector)
  result <- compare_vector[bool]
  
  return(result)
}



Reactive_TTM_fetch <- function(current_fst, thesisdata, postal_f) {
  
  # Use this function to calculate all necessary columns for the comparison
  # application to work. "current_fst" represents one fst file on disk,
  # "thesisdata" is the thesis survey data, and postal_f is a fortified version
  # of postal code area polygon data (this dataframe sets the output function
  # nrow of 14316)
  #
  # In the comparison app this function is used
  # 1)  to get all possible column values to calculate minimums and maximums for
  #     all columns (locked class breaks)
  # 2)  Any time user requests a new origin postal code area, this function is
  #     run
  
  result <-
    fst::read_fst(current_fst, as.data.table = TRUE) %>%
    
    dplyr::left_join(., thesisdata, by = "zipcode") %>%
    dplyr::left_join(postal_f, ., by = "zipcode") %>%
    
    # Generate drivetime (min) and pct (%) columns for TTM18 data.
    # NB! Use a mean of r, m, and sl for all the columns "sl".
    dplyr::mutate(ttm_all_avg = rowMeans(select(., c(ttm_r_avg, ttm_m_avg, ttm_sl_avg))),
                  ttm_r_drivetime = ttm_r_avg - ttm_sfp - ttm_wtd,
                  ttm_m_drivetime = ttm_m_avg - ttm_sfp - ttm_wtd,
                  ttm_all_drivetime = ttm_all_avg - ttm_sfp - ttm_wtd,
                  ttm_r_pct = (ttm_sfp + ttm_wtd) / ttm_r_avg,
                  ttm_m_pct = (ttm_sfp + ttm_wtd) / ttm_m_avg,
                  ttm_all_pct = (ttm_sfp + ttm_wtd) / ttm_all_avg) %>%
    dplyr::mutate_at(vars(ttm_all_avg, ttm_all_drivetime, ttm_r_pct, ttm_m_pct, ttm_all_pct),
                     ~round(., 2)) %>%
    
    # If zipcode is NA, then convert all calculated data to NA as well
    dplyr::mutate(ttm_r_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_avg),
                  ttm_m_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_avg),
                  ttm_sl_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sl_avg),
                  ttm_all_avg = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_all_avg),
                  ttm_sfp = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_sfp),
                  ttm_wtd = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_wtd),
                  ttm_r_drivetime = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_drivetime),
                  ttm_m_drivetime = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_drivetime),
                  ttm_all_drivetime = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_all_drivetime),
                  ttm_r_pct = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_r_pct),
                  ttm_m_pct = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_m_pct),
                  ttm_all_pct = case_when(is.na(zipcode) ~ NA_real_, TRUE ~ ttm_all_pct)) %>%
    dplyr::mutate_at(vars(ttm_r_avg, ttm_m_avg, ttm_all_avg),
                     ~dplyr::na_if(., -1)) %>%
    
    # Add the rest of thesis_ columns. with if_else() change possible NA's to
    # zeros so that calculations are not rendered NA
    dplyr::mutate(thesis_r_drivetime = ttm_r_drivetime -
                    if_else(is.na(thesis_r_sfp), 0, thesis_r_sfp) -
                    if_else(is.na(thesis_r_wtd), 0, thesis_r_wtd),
                  
                  thesis_m_drivetime = ttm_m_drivetime -
                    if_else(is.na(thesis_m_sfp), 0, thesis_m_sfp) -
                    if_else(is.na(thesis_m_wtd), 0, thesis_m_wtd),
                  
                  thesis_all_drivetime = ttm_all_drivetime -
                    if_else(is.na(thesis_all_sfp), 0, thesis_all_sfp) -
                    if_else(is.na(thesis_all_wtd), 0, thesis_all_wtd),
                  
                  # Forgo if_else() here so that if thesis results sfp or wtd
                  # are missing for a postal code, pct value also gets NA
                  thesis_r_pct = (thesis_r_sfp + thesis_r_wtd) / ttm_r_drivetime,
                  thesis_m_pct = (thesis_m_sfp + thesis_m_wtd) / ttm_m_drivetime,
                  
                  thesis_all_pct = (
                    if_else(is.na(thesis_all_sfp), 0, thesis_all_sfp) +
                      if_else(is.na(thesis_all_wtd), 0, thesis_all_wtd)) / ttm_all_drivetime) %>%
    
    dplyr::mutate_at(vars(thesis_r_drivetime, thesis_m_drivetime,
                          thesis_all_drivetime, thesis_r_pct, thesis_m_pct,
                          thesis_all_pct),
                     ~round(., 2)) %>%
    
    # Add TTM18/thesis comparison columns
    dplyr::mutate(comp_r_sfp = thesis_r_sfp / ttm_sfp,
                  comp_m_sfp = thesis_m_sfp / ttm_sfp,
                  comp_all_sfp = thesis_all_sfp / ttm_sfp,
                  comp_r_wtd = thesis_r_wtd / ttm_wtd,
                  comp_m_wtd = thesis_m_wtd / ttm_wtd,
                  comp_all_wtd = thesis_all_wtd / ttm_wtd,
                  comp_r_drivetime = thesis_r_drivetime / ttm_r_drivetime,
                  comp_m_drivetime = thesis_m_drivetime / ttm_m_drivetime,
                  comp_all_drivetime = thesis_all_drivetime / ttm_all_drivetime,
                  comp_r_pct = thesis_r_pct / ttm_r_pct,
                  comp_m_pct = thesis_m_pct / ttm_m_pct,
                  comp_all_pct = thesis_all_pct / ttm_all_pct) %>%
    dplyr::mutate_at(vars(comp_r_sfp, comp_m_sfp, comp_all_sfp, comp_r_wtd,
                          comp_m_wtd, comp_all_wtd, comp_r_drivetime,
                          comp_m_drivetime, comp_all_drivetime, comp_r_pct,
                          comp_m_pct, comp_all_pct),
                     ~round(., 2))
  return(result)
}