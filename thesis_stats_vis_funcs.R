
# Sampo Vesanen's Master's thesis statistical tests and visualisation
# Stats and visualisation functions and essential variables

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 13.10.2020

# Initialise
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(rlang)
library(classInt)
library(ggplot2)
library(RColorBrewer)




# These custom infix operators work in the manner of += and ++ in C++/C# and
# Java. Save some space and increase readability.
`%+=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 + e2))
`%-=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 - e2))



CalcBoxplotTooltip <- function(inputdata, resp_col, expl_col) {
  
  # This function calculates IQR values for the use of ggiraph's 
  # geom_boxplot_interactive().
  
  # Calculate min, IQR and max for boxplot tooltip. Creates new columns for
  # each.
  resp_data <- inputdata[, resp_col]
  expl_data <- inputdata[, expl_col]
  
  # Interquartile range (IQR)
  q1 <- tapply(resp_data, expl_data, quantile, probs = 0.25)
  q1 <- as.numeric(q1[match(expl_data, names(q1))])
  inputdata$tooltip_q1 <- q1
  
  mdn <- sapply(split(resp_data, expl_data), median)
  mdn <- as.numeric(mdn[match(inputdata[, expl_col], names(mdn))])
  inputdata$tooltip_mdn <- mdn
  
  q3 <- tapply(resp_data, expl_data, quantile, probs = 0.75)
  q3 <- as.numeric(q3[match(expl_data, names(q3))])
  inputdata$tooltip_q3 <- q3
  
  # Whiskers: Max and min. In ggplot, the max and min are only as large/small as 
  # the corresponding value in resp_col. Therefore, parktime whisker max may be 
  # in actuality 15.5, but that value could not have been inputted to the survey. 
  # Closest user inputted value is 15, so ggplot whisker reaches only that.
  # First calculate max and min as they one would normally do. Then, use dplyr
  # to get the max values used by ggplot for each group.
  # - Maximum is the name for unaltered maximum. tooltip_max will be inputted to
  # boxplot tooltip. 
  # - NB! The same treatment is not given to minimum! This is an oversight, but 
  # the minimum seems to hang around zero 99,99 % of the time.
  inputdata$maximum <- inputdata$tooltip_q3 + 1.5 * (inputdata$tooltip_q3 - inputdata$tooltip_q1)
  inputdata$tooltip_min <- inputdata$tooltip_q1 - 1.5 * (inputdata$tooltip_q3 - inputdata$tooltip_q1)
  inputdata$tooltip_min[inputdata$tooltip_min < 0] <- 0
  
  ggplot_max <- inputdata %>%
    dplyr::select(!!rlang::sym(expl_col), !!rlang::sym(resp_col), maximum) %>%
    dplyr::group_by(!!rlang::sym(expl_col)) %>%
    dplyr::filter(!!rlang::sym(resp_col) <= maximum) %>%
    dplyr::select(-maximum) %>%
    dplyr::summarise_all(max) %>%
    as.data.frame()

  # Transform dataframe of two columns to named vector. Named vector is then
  # matched with all the values in explanatory column.
  named_vec <- ggplot_max[, resp_col]
  names(named_vec) <- ggplot_max[, expl_col]
  named_vec <- as.numeric(named_vec[match(expl_data, names(named_vec))])
  
  inputdata$tooltip_max <- named_vec

  return(inputdata)
}



LabelBuilder <- function(plot_obj, expl, checkGroup, subdivGroup) {
  
  # Download helper function.
  
  if(length(checkGroup) == 0 & length(subdivGroup) == 0) {
    
    # Return inputted ggplot object if there are no values in checkGroup or
    # subdivGroup
    result_plot <- plot_obj
    
  } else {
    # Add conditional disclaimer about excluded groups and/or subdivisions.
    # Make use of Every8th() to divide long vectors into many rows
    if(length(checkGroup) > 0) {
      checklab <- paste("- Groups excluded from the explanatory variable ", expl, ":\n", 
                          Every8th(c(checkGroup)), sep = "")
    }
    if(length(subdivGroup) > 0) {
      subdivlab <- paste("- Subdivisions excluded:\n", 
                         Every8th(c(subdivGroup)), sep = "")
    } 
  
    # Build caption label
    if (!exists("checklab") & exists("subdivlab")) {
      full_lab <- subdivlab
    } else if (exists("checklab") & !exists("subdivlab")) {
      full_lab <- checklab
    } else {
      full_lab <- paste0(checklab, "\n", subdivlab)
    }
    
    # Make additions to ggplot object
    result_plot <- plot_obj + 
      labs(caption = full_lab) +
      theme(plot.caption = element_text(size = 15, hjust = 0, face = "italic"),
            plot.caption.position =  "plot")
  }
  
  return(result_plot)
}



Every8th <- function(input) {
  
  # Download helper function.
  
  # This function splits input$checkGroup and input$subdivGroup into bits of
  # eight separated by a newline. For the use with downloadable versions of
  # plots
  
  # Prevent situation where an empty input is fed to split()
  if(length(input) < 1) {
    return("")
    
  } else {
    result <- split(input, ceiling(seq_along(input) / 8))
    result <- sapply(result, function(x) paste0(x, collapse = ", "))
    result <- capture.output(cat(paste(result, collapse = "\n")))
    result <- paste0(result, collapse = "\n")
  }
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



InterpolateGgplotColors <- function(plot_obj, active_items, palette_max_cols, 
                                    palettename) {
  
  # Use RColorBrewer for the color scale in ggplot. If there are more active 
  # items to be mapped than the maximum color amount in selected RColorBrewer 
  # palette, interpolate the extra colors.
  if (length(active_items) > palette_max_cols) {
    cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(palette_max_cols, palettename))
    myPal <- cols(length(active_items))
    result <- plot_obj + scale_fill_manual(values = myPal)
  
  # Selected RColorBrewer palette works without any tricks
  } else {
    result <- plot_obj + scale_fill_brewer(palette = palettename)
  }
  
  return(result)
}



CreateJenksColumn <- function(fortified, postal, datacol, newcolname, classes_n = 5) {
  
  # Use this function to create a column in fortified dataframe that can be
  # used to portray Jenks breaks colouring in a ggplot map. Dplyr note: to
  # enable parameters as column names in dplyr, apply !! and := for the left
  # side and for the right side !!rlang::sym().
  #
  # Adapted from:
  # https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
  
  # Suppress n jenks warnings, problem probably handled
  classes <- suppressWarnings(
    classInt::classIntervals(postal[, datacol], n = classes_n, style = "jenks"))
  
  # When sample size is reduced drastically, median columns tended to receive
  # class intervals starting in the negative. Not possible in data, so fix it.
  if(classes$brks[1] < 0) {
    classes$brks[1] <- 0
  }
  
  # classes$brk has to be wrapped with unique(), otherwise we can't get more
  # than six classes for parktime_median or walktime_median
  result <- fortified %>%
    dplyr::mutate(!!newcolname := cut(!!rlang::sym(datacol), 
                                      unique(classes$brks), 
                                      include.lowest = T))
  
  # Reverse column values to enable rising values from bottom to top in ggplot.
  # In ggplot, use scale_fill_brewer(direction = -1) with this operation to flip
  # the legend.
  result[, newcolname] = factor(result[, newcolname], 
                                levels = rev(levels(result[, newcolname])))
  
  return(result)
}



SigTableToShiny <- function(sigTable, hasHeading) {
  
  # Use this function to show significance tables in Shiny. It will be useful
  # with Levene and ANOVA results. 
  
  # Due to the format of the significance table it is difficult to present it
  # in Shiny. The main functionality of this method is to make the significance
  # star available in the app.
  
  # Levene test dataframe requires transposing. Levene table has an attribute 
  # heading while ANOVA doesn't. Use this.
  if (is.null(attributes(sigTable)$heading)) {
    # ANOVA
    res <- as.data.frame(do.call(rbind, sigTable))
  } else {
    # Levene
    res <- t(as.data.frame(do.call(rbind, sigTable)))
  }
  
  # Take into account that the table may have an attribute heading. Ask if this 
  # is the case
  if (hasHeading == FALSE) {
    sigTablePosition <- 2
  } else {
    sigTablePosition <- 3
  }
  
  # Get the location of the signif.star
  signif_ncol <- ncol(read.table(
    textConnection(capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE,
    stringsAsFactors = TRUE))
  
  # get signif.star
  signif_star <- read.table(
    textConnection(capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE,
    stringsAsFactors = TRUE)[[signif_ncol]]

  # Detect if signif_star is something else than factor. If so, the function
  # has picked up a value from probability column and the current analysis is 
  # not significant. Change value to " ".
  if(!is.factor(signif_star)) {
    signif_star <- " "
  }
  
  # repeated_na takes into account that the significance table may have more
  # rows than two.
  repeated_na <- rep("NA", nrow(res) - 1)
  signif_star <- c(as.character(signif_star), repeated_na)
  
  # Bind column signif_star to result.
  res <- cbind.data.frame(res, signif_star)
  
  # Name rows. Try to detect differences in Levene and ANOVA summary tables.
  if(is.null(rownames(sigTable[[1]]))) {
    # Levene
    rownames(res) <- rownames(sigTable)
  } else {
    # ANOVA
    rownames(res) <- rownames(sigTable[[1]])
  }
  
  return(res)
}



#### Dygraph event timestamps #### 
# These are manually collected from my email correspondence and Facebook history. 
# If many areas are in the same variable, the earliest timestamp is selected

# Twitter: @Digigeolab, @AccessibilityRG
twitter <- as.POSIXct("2019-05-07 10:43:00 EEST") 

# Maantieteen opiskelijat student organisation email list
mao <- as.POSIXct("2019-05-09 13:24:00 EEST") 

# Student email list: Vasara, Resonanssi, Matrix, Geysir, Synop, Meridiaani, 
# Tyyppi-arvo, HYK, TKO-ÄLY, Symbioosi, Helix, MYY, Sampsa, MYO, Lipidi,
# Vuorovaikeutus, YFK, Oikos
emails <- as.POSIXct("2019-05-14 10:58:00 EEST") 

# Lisää kaupunkia Helsinkiin group and own Facebook wall. Also six private
# WhatsApp groups
fb <- as.POSIXct("2019-05-15 10:33:00 EEST") 

# Facebook neighborhood group advertisement begins:
# Haukilahti/Westend asukkaat, Pohjois-Espoon asukasfoorumi, Lippajärvi, 
# Matinkylä/Olari, Leppävaara, Soukka-Sökö, Suur-Espoonlahti, Puskaradio Tapiola,
# Puskaradio Kauniainen/Grankulla, Enemmän Tapiolaa!, Kivenlahden ystävät
espoo1 <- as.POSIXct("2019-05-22 18:00:00 EEST") 

# Kalasatama-Fiskehamnen, Sörnäinen, Punavuori, Laajasalo, Herttoniemi, 
# Mankkaan lapsiperheet, 
misc1 <- as.POSIXct("2019-05-23 21:11:00 EEST") 

# Oulunkylä kierrättää ja keskustelee, Käpylä Helsinki, Kumpula, Arabian alue,
# Vallilan ja Hermannin alue, Hermanni-liike, Jätkäsaari-liike, Ruoholahti asuu,
# Pasila Böle, Pasila-liike, Ruskeasuo, Meilahden kylä, Töölö-liike, Töölö-
# Seura ry
helsinki1 <- as.POSIXct("2019-05-24 16:28:00 EEST") 

# Pihlajamäki, Malmi, Pukinmäen foorumi, Viikki ja Latokartano ympäristöineen,
# Kuninkaantammi, Maununneva-Hakuninmaa, Kannelmäki-liike, Lauttasaari, 
# Munkkivuori, Niemenmäki, Etelä-Haaga, Haagan ilmoitustaulu
helsinki2 <- as.POSIXct("2019-05-25 14:56:00 EEST") 

# Östersundom, Aurinkolahti, Vuosaari, Kruunuvuorenranta, Kontula, Mellunmäki/
# Mellunkylä, Pitäjänmäki, Munkkiniemi, ITÄ-HELSINKI, Itäkeskus-itästadilaista
# laiffii, Marjaniemi Helsinki, Puotila & Vartsika, Vartiokylä/Vartioharju,
# Tammisalo, Herttoniemenranta, Roihuvuori, Kulosaari-HYGGE-Brandö, Suutarilassa
# tapahtuu, Tapaninvainion foorumi, Tapanila-Mosabacka, Paloheinä-Pakila-
# Torpparinmäki ilmoittaa (se aito ja alkuperäinen)
helsinki3 <- as.POSIXct("2019-05-26 12:55:00 EEST") 

# Pitäjänmäkeläiset, Landbo, Kamppi-Punavuori-Hietalahti
helsinki4 <- as.POSIXct("2019-05-29 16:49:00 EEST") 

# Perusmäki Espoo, As Oy Helsingin Arabianrinne, Rajakylä Vantaa, Leppäkorpi
# Vantaa, Korso, Rekola, Tikkurila, Aviapolis-asukkaiden alue, Kivistö,
# Kivistön suuralue, ASKISTO, Vantaanlaakson lapsiperheet, Vapaala, Pähkinärinne,
# Alppikylä, Laajalahti-ryhmä (suljettu), Kilo Espoo, Karakallio, Saunalahti
# tapahtumat ja palvelut, Nöykkiö Espoo Foorumi, Henttaalaiset, Myyrmäki
misc2 <- as.POSIXct("2019-06-06 20:57:00 EEST") 

# Pohjois-Kirkkonummelaiset, Puskaradio Sipoo, Sibbo-Sipoo, Järvenpää, We <3
# Kerava, Lisää kaupunkia Hyrylään, Tuusula, Nurmijärvi, Nurmijärven
# viidakkorumpu, Vihtiläiset, Vihdin Nummela, Kirkkonummelaiset (sana vapaa)
peri <- as.POSIXct("2019-06-09 11:01:00 EEST") 

# Reminders to the largest Facebook groups: Vantaa Puskaradio, Sipoo-Sibbo, 
# Järvenpää, WE <3 KERAVA, Tuusula, Nurmijärven  viidakkorumpu, Vihtiläiset, 
# Korso, Käpylä Helsinki, Laajasalo, Vuosaari, ITÄ-HELSINKI, Lauttasaari, 
# Haagan ilmoitustaulu
reminder <- as.POSIXct("2019-06-26 15:33:00 EEST") 

# They only accepted to display my message in Nikinmäki and Puskaradio Espoo at
# this late date
nikinmaki <- as.POSIXct("2019-06-27 14:26:00 EEST")
puskaradioespoo <- as.POSIXct("2019-06-27 21:30:00 EEST")

# A reminder to Lisää kaupunkia Helsinkiin
lisaakaupunkia2 <- as.POSIXct("2019-07-02 12:06:00 EEST")

# A reminder for email lists, all except MaO. A new ad to GIS-velhot FB group
misc3 <- as.POSIXct("2019-07-05 10:29:00 EEST")