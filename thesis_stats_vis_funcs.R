
# Sampo Vesanen's Master's thesis statistical tests and visualisation
# Stats and visualisation functions and essential variables

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 11.5.2020

# Initialise
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(rlang)
library(classInt)
library(ggplot2)
library(RColorBrewer)



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
                                dplyr::pull() %>%
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


GetANOVA <- function(thisFormula, response, explanatory, inputdata, 
                     columnsToRemove) {
  
  # NB! Make note that this function is superseded by the ShinyApp described in
  # thesis_stats_vis.R.
  #
  # Get all parts of ANOVA we need in this thesis:
  #
  # Descriptives
  # Levene test
  # One-way ANOVA
  # Brown-Forsythe test
  #
  # Input:  thisFormula:  formula as formula object
  #         response:     column name as string (continuous variable)
  #         explanatory:  column name as string (ordinal variable)
  #         inputdata:    dataframe, inputdata in this thesis
  #         columnsToRemove: This determines which column indices are to be
  #                       removed from analyses

  
  #### Descriptive statistics ####
  
  # In SPSS, follow these steps to get equal results
  # SPSS -> Analyze -> Compare means --> One-way anova 
  # In "Options" select: Descriptive, Homogeinity of variance test (Levene), 
  # Brown-Forsythe
  
  # One-way anova descriptives, describe()
  # Order of columns in SPSS: "N", "Mean", "Std. Deviation", "Std. Error",
  # "95 % Confidence Interval for Mean" ("Lower Bound", "Upper Bound"), "Minimum",
  # "Maximum"
  desc <- describe(thisFormula, inputdata[-columnsToRemove])
  
  ### Std. Error
  # clumsily calculate mean, so that we can preserve column names in the next
  # phase
  stder <- aggregate(
    thisFormula, 
    data = inputdata,
    FUN = function(x) c(mean = mean(x), "Std.Error" = plotrix::std.error(x)))
  
  # Remove column mean 
  stder <- subset(stder[[2]], select = -mean)
  desc <- cbind(desc, stder)
  
  # Confidence intervals for mean
  confs <- aggregate(
    thisFormula, data = inputdata, 
    FUN = function(x) c("CI for mean, Lower Bound" = mean(x) - 2 * plotrix::std.error(x), 
                        "CI for mean, Upper Bound" = mean(x) + 2 * plotrix::std.error(x)))
  confs <- confs[[2]]
  desc <- cbind(desc, confs)
  
  # Reorder to SPSS order
  desc <- desc[c("n", "Median", "Mean", "Std.Dev", "Std.Error", 
                 "CI for mean, Lower Bound", "CI for mean, Upper Bound", "Min", 
                 "Max", "25th", "75th", "Skewness", "Kurtosis", "NA")]
  
  # Add total row. We will add total values for all columns in this inconvenient
  # manner.
  vect <- c()
  vect[1] <- sapply(1, function(x) sum(desc[, x]))
  vect[2] <- sapply(2, function(x) median(desc[, x]))
  vect[3] <- sapply(3, function(x) mean(desc[, x]))
  vect[4] <- sd(response)
  vect[5] <- plotrix::std.error(response)
  vect[6] <- mean(response) - 2 * plotrix::std.error(response)
  vect[7] <- mean(response) + 2 * plotrix::std.error(response)
  vect[8] <- min(response)
  vect[9] <- max(response)
  vect[10] <- quantile(response)[2]
  vect[11] <- quantile(response)[4]
  vect[12] <- moments::skewness(response)
  vect[13] <- moments::kurtosis(response)
  vect[13] <- sapply(14, function(x) sum(is.na(desc[, x])))
  
  # Add all values vector to desc, then name the new row and round all values in
  # desc.
  desc <- rbind(desc, vect)
  row.names(desc)[nrow(desc)] <- "Total" #last row
  desc <- round(desc, 3)
  
  #### Levene test ####
  levene <- car::leveneTest(thisFormula, inputdata, center = mean)
  leveneVal <- levene[[3]][1]
  
  #### One-way ANOVA ####
  res.aov <- aov(thisFormula, data = inputdata)
  anovasummary <- summary(res.aov)
  
  #### Brown-Forsythe test #### 
  # Need to remove first four columns for this to work
  # not in use, we only need to run this in the printing section
  #brownf <- bf.test(thisFormula, data = inputdata[-columnsToRemove])
  
  

  #### Printing of results ####
  
  # Desc
  cat("\n--------------------------\n")
  cat("# Descriptive Statistics #")
  cat("\n--------------------------\n")
  print("If N is distributed somewhat equally, Levene test is not required.")
  cat("\n")
  print(desc)
  
  # Levene
  cat("\n\n------------------------------------\n")
  cat("# Test of Homogeneity of Variances #")
  cat("\n------------------------------------\n\n")
  
  if(leveneVal < 0.05){
    print(paste0("Levene test is under 0.05 (", 
                 round(leveneVal, 5), 
                 "), need to perform Brown-Forsythe"))
    cat("\n")
  }
  
  print(levene)
  
  # ANOVA
  cat("\n\n---------\n")
  cat("# ANOVA #")
  cat("\n---------\n\n")
  print(anovasummary)
  
  # Brown-Forsythe
  cat("\n\n-------------------------------------\n")
  cat("# Robust Tests of Equality of Means #")
  cat("\n-------------------------------------\n")
  onewaytests::bf.test(thisFormula, data = inputdata[-columnsToRemove])
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
  signif_ncol <- ncol(read.table(textConnection(
    capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE))
  
  # get signif.star
  signif_star <- read.table(textConnection(
    capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE)[[signif_ncol]]
  
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