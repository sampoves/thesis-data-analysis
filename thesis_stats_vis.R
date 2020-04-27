
# Sampo Vesanen's Master's thesis statistical tests and visualisation
#####################################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 27.4.2020
#
# This is an interactive tool for analysing the results of my research survey.

# Reference material for the tests
#https://stats.stackexchange.com/a/124618/262051
#https://rcompanion.org/handbook/E_01.html
#https://www.st-andrews.ac.uk/media/capod/students/mathssupport/OrdinalexampleR.pdf
#https://www.r-bloggers.com/box-plot-with-r-tutorial/

# Descriptive statistics
#https://www.fsd.uta.fi/menetelmaopetus/varianssi/harjoitus1.html
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r
#https://datascienceplus.com/oneway-anova-explanation-and-example-in-r-part-1/
#http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r

# About One-way ANOVA, Levene and BF
#https://en.wikipedia.org/wiki/One-way_analysis_of_variance
#https://www.statisticshowto.datasciencecentral.com/brown-forsythe-test/
#https://www.statisticshowto.datasciencecentral.com/levene-test/
#https://www.statisticshowto.datasciencecentral.com/homoscedasticity/



#### Initialise ----------------------------------------------------------------
rm(list = ls())
gc()

#install.packages("onewaytests")
#install.packages("car")
#install.packages("plotrix")
#install.packages("moments")
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("dygraphs")
#install.packages("xts")
#install.packages("rgdal")
#install.packages("RColorBrewer")
#install.packages("shinyjs")
#install.packages("ggiraph")
#install.packages("widgetframe")
#install.packages("rgeos")
#install.packages("classInt")
#install.packages("rlang")

# Libraries
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(htmltools)
library(rgdal)
library(RColorBrewer)
library(shinyjs)
library(ggiraph)
library(widgetframe)
library(rgeos)



# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Python prepared data directories
datapath <- file.path(wd, "records_for_r.csv")
visitorpath <- file.path(wd, "leaflet_survey_results/visitors.csv")
postal_path <- file.path(wd, "postal_for_r.csv")

# Spatial data paths
suuraluepath <- file.path(wd, "python/suuralueet/PKS_suuralue.kml")
munsclippedpath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")
munspath <- file.path(wd, "python/paavo/hcr_muns.shp")

# Source functions and postal code variables
source(file.path(wd, "python/thesis_stats_vis_funcs.R"))



#### Preparation --------------------------------------------------------------- 

# These variables are used to subset dataframe thesisdata inside ShinyApp
continuous <- c("parktime", "walktime")
ordinal <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", 
             "subdiv")
supportcols <- c("id", "timestamp", "ip")


# Read in csv data. Define column types. Name factor levels. Determine order of 
# factor levels for plotting. Lastly, remove column "X"
thesisdata <- 
  read.csv(file = datapath,
           colClasses = c(timestamp = "POSIXct", zipcode = "character", 
                          ip = "character", timeofday = "factor", 
                          parkspot = "factor", likert = "factor",
                          ua_forest = "factor", ykr_zone = "factor", 
                          subdiv = "factor"),
           header = TRUE, 
           sep = ",") %>%
  
  dplyr::mutate(parkspot = dplyr::recode(parkspot, 
                                         `1` = "On the side of street",
                                         `2` = "Parking lot",
                                         `3` = "Parking garage",
                                         `4` = "Private or reserved",
                                         `5` = "Other"),
                
                likert = dplyr::recode(likert, 
                                       `1` = "Extremely familiar",
                                       `2` = "Moderately familiar",
                                       `3` = "Somewhat familiar",
                                       `4` = "Slightly familiar",
                                       `5` = "Not at all familiar"),
                
                timeofday = dplyr::recode(timeofday, 
                                          `1` = "Weekday, rush hour",
                                          `2` = "Weekday, other than rush hour",
                                          `3` = "Weekend",
                                          `4` = "Can't specify, no usual time"),
                
                # SYKE does not provide official translations for 
                # "Yhdyskuntarakenteen vyöhykkeet".
                ykr_zone = dplyr::recode(ykr_zone, 
                                         `1` = "keskustan jalankulkuvyöhyke",
                                         `2` = "keskustan reunavyöhyke",
                                         `3` = "alakeskuksen jalankulkuvyöhyke",
                                         `4` = "intensiivinen joukkoliikennevyöhyke",
                                         `5` = "joukkoliikennevyöhyke",
                                         `6` = "autovyöhyke",
                                         `7` = "novalue"),
                
                ua_forest = dplyr::recode(ua_forest, 
                                       `1` = "Predominantly forest",
                                       `2` = "Mostly forest",
                                       `3` = "Moderate forest",
                                       `4` = "Some forest",
                                       `5` = "Scarce forest")) %>%
  dplyr::select(-X)



#### Context map for ShinyApp --------------------------------------------------

# Prepare a context map for to visualise currently active areas in analysis
# ShinyApp. left_join() preserves suuralue dataframe data. See ?"%>%" and "Using 
# the dot for secondary purposes" for more information about the curly brackets.
# In short, it allows the use of dot as self for both fortify() and 
# as.data.frame(). Then align area names with thesisdata$subdiv with mutate().
# Finally, factor levels by their new names.
suuralue_f <- 
  rgdal::readOGR(suuraluepath, use_iconv = TRUE, encoding = "UTF-8") %>%
  {dplyr::left_join(ggplot2::fortify(.), 
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  
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


# Get municipality borders. Fortify SP DataFrame for ggplot. Shapefile data is 
# Regional population density 2012, Statistics Finland.
# http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226. 
muns_clipped_f <- 
  rgdal::readOGR(munsclippedpath) %>%
  sp::spTransform(., sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  {dplyr::left_join(ggplot2::fortify(.), 
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} 

# Annotate municipalities in ggplot. Adapted from:
# https://stackoverflow.com/a/28963405/9455395
centroids <- setNames(
  do.call("rbind.data.frame", by(muns_clipped_f, muns_clipped_f$nimi, function(x) 
  {Polygon(x[c("long", "lat")])@labpt})), c("long", "lat")) 

centroids2 <- setNames(
  do.call("rbind.data.frame", by(suuralue_f, suuralue_f$Name, function(x) 
  {Polygon(x[c("long", "lat")])@labpt})), c("long", "lat"))

# Manually set better location for the annotation of Helsinki and Espoo
centroids[2, "lat"] <- centroids[2, "lat"] + 0.02
centroids[1, "lat"] <- centroids[1, "lat"] + 0.01
centroids$label <- c("Espoo", "Helsinki", "Kauniainen", "Vantaa")

# Set color gradients for municipalities. Kauniainen will be a single color set 
# below. These color gradients may be confusing. Investigate better colouring
# in the future
c_esp <- RColorBrewer::brewer.pal(7, "YlOrRd")
c_hel <- RColorBrewer::brewer.pal(8, "PuBu")
c_van <- RColorBrewer::brewer.pal(7, "BuGn")

# In the next phase, each color has to be repeated as many times as they show
# up in suuralue_f. Find out how many.
amounts <- unname(table(suuralue_f$Name))

# Assign hex codes to the color column using the order of subdivisions and 
# amount of rows per subdivision. First reorder dataframe by subdivision
suuralue_f <- suuralue_f[order(suuralue_f$Name), ]
suuralue_f$color <- c(rep(c_esp[1], amounts[1]), rep(c_esp[2], amounts[2]), 
                      rep(c_esp[3], amounts[3]), rep(c_esp[4], amounts[4]), 
                      rep(c_esp[5], amounts[5]), rep(c_esp[6], amounts[6]), 
                      rep(c_esp[7], amounts[7]), rep(c_hel[1], amounts[8]), 
                      rep(c_hel[2], amounts[9]), rep(c_hel[3], amounts[10]),
                      rep(c_hel[4], amounts[11]), rep(c_hel[5], amounts[12]),
                      rep(c_hel[6], amounts[13]), rep(c_hel[7], amounts[14]),
                      rep(c_hel[8], amounts[15]), rep("#98817B", amounts[16]),
                      rep(c_van[1], amounts[17]), rep(c_van[2], amounts[18]),
                      rep(c_van[3], amounts[19]), rep(c_van[4], amounts[20]),
                      rep(c_van[5], amounts[21]), rep(c_van[6], amounts[22]),
                      rep(c_van[7], amounts[23]))

# Colors to factors and reorder subdivision names to facilitate ggplot
suuralue_f <- 
  suuralue_f %>% 
  dplyr::mutate(color = as.factor(color)) # to factor

# Name labels here so that all the reordering doesn't mix up stuff. Remove
# munnames from subdiv annotations
centroids2$label <- gsub(".* ", "", unique(suuralue_f$Name)) 

# Manually move Espoonlahti and Southeastern to be visible. Remove subdiv label
# for Kauniainen
centroids2[2, "lat"] <- centroids2[2, "lat"] + 0.05
centroids2[2, "long"] <- centroids2[2, "long"] - 0.04
centroids2[12, "lat"] <- centroids2[12, "lat"] + 0.08
centroids2[12, "long"] <- centroids2[12, "long"] + 0.05
centroids2[16, "label"] <- ""



### Interactive map for ShinyApp ----------------------------------------------- 
# Created with the help of:
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html

# Get postal code area data calculated in Python. It contains some interesting
# variables for visualisation. Select essential columns and multiply column 
# "ua_forest" with 100 for easier to view plotting
postal <- 
  read.csv(file = postal_path,
           colClasses = c(zipcode = "factor", kunta = "factor"),
           header = TRUE, 
           sep = ",") %>%
  dplyr::select(c(2, 3, 6, 108:121)) %>%
  dplyr::mutate(ua_forest = ua_forest * 100)

# Create column which reports the largest ykr zone in each postal code area
largest_ykr <- colnames(postal[, 5:11])[apply(postal[, 5:11], 1, which.max)]
largest_ykr <- gsub("ykr_", "", largest_ykr)
largest_ykr_no <- as.numeric(apply(postal[, 5:11], 1, max)) * 100
postal <- cbind(postal, largest_ykr = paste(largest_ykr, largest_ykr_no))

# "postal" geometries are in well-known text format. Some processing is needed to
# utilise these polygons in R.
crs <- sp::CRS("+init=epsg:3067")
geometries <- lapply(postal[, "geometry"], "readWKT", p4s = crs) #rgeos::readWKT()
sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(postal[, 1]))
row.names(postal) <- postal[, 1]

data <- sp::SpatialPolygonsDataFrame(
  sp::SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)), 
                      proj4string = crs), data = postal)
data_f <- merge(ggplot2::fortify(data), as.data.frame(data), by.x = "id", by.y = 0)

# Get municipality borders from shapefile
munsf <- 
  rgdal::readOGR(munspath) %>%
  sp::spTransform(., crs) %>%
  {dplyr::left_join(ggplot2::fortify(.), 
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} 



#### Analysis ShinyApp ---------------------------------------------------------

# This ShinyApp is a versatile tool to study the thesis survey data. One can
# choose parameters with lots of freedom and exclude options as seen fit.

server <- function(input, output, session){
  
  #### Listener functions ------------------------------------------------------
  
  # Listen to clear subdivs button. Resetting uses library shinyjs -------------
  observeEvent(input$resetSubdivs, {
    reset("subdivGroup")
  })
  
  
  # Detect user setting for maximum parktime and walktime ----------------------
  
  # currentdata() is the currently active rows of the original thesisdata 
  # DataFrame. Use currentdata() in the rest of the application to not interfere
  # with the original dataset.
  currentdata <- reactive(
    thesisdata %>%
      dplyr::filter(parktime <= input$parktime_max,
                    walktime <= input$walktime_max))
  
  
  observe({
    # Detect changes in selectInput to modify available check boxes ------------
    x <- input$expl

    updateCheckboxGroupInput(
      session, 
      "checkGroup", 
      label = NULL, 
      choiceNames = levels(thesisdata[, x]),
      choiceValues = levels(thesisdata[, x]),)
    
    
    # Determine availability of barplot ----------------------------------------
    
    # aka availability of "Distribution of ordinal variables"
    available <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", 
                   "subdiv")
    updateSelectInput(
      session, 
      "barplot",
      label = NULL,
      choices = available[!available == x])
    
    
    # Do not allow selection of all checkboxes in Jenks ------------------------
    if(length(input$kunta) == 3) {
      threevalues <<- input$kunta
    }
    if(length(input$kunta) > 3) {
      
      updateCheckboxGroupInput(
        session, 
        "kunta", 
        selected = threevalues)
    }
    
    
    # A clumsy implementation to listen for too large jenks breaks -------------
    inputpostal <- postal[!postal$kunta %in% c(input$kunta), ]

    if(input$karttacol == "jenks_ua_forest") {
      datacol <- "ua_forest"
    } else if (input$karttacol == "jenks_walk_median") {
      datacol <- "walktime_median"
    } else if (input$karttacol == "jenks_park_median") {
      datacol <- "parktime_median"
    } else if (input$karttacol == "jenks_answer_count") {
      datacol <- "answer_count"
    } else if (input$karttacol == "jenks_walk_mean") {
      datacol <- "walktime_mean"
    } else if (input$karttacol == "jenks_park_mean") {
      datacol <- "parktime_mean"
    }
    
    # Only test classIntervals() and change SliderInput value if statement holds.
    # Suppress warnings in the test.
    if(nrow(inputpostal) > 1) {
      classes_test <- suppressWarnings(
        classInt::classIntervals(inputpostal[, datacol], n = input$jenks_n, style = "jenks"))
      
      # object returned from classIntervals() has an attribute nobs which I
      # use to detect cases where too large input$jenks_n is inputted to
      # CreateJenksColumn() function
      if(attributes(classes_test)$nobs < input$jenks_n) {
        updateSliderInput(session,
                          "jenks_n",
                          value = attributes(classes_test)$nobs)
      }
    }
  })
  
  

  #### Descriptive statistics --------------------------------------------------
  output$descri <- renderTable({
    
    # Vital variables
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    resp_col <- input$resp
    expl_col <- input$expl
    
    # In each output, define reactive variable as "inpudata". According to
    # this Stack Overflow answer, it prevents errors down the line
    # https://stackoverflow.com/a/53989498/9455395
    # And as a reminder, currentdata() is called to keep track of maximum
    # parktime and walktime values
    inputdata <- currentdata()
    response <- inputdata[[resp_col]]

    # Take subdiv checkbox group into account
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), 
                            !names(inputdata) %in% supportcols]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]

    # Basic descriptive statistics
    desc <- onewaytests::describe(thisFormula, inputdata)
    
    ### Std. Error
    # Clumsily calculate mean, so that we can preserve column names in the next
    # phase. Adapt code from: https://stackoverflow.com/a/41029914/9455395
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
    
    # Reorder to SPSS descriptive statistics order
    desc <- desc[c("n", "Median", "Mean", "Std.Dev", "Std.Error", 
                   "CI for mean, Lower Bound", "CI for mean, Upper Bound", "Min", 
                   "Max", "25th", "75th", "Skewness", "Kurtosis", "NA")]
    
    # Add a total row. We will add total values for all columns in this 
    # inconvenient manner.
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
    desc
  }, 
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  
  #### Histogram for parktime or walktime --------------------------------------
  output$hist <- renderPlot({
    
    resp_col <- input$resp
    expl_col <- input$expl
    binwidth <- input$bin
    
    inputdata <- currentdata()
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    p <- ggplot(inputdata, aes(x = !!sym(resp_col))) + 
      geom_histogram(color = "black", fill = "grey", binwidth = binwidth) +
      
      # Vertical lines for mean and median, respectively
      geom_vline(aes(xintercept = mean(!!sym(resp_col)),
                     color = "mean"),
                 linetype = "longdash", 
                 size = 1) +
      geom_vline(aes(xintercept = median(!!sym(resp_col)),
                     color = "median"),
                 linetype = "longdash", 
                 size = 1) +
      
      # This is kernel density estimate, a smoothed version of the histogram.
      # Usually geom_density() sets the scale for y axis, but here we will
      # continue using count/frequency. This requires some work on our behalf.
      # Idea from here: https://stackoverflow.com/a/27612438/9455395
      geom_density(aes(y = ..density.. * (nrow(inputdata) * binwidth),
                       color = "kernel density\nestimate"),
                   show.legend = FALSE,
                   adjust = binwidth) +
      
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            legend.spacing.y = unit(0.2, "cm"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
      
      # build legend, override colors to get visible color for density
      scale_color_manual(name = paste("Legend for\n", resp_col), 
                         values = c(median = "blue", 
                                    mean = "red", 
                                    "kernel density\nestimate" = alpha("black", 0.4))) +
      guides(color = guide_legend(
        override.aes = list(color = c("darkgrey", "red", "blue")))) +
      
      # Conditional histogram bar labeling. No label for zero
      stat_bin(binwidth = binwidth, 
               geom = "text", 
               aes(label = ifelse(..count.. > 0, ..count.., "")), 
               vjust = -0.65)
    p
  })

  
  #### Boxplot -----------------------------------------------------------------
  output$boxplot <- renderPlot({
    
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    expl_col <- input$expl
    
    # Listen to user choices
    inputdata <- currentdata()
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    legendnames <- levels(unique(inputdata[[expl_col]]))
    
    # ggplot2 plotting. Rotate labels if enough classes
    if(length(legendnames) > 5) {
      
      p <- ggplot(inputdata, aes_string(x = input$expl, y = input$resp)) + 
        geom_boxplot() + 
        theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
    } else {
      
      p <- ggplot(inputdata, aes_string(x = input$expl, y = input$resp)) + 
        geom_boxplot() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
    }
    p
  })

  
  #### Barplot -----------------------------------------------------------------
  output$barplot <- renderPlot({
    
    # See distribution of ordinal variables through a grouped bar plot
    expl_col <- input$expl
    barplotval <- input$barplot
    yax <- paste("sum of", barplotval)

    # Listen to user choices
    inputdata <- currentdata()
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # Plot maximum y tick value. Use dplyr to group the desired max amount.
    # In dplyr, use !!as.symbol(var) to signify that we are using variables
    # as column names
    maximum <- inputdata %>% 
      dplyr::group_by(!!as.symbol(expl_col), !!as.symbol(barplotval)) %>% 
      dplyr::summarise(amount = length(!!as.symbol(barplotval))) %>% 
      dplyr::top_n(n = 1) %>%
      dplyr::pull(amount) %>%
      max()
      
    if (maximum <= 200) {
      tick_interval <- 50
    } else {
      tick_interval <- 200
    }
    
    plo <- 
      ggplot(inputdata, aes(x = get(expl_col), 
                            y = factor(get(barplotval)), 
                            fill = get(barplotval))) +
      geom_bar(aes(y = stat(count)), position = "dodge") + 
      scale_y_continuous(breaks = seq(0, maximum, by = tick_interval),
                         expand = expansion(mult = c(0, .1))) +
      xlab(expl_col) +
      ylab(yax) +
      scale_fill_discrete(name = barplotval) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
    plo
  })
  
  
  #### Levene test -------------------------------------------------------------
  output$levene <- renderTable({
    
    expl_col <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", expl_col))
    
    inputdata <- currentdata()
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    levene <- car::leveneTest(thisFormula, inputdata, center = mean)

    res <- SigTableToShiny(levene, TRUE)
    res
  }, 
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)

  
  #### One-way ANOVA -----------------------------------------------------------
  output$anova <- renderTable({
    
    expl_col <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", expl_col))
    
    inputdata <- currentdata()
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    #### One-way ANOVA
    res.aov <- aov(thisFormula, data = inputdata)
    anovasummary <- summary(res.aov)
    
    # Use this function to communicate table correctly to Shiny
    anovasummary <- SigTableToShiny(anovasummary, FALSE)
    anovasummary
  },
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  ### Brown-Forsythe test ------------------------------------------------------
  output$brownf <- renderPrint({
    
    resp_col <- input$resp
    expl_col <- input$expl
    thisFormula <- as.formula(paste(resp_col, "~", expl_col))
    
    inputdata <- currentdata()
    inputdata <- inputdata[!inputdata[[expl_col]] %in% c(input$checkGroup), 
                            !names(inputdata) %in% supportcols]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(onewaytests::bf.test(thisFormula, data = inputdata), 
                               file = NULL, 
                               append = TRUE)
    cat(captured, sep = "\n")
  })
  
  ### Context map --------------------------------------------------------------
  output$map <- renderggiraph({
    
    # Count active subdivs
    active_subdivs <- length(unique(thesisdata$subdiv)) - length(input$subdivGroup)
    
    g2 <- ggplot() +
      # Background grey subdivs appear when inactive subdivs present
      geom_polygon(
        data = suuralue_f,
        aes(long, lat, group = group, fill = "#3d3d3d"),
        colour = NA) +
      
      # Subdivisions proper
      geom_polygon_interactive(
        data = suuralue_f[!suuralue_f$Name %in% c(input$subdivGroup), ], 
        size = 0.2,
        aes(long, lat, group = group, fill = color),
        colour = "grey") +
      
      # Municipality borders
      geom_polygon(
        data = muns_clipped_f,
        aes(long, lat, group = group),
        fill = NA,
        color = "black",
        size = 0.4) +
      coord_map(ylim = c(60.07, 60.42)) +
      
      # Legend contents
      scale_fill_identity(paste0("Currently active\nsubdivisions\n(", 
                                 active_subdivs, " out of 23)"), 
                          labels = suuralue_f$Name, 
                          breaks = suuralue_f$color, 
                          guide = "legend") +
      
      # Annotations. centroids2 is subdiv labels, centroids is municipality
      # labels.
      with(centroids, annotate(geom = "text", 
                               x = long, 
                               y = lat, 
                               label = label, 
                               size = 5,
                               fontface = 2)) +
      with(centroids2[!centroids2$label %in% gsub(".* ", "", c(input$subdivGroup)), ], 
           annotate(geom = "text", 
                    x = long, 
                    y = lat, 
                    label = label, 
                    size = 4)) +
      
      # Tight layout and legend properties
      theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            legend.position = "bottom")
    
    ggiraph(code = print(g2), 
            width_svg = 16, 
            height_svg = 14, 
            options = list(opts_sizing(rescale = FALSE)))
  })
  
  ### Interactive map ----------------------------------------------------------
  output$interactive <- renderggiraph({
    
    # only select municipalities selected by user. Do the same for "postal",
    # we fetch jenks breaks from there.
    inputdata <- data_f[!data_f$kunta %in% c(input$kunta), ]
    inputpostal <- postal[!postal$kunta %in% c(input$kunta), ]
    
    # Set interactive map extent by what's active on inputdata
    minlat <- plyr::round_any(min(inputdata$lat), 100, f = floor)
    maxlat <- plyr::round_any(max(inputdata$lat), 1000, f = ceiling)
    minlon <- plyr::round_any(min(inputdata$lon), 100, f = floor)
    maxlon <- plyr::round_any(max(inputdata$lon), 1000, f = ceiling)
    
    # Set properties for interactive map for each input$karttacol value
    if(input$karttacol == "jenks_ua_forest") {
      datacol <- "ua_forest"
      brewerpal <- "YlGn"
      legendname <- "Forest amount (%)"
      
    } else if (input$karttacol == "jenks_park_mean") {
      datacol <- "parktime_mean"
      brewerpal <- "BuPu"
      legendname <- "Parking time,\nmean (min)"
      
    } else if (input$karttacol == "jenks_walk_mean") {
      datacol <- "walktime_mean"
      brewerpal <- "Oranges"
      legendname <- "Parking time,\nmean (min)"
      
    } else if (input$karttacol == "jenks_park_median") {
      datacol <- "parktime_median"
      brewerpal <- "BuGn"
      legendname <- "Parking time,\nmedian (min)"
      
    } else if (input$karttacol == "jenks_walk_median") {
      datacol <- "walktime_median"
      brewerpal <- "OrRd"
      legendname <- "Walking time,\nmedian (min)"
      
    } else if (input$karttacol == "jenks_answer_count") {
      datacol <- "answer_count"
      brewerpal <- "Reds"
      legendname <- "Answer count"
    }
    
    # Create jenks breaks columns here so that user gets the control of Jenks
    # breaks classes
    inputdata <- CreateJenksColumn(inputdata, inputpostal, datacol, input$karttacol, input$jenks_n)
    
    # Format map labels. Remove [, ], (, and ). Also add list dash
    labels <- gsub("(])|(\\()|(\\[)", "", levels(inputdata[, input$karttacol]))
    labels <- gsub(",", " \U2012 ", labels)
    
    tooltip_content <- paste0(
      "<div>%s, %s<br/>",
      "Answer count: <b>%s</b></div>",
      "<div style='padding-top: 3px;'>Mean parktime: %s</br>",
      "Median parktime: %s</div>",
      "<div style='padding-top: 3px;'>Mean walktime: %s</br>",
      "Median walktime: %s</div>",
      "<div style='padding-top: 3px;'>Forest (%%): %s</br>",
      "Largest YKR (%%): %s</div>")
    
    g <- ggplot(inputdata) +
      geom_polygon_interactive(
        color = "black",
        size = 0.2,
        
        # aes_string() is to facilitate interactive map tooltip creation
        aes_string("long", "lat",
                   group = "group", 
                   fill = input$karttacol,
                   tooltip = substitute(sprintf(tooltip_content,
                      id, nimi, answer_count, parktime_mean, parktime_median, 
                      walktime_mean, walktime_median, ua_forest, largest_ykr)))) +
      
      # Jenks classes colouring and labels
      scale_fill_brewer(palette = brewerpal,
                        direction = -1,
                        name = legendname,
                        labels = labels) +
      
      # Municipality borders
      geom_polygon(data = munsf,
                   aes(long, lat, group = group),
                   linetype = "longdash",
                   color = alpha("black", 0.6), 
                   fill = "NA",
                   size = 0.4) +
      coord_fixed(xlim = c(minlon, maxlon),
                  ylim = c(minlat, maxlat)) +
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14))
    
    ggiraph(code = print(g), 
            width_svg = 16, 
            height_svg = 14, 
            options = list(opts_sizing(rescale = FALSE)))
  })
}

### ShinyApp UI elements ------------------------------------------------------- 
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("slate"),
  
  ### ShinyApp UI CSS ---------------------------------------------------------- 
  
  # Edit various CSS features of the ShinyApp: 
  # - the Brown-Forsythe test box 
  # - sidebarPanel (form.well) width. sidebarPanel width setting is important 
  #   because the long explanations would break it otherwise. 
  # - manually set sidebarPanel z-index to make the element always appear on top
  # - .checkbox input achieves strikeout on selected checkboxes
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        scroll-behavior: smooth;
      }
      #brownf {
        color: #c8c8c8;
        background: #2e3338;
        border: 1px solid #1c1e22;
        max-width: 1000px;
      }
      #descri {
        overflow-x: auto;
        max-height: 80vh;
        max-width: 1200px;
      }
      #boxplot, #barplot, #hist {
        max-width: 1200px;
      }
      #resetSubdivs {
        width: 100%;
        padding: 8px 0px 8px 0px;
      }
      #contents {
        border: 5px solid #2e3338;
        border-radius: 5px;
        padding: 4px;
        margin-bottom: 15px;
      }
      form.well {
        display: 100%;
        position: fixed;
        overflow: visible;
        overflow-y: auto;
        max-height: 90vh;
        max-width: 80vh;
        width: 250px;
        z-index: 50;
        scroll-behavior: smooth;
      }
      .girafe_container_std {
        text-align: left;
      }
      .checkbox input[type=checkbox]:checked + span{
        text-decoration: line-through;
        color: dimgray;
      }"
    ))
  ),
  
  ### Sidebar layout -----------------------------------------------------------
  titlePanel("Sampo Vesanen MSc thesis research survey results ShinyApp"),
  sidebarLayout(
    sidebarPanel(
      HTML("<div id='contents'>"),
      HTML("<a href='#descrilink'>1 Descriptives</a> &mdash;"),
      HTML("<a href='#histlink'>2 Histogram</a> &mdash;"),
      HTML("<a href='#barplotlink'>3 Barplot</a> &mdash;"),
      HTML("<a href='#boxplotlink'>4 Boxplot</a> &mdash;"),
      HTML("<a href='#levenelink'>5 Levene</a> &mdash;"),
      HTML("<a href='#anovalink'>6 ANOVA</a> &mdash;"),
      HTML("<a href='#brownlink'>7 Brown-Forsythe</a> &mdash;"),
      HTML("<a href='#maplink'>8 Context map</a> &mdash;"),
      HTML("<a href='#intmaplink'>9 Interactive map</a>"),
      HTML("</div>"),
      
      # Set allowed maximum for parktime and walktime. Default is 60 for both.
      HTML("<div id='contents'>"),
      sliderInput(
        "parktime_max",
        HTML("Set maximum allowed value for parktime (min),
             <p style='font-size: 9px'>default 59</p>"), 
        min = min(thesisdata$parktime),
        max = max(thesisdata$parktime),
        value = 59,
        step = 1),
      sliderInput(
        "walktime_max",
        HTML("Set maximum allowed value for walktime (min),
             <p style='font-size: 9px'>default 59</p>"), 
        min = min(thesisdata$walktime),
        max = max(thesisdata$walktime),
        value = 59,
        step = 1),
      HTML("</div>"),
      
      # Select walktime or parktime
      HTML("<div id='contents'>"),
      selectInput(
        "resp", 
        HTML("<p style='font-size: 9px'>(These selections affect sections", 
             "1&mdash;8)</p>Response (continuous)"),
        names(thesisdata[continuous])),
      
      # All others
      selectInput(
        "expl",
        "Explanatory (ordinal)", 
        names(thesisdata[ordinal])),
      
      # These are changed with the observer function
      checkboxGroupInput(
        "checkGroup", 
        "Select inactive groups in current explanatory variable",
        choiceNames = c("Item A", "Item B", "Item C"),
        choiceValues = c("a", "b", "c")),
      HTML("</div>"),
      
      # Allow user to access histogram binwidth
      HTML("<div id='contents'>"),
      sliderInput(
        "bin",
        HTML("Select binwidth for the current response variable",
             "<p style='font-size: 9px'>(2 Histogram)</p>"), 
        min = 1, 
        max = 10, 
        value = 2),
      HTML("</div>"),
      
      # Provide user possibility to see distribution of answers within the
      # ordinal variables.
      # The values of this conditionalPanel are changed with the observer
      # function
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        
        HTML("<div id='contents'>"),
        selectInput(
          "barplot", 
          HTML("Y axis for Distribution of ordinal variables <p style='font-size: 9px'>",
               "(3 Distribution of ordinal variables)</p>"),
          names(thesisdata[c("zipcode", "likert", "walktime")]),
        ),
        HTML("</div>")),
      
      # Select to inactivate subdivs. Overrides all options (except interactive 
      # map) 
      HTML("<div id='contents'>"),
      checkboxGroupInput(
        "subdivGroup",
        HTML("Select inactive subdivisions <p style='font-size: 9px'>",
          "(Affects sections 1&mdash;8. Please be aware that these selections", 
          "override Explanatory (ordinal) variable 'subdiv')</p>"),
        choiceNames = sort(as.character(unique(thesisdata$subdiv))),
        choiceValues = sort(as.character(unique(thesisdata$subdiv)))),

      # Reset inactivations with this button
      actionButton(
        "resetSubdivs", 
        "Clear inactive subdivisions"),
      HTML("</div>"),
      
      # Interactive map jenks breaks options
      HTML("<p style='visibility: hidden' id='intmap-settings-link'></p>"),
      HTML("<div id='contents'>"),
      checkboxGroupInput(
        "kunta",
        HTML("Select extent for the interactive map", 
             "<p style='font-size: 9px'>(9 Interactive map)</p>"),
        choiceNames = c("Helsinki", "Vantaa", "Espoo", "Kauniainen"),
        choiceValues = c("091", "092", "049", "235")),
      
      selectInput(
        "karttacol",
        HTML("Select Jenks breaks parameter for the interactive map",
             "<p style='font-size: 9px'>(9 Interactive map)</p>"),
        c("jenks_answer_count", "jenks_park_mean", "jenks_park_median", 
          "jenks_walk_mean", "jenks_walk_median", "jenks_ua_forest")),
      
      sliderInput(
        "jenks_n",
        "Select amount of Jenks classes",
        min = 2, 
        max = 8, 
        value = 5),
      
      HTML("</div>"),
      
      width = 3
    ),
  
    ### mainPanel layout -------------------------------------------------------
    mainPanel(
      HTML("<div id='descrilink'</div>"),
      h3("1 Descriptive statistics"),
      tableOutput("descri"),
      hr(),
      
      HTML("<div id='histlink'</div>"),
      h3("2 Histogram"),
      p("For the response (continuous) variables"),
      plotOutput("hist"),
      hr(),
      
      HTML("<div id='barplotlink'</div>"),
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        h3("3 Distribution of ordinal variables"),
        p("This plot appears when likert, parkspot or timeofday is selected as explanatory (ordinal) variable"),
        plotOutput("barplot"),
        hr()
      ),
      
      HTML("<div id='boxplotlink'</div>"),
      h3("4 Boxplot"),
      plotOutput("boxplot", height = "500px"),
      hr(),
      
      HTML("<div id='levenelink'</div>"),
      h3("5 Test of Homogeneity of Variances"),
      p("Levene value needs to be at least 0.05 for ANOVA test to be meaningful. If under 0.05, employ Brown-Forsythe test."),
      tableOutput("levene"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px;margin-top:-12px"),
      hr(),
      
      HTML("<div id='anovalink'</div>"),
      h3("6 Analysis of variance (ANOVA)"),
      tableOutput("anova"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px;margin-top:-12px"),
      hr(),
      
      HTML("<div id='brownlink'</div>"),
      h3("7 Brown-Forsythe test"),
      p("Please note that Brown-Forsythe test fails when selected response", 
        "variable maximum value is set to 0. The test requires a p.value that's", 
        "not NaN."),
      verbatimTextOutput("brownf"),
      hr(),
      
      HTML("<div id='maplink'</div>"),
      h3("8 Active subdivisions"),
      ggiraphOutput("map"),
      hr(),
  
      HTML("<div id='intmaplink'</div>"),
      h3("9 Survey results on research area map"),
      HTML("<a style='font-size: 12px' href='#intmap-settings-link'>",
           "View the settings for this map</a>"),
      ggiraphOutput("interactive"),
      hr(),
      
      h3("Data providers"),
      HTML("<a href='https://hri.fi/data/dataset/paakaupunkiseudun-aluejakokartat'>",
           "Municipality subdivisions</a>",
           "(C) Helsingin, Espoon, Vantaan ja Kauniaisten mittausorganisaatiot",
           "2011. Aineisto on muokkaamaton. License <a href='https://creativecommons.org/licenses/by/4.0/deed.en'> CC BY 4.0</a>.",
           
           "<br><a href='https://www.stat.fi/tup/paavo/index_en.html'>",
           "Postal code area boundaries</a> (C) Statistics Finland 2019.", 
           "Retrieved 27.6.2019. License <a href='https://creativecommons.org/licenses/by/4.0/deed.en'>",
           "CC BY 4.0</a>.",
           
           "<br><a href='http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226'>",
           "Regional population density 2012</a> (C) Statistics Finland 2019.", 
           "Retrieved 13.3.2020. License <a href='http://www.nic.funet.fi/index/geodata/tilastokeskus/Tilastokeskus_terms_of_use_2018.pdf'>",
           "Other (Open)</a>.",
           
           "<br><a href='https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012'>",
           "Urban Atlas 2012</a> (C) European Environment Agency 2016.", 
           "Retrieved 27.6.2019. License <a href='https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012?tab=metadata'>",
           "available at Copernicus.eu</a>.",
           
           "<br><a href='http://metatieto.ymparisto.fi:8080/geoportal/catalog/search/resource/details.page?uuid={B374BBB2-1EDF-4CF6-B11B-04E0017E9A26}'>",
           "Yhdyskuntarakenteen vyöhykkeet 2017</a> (C) Finnish Environment Institute 2019.", 
           "Retrieved 27.6.2019. License <a href='https://creativecommons.org/licenses/by/4.0/deed.en'>",
           "CC BY 4.0</a>.")
    )
  )
))
shinyApp(ui = ui, server = server)




#### Visitor ShinyApp ----------------------------------------------------------

# Use this ShinyApp to explore the development in amounts of survey respondents.
# Take note! This app uses the unedited visitors table. No IP codes have been
# deleted.

visitordata <- read.csv(file = visitorpath,
                       colClasses = c(X = "integer", id = "integer", 
                                      ip = "factor", ts_first = "POSIXct", 
                                      ts_latest = "POSIXct", count = "integer"),
                       header = TRUE, sep = ",")

# The survey visitors table saved visitor timestamps as NOW() and that is UTC in 
# MySQL. Change POSIXct object timezone to UTC+3, Helsinki summer time
visitordata$ts_first <- visitordata$ts_first + 3 * 60 * 60
visitordata$ts_latest <- visitordata$ts_latest + 3 * 60 * 60

# First sort by timestamp, then rename X or id. This fixes responses with 
# multiple zipcodes. These records have the exact same timestamps and that proved 
# tricky for dygraph to understand
thesisdata_for_xts <- thesisdata[order(thesisdata$timestamp, decreasing = FALSE), ]
thesisdata_for_xts$X <- seq(1:length(thesisdata_for_xts$X))

# Create xts objects necessary to use dygraph
visitor_xts <- xts(x = visitordata$X, order.by = visitordata$ts_first)
records_xts <- xts(x = thesisdata_for_xts$X, 
                   order.by = thesisdata_for_xts$timestamp)



# Event timestamps are read from thesis_stats_vis_funcs.R
visitor_server <- function(input, output) {
  
  output$dygraph <- renderUI({
    visitor_graph <- list(
      dygraph(records_xts, main = "Received records", group = "thesis") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2) %>%
        dyRangeSelector(height = 70)  %>%
        
        dyEvent(twitter, "@Digigeolab, @AccessibilityRG", labelLoc = "bottom") %>%
        dyEvent(mao, "MaO email list") %>%
        dyEvent(emails, "Kumpula and Viikki student email lists") %>%
        dyEvent(fb, "Lisää kaupunkia Helsinkiin, own Facebook wall and 6 WhatsApp groups") %>%
        dyEvent(espoo1, "Espoo, 11 groups") %>%
        dyEvent(misc1, "Espoo, Mankkaan lapsiperheet; Helsinki, 5 groups") %>%
        dyEvent(helsinki1, "Helsinki, 14 groups", labelLoc = "bottom") %>%
        dyEvent(helsinki2, "Helsinki, 12 groups", labelLoc = "bottom") %>%
        dyEvent(helsinki3, "Helsinki, 21 groups", labelLoc = "bottom") %>%
        dyEvent(helsinki4, "Helsinki, 3 groups", labelLoc = "bottom") %>%
        dyEvent(misc2, "Espoo, 7 groups; Vantaa, 13 groups; Helsinki, 2 groups", labelLoc = "bottom") %>%
        dyEvent(peri, "Surrounding municipalities, 12 groups", labelLoc = "bottom") %>%
        dyEvent(reminder, "Reminders, largest communities, 14 groups", labelLoc = "bottom") %>%
        dyEvent(nikinmaki, "Vantaa, Nikinmäki", labelLoc = "bottom") %>%
        dyEvent(puskaradioespoo, "Espoo, Puskaradio Espoo", labelLoc = "bottom") %>%
        dyEvent(lisaakaupunkia2, "Reminder, Lisää kaupunkia Helsinkiin", labelLoc = "bottom") %>%
        dyEvent(misc3, "Email list reminders, GIS-velhot FB group", labelLoc = "bottom"),
      
      dygraph(visitor_xts, main = "Unique first visits", group = "thesis") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2) %>%
        dyRangeSelector(height = 70)  %>%
        
        dyEvent(twitter, "@Digigeolab, @AccessibilityRG", labelLoc = "bottom") %>%
        dyEvent(mao, "MaO email list") %>%
        dyEvent(emails, "Kumpula and Viikki student email lists") %>%
        dyEvent(fb, "Lisää kaupunkia Helsinkiin, own Facebook wall and 6 WhatsApp groups") %>%
        dyEvent(espoo1, "Espoo, 11 groups") %>%
        dyEvent(misc1, "Espoo, Mankkaan lapsiperheet; Helsinki, 5 groups") %>%
        dyEvent(helsinki1, "Helsinki, 14 groups", labelLoc = "bottom") %>%
        dyEvent(helsinki2, "Helsinki, 12 groups", labelLoc = "bottom") %>%
        dyEvent(helsinki3, "Helsinki, 21 groups", labelLoc = "bottom") %>%
        dyEvent(helsinki4, "Helsinki, 3 groups", labelLoc = "bottom") %>%
        dyEvent(misc2, "Espoo, 7 groups; Vantaa, 13 groups; Helsinki, 2 groups", labelLoc = "bottom") %>%
        dyEvent(peri, "Surrounding municipalities, 12 groups", labelLoc = "bottom") %>%
        dyEvent(reminder, "Reminders, largest communities, 14 groups", labelLoc = "bottom") %>%
        dyEvent(nikinmaki, "Vantaa, Nikinmäki", labelLoc = "bottom") %>%
        dyEvent(puskaradioespoo, "Espoo, Puskaradio Espoo", labelLoc = "bottom") %>%
        dyEvent(lisaakaupunkia2, "Reminder, Lisää kaupunkia Helsinkiin", labelLoc = "bottom") %>%
        dyEvent(misc3, "Email list reminders, GIS-velhot FB group", labelLoc = "bottom"))
    
    browsable(tagList(visitor_graph))
  })
}

### Visitor UI elements --------------------------------------------------------
visitor_ui <- basicPage(
  
  # CSS tricks. Most importantly create white background box for the dygraph
  # and center it. In centering parent and child element are essential and
  # their text-align: center; and display: inline-block; parameters.
  tags$head(
    tags$style(HTML("
      html, body {
        width: 100%;
        text-align: center;
        background-color: #272b30;
      }
      h2, p {
        color: #c8c8c8;
      }
      .contentsp {
        text-align: center;
      }
      .contentsc {
        border: 5px solid #2e3338;
        border-radius: 10px;
        padding: 12px;
        margin-top: 15px;
        background: white;
        display: inline-block;
      }"
    ))
  ),

  titlePanel("Sampo Vesanen MSc thesis research survey: received responses and survey page first visits"),
  p("Click and hold, then drag and release to zoom to a period of time. Double click to return to the full view."),
  HTML("<div class='contentsp'><div class='contentsc'>"),
  uiOutput("dygraph"),
  HTML("</div></div>")
)
shinyApp(visitor_ui, visitor_server)




#### Obsolete material ####

# All of the functionality below is superseded by the analysis ShinyApp 
# presented above. Preserve these for the sake of alternatives and simplicity.

# Run tests with GetANOVA()

# We use GetANOVA() function, which performs multiple analyses with the 
# information fed to it. 

# Get walktime by timeofday
GetANOVA(walktime ~ timeofday, thesisdata$walktime, thesisdata$timeofday,
         thesisdata, c(1, 2, 3, 4, 5))

# Get walktime by timeofday, remove "Can't specify"
GetANOVA(walktime ~ timeofday, thesisdata$walktime, thesisdata$timeofday,
         thesisdata[-which(as.integer(thesisdata$timeofday) == 4), ],
         c(1, 2, 3, 4, 5))

# parktime by parkspot
GetANOVA(parktime ~ parkspot, thesisdata$parktime, thesisdata$parkspot,
         thesisdata, c(1, 2, 3, 4, 5))

# parktime by subdivision
GetANOVA(parktime ~ subdiv, thesisdata$parktime, thesisdata$subdiv,
         thesisdata, c(1, 2, 3, 4, 5))

# walktime by ykr zone
GetANOVA(walktime ~ ykr_zone, thesisdata$walktime, thesisdata$ykr_zone,
         thesisdata, c(1, 2, 3, 4, 5))