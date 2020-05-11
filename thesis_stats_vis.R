
#### Sampo Vesanen's Master's thesis statistical tests and visualisation #######


# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 11.5.2020
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



#### 1 Initialise --------------------------------------------------------------
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
#install.packages("Rmisc")
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
#install.packages("shinyWidgets")
#install.packages("grid")

# Libraries
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(Rmisc)
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
library(shinyWidgets)
library(grid)

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

# CSS data
csspath <- file.path(wd, "python/thesis_stats_vis_style.css")

# Source functions and postal code variables
source(file.path(wd, "python/thesis_stats_vis_funcs.R"))



#### 2 Preparation ------------------------------------------------------------- 

# These variables are used to subset dataframe thesisdata inside ShinyApp
continuous <- c("parktime", "walktime")
ordinal <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", 
             "subdiv")
supportcols <- c("id", "timestamp", "ip")
int_cols <- c("n", "Min", "Max", "NA")


# Read in csv data. Define column types. Name factor levels. Determine order of 
# factor levels for plotting. Lastly, remove column "X"
thesisdata <- 
  read.csv(file = datapath,
           header = TRUE, 
           sep = ",",
           colClasses = c(timestamp = "POSIXct", zipcode = "character", 
                          ip = "character", timeofday = "factor", 
                          parkspot = "factor", likert = "factor",
                          ua_forest = "factor", ykr_zone = "factor", 
                          subdiv = "factor"),
           stringsAsFactors = TRUE) %>%
  
  dplyr::mutate(parkspot = dplyr::recode(
                  parkspot, 
                  `1` = "On the side of street",
                  `2` = "Parking lot",
                  `3` = "Parking garage",
                  `4` = "Private or reserved",
                  `5` = "Other"),
                
                likert = dplyr::recode(
                  likert, 
                  `1` = "Extremely familiar",
                  `2` = "Moderately familiar",
                  `3` = "Somewhat familiar",
                  `4` = "Slightly familiar",
                  `5` = "Not at all familiar"),
                
                timeofday = dplyr::recode(
                  timeofday, 
                  `1` = "Weekday, rush hour",
                  `2` = "Weekday, other than rush hour",
                  `3` = "Weekend",
                  `4` = "Can't specify, no usual time"),
                
                ua_forest = forcats::fct_relevel(
                  ua_forest, 
                  c("Predominantly forest", 
                    "Mostly forest", 
                    "Moderate forest",
                    "Some forest", 
                    "Scarce forest")),
                
                # SYKE does not provide official translations for 
                # "Yhdyskuntarakenteen vyohykkeet".
                ykr_zone = forcats::fct_relevel(
                  ykr_zone, 
                  c("keskustan jalankulkuvyöhyke", 
                    "keskustan reunavyöhyke", 
                    "alakeskuksen jalankulkuvyöhyke",
                    "intensiivinen joukkoliikennevyöhyke", 
                    "joukkoliikennevyöhyke",
                    "autovyöhyke", 
                    "novalue"))) %>%
  dplyr::select(-X)



#### 3 Context map for ShinyApp ------------------------------------------------

#### 3.1 Subdivisions ----------------------------------------------------------

# Prepare a context map for to visualise currently active areas in analysis
# ShinyApp. left_join() preserves suuralue dataframe data. See ?"%>%" and "Using 
# the dot for secondary purposes" for more information about the curly brackets.
# In short, it allows the use of dot as self for both fortify() and 
# as.data.frame(). Then align area names with thesisdata$subdiv with mutate().
# Remove unnecessary column "Description" to save memory. Finally, factor levels 
# by their new names.
suuralue_f <- 
  rgdal::readOGR(suuraluepath, 
                 use_iconv = TRUE, 
                 encoding = "UTF-8", 
                 stringsAsFactors = TRUE) %>%
  
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
suuralue_f <- suuralue_f %>% 
  dplyr::mutate(color = as.factor(color)) # to factor


#### 3.2 Municipality borders --------------------------------------------------

# Get municipality borders. Fortify SP DataFrame for ggplot. Remove unnecessary
# columns to save memory.
# Shapefile data is Regional population density 2012, Statistics Finland.
# http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226.
muns_crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

muns_clipped_f <-
  rgdal::readOGR(munsclippedpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., muns_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(namn, vaestontih, km2, vakiluku))


#### 3.3 Annotation ------------------------------------------------------------

# Annotate municipalities for ggplot2
muns_cntr <- GetCentroids(muns_clipped_f, "nimi", "nimi")
subdiv_cntr <- GetCentroids(suuralue_f, "Name", "Name")

# Manually set better location for the annotation of Helsinki and Espoo
muns_cntr[2, "lat"] <- muns_cntr[2, "lat"] + 0.02
muns_cntr[1, "lat"] <- muns_cntr[1, "lat"] + 0.01
muns_cntr$label <- c("Espoo", "Helsinki", "Kauniainen", "Vantaa")

# Name labels here so that all the reordering doesn't mix stuff up. Remove
# munnames from subdiv annotations
subdiv_cntr$label <- gsub(".* ", "", unique(suuralue_f$Name)) 

# Manually move Espoonlahti and Southeastern to be visible. Remove subdiv label
# for Kauniainen
subdiv_cntr[2, "lat"] <- subdiv_cntr[2, "lat"] + 0.05
subdiv_cntr[2, "long"] <- subdiv_cntr[2, "long"] - 0.04
subdiv_cntr[12, "lat"] <- subdiv_cntr[12, "lat"] + 0.08
subdiv_cntr[12, "long"] <- subdiv_cntr[12, "long"] + 0.05
subdiv_cntr[16, "label"] <- ""



### 4 Interactive map for ShinyApp --------------------------------------------- 
# Created with the help of:
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html

zips <- unique(thesisdata$zipcode)

# Get postal code area data calculated in Python. It contains some interesting
# variables for visualisation. Select essential columns and multiply column 
# "ua_forest" with 100 for easier to view plotting
postal <- 
  read.csv(file = postal_path,
           header = TRUE, 
           sep = ",",
           colClasses = c(zipcode = "factor", kunta = "factor"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(c(2, 3, 6, 108:121)) %>%
  dplyr::mutate(ua_forest = ua_forest * 100)

# Create the column in "postal" which reports the largest ykr zone in each postal 
# code area
largest_ykr <- colnames(postal[, 5:11])[apply(postal[, 5:11], 1, which.max)]
largest_ykr <- gsub("ykr_", "", largest_ykr)
largest_ykr_no <- as.numeric(apply(postal[, 5:11], 1, max)) * 100
postal <- cbind(postal, largest_ykr = paste(largest_ykr, largest_ykr_no))

# "postal" geometries are in well-known text format. Some processing is needed 
# to utilise these polygons in R. readWKT()
postal_crs <- sp::CRS("+init=epsg:3067")
geometries <- lapply(postal[, "geometry"], "readWKT", p4s = postal_crs)
sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(postal[, 1]))
row.names(postal) <- postal[, 1]

data_f <- sp::SpatialPolygonsDataFrame(
  sp::SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)),
                      proj4string = postal_crs), data = postal) %>%

  # Fortify and preserve Polygon attribute data
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(zipcode)),
                    by = "id")}

# Get municipality borders from shapefile. Remove unnecessary columns to save 
# memory
muns_f <- 
  rgdal::readOGR(munspath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., postal_crs) %>%
  {dplyr::left_join(ggplot2::fortify(.), 
                    as.data.frame(.) %>%
                      dplyr::mutate(id = as.character(dplyr::row_number() - 1)))} %>%
  dplyr::select(-c(namn, vaestontih, km2, vakiluku))




#### 5 Analysis ShinyApp -------------------------------------------------------

# This ShinyApp is a versatile tool to study the thesis survey data. One can
# choose parameters with lots of freedom and exclude options as seen fit.

server <- function(input, output, session){
  
  #### 5.1 Listener functions --------------------------------------------------
  
  # 5.1.1 Listen to clear subdivs button. Resetting uses library shinyjs -------
  observeEvent(input$resetSubdivs, {
    reset("subdivGroup")
  })
  
  observeEvent(input$resetParkWalk, {
    reset("parktime_max")
    reset("walktime_max")
  })
  
  
  #### 5.1.2 Reactive data -----------------------------------------------------
  
  # currentdata(), currentpostal() and current_data_f() are reactive objects
  # made to keep track of changes the Shiny application. Most of the app uses
  # only currentdata(), but the interactive map utilises all of them. 
  
  # Detect user setting for maximum parktime and walktime.
  # currentdata() is the currently active rows of the original thesisdata 
  # DataFrame. Use currentdata() in the rest of the application to not interfere
  # with the original dataset.
  currentdata <- reactive(

    # Use ! negation to remove checkGroup and subdivGroup inputs from 
    # thesisdata.
    dplyr::filter(thesisdata,
      !(!!rlang::sym(input$expl)) %in% input$checkGroup,
      !subdiv %in% input$subdivGroup,
      parktime <= input$parktime_max,
      walktime <= input$walktime_max)
  )


  # currentpostal() recalculates, when needed, new answer counts, means, and 
  # medians for the values currently active in currentdata(). This is needed to 
  # make the interactive map tooltip values responsive to changes. In this phase,
  # we calculate the required data, the actual mapping part is in current_data_f()
  currentpostal <- reactive({
    
    currentdata <- currentdata()
    
    # tidyr::complete() helps find missing zipcodes and give them n=0
    result <- postal %>%
      dplyr::mutate(answer_count = currentdata %>% 
                      dplyr::group_by(zipcode) %>% 
                      dplyr::tally() %>% 
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(n),
                    
                    parktime_mean = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(mean(parktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    parktime_median = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(median(parktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    walktime_mean = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(mean(walktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    walktime_median = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(median(walktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    # this enables turning off subdivs and the result being
                    # visible on interactive map
                    ua_forest = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(mean(ua_forest_vals)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull()) %>%
      
      dplyr::mutate(ua_forest = ua_forest * 100)
    
    result$parktime_mean <- sapply(result[, "parktime_mean"], round, 2)
    result$walktime_mean <- sapply(result[, "walktime_mean"], round, 2)
    result
  })
  
  
  # current_data_f() produces the currently needed interactive map out of the
  # data contained in currentpostal(). This is a copy of the code above, seen
  # in "Interactive map for ShinyApp". Please See code comments in the original.
  current_data_f <- reactive({

    currentpostal <- currentpostal()

    geometries <- lapply(currentpostal[, "geometry"], "readWKT", p4s = postal_crs)
    sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(currentpostal[, 1]))
    row.names(currentpostal) <- currentpostal[, 1]

    data_f <- sp::SpatialPolygonsDataFrame(
      sp::SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)),
                          proj4string = postal_crs), data = currentpostal) %>%
      
      {dplyr::left_join(ggplot2::fortify(.),
                        as.data.frame(.) %>%
                          dplyr::mutate(id = as.character(zipcode)),
                        by = "id")}
    data_f
  })
  
  
  observe({
    # 5.1.3 Detect changes in selectInput to modify available check boxes ------
    x <- input$expl

    updateCheckboxGroupInput(
      session, 
      "checkGroup", 
      label = NULL, 
      choiceNames = levels(thesisdata[, x]),
      choiceValues = levels(thesisdata[, x]), )
    
    
    # 5.1.4 Determine availability of barplot ----------------------------------
    
    # aka availability of "Distribution of ordinal variables"
    available <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", 
                   "subdiv")
    updateSelectInput(
      session, 
      "barplot",
      label = NULL,
      choices = available[!available == x])
    
    
    # 5.1.5 Do not allow selection of all checkboxes in Jenks ------------------
    if(length(input$kunta) == 3) {
      threevalues <<- input$kunta
    }
    if(length(input$kunta) > 3) {
      
      updateCheckboxGroupInput(
        session, 
        "kunta", 
        selected = threevalues)
    }
    
    
    # 5.1.6 A clumsy implementation to listen for too large Jenks breaks -------
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
      
      # Object returned from classIntervals() has an attribute nobs which I
      # use to detect cases where too large input$jenks_n is inputted to
      # CreateJenksColumn() function
      if(attributes(classes_test)$nobs < input$jenks_n) {
        updateSliderInput(session,
                          "jenks_n",
                          value = attributes(classes_test)$nobs)
      }
    }
  })
  
  

  #### 5.2 Descriptive statistics ----------------------------------------------
  output$descri <- renderTable({
    
    # Vital variables
    resp_col <- input$resp
    expl_col <- input$expl
    thisFormula <- as.formula(paste(resp_col, '~', expl_col))
    
    # In each output, define reactive variable as "inpudata". According to
    # this Stack Overflow answer, it prevents errors down the line
    # https://stackoverflow.com/a/53989498/9455395
    
    # Reminder: the reactive currentdata() is called to keep track of maximum
    # allowed parktime and walktime values, and see changes in input$checkGroup 
    # and input$subdivGroup
    inputdata <- currentdata()

    # Take subdiv checkbox group into account
    inputdata <- inputdata[, !names(inputdata) %in% supportcols]
    response <- inputdata[[resp_col]]
    
    # Basic descriptive statistics
    desc <- onewaytests::describe(thisFormula, inputdata)
    
    ### Mean and standard error
    # Clumsily calculate mean, so that we can preserve column names in the next
    # phase. Adapt code from: https://stackoverflow.com/a/41029914/9455395
    stder <- aggregate(
      thisFormula, 
      data = inputdata,
      FUN = function(x) c(mean = mean(x), "Std.Error" = plotrix::std.error(x)))
    
    # Remove column "mean"
    stder <- subset(stder[[2]], select = -mean)
    desc <- cbind(desc, stder)
    
    # Confidence intervals for mean
    confs <- aggregate(
      thisFormula,
      data = inputdata,
      FUN = function(x) c("CI for mean, lower bound" = Rmisc::CI(x)[[3]], 
                          "CI for mean, upper bound" = Rmisc::CI(x)[[1]]))
    confs <- confs[[2]]
    desc <- cbind(desc, confs)
    
    # Reorder to SPSS descriptive statistics order
    desc <- desc[c("n", "Median", "Mean", "Std.Dev", "Std.Error", 
                   "CI for mean, lower bound", "CI for mean, upper bound", "Min", 
                   "Max", "25th", "75th", "Skewness", "Kurtosis", "NA")]
    
    # Add the total row. We will add total values for all columns in this manner,
    # using "response" which is all currently active parktime or walktime values.
    vect <- c()
    vect[1] <- length(response)
    vect[2] <- median(response)
    vect[3] <- mean(response)
    vect[4] <- sd(response)
    vect[5] <- plotrix::std.error(response)
    vect[6] <- Rmisc::CI(response)[[3]]
    vect[7] <- Rmisc::CI(response)[[1]]
    vect[8] <- min(response)
    vect[9] <- max(response)
    vect[10] <- quantile(response)[2]
    vect[11] <- quantile(response)[4]
    vect[12] <- moments::skewness(response)
    vect[13] <- moments::kurtosis(response)
    vect[14] <- sum(is.na(response))
    
    # Add "vect" to "desc" as a new row, then name the new row. Finally, set
    # specific columns as integer to prevent useless decimal places in 
    # tableOutput
    desc <- rbind(desc, vect)
    row.names(desc)[nrow(desc)] <- "Total" #name the last row
    desc[int_cols] <- sapply(desc[int_cols], as.integer)
    desc
  }, 
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE,
  digits = 2)
  
  
  #### 5.3 Histogram for parktime or walktime ----------------------------------
  output$hist <- renderPlot({
    
    resp_col <- input$resp
    expl_col <- input$expl
    binwidth <- input$bin
    
    inputdata <- currentdata()
    resp_vect <- inputdata[[resp_col]] # for vertical line labels
    
    p <- ggplot(inputdata, aes(x = !!sym(resp_col))) + 
      geom_histogram(color = "black", fill = "grey", binwidth = binwidth) +
      
      # Vertical lines for mean and median, respectively. Also display exact
      # values with annotate_custom() and textGrobs.
      geom_vline(aes(xintercept = mean(!!sym(resp_col)),
                     color = "mean"),
                 linetype = "longdash", 
                 size = 1) +
      annotation_custom(
        grob = grid::textGrob(label = round(mean(resp_vect), 2), 
                              hjust = -0.3,
                              vjust = 1.2, 
                              gp = grid::gpar(cex = 1, col = "red")),
        ymin = 0,
        ymax = 0,
        xmin = round(mean(resp_vect), 2),
        xmax = round(mean(resp_vect), 2)) +
      
      geom_vline(aes(xintercept = median(!!sym(resp_col)),
                     color = "median"),
                 linetype = "longdash", 
                 size = 1) +
      annotation_custom(
        grob = grid::textGrob(label = round(median(resp_vect), 2), 
                              hjust = 2,
                              vjust = 1.2, 
                              gp = grid::gpar(cex = 1, col = "blue")),
        ymin = 0,
        ymax = 0,
        xmin = round(median(resp_vect), 2),
        xmax = round(median(resp_vect), 2)) +
      
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
      
      # Build legend, override colors to get visible color for density. Also,
      # override linetype to get a solid line for density.
      scale_color_manual(name = paste("Legend for\n", resp_col), 
                         values = c(median = "blue", 
                                    mean = "red", 
                                    "kernel density\nestimate" = alpha("black", 0.4))) +
      guides(color = guide_legend(
        override.aes = list(color = c("darkgrey", "red", "blue"),
                            linetype = c("solid", "longdash", "longdash")))) +
      
      # Conditional histogram bar labeling. No label for zero
      stat_bin(binwidth = binwidth, 
               geom = "text", 
               aes(label = ifelse(..count.. > 0, ..count.., "")),
               vjust = -0.65)
    p
  })

  
  #### 5.4 Barplot -------------------------------------------------------------
  output$barplot <- renderPlot({
    
    # See distribution of ordinal variables through a grouped bar plot
    expl_col <- input$expl
    barplotval <- input$barplot
    yax <- paste("count of", barplotval)

    # Listen to user choices
    inputdata <- currentdata()
    
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
      
      # Setting width and position_dodge adds space between bars
      geom_bar(aes(y = stat(count)),
               width = 0.8,
               position = position_dodge(width = 0.9)) +
      
      scale_y_continuous(breaks = seq(0, maximum, by = tick_interval),
                         expand = expansion(mult = c(0, .1))) +
      xlab(expl_col) +
      ylab(yax) +
      labs(fill = barplotval) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
    
    # Use RColorBrewer color scale. Paired has 12 set colors, interpolate if
    # there are more values to map than that.
    legendnames <- unique(inputdata[[barplotval]])
    plo <- InterpolateGgplotColors(plo, legendnames, 12, "Paired")
    
    plo
  })
  
  
  #### 5.5 Boxplot -------------------------------------------------------------
  output$boxplot <- renderPlot({
    
    expl_col <- input$expl
    resp_col <- input$resp
    thisFormula <- as.formula(paste(resp_col, '~', expl_col))
    
    # Listen to user choices
    inputdata <- currentdata()
    legendnames <- levels(unique(inputdata[[expl_col]]))
    
    # ggplot2 plotting. Rotate labels if enough classes. Use scale_fill_hue()
    # to distinguish boxplot colors from barplot colors.
    p <- ggplot(inputdata, aes_string(x = expl_col, y = resp_col, fill = expl_col)) + 
      geom_boxplot() + 
      ylab(paste(resp_col, "(min)")) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "none")
    
    # Use RColorBrewer color scale. Set3 has 12 set colors, interpolate if
    # there are more values to map than that.
    p <- InterpolateGgplotColors(p, legendnames, 12, "Set3")

    # Diagonal labels if more values to map than five
    if(length(legendnames) > 5) {
      p <- p + theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
    }
    
    p
  })
  
  
  #### 5.6 Levene test ---------------------------------------------------------
  output$levene <- renderTable({
    
    expl_col <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", expl_col))
    inputdata <- currentdata()
    
    levene <- car::leveneTest(thisFormula, inputdata, center = mean)

    res <- SigTableToShiny(levene, TRUE)
    res
  }, 
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)

  
  #### 5.7 One-way ANOVA -------------------------------------------------------
  output$anova <- renderTable({
    
    expl_col <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", expl_col))
    inputdata <- currentdata()
    
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
  
  
  ### 5.8 Brown-Forsythe test --------------------------------------------------
  output$brownf <- renderPrint({
    
    resp_col <- input$resp
    expl_col <- input$expl
    thisFormula <- as.formula(paste(resp_col, "~", expl_col))
    
    inputdata <- currentdata()
    inputdata <- inputdata[, !names(inputdata) %in% supportcols]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(onewaytests::bf.test(thisFormula, data = inputdata), 
                               file = NULL, 
                               append = TRUE)
    cat(captured, sep = "\n")
  })
  
  
  ### 5.9 Context map ----------------------------------------------------------
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
      
      # Annotations. subdiv_cntr is subdiv labels, muns_cntr is municipality
      # labels.
      with(muns_cntr,
           annotate(geom = "text", 
                    x = long, 
                    y = lat, 
                    label = label, 
                    size = 5,
                    fontface = 2)) +
      with(subdiv_cntr[!subdiv_cntr$label %in% gsub(".* ", "", c(input$subdivGroup)), ], 
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
            width_svg = 16.7, 
            height_svg = 14.7, 
            options = list(opts_sizing(rescale = FALSE)))
  })
  
  
  ### 5.10 Interactive map -----------------------------------------------------
  output$interactive <- renderggiraph({

    # Use reactive data_f and postal
    # Only select municipalities selected by user. Do the same for "postal",
    # we fetch Jenks breaks from there.
    inputdata <- current_data_f()
    inputdata <- inputdata[!inputdata$kunta %in% c(input$kunta), ]
    
    inputpostal <- currentpostal()
    inputpostal <- inputpostal[!inputpostal$kunta %in% c(input$kunta), ]
    
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
    
    # Create Jenks breaks columns here so that user gets the control of Jenks
    # breaks classes. classInt::classIntervals() together with cut() creates
    # non-overlapping intervals, which are denoted as follows, for example:
    # Levels: (183,271] (102,183] (56,102] (24,56] [1,24]
    # In this notation ( means "is not included" and [ or ] means "is included"
    inputdata <- CreateJenksColumn(inputdata, inputpostal, datacol, input$karttacol, 
                                   input$jenks_n)
    
    # Get centroids for labelling polygons
    current_centr <- GetCentroids(inputdata, "zipcode", datacol)

    # Finetune locations for certain labels
    current_centr[15, 1] <- current_centr[15, 1] + 500 # Taka-Töölö
    current_centr[83, 1] <- current_centr[83, 1] - 850 # Etelä-Vuosaari
    current_centr[107, 2] <- current_centr[107, 2] - 300 # Hämevaara 
    current_centr[116, 2] <- current_centr[116, 2] - 500 # Vantaanpuisto
    current_centr[144, 1] <- current_centr[144, 1] + 2400 # Suvisaaristo
    current_centr[162, 1] <- current_centr[162, 1] + 1000 # Nupuri-Nuuksio
    
    # Format map labels. Remove [, ], (, and ). Also add list dash
    labels <- gsub("(])|(\\()|(\\[)", "", levels(inputdata[, input$karttacol]))
    labels <- gsub(",", " \U2012 ", labels)
    
    tooltip_content <- paste0(
      "<div>%s, %s<br/>",
      "Answer count: <b>%s</b></div>",
      "<hr style='margin-top:2px; margin-bottom:2px;'>",
      "<div style='padding-top: 3px;'>Parktime, mean: %s</br>",
      "Parktime, median: %s</div>",
      "<div style='padding-top: 3px;'>Walktime, mean: %s</br>",
      "Walktime, median: %s</div>",
      "<div style='padding-top: 3px;'>Forest (%%): %s</div>",
      "<div style='padding-top: 3px; line-height: 1.2;'>Largest YKR<br/>zone (%%): %s</div>")
    
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
                        labels = labels,
                        na.value = "#ebebeb") +
      
      # Municipality borders
      geom_polygon(data = muns_f,
                   aes(long, lat, group = group),
                   linetype = "longdash",
                   color = alpha("black", 0.6), 
                   fill = "NA",
                   size = 0.4) +
      coord_fixed(xlim = c(minlon, maxlon),
                  ylim = c(minlat, maxlat)) +
      
      # Class intervals disclaimer
      labs(caption = paste("The Jenks breaks classes are non-overlapping. The",
                           "lowest value of each range, with the exception of",
                           "the most bottom one, is not included in that class.")) +
      
      # Legend settings
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            plot.caption = element_text(size = 13, hjust = 0.5))
    
    # Label switch boolean test
    if(input$show_int_labels == TRUE) {
      g = g + with(current_centr,
                   annotate(geom = "text",
                            x = long, 
                            y = lat, 
                            label = label, 
                            size = 4))
    }
    
    ggiraph(code = print(g),
            width_svg = 16.7,
            height_svg = 14.7)
  })
}


### 6 ShinyApp UI elements ----------------------------------------------------- 
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("slate"),
  
  
  ### 6.1 ShinyApp header ------------------------------------------------------ 
  
  # Edit various features of the ShinyApp: 
  # - sidebarPanel (form.well) width. sidebarPanel width setting is important 
  #   because the long explanations would break it otherwise. 
  # - manually set sidebarPanel z-index to make the element always appear on top
  # - .checkbox input achieves strikeout on selected checkboxes
  # - pointer-events: none; makes zipcode labels invisible to the cursor
  # - noselect makes selecting ggiraph elements not possible
  # - :last-child pseudo-selector makes last row of descriptive statistics bold
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css"),
            htmltools::includeCSS(csspath)),
  
  
  ### 6.2 Sidebar layout -------------------------------------------------------
  titlePanel(NULL, windowTitle = "Sampo Vesanen MSc thesis research survey results"),
  sidebarLayout(
  sidebarPanel(
      # &nbsp; is a non-breaking space. Will not be cut off at any situation
      HTML("<div id='contents'>"),
      HTML("<p id='linkheading_t'>Analysis</p>"),
      HTML("<a href='#descrilink'>1&nbsp;Descriptive statistics</a> &mdash;"),
      HTML("<a href='#histlink'>2&nbsp;Histogram</a> &mdash;"),
      HTML("<a href='#barplotlink'>3&nbsp;Barplot</a> &mdash;"),
      HTML("<a href='#boxplotlink'>4&nbsp;Boxplot</a> &mdash;"),
      HTML("<a href='#levenelink'>5&nbsp;Levene</a> &mdash;"),
      HTML("<a href='#anovalink'>6&nbsp;ANOVA</a> &mdash;"),
      HTML("<a href='#brownlink'>7&nbsp;Brown-Forsythe</a><br>"),
      HTML("<p id='linkheading_b'>Visualisation</p>"),
      HTML("<a href='#maplink'>8&nbsp;Context map</a> &mdash;"),
      HTML("<a href='#intmaplink'>9&nbsp;Interactive map</a>"),
      HTML("</div>"),
      
      # Set allowed maximum for parktime and walktime. Default is 60 for both.
      HTML("<div id='stats-settings-link'><div id='contents'>"),
      sliderInput(
        "parktime_max",
        HTML("<p style='font-size: 9px'>(These selections affect sections", 
             "1&mdash;7, 9)</p>Set maximum allowed value for parktime (min)"),
        min = min(thesisdata$parktime),
        max = max(thesisdata$parktime),
        value = 59,
        step = 1),
      
      sliderInput(
        "walktime_max",
        HTML("Set maximum allowed value for walktime (min)"), 
        min = min(thesisdata$walktime),
        max = max(thesisdata$walktime),
        value = 59,
        step = 1),
      
      actionButton(
        "resetParkWalk", 
        HTML("Revert values to default (59&nbsp;min)")),
      
      HTML("</div>"),
      
      # Select walktime or parktime
      HTML("<div id='contents'>"),
      selectInput(
        "resp", 
        HTML("<p style='font-size: 9px'>(These selections affect sections", 
             "1&mdash;7, 9)</p>Response (continuous)"),
        names(thesisdata[continuous])),
      
      # likert, parkspot, timeofday, ua_forest, ykr_zone, subdiv
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
      HTML("</div></div>"),
      
      # Allow user to access histogram binwidth
      HTML("<div id='hist-settings-link'><div id='contents'>"),
      sliderInput(
        "bin",
        HTML("Select binwidth for the current response variable",
             "<a style='font-size: 9px' href='#histlink'>(2 Histogram)</a>"), 
        min = 1, 
        max = 10, 
        value = 2),
      HTML("</div></div>"),
      
      # Provide user possibility to see distribution of answers within the
      # ordinal variables.
      # The values of this conditionalPanel are changed with the observer
      # function
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        
        HTML("<div id='barplot-settings-link'><div id='contents'>"),
        selectInput(
          "barplot", 
          HTML("Y axis for Distribution of ordinal variables <a",
               "style='font-size: 9px' href='#barplotlink'>",
               "(3 Distribution of ordinal variables)</a>"),
          names(thesisdata[c("zipcode", "likert", "walktime")]),
        ),
        HTML("</div></div>")
      ),
      
      # Select to inactivate subdivs. Overrides all options (except interactive 
      # map) 
      HTML("<div id='subdiv-settings-link'><div id='contents'>"),
      checkboxGroupInput(
        "subdivGroup",
        HTML("Select inactive subdivisions <p style='font-size: 9px'>",
          "(Affects sections 1&mdash;9. Please be aware that these selections", 
          "override Explanatory (ordinal) variable 'subdiv')</p>"),
        choiceNames = sort(as.character(unique(thesisdata$subdiv))),
        choiceValues = sort(as.character(unique(thesisdata$subdiv)))),

      # Reset inactivations with this button
      actionButton(
        "resetSubdivs", 
        "Clear inactive subdivisions"),
      HTML("</div></div>"),
      
      # Interactive map jenks breaks options
      HTML("<div id='intmap-settings-link'><div id='contents'>"),
      checkboxGroupInput(
        "kunta",
        HTML("Select extent for the interactive map <a",
             "style='font-size: 9px' href='#intmaplink'>",
             "(9 Interactive map)</a>"),
        choiceNames = c("Helsinki", "Vantaa", "Espoo", "Kauniainen"),
        choiceValues = c("091", "092", "049", "235")),
      
      selectInput(
        "karttacol",
        HTML("Select Jenks breaks parameter for the interactive map"),
        c("jenks_answer_count", "jenks_park_mean", "jenks_park_median", 
          "jenks_walk_mean", "jenks_walk_median", "jenks_ua_forest")),
      
      # Switch for interactive map labels
      HTML("<label class='control-label' for='show_int_labels'>Show labels</label>"),
      shinyWidgets::switchInput(
        inputId = "show_int_labels", 
        value = TRUE),
      
      sliderInput(
        "jenks_n",
        "Select the amount of classes",
        min = 2, 
        max = 8, 
        value = 5),
      
      HTML("</div></div>"),
      
      HTML("<p style='font-size: 11px; color: grey; margin-top: -10px;'>",
           "Analysis app version 11.5.2020</p>"),
      
      width = 3
    ),
  
    
    ### 6.3 mainPanel layout ---------------------------------------------------
    mainPanel(
      h2("Sampo Vesanen MSc thesis research survey results"),
      HTML("<p style='max-width: 1200px;'>Welcome to the analysis application",
           "for the survey results for the thesis 'Parking of private cars and",
           "spatial accessibility in Helsinki Capital Region'. Please see the",
           "thesis and GitHub repository for additional information and",
           "instructions how to use this application.</p>"),
      hr(),
      
      HTML("<div id='descrilink'>"),
      HTML("<h3>1 Descriptive statistics&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      tableOutput("descri"),
      HTML("</div>"),
      hr(),
      
      HTML("<div id='histlink'>"),
      HTML("<h3>2 Histogram&ensp;",
           "<a href='#hist-settings-link'><i class='icon wrench' title='Go to histogram settings'></i></a>",           
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      p("For the response (continuous) variables"),
      plotOutput("hist"),
      HTML("</div>"),
      hr(),
      
      HTML("<div id='barplotlink'>"),
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        HTML("<h3>3 Distribution of ordinal variables&ensp;",
             "<a href='#barplot-settings-link'><i class='icon wrench' title='Go to barplot settings'></i></a>",           
             "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
             "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
        p("This plot appears when likert, parkspot or timeofday is selected as explanatory (ordinal) variable"),
        plotOutput("barplot"),
        hr()
      ),
      HTML("</div>"),
      
      HTML("<div id='boxplotlink'>"),
      HTML("<h3>4 Boxplot&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      plotOutput("boxplot", height = "500px"),
      HTML("</div>"),
      hr(),
      
      HTML("<div id='levenelink'>"),
      HTML("<h3>5 Test of Homogeneity of Variances (Levene's test)&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      p("Levene value needs to be at least 0.05 for ANOVA test to be meaningful. If under 0.05, employ Brown-Forsythe test."),
      tableOutput("levene"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px;margin-top:-12px"),
      HTML("</div>"),
      hr(),
      
      HTML("<div id='anovalink'>"),
      HTML("<h3>6 Analysis of variance (ANOVA)&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      tableOutput("anova"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px;margin-top:-12px"),
      HTML("</div>"),
      hr(),
      
      HTML("<div id='brownlink'>"),
      HTML("<h3>7 Brown-Forsythe test&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      p("Please note that Brown-Forsythe test fails when selected response", 
        "variable maximum value is set to 0. The test requires a p.value that's", 
        "not NaN."),
      verbatimTextOutput("brownf"),
      HTML("</div>"),
      hr(),
      
      HTML("<div id='maplink'>"),
      HTML("<h3>8 Active subdivisions&ensp;",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      ggiraphOutput("map"),
      HTML("</div>"),
      hr(),
  
      HTML("<div id='intmaplink'>"),
      HTML("<h3>9 Survey results on research area map&ensp;",
           "<a href='#intmap-settings-link'><i class='icon wrench' title='Go to interactive map settings'></i></a>",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a></h3>"),
      HTML("<div class='noselect'>"),
      ggiraphOutput("interactive"),
      HTML("</div></div>"),
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




#### 7 Visitor ShinyApp --------------------------------------------------------

# Use this ShinyApp to explore the development in amounts of survey respondents.
# Take note! This app uses the unedited visitors table. No IP codes have been
# deleted.

visitordata <- read.csv(file = visitorpath,
                        header = TRUE, 
                        sep = ",",
                        colClasses = c(X = "integer", id = "integer", 
                                       ip = "factor", ts_first = "POSIXct", 
                                       ts_latest = "POSIXct", count = "integer"),
                       stringsAsFactors = TRUE)

# The survey visitors table saved visitor timestamps as NOW() and that is UTC in 
# MySQL. Change POSIXct object timezone to UTC+3, Helsinki summer time
visitordata$ts_first <- visitordata$ts_first + 3 * 60 * 60
visitordata$ts_latest <- visitordata$ts_latest + 3 * 60 * 60

# First sort by column "timestamp", then give column "id" sequential values. 
# This fixes responses with multiple zipcodes. These records have the exact same 
# timestamps and that proved tricky for dygraph to understand. Also, xts objects
# seem to need a column to that as sequential values, in our case, column "id".
thesisdata_for_xts <- thesisdata[order(thesisdata$timestamp, decreasing = FALSE), ]
thesisdata_for_xts$id <- seq(1:length(thesisdata_for_xts$id))

# Create xts objects necessary to use dygraph
visitor_xts <- xts(x = visitordata$id, order.by = visitordata$ts_first)
records_xts <- xts(x = thesisdata_for_xts$id, 
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

### 8 Visitor UI elements ------------------------------------------------------
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
  HTML("<p style='font-size: 11px; color: grey; margin-top: -10px;'>",
       "Analysis app version 3.5.2020</p>"),
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
#GetANOVA(walktime ~ timeofday, thesisdata$walktime, thesisdata$timeofday,
#         thesisdata, c(1, 2, 3, 4, 5))

# Get walktime by timeofday, remove "Can't specify"
#GetANOVA(walktime ~ timeofday, thesisdata$walktime, thesisdata$timeofday,
#         thesisdata[-which(as.integer(thesisdata$timeofday) == 4), ],
#         c(1, 2, 3, 4, 5))

# parktime by parkspot
#GetANOVA(parktime ~ parkspot, thesisdata$parktime, thesisdata$parkspot,
#         thesisdata, c(1, 2, 3, 4, 5))

# parktime by subdivision
#GetANOVA(parktime ~ subdiv, thesisdata$parktime, thesisdata$subdiv,
#         thesisdata, c(1, 2, 3, 4, 5))

# walktime by ykr zone
#GetANOVA(walktime ~ ykr_zone, thesisdata$walktime, thesisdata$ykr_zone,
#         thesisdata, c(1, 2, 3, 4, 5))