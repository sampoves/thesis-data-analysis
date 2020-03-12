
# Sampo Vesanen's Master's thesis statistical tests and visualisation
#####################################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 10.3.2020
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

# Standard error
#https://stackoverflow.com/a/41029914/9455395

# Confidence intervals for mean
#https://www.r-bloggers.com/compare-regression-results-to-a-specific-factor-level-in-r/

# One-way ANOVA
#https://en.wikipedia.org/wiki/One-way_analysis_of_variance

# About ANOVA, Levene and BF
#https://www.statisticshowto.datasciencecentral.com/brown-forsythe-test/
#https://www.statisticshowto.datasciencecentral.com/levene-test/
#https://www.statisticshowto.datasciencecentral.com/homoscedasticity/



#### Initialise ----------------------------------------------------------------
rm(list = ls())
gc()

#install.packages("onewaytests") # brown-forsythe
#install.packages("car")
#install.packages("plotrix") # std.error
#install.packages("moments") # quantile
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


#### Preparation ####

# Important directories
wd <- "C:/Sampon/Maantiede/Master of the Universe"
datapath <- file.path(wd, "pythonrecords.csv")
visitorpath <- file.path(wd, "leaflet_survey_results/visitors.csv")
suuraluepath <- file.path(wd, "python/suuralueet/PKS_suuralue.kml")
munsclippedpath <- file.path(wd, "python/paavo/hcr_muns_clipped.shp")




# POSTAL TEST
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html#using_leaflet
#install.packages("ggiraph")
#install.packages("widgetframe")
#install.packages("hrbrthemes")
#install.packages("colormap")
library(colormap)
library(ggiraph)
library(widgetframe)
library(hrbrthemes)
library(rgeos)
postal_path <- file.path(wd, "pythonpostal.csv")

postal <- read.csv(file = postal_path, 
                   colClasses = c(posti_alue = "factor", kunta = "factor"),
                   header = TRUE, sep = ",")
postal <- postal[, c(2, 108:119)]
#postal <- rangeMapper::WKT2SpatialPolygonsDataFrame(postal, "geometry", "posti_alue")
##postal <- sf::st_as_sf(postal, wkt = "geometry")
#postal <- ggplot2::fortify(postal)
crs <- sp::CRS("+init=epsg:3067")
geometries <- lapply(postal[, "geometry"], "readWKT", p4s = crs)
sp_tmp_ID <- mapply(spChFIDs, geometries, as.character(postal[, 1]))
row.names(postal) <- postal[, 1]
data <- SpatialPolygonsDataFrame(
  SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)), 
                  proj4string = crs),
  data = postal[, -ncol(postal)])

#data_f <- ggplot2::fortify(data)
# preserve dataframe data
data_f <- merge(ggplot2::fortify(data), as.data.frame(data), by.x = "id", 
                by.y = 0)

#SpatialPolygons(lapply(geometries, function(x) {x@polygons[[1]]}))
#rgeos::readWKT(postal$geometry[1], p4s = sp::CRS("+init=epsg:3067"))
#writeWKT(spgeom, byid = FALSE)

g <- ggplot(data_f) +
  geom_polygon_interactive(
    color = "black",
    aes(long, lat,
        group = group, 
        fill = ua_forest,
        tooltip = sprintf("%s<br/>%s", id, ua_forest))) +
  hrbrthemes::theme_ipsum() +
  colormap::scale_fill_colormap(
    colormap = colormap::colormaps$greens, reverse = T) +
  labs(title = "Internet Usage in Africa in 2015", 
       subtitle = "As Percent of Population",
       caption = "Source: World Bank Open Data")

widgetframe::frameWidget(ggiraph(code = print(g)))




# Source functions and postal code variables
source(file.path(wd, "python/thesis_stats_vis_funcs.R"))

# These variables are used to subset dataframe thesisdata inside ShinyApp
continuous <- c("parktime", "walktime") 
ordinal <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", 
             "subdiv") 
supportcols <- c("X", "id", "timestamp", "ip")


# Read in csv data. Define column types
thesisdata <- read.csv(file = datapath,
                colClasses = c(timestamp = "POSIXct", zipcode = "character", 
                               ip = "character", timeofday = "factor", 
                               parkspot = "factor", likert = "factor", 
                               ua_forest = "factor", ykr_zone = "factor", 
                               subdiv = "factor"),
                header = TRUE, sep = ",")

# Name factor levels. Determine order of factor levels for plotting
levels(thesisdata$parkspot) <- list("On the side of street" = 1,
                                    "Parking lot" = 2,
                                    "Parking garage" = 3,
                                    "Private or reserved" = 4,
                                    "Other" = 5)

levels(thesisdata$likert) <- list("Extremely familiar" = 1,
                                  "Moderately familiar" = 2,
                                  "Somewhat familiar" = 3,
                                  "Slightly familiar" = 4,
                                  "Not at all familiar" = 5)

levels(thesisdata$timeofday) <- list("Weekday, rush hour" = 1,
                                     "Weekday, other than rush hour" = 2,
                                     "Weekend" = 3,
                                     "Can't specify, no usual time" = 4)

# SYKE does not provide official translations for these zones
levels(thesisdata$ykr_zone) <- list("keskustan jalankulkuvyöhyke" = 1,
                                    "keskustan reunavyöhyke" = 2,
                                    "alakeskuksen jalankulkuvyöhyke" = 3,
                                    "intensiivinen joukkoliikennevyöhyke" = 4,
                                    "joukkoliikennevyöhyke" = 5,
                                    "autovyöhyke" = 6,
                                    "novalue" = 7)

levels(thesisdata$ua_forest) <- list("Predominantly forest" = 1,
                                     "Mostly forest" = 2,
                                     "Moderate forest" = 3,
                                     "Some forest" = 4,
                                     "Scarce forest" = 5)

# Remove column "index". Remove X, pinta_ala
thesisdata <- subset(thesisdata, select = -c(index))



#### Prepare the context map for ShinyApp ####

# Prepare a context map for to visualise currently active areas in analysis
# ShinyApp. Updating this map makes the app a bit more sluggish. Delete this
# code if you get sufficiently annoyed with the sluggishness.
suuralue <- readOGR(suuraluepath, use_iconv = TRUE, encoding = "UTF-8")

# Preserve suuralue dataframe data
suuralue_f <- merge(fortify(suuralue), as.data.frame(suuralue), by.x = "id", 
                    by.y = 0)

# Align area names with thesisdata$subdiv
levels(suuralue_f$Name) <- c("Vantaa Aviapolis", "Helsinki Southern", 
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
                             "Helsinki Östersundom")
suuralue_f$Name <- factor(suuralue_f$Name, levels = sort(levels(suuralue_f$Name)))

# Get municipality borders
muns_clipped <- readOGR(munsclippedpath)
muns_clipped <- spTransform(muns_clipped, 
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
muns_clipped_f <- merge(fortify(muns_clipped), as.data.frame(muns_clipped), 
                        by.x = "id", by.y = 0)

# Annotate muns in ggplot:
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
c_esp <- brewer.pal(7, "YlOrRd")
c_hel <- brewer.pal(8, "PuBu")
c_van <- brewer.pal(7, "BuGn")

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
suuralue_f <- suuralue_f %>% mutate(color = as.factor(color)) # to factor

# name labels here so that all the reordering doesn't mix up stuff. Remove
# munnames from subdiv annotations
centroids2$label <- gsub(".* ", "", unique(suuralue_f$Name)) 

# Manually move Espoonlahti and Southeastern to be visible. Remove subdiv label
# for Kauniainen
centroids2[2, "lat"] <- centroids2[2, "lat"] + 0.05
centroids2[2, "long"] <- centroids2[2, "long"] - 0.04
centroids2[12, "lat"] <- centroids2[12, "lat"] + 0.08
centroids2[12, "long"] <- centroids2[12, "long"] + 0.05
centroids2[16, "label"] <- ""


# Independent ggplot maptest
# ggplot() +
#   geom_polygon(
#     data = suuralue_f,
#     aes(long, lat, group = group, fill = color),
#     colour = "grey") +
#   geom_polygon(
#     data = muns_clipped_f,
#     aes(long, lat, group = group),
#     fill = NA,
#     colour = "black") +
#   coord_map(ylim = c(60.07, 60.42)) +
#   scale_fill_identity("Currently active\nsubdivisions", labels = suuralue_f$Name,
#                       breaks = suuralue_f$color, guide = "legend") +
#   with(centroids, annotate(geom = "text", x = long, y = lat, label = label,
#                            size = 4)) +
#   with(centroids2, annotate(geom = "text", x = long, y = lat, label = label,
#                            size = 3)) +
#   theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"), legend.position = "bottom")



#### Analysis ShinyApp ---------------------------------------------------------

# This ShinyApp is a versatile tool to study the thesis survey data. One can
# choose parameters with lots of freedom and exclude options as seen fit.

server <- function(input, output, session){
  
  #### Listener functions ####
  
  # Listen to clear subdivs button. Resetting uses library shinyjs
  observeEvent(input$resetSubdivs, {
    reset("subdivGroup")
  })
  
  # Detect changes in selectInput to modify available check boxes
  observe({
    x <- input$expl

    updateCheckboxGroupInput(session, "checkGroup", 
      label = NULL, 
      choiceNames = levels(thesisdata[, x]),
      choiceValues = levels(thesisdata[, x]),)
    
    available <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", 
                   "subdiv")
    updateSelectInput(session, "barplot",
                      label = NULL,
                      choices = available[!available == x])
  })
  
  

  #### Descriptive statistics ####
  output$descri <- renderTable({
    
    # Vital variables
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    responsecol <- input$resp
    response <- thesisdata[[responsecol]]
    colname <- input$expl
    
    # Take subdiv checkbox group into account
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), 
                            !names(thesisdata) %in% supportcols]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]

    # Basic descriptive statistics
    desc <- describe(thisFormula, inputdata)
    
    ### Std. Error
    # Clumsily calculate mean, so that we can preserve column names in the next
    # phase
    stder <- aggregate(thisFormula, data = inputdata,
                       FUN = function(x) c(mean = mean(x),
                                           "Std.Error" = std.error(x)))
    
    # Remove column mean 
    stder <- subset(stder[[2]], select = -mean)
    desc <- cbind(desc, stder)
    
    # Confidence intervals for mean
    confs <- aggregate(
      thisFormula, data = inputdata, 
      FUN = function(x) c("CI for mean, Lower Bound" = mean(x) - 2 * std.error(x), 
                          "CI for mean, Upper Bound" = mean(x) + 2 * std.error(x)))
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
    vect[5] <- std.error(response)
    vect[6] <- mean(response) - 2 * std.error(response)
    vect[7] <- mean(response) + 2 * std.error(response)
    vect[8] <- min(response)
    vect[9] <- max(response)
    vect[10] <- quantile(response)[2]
    vect[11] <- quantile(response)[4]
    vect[12] <- skewness(response)
    vect[13] <- kurtosis(response)
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
  
  
  #### Histogram for parktime or walktime ####
  output$hist <- renderPlot({
    
    responsecol <- input$resp
    explanatorycol <- input$expl
    
    inputdata <- thesisdata[!thesisdata[[explanatorycol]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    hist(inputdata[[responsecol]],
         main = paste("Histogram for", responsecol),
         xlab = responsecol)
  })
  
  
  #### Boxplot ####
  output$boxplot <- renderPlot({
    
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    explanatorycol <- input$expl
    
    # Listen to user choices
    inputdata <- thesisdata[!thesisdata[[explanatorycol]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    legendnames <- levels(unique(inputdata[[explanatorycol]]))
    
    # ggplot2 plotting. Rotate labels if enough classes
    if(length(legendnames) > 5){
      
      p <- ggplot(inputdata, aes_string(x = input$expl, y = input$resp)) + 
        geom_boxplot() + 
        theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
    } else {
      
      p <- ggplot(inputdata, aes_string(x = input$expl, y = input$resp)) + 
        geom_boxplot()
    }
    p
    
  })

  
  #### Barplot ####
  output$barplot <- renderPlot({
    
    # See distribution of ordinal variables through a grouped bar plot
    explanatorycol <- input$expl
    barplotval <- input$barplot
    yax <- paste("sum of", barplotval)

    # Listen to user choices
    inputdata <- thesisdata[!thesisdata[[explanatorycol]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # Plot maximum y tick value. Use dplyr to group the desired max amount.
    # In dplyr, use !!as.symbol(var) to notify that we are using variables
    # to denote column names
    maximum <- inputdata %>% 
      group_by(!!as.symbol(explanatorycol), !!as.symbol(barplotval)) %>% 
      summarise(amount = length(!!as.symbol(barplotval))) %>% 
      top_n(n = 1)
    maximum <- max(as.data.frame(maximum$amount))
    
    if (maximum <= 200){
      tick_interval <- 50
    } else {
      tick_interval <- 200
    }
    
    # Draw ggplot2 plot
    plo <- 
      ggplot(inputdata, aes(x = get(explanatorycol), 
                            y = factor(get(barplotval)), 
                            fill = get(barplotval))) +
      geom_bar(aes(y = stat(count)), position = "dodge") + 
      scale_y_continuous(breaks = seq(0, maximum, by = tick_interval),
                         expand = expansion(mult = c(0, .1))) +
      xlab(explanatorycol) +
      ylab(yax) +
      scale_fill_discrete(name = barplotval)
      theme(legend.position = "right")
    
    plo
  })
  
  
  #### Levene test ####
  output$levene <- renderTable({
    
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", input$expl))
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    levene <- leveneTest(thisFormula, inputdata, center = mean) #car

    res <- SigTableToShiny(levene, TRUE)
    res
  }, 
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)

  
  #### One-way ANOVA ####
  output$anova <- renderTable({
    
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", input$expl))
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), ]
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
  
  ### Brown-Forsythe test ####
  output$brownf <- renderPrint({
    
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", input$expl))
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), 
                            !names(thesisdata) %in% supportcols]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(bf.test(thisFormula, data = inputdata), 
                               file = NULL, append = TRUE)
    cat(captured, sep = "\n")
  })
  
  ### Context map ####
  output$map <- renderPlot({
    
    # Count active subdivs
    active_subdivs <- 23 - length(input$subdivGroup)
    
    mapp <- ggplot() + 
      geom_polygon(data = suuralue_f,
                   aes(long, lat, group = group, fill = "#3d3d3d"),
                   colour = NA) +
      geom_polygon(
        data = suuralue_f[!suuralue_f$Name %in% c(input$subdivGroup), ], 
        aes(long, lat, group = group, fill = color),
        colour = "grey") +
      geom_polygon(
        data = muns_clipped_f, 
        aes(long, lat, group = group), 
        fill = NA, 
        colour = "black") +
      coord_map(ylim = c(60.07, 60.42)) +
      scale_fill_identity(paste0("Currently active\nsubdivisions\n(", 
                                 active_subdivs, " out of 23)"), 
                          labels = suuralue_f$Name, breaks = suuralue_f$color, 
                          guide = "legend") +
      with(centroids, annotate(geom = "text", x = long, y = lat, label = label, 
                               size = 4)) +
      with(centroids2[!centroids2$label %in% gsub(".* ", "", c(input$subdivGroup)), ], 
           annotate(geom = "text", x = long, y = lat, label = label, size = 3)) +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"), 
            legend.position = "bottom")
    mapp
  },
  width = 720,
  height = 700)
}

### ShinyApp UI elements ####
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("slate"),
  
  # Edit various CSS features of the ShinyApp such as the Brown-Forsythe test 
  # box and sidebarPanel (form.well) width. sidebarPanel width setting is 
  # important because the long explanations would break it otherwise. Also
  # manually set sidebarPanel z-index to make the element always appear on top.
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
        max-width: 1000px;
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
      }"
    ))
  ),                    
  
  titlePanel("Sampo Vesanen MSc thesis research survey results ShinyApp"),
  sidebarLayout(
    sidebarPanel(
      HTML("<div id='contents'>"),
      HTML("<a href='#descrilink'>Descriptives</a> &mdash;"),
      HTML("<a href='#histlink'>Histogram</a> &mdash;"),
      HTML("<a href='#barplotlink'>Barplot</a> &mdash;"),
      HTML("<a href='#boxplotlink'>Boxplot</a> &mdash;"),
      HTML("<a href='#levenelink'>Levene</a> &mdash;"),
      HTML("<a href='#anovalink'>ANOVA</a> &mdash;"),
      HTML("<a href='#brownlink'>Brown-Forsythe</a> &mdash;"),
      HTML("<a href='#maplink'>Context map</a>"),
      HTML("</div>"),
      
      # walktime or parktime
      selectInput("resp", 
                 "Response (continuous)",
                 names(thesisdata[continuous])),
      # All others
      selectInput("expl",
                 "Explanatory (ordinal)", 
                 names(thesisdata[ordinal])),
      
      # Provide user possibility to see distribution of answers within the
      # ordinal variables.
      # The values of this conditionalPanel are changed with the observer
      # function
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        selectInput(
          "barplot", "Y axis for Distribution of ordinal variables",
          names(thesisdata[c("zipcode", "likert", "walktime")]),
      )),
      
      # These are changed with the observer function
      checkboxGroupInput(
        "checkGroup", 
        "Select inactive groups",
        choiceNames = c("Item A", "Item B", "Item C"),
        choiceValues = c("a", "b", "c")),

      checkboxGroupInput(
        "subdivGroup",
        HTML("Select inactive subdivisions <p style='font-size: 9px'>",
          "(NB! selections here override Explanatory (ordinal) variable subdiv!)</p>"),
        choiceNames = sort(as.character(unique(thesisdata$subdiv))),
        choiceValues = sort(as.character(unique(thesisdata$subdiv)))),

      actionButton("resetSubdivs", "Clear inactive subdivisions"),
      
      width = 3
    ),
  
    mainPanel(
      HTML("<div id='descrilink'</div>"),
      h3("Descriptive statistics"),
      p("If N is distributed somewhat equally, Levene test is not required."),
      tableOutput("descri"),
      hr(),
      
      HTML("<div id='histlink'</div>"),
      h3("Histogram"),
      plotOutput("hist"),
      hr(),
      
      HTML("<div id='barplotlink'</div>"),
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        h3("Distribution of ordinal variables"),
        p("This plot appears when likert, parkspot or timeofday is selected as explanatory (ordinal) variable"),
        plotOutput("barplot"),
        hr()
      ),
      
      HTML("<div id='boxplotlink'</div>"),
      h3("Boxplot"),
      plotOutput("boxplot", height = "500px"),
      hr(),
      
      HTML("<div id='levenelink'</div>"),
      h3("Test of Homogeneity of Variances"),
      p("Levene value needs to be at least 0.05 for ANOVA test to be meaningful. If under 0.05, employ Brown-Forsythe test."),
      tableOutput("levene"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px;margin-top:-12px"),
      hr(),
      
      HTML("<div id='anovalink'</div>"),
      h3("Analysis of variance (ANOVA)"),
      tableOutput("anova"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px;margin-top:-12px"),
      hr(),
      
      HTML("<div id='brownlink'</div>"),
      h3("Brown-Forsythe"),
      verbatimTextOutput("brownf"),
      hr(),
      
      HTML("<div id='maplink'</div>"),
      h3("Active subdivisions"),
      plotOutput("map"),
      hr(),
      
      # This is an unfortunate hack to prevent the data providers from appearing
      # on top of the context map
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), hr(),
      h3("Data providers"),
      HTML("<a https://hri.fi/data/dataset/paakaupunkiseudun-aluejakokartat>",
           "Municipality subdivisions</a>",
           "(C) Helsingin, Espoon, Vantaan ja Kauniaisten mittausorganisaatiot",
           "2011. Aineisto on muokkaamaton. License",
           "<a https://creativecommons.org/licenses/by/4.0/deed.en> CC BY 4.0</a>",
           "<br><a https://www.stat.fi/tup/paavo/index_en.html>",
           "Postal code area boundaries</a> (C) Statistics Finland 2019.", 
           "Retrieved 27.6.2019. License <a https://creativecommons.org/licenses/by/4.0/deed.en>",
           "CC BY 4.0</a>"),
      br()
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
      dygraph(records_xts, main = "records", group = "thesis") %>%
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
      
      dygraph(visitor_xts, main = "visitors", group = "thesis") %>%
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

visitor_ui <- basicPage(
  tags$head(
    tags$style(HTML("
      html, body {
        width: 100%;
        text-align: center;
        background-color: grey;
      }
      #dygraph {
        display: inline-block;
      }
      .verticalLine {
      border-radius: 25px;
        border-left: 750px solid white; 
        height: 1050px; 
        position: absolute; 
        left: 40%; 
      }
      .verticalLine2 {
      border-radius: 25px;
        border-left: 700px solid white; 
        height: 1050px; 
        position: absolute; 
        right: 40%; 
      }"
                    
    ))
  ),
  titlePanel("Received responses and survey page first visits"),
  tags$div(class = "verticalLine"),
  tags$div(class = "verticalLine2"),
  uiOutput("dygraph")
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