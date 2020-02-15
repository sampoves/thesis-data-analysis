
# Master's thesis statistical tests and visualisation
#####################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 15.2.2020

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



#### Initialise ####
rm(list = ls())

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



#### Preparation ####

# Working path
wd <- "C:/Sampon/Maantiede/Master of the Universe"
datapath <- file.path(wd, "pythonrecords.csv")
postal_path <- file.path(wd, "pythonpostal.csv")
source(file.path(wd, "python/thesis_stats_vis_funcs.R"))

# Variables used to subset thesisdata inside ShinyApp
continuous <- c("parktime", "walktime") 
ordinal <- c("likert", "parkspot", "timeofday", "ua_forest", "ykr_zone", "subdiv") 
supportcols <- c("X", "id", "timestamp", "ip")

# Read in csv data. Define column types
thesisdata <- read.csv(file = datapath,
                colClasses = c(timestamp = "POSIXct", zipcode = "character", 
                               ip = "character", timeofday = "factor", 
                               parkspot = "factor", likert = "factor", 
                               ua_forest = "factor", ykr_zone = "factor", 
                               subdiv = "factor"),
                header = TRUE, sep = ",")

# Postal code data
postal <- read.csv(file = postal_path, 
                   colClasses = c(posti_alue = "factor", kunta = "factor"),
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


# Remove column "index". Remove X from postal
thesisdata <- subset(thesisdata, select = -c(index))
postal <- postal[-c(1, 5, 6)]



#### ShinyApp ####

# This ShinyApp is a versatile tool to study the thesis survey data. One can
# choose parameters with freedom and exclude options as seen fit.

server <- function(input, output, session){
  
  #### Listener function ####
  # Detect changes in selectInput to modify available check boxes.
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
    
    # take subdiv checkbox group into account
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), 
                            !names(thesisdata) %in% supportcols]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]

    # Basic descriptive statistics
    desc <- describe(thisFormula, inputdata)
    
    ### Std. Error
    # clumsily calculate mean, so that we can preserve column names in the next
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
    
    # Show
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
    
    # listen to user choices
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

    # listen to user choices
    inputdata <- thesisdata[!thesisdata[[explanatorycol]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # Plot maximum y tick value. Use dplyr to group the desired max amount.
    # in dplyr, use !!as.symbol(var) to notify that we are using variables
    # to denote column names
    maximum <- inputdata %>% 
      group_by(!!as.symbol(explanatorycol), !!as.symbol(barplotval)) %>% 
      summarise(amount = length(!!as.symbol(barplotval))) %>% 
      top_n(n=1)
    maximum <- max(as.data.frame(maximum$amount))
    
    if (maximum <= 200){
      tick_interval <- 50
    } else {
      tick_interval <- 200
    }
    
    # Draw ggplot2 plot
    plo <- 
      ggplot(inputdata, aes(x = get(explanatorycol), y = factor(get(barplotval)), 
                            fill = get(barplotval))) +
      geom_bar(aes(y = stat(count)), position = "dodge") + 
      scale_y_continuous(breaks = seq(0, maximum, by = tick_interval),
                         expand = expand_scale(mult = c(0, .1))) +
      xlab(explanatorycol) +
      ylab(yax) +
      scale_fill_discrete(name = barplotval)
      theme(legend.position = "right")
    
    plo
  })
  
  
  #### Levene test ####
  output$levene <- renderTable({
    
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    levene <- leveneTest(thisFormula, inputdata, center = mean) #car
    #leveneVal <- levene[[3]][1]

    res <- SigTableToShiny(levene, TRUE)
    
    # show
    res
    
  }, 
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)

  
  #### One-way ANOVA ####
  output$anova <- renderTable({
    
    # needed variables
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    #### One-way ANOVA #
    res.aov <- aov(thisFormula, data = inputdata)
    anovasummary <- summary(res.aov)
    
    # Use this function to communicate table correctly to Shiny
    anovasummary <- SigTableToShiny(anovasummary, FALSE)
    
    # Show
    anovasummary
  },
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  ### Brown-Forsythe test ####
  output$brownf <- renderPrint({
    
    # needed variables
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), 
                            !names(thesisdata) %in% supportcols]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(bf.test(thisFormula, data = inputdata), 
                               file = NULL, 
                               append = TRUE)
    cat(captured, sep = "\n")
  })
}

### ShinyApp UI elements ####
ui <- shinyUI(fluidPage(theme = shinytheme("slate"),
  
  # Edit various CSS features such as the Brown-Forsythe test box
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
      }
      #brownf {
        color: #c8c8c8;
        background: #2e3338;
        border: 1px solid #1c1e22;
      }
      #descri {
        overflow-x: auto;
        max-height: 80vh;
      }
      form.well {
        display: 100%;
        position: fixed;
        overflow: visible;
        overflow-y: auto;
        max-height: 90vh;
        max-width: 80vh;
      }"
    ))
  ),                    
                     
  titlePanel("Sampo Vesanen thesis statistics ShinyApp"),
  sidebarLayout(
    sidebarPanel(
      
      # walktime or parktime
      selectInput("resp", 
                 "Response (continuous)",
                 names(thesisdata[continuous])),
      
      # all others
      selectInput("expl",
                 "Explanatory (ordinal)", 
                 names(thesisdata[ordinal])),
      
      # Provide user possibility to see distribution of answers within the
      # ordinal variables.
      # The values of this conditionalPanel are changed with the observer
      # function
      conditionalPanel(
        condition = "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        selectInput("barplot",
                    "Y axis for barplot",
                    names(thesisdata[c("zipcode", "likert", "walktime")]),
      )),
      
      # these are changed with the observer function
      checkboxGroupInput("checkGroup", 
                         "Select inactive groups",
                         choiceNames = c("Item A", "Item B", "Item C"),
                         choiceValues = c("a", "b", "c")),
      checkboxGroupInput("subdivGroup",
                         "Select inactive subdivisions",
                         choiceNames = sort(as.character(unique(thesisdata$subdiv))),
                         choiceValues = sort(as.character(unique(thesisdata$subdiv)))),
      width = 3
    ),
  
    mainPanel(
      h3("Descriptive statistics"),
      p("If N is distributed somewhat equally, Levene test is not required."),
      tableOutput("descri"),
      hr(),
      
      h3("Histogram"),
      plotOutput("hist"),
      hr(),
      
      conditionalPanel(
        condition = "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        h3("Distribution of ordinal variables"),
        p("This plot appears when likert, parkspot or timeofday is selected as explanatory (ordinal) variable"),
        plotOutput("barplot"),
        hr()
      ),
      
      h3("Boxplot"),
      plotOutput("boxplot", height = "500px"),
      hr(),
      
      h3("Test of Homogeneity of Variances"),
      p("Levene value needs to be at least 0.05 for ANOVA test to be meaningful. If under 0.05, employ Brown-Forsythe test."),
      tableOutput("levene"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px"),
      hr(),
      
      h3("ANOVA"),
      tableOutput("anova"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
        style = "font-size:12px"),
      hr(),
      
      h3("Brown-Forsythe"),
      verbatimTextOutput("brownf"),
    )
  )
))
shinyApp(ui = ui, server = server)



#### Visitor ShinyApp ####
visitorpath <- file.path(wd, "leaflet_survey_results/visitors.csv")
visitordata <- read.csv(file = visitorpath,
                       colClasses = c(X = "integer", id = "integer", 
                                      ip = "factor", ts_first = "POSIXct", 
                                      ts_latest = "POSIXct", count = "integer"),
                       header = TRUE, sep = ",")

# NOW() in MySQL is UTC, change POSIXct object timezone to UTC+3, Helsinki
# summer time
visitordata$ts_first <- visitordata$ts_first + 3 * 60 * 60
visitordata$ts_latest <- visitordata$ts_latest + 3 * 60 * 60

# First sort by timestamp, then rename X or id. This fixes responses with 
# multiple zipcodes. These records have exact same timestamps, that proved 
# tricky for dygraphs to understand
thesisdata_for_xts <- thesisdata[order(thesisdata$timestamp, decreasing = FALSE), ]
thesisdata_for_xts$X <- seq(1:length(thesisdata_for_xts$X))

# Create xts object necessary to use dygraph
visitor_xts <- xts(x = visitordata$X, order.by = visitordata$ts_first)
records_xts <- xts(x = thesisdata_for_xts$X, order.by = thesisdata_for_xts$timestamp)



# Event timestamps. Manually picked. If many areas are in the same variable,
# the earliest timestamp is selected
twitter <- as.POSIXct("2019-05-07 10:43:00 EEST") 

# Maantieteen opiskelijat email group
mao <- as.POSIXct("2019-05-09 13:24:00 EEST") 

# email groups: Vasara, Resonanssi, Matrix, Geysir, Synop, Meridiaani, Tyyppi-arvo,
# HYK, TKO-ÄLY, Symbioosi, Helix, MYY, Sampsa, MYO, Lipidi, Vuorovaikeutus,
# YFK, Oikos
emails <- as.POSIXct("2019-05-14 10:58:00 EEST") 

# FB
lisaakaupunkia <- as.POSIXct("2019-05-15 10:33:00 EEST") 

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

# Vantaa Puskaradio, Sipoo-Sibbo, Järvenpää, WE <3 KERAVA, Tuusula, Nurmijärven 
# viidakkorumpu, Vihtiläiset, Korso, Käpylä Helsinki, Laajasalo, Vuosaari,
# ITÄ-HELSINKI, Lauttasaari, Haagan ilmoitustaulu, REMINDERS
muistutus <- as.POSIXct("2019-06-26 15:33:00 EEST") 

# they only accepted my message at this date for Nikinmäki and Puskaradio Espoo
nikinmaki <- as.POSIXct("2019-06-27 14:26:00 EEST")
puskaradioespoo <- as.POSIXct("2019-06-27 21:30:00 EEST")

# reminder
lisaakaupunkia2 <- as.POSIXct("2019-07-02 12:06:00 EEST")

# remind email groups except mao, GIS-velhot
misc3 <- as.POSIXct("2019-07-05 10:29:00 EEST")

visitor_server <- function(input, output) {
  output$dygraph <- renderUI({
    das <- list(
      dygraph(records_xts, main = "records", group = "thesis") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2) %>%
        dyRangeSelector(height = 70)  %>%
        
        dyEvent(twitter, "@Digigeolab, @AccessibilityRG", labelLoc = "bottom") %>%
        dyEvent(mao, "MaO") %>%
        dyEvent(emails, "Email groups") %>%
        dyEvent(lisaakaupunkia, "FB: Lisää kaupunkia Helsinkiin") %>%
        dyEvent(espoo1, "Espoo 1") %>%
        dyEvent(misc1, "Mankkaan lapsiperheet, Helsinki 1") %>%
        dyEvent(helsinki1, "Helsinki 1", labelLoc = "bottom") %>%
        dyEvent(helsinki2, "Helsinki 2", labelLoc = "bottom") %>%
        dyEvent(helsinki3, "Helsinki 3", labelLoc = "bottom") %>%
        dyEvent(helsinki4, "Helsinki 4", labelLoc = "bottom") %>%
        dyEvent(misc2, "Espoo 2, Vantaa, Alppikylä", labelLoc = "bottom") %>%
        dyEvent(peri, "Kehyskunnat", labelLoc = "bottom") %>%
        dyEvent(muistutus, "Muistutuksia", labelLoc = "bottom") %>%
        dyEvent(nikinmaki, "Vantaa Nikinmäki", labelLoc = "bottom") %>%
        dyEvent(puskaradioespoo, "Puskaradio Espoo", labelLoc = "bottom") %>%
        dyEvent(lisaakaupunkia2, "Muistutus, Lisää kaupunkia Helsinkiin", labelLoc = "bottom") %>%
        dyEvent(misc3, "emails reminder, GIS-velhot FB", labelLoc = "bottom"),
      
      dygraph(visitor_xts, main = "visitors", group = "thesis") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2) %>%
        dyRangeSelector(height = 70)  %>%
        
        dyEvent(twitter, "@Digigeolab, @AccessibilityRG", labelLoc = "bottom") %>%
        dyEvent(mao, "MaO") %>%
        dyEvent(emails, "Email groups") %>%
        dyEvent(lisaakaupunkia, "FB: Lisää kaupunkia Helsinkiin") %>%
        dyEvent(espoo1, "Espoo 1") %>%
        dyEvent(misc1, "Mankkaan lapsiperheet, Helsinki 1") %>%
        dyEvent(helsinki1, "Helsinki 1", labelLoc = "bottom") %>%
        dyEvent(helsinki2, "Helsinki 2", labelLoc = "bottom") %>%
        dyEvent(helsinki3, "Helsinki 3", labelLoc = "bottom") %>%
        dyEvent(helsinki4, "Helsinki 4", labelLoc = "bottom") %>%
        dyEvent(misc2, "Espoo 2, Vantaa, Alppikylä", labelLoc = "bottom") %>%
        dyEvent(peri, "Kehyskunnat", labelLoc = "bottom") %>%
        dyEvent(muistutus, "Muistutuksia", labelLoc = "bottom") %>%
        dyEvent(nikinmaki, "Vantaa Nikinmäki", labelLoc = "bottom") %>%
        dyEvent(puskaradioespoo, "Puskaradio Espoo", labelLoc = "bottom") %>%
        dyEvent(lisaakaupunkia2, "Muistutus, Lisää kaupunkia Helsinkiin", labelLoc = "bottom") %>%
        dyEvent(misc3, "emails reminder, GIS-velhot FB", labelLoc = "bottom"))
    
    browsable(tagList(das))
  })
}

visitor_ui <- basicPage(
  titlePanel("Received responses and survey page first visits"),
  uiOutput("dygraph")
)
shinyApp(visitor_ui, visitor_server)




#### Obsolete material ####

# All of the functionality below is superseded by the ShinyApp described above.
# Preserve these for the sake of alternatives and simplicity.


# Run tests with GetANOVA()

# We use GetANOVA() function, which performs multiple analyses with the 
# information fed to it. 

# Get walktime by timeofday
GetANOVA(walktime ~ timeofday, thesisdata$walktime, thesisdata$timeofday,
         thesisdata, c(1, 2, 3, 4))

# Get walktime by timeofday, remove "Can't specify"
GetANOVA(walktime ~ timeofday, thesisdata$walktime, thesisdata$timeofday,
         thesisdata[-which(as.integer(thesisdata$timeofday) == 4), ],
         c(1, 2, 3, 4))

# parktime by parkspot
GetANOVA(parktime ~ parkspot, thesisdata$parktime, thesisdata$parkspot,
         thesisdata, c(1, 2, 3, 4))

# parktime by subdivision
GetANOVA(parktime ~ subdiv, thesisdata$parktime, thesisdata$subdiv,
         thesisdata, c(1, 2, 3, 4))

# walktime by ykr zone
GetANOVA(walktime ~ ykr_zone, thesisdata$walktime, thesisdata$ykr_zone,
         thesisdata, c(1, 2, 3, 4))