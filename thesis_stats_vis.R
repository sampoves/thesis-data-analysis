

# Master's thesis statistical tests and visualisation
#####################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 27.11.2019

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

# Libraries
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)



#### Preparation ####

# Working path
wd <- "C:/Sampon/Maantiede/Master of the Universe"
datapath <- file.path(wd, "pythonrecords.csv")
postal_path <- file.path(wd, "pythonpostal.csv")
source(file.path(wd, "python/thesis_stats_vis_funcs.R"))

# Read in csv data. Define column types
thesisdata <- read.csv(file = datapath,
                colClasses = c(zipcode = "character", ip = "character",
                               timeofday = "factor", parkspot = "factor",
                               likert = "factor", ua_forest = "factor",
                               ykr_zone = "factor", subdiv = "factor"),
                header = TRUE, sep = ",")

# Postal code data
postal <- read.csv(file = postal_path, 
                   colClasses = c(posti_alue = "factor", kunta = "factor"),
                   header = TRUE, sep = ",")


# Name factor levels. These factor levels break some functionality I wrote
# earlier
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

# Remove two columns "X" and "index". X from postal
thesisdata <- subset(thesisdata, select = -c(X, index))
postal <- postal[-c(1, 5, 6)]

# Timestampify
timefunc <- function(x) strptime(x, "%Y-%m-%d %H:%M:%S", tz = "Europe/Helsinki")
thesisdata["timestamp"] <- lapply(thesisdata["timestamp"], timefunc)




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
    
    available <- c("likert", "parkspot", "timeofday")
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
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), -c(1, 2, 3, 4)]
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
    
    inputdata <- thesisdata[!thesisdata[[explanatorycol]] %in% c(input$checkGroup), ]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    legendnames <- levels(unique(inputdata[[explanatorycol]]))
    
    # ggplot2. Rotate labels if enough classes
    if(length(legendnames) > 5){
      
      p <- ggplot(inputdata, aes_string(x=input$expl, y=input$resp)) + 
        geom_boxplot() + theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
    } else {
      
      p <- ggplot(inputdata, aes_string(x=input$expl, y=input$resp)) + 
        geom_boxplot()
    }
    p
    
  })

  
  #### Barplot ####
  output$barplot <- renderPlot({
    
    # See distribution of ordinal variables through a grouped bar plot
    explanatorycol <- input$expl
    barplotval <- input$barplot
    inputdata <- thesisdata[!thesisdata[[explanatorycol]] %in% c(input$checkGroup), ]
    yax <- paste("sum of", barplotval)
    maximum <- max(tapply(thesisdata[, barplotval], thesisdata[, barplotval], length))
    
    # Draw ggplot2 plot
    plo <- ggplot(inputdata, aes(x = get(explanatorycol), y = factor(get(barplotval)), fill = get(barplotval))) +
      geom_bar(aes(y = stat(count)), position = "dodge") + 
      scale_y_continuous(breaks = seq(0, maximum, by = 200),
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
    
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), -c(1, 2, 3, 4)]
    inputdata <- inputdata[!inputdata$subdiv %in% c(input$subdivGroup), ]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(bf.test(thisFormula, data = inputdata), 
                               file = NULL, 
                               append = TRUE)
    cat(captured, sep = "\n")
  })
}

# ShinyApp UI elements
ui <- shinyUI(fluidPage(theme = shinytheme("slate"),
  
  # Edit various CSS features such as the Brown-Forsythe test box
  tags$head(
    tags$style(HTML("
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
        position: fixed;
        overflow: visible;
        overflow-y: auto;
        max-height: 80vh;
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
                 names(thesisdata[-c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)])),
      
      # all others
      selectInput("expl",
                 "Explanatory (ordinal)", 
                 names(thesisdata[-c(1, 2, 3, 4, 7, 8)])),
      
      # Provide user possibility to see distribution of answers within the
      # ordinal variables.
      # The values of this conditionalPanel are changed with the observer
      # function
      conditionalPanel(
        condition = "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        selectInput("barplot",
                    "Barplotti",
                    names(thesisdata[c(5, 6, 9)]),
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
        p("This plot appears when likert, parkspot or timeofday is selected as explanatory variable"),
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



# Visualise

# How familiar have each parking spots been to respondents?
barplot(table(thesisdata$likert, thesisdata$parkspot), beside = T,
        cex.names = 0.7, 
        names = c("Side of the road", "lot", "garage", "reserved/private", 
                  "other"),
        legend.text = c("Extremely familiar", "Moderately familiar", 
                        "Somewhat familiar", "Slightly familiar", 
                        "Not at all familiar"),
        args.legend = list(x = 30, y = 1000, cex = 0.8),
        col = c("pink", "light blue", "red", "blue", "green"))


# ggplot2 method
ggplot(thesisdata, aes(x = parkspot, y = factor(likert), fill = likert)) +
  geom_bar(aes(y = stat(count)), position = "dodge") + 
  scale_y_continuous(breaks = seq(0, max(tapply(thesisdata$likert, thesisdata$likert, length)), by = 200),
                     expand = expand_scale(mult = c(0, .1))) +
  theme(legend.position = "right")


# time of day compared to parkspot
barplot(table(thesisdata$timeofday, thesisdata$parkspot), beside = T,
        cex.names = 0.7, 
        names = c("Side of the road", "lot", "garage", "reserved/private", 
                  "other"),
        legend.text = c("Weekday, rush hour", "Weekday, other than rush hour", 
                        "Weekend", "None of the above, no usual time"),
        args.legend = list(x = 25, y = 900, cex = 0.8),
        col = c("pink", "light blue", "red", "blue"))

# parktime compared to timeofday
barplot(table(thesisdata$parkspot, thesisdata$timeofday), beside = T,
        cex.names = 0.7, 
        names = c("Weekday, rush hour", "Weekday, other than rush hour", 
                  "Weekend", "None of the above, no usual time"),
        legend.text = c("Side of the road", "lot", "garage", "reserved/private", 
                        "other"),
        args.legend = list(x = 24, y = 800, cex = 0.8),
        col = c("pink", "light blue", "red", "blue", "green"))
