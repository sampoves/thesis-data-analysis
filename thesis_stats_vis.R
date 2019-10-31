

# Master's thesis statistical tests and visualisation
#####################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 13.10.2019

# Reference material for the tests
#https://stats.stackexchange.com/a/124618/262051
#https://rcompanion.org/handbook/E_01.html
#https://www.st-andrews.ac.uk/media/capod/students/mathssupport/OrdinalexampleR.pdf
#https://www.r-bloggers.com/box-plot-with-r-tutorial/
#http://www.biostathandbook.com/kruskalwallis.html

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



#### Run tests with GetANOVA() ####

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









# ShinyApp
server <- function(input, output, session){
  
  # Listener function. Detect changes in selectInput to modify available
  # check boxes.
  observe({
    x <- input$expl

    updateCheckboxGroupInput(session, "checkGroup", 
      # choices = unique(thesisdata[, x]),
      # selected = tail(x, 1)
      label = NULL, 
      choiceNames = levels(thesisdata[, x]),
      choiceValues = levels(thesisdata[, x]),
      #choices = levels(thesisdata[, x]),
      #selected = levels(thesisdata[, x]) # all
    )
  })

  # DESCRIPTIVE STATISTICS
  output$descri <- renderTable({
    
    # Render descriptive statistics
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    responsecol <- input$resp
    response <- thesisdata[[responsecol]]
    colname <- input$expl
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), -c(1, 2, 3, 4)]
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
    
    # show
    desc
  }, 
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  # LEVENE TEST
  output$levene <- renderTable({
    
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), ]
    
    levene <- leveneTest(thisFormula, inputdata, center = mean) #car
    #leveneVal <- levene[[3]][1]

    res <- SigTableToShiny(levene, TRUE)
    
    # signif.code row, not in use. Just copied as text to the UI
    #signig_codes <- read.table(textConnection(capture.output(levenet)[6]), fill = TRUE)
    #signig_codes <- apply(signig_codes, 1, paste, collapse = " ")
    
    # show
    res
    
  }, 
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  output$anova <- renderTable({
    
    # needed variables
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), ]
    
    #### One-way ANOVA ####
    res.aov <- aov(thisFormula, data = inputdata)
    anovasummary <- summary(res.aov)
    
    # Use this function to communicate table correctly to Shiny
    anovasummary <- SigTableToShiny(anovasummary, FALSE)
    
    # show
    anovasummary
  },
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  output$brownf <- renderPrint({
    
    # needed variables
    colname <- input$expl
    thisFormula <- as.formula(paste(input$resp, '~', input$expl))
    inputdata <- thesisdata[!thesisdata[[colname]] %in% c(input$checkGroup), -c(1, 2, 3, 4)]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(bf.test(thisFormula, data = inputdata), 
                               file = NULL, 
                               append = TRUE)
    cat(captured, sep = "\n")
  })
  
  output$formu <- renderText({
    paste(input$resp, "~", input$expl)
  })
}

ui <- shinyUI(fluidPage(theme = shinytheme("slate"),
  
  # Edit CSS features of Brown-Forsythe test box
  tags$head(
    tags$style(HTML("
      #brownf {
        color: #c8c8c8;
        background: #2e3338;
        border: 1px solid #1c1e22;
      }"
    ))
  ),                      
                        
  titlePanel("Sampo Vesanen thesis statistics ShinyApp"),
  sidebarLayout(
    sidebarPanel(
      
      #walktime or parktime
      selectInput("resp", 
                 "response (continuous)",
                 names(thesisdata[-c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)])),
      # all others
      selectInput("expl",
                 "explanatory (ordinal)", 
                 names(thesisdata[-c(1, 2, 3, 4, 7, 8)])),
      
      checkboxGroupInput("checkGroup", 
                         "Select inactive groups",
                         choiceNames = c("Item A", "Item B", "Item C"),
                         choiceValues = c("a", "b", "c")),
      width = 3
    ),
  
    mainPanel(
      h3("Descriptive statistics"),
      p("If N is distributed somewhat equally, Levene test is not required."),
      tableOutput("descri"),
      hr(),
      h3("Test of Homogeneity of Variances"),
      tableOutput("levene"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"),
      hr(),
      h3("ANOVA"),
      tableOutput("anova"),
      p("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"),
      hr(),
      h3("Brown-Forsythe"),
      verbatimTextOutput("brownf"),
      textOutput("formu"),
    )
  )
))
shinyApp(ui = ui, server = server, options = list("test.mode"))










#### Visualise ####

boxplot(likert ~ parktime, data = thesisdata)

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



#### Visualise 2 ####
# Histograms. Only works with numeric data
hist(thesisdata$walktime)
hist(thesisdata$parktime)



# Boxplots

# parktime compared to likert
boxplot(parktime ~ likert, 
        data = thesisdata, 
        names = c("Extremely familiar", "Moderately familiar", 
                  "Somewhat familiar", "Slightly familiar", 
                  "Not at all familiar"))

# walktime compared to likert
boxplot(walktime ~ likert, 
        data = thesisdata, 
        names = c("Extremely familiar", "Moderately familiar", 
                  "Somewhat familiar", "Slightly familiar", 
                  "Not at all familiar"))

# parkspot
boxplot(parktime ~ parkspot, 
        data = thesisdata, 
        names = c("Side of the road", "lot", "garage", "reserved/private", 
                  "other"))

boxplot(walktime ~ parkspot, 
        data = thesisdata, 
        names = c("Side of the road", "lot", "garage", "reserved/private", 
                  "other"))

#timeofday
boxplot(parktime ~ timeofday, 
        data = thesisdata, 
        names = c("Weekday, rush hour", "Weekday, other than rush hour", 
                  "Weekend", "None of the above, no usual time"))

boxplot(walktime ~ timeofday, 
        data = thesisdata, 
        names = c("Weekday, rush hour", "Weekday, other than rush hour", 
                  "Weekend", "None of the above, no usual time"))