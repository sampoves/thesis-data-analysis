

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

# Libraries
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(knitr)
library(kableExtra)


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








# KableExtra test
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
desc <- describe(walktime ~ timeofday, thesisdata[-c(1, 2, 3, 4)])





# TOIMII
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/
#install.packages("shiny")
library(shiny)
library(stats)

# ui = fluidPage(
#   fluidRow(column(1, HTML(kable(desc))),
#            column(24, HTML(kable(desc))))
# )
# shinyApp(ui = ui, server = server)



server <- function(input, output){
  
  output$mytable <- renderTable({
    describe(as.formula(paste(input$resp, '~', input$expl)), thesisdata[-c(1, 2, 3, 4)])
  }, striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  output$formu <- renderText({
    paste(input$resp, "~", input$expl)
  })
}

ui <- shinyUI(fluidPage(
  
  titlePanel("thing"),
  
  sidebarLayout(
    sidebarPanel(
      
      #walktime or parktime
      selectInput("resp", 
                 "response (continuous)", 
                 names(thesisdata[-c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)])),
     
      # all others
      selectInput("expl", 
                 "explanatory (ordinal)", 
                 names(thesisdata[-c(1, 2, 3, 4, 7, 8)]))
    ),
  
    mainPanel(
      h3("Descriptive statistics"),
      tableOutput("mytable"),
      textOutput("formu"),
    )
  )
))

shinyApp(ui = ui, server = server, options = list("test.mode"))















desc %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                      "responsive"), full_width = F)
mtcars %>%
  kable()  %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                      "responsive"), full_width = F)

print(kable(desc, format="html") %>% 
        kable_styling(full_width=FALSE) %>% 
        collapse_rows(columns=1, valign="top"))






a <- 1:5
markobj <- c('---',
             'title: "test"',
             'output: html_document',
             '---',
             '',
             '## R Markdown',
             '',
             'This is an R Markdown document.',
             '```{r}',
             'b <- 11:15',
             "desc %>% kable() %>% kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\", \"responsive\"))",
             "```",
             "mtcars %>% kable() %>% kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\", \"responsive\"))")

markdown::markdownToHTML(text = knitr::knit(text = markobj), output = "test.html")

browseURL("test.html")



# test343

filepath_to_markdown <- file.path(wd, "python/anova_markdown.R")

ANOVAreport <- function(filepath_to_markdowndesc, levene, anovasummary, 
                        brownforsythe) {
  
  testtt <- sprintf(filepath_to_markdown, desc, levene, anovasummary, 
                    brownforsythe)
  
  markdown::markdownToHTML(text = knitr::knit(text = testtt), output = "test.html")
  browseURL("test.html")
}












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