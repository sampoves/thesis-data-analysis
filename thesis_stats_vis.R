

# Master's thesis statistical tests and visualisation
#####################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen

# Reference material for the tests
# https://en.wikipedia.org/wiki/One-way_analysis_of_variance
# https://stats.stackexchange.com/a/124618/262051
# https://rcompanion.org/handbook/E_01.html
# https://www.st-andrews.ac.uk/media/capod/students/mathssupport/OrdinalexampleR.pdf
# https://www.r-bloggers.com/box-plot-with-r-tutorial/
# http://www.biostathandbook.com/kruskalwallis.html



#### Initialise ####
rm(list = ls())

# These enable Brown-Forsythe test
#install.packages("onewaytests")
#install.packages("car")

# Libraries
library(onewaytests)
library(car)



#### Preparation ####

# Working path
wd <- "C:/Sampon/Maantiede/Master of the Universe"
datapath <- file.path(wd, "pythonrecords.csv")

# Read in csv
thesisdata <- read.csv(file = datapath,
                colClasses = c(zipcode = "character", ip = "character",
                               timeofday = "factor", parkspot = "factor",
                               likert = "factor"),
                header = TRUE, sep = ",")

# Name factor levels. These factor levels break some functionality down there
levels(thesisdata$parkspot) <- list("side of street" = 1,
                             "lot" = 2,
                             "garage" = 3,
                             "private-reserved" = 4,
                             "other" = 5)

 levels(thesisdata$likert) <- list("Extremely familiar" = 1,
                           "Moderately" = 2,
                           "Somewhat " = 3,
                           "Slightly" = 4,
                           "Not at all" = 5)

  levels(thesisdata$timeofday) <- list("Weekday, rush hour" = 1,
                              "Weekday, other than" = 2,
                              "Weekend" = 3,
                              "Can't specify" = 4)

# Remove two columns "X" and "index"
thesisdata <- subset(thesisdata, select = -c(X, index))

# timestampify
timefunc <- function(x) strptime(x, "%Y-%m-%d %H:%M:%S", tz = "Europe/Helsinki")
thesisdata["timestamp"] <- lapply(thesisdata["timestamp"], timefunc)



#### I will attempt to reconstruct SPSS tests for anova ####
# https://www.fsd.uta.fi/menetelmaopetus/varianssi/harjoitus1.html
# Quite successful!

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# https://datascienceplus.com/oneway-anova-explanation-and-example-in-r-part-1/
# http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r

# SPSS -> Analyze -> Compare means --> One-way anova 
# In "Options" select: Descriptive, Gomogeinity of variance test (Levene), 
# Brown-Forsythe

# One-way anova descriptives, describe() 
describe(walktime ~ timeofday, thesisdata[-c(1,2,3,4)])

# Levene test
# samat tulokset carista ja lawstatista
leveneTest(walktime ~ timeofday, thesisdata, center = mean) #car
#library("lawstat")
#levene.test(thesisdata$walktime, thesisdata$timeofday) #lawstat

# Oneway ANOVA
res.aov <- aov(walktime ~ timeofday, data = thesisdata)
summary(res.aov)
#TukeyHSD(res.aov)

# Brown-Forsythe test
bf.test(walktime ~ timeofday, data = thesisdata[-c(1,2,3,4)])





#### Kruskal Wallis test ####
# "The most common use of the Kruskal-Wallis test is when you have one nominal 
# variable and one measurement variable"

# http://www.biostathandbook.com/kruskalwallis.html
# measurement variable ~ nominal variable
kruskal.test(parktime ~ parkspot, data = thesisdata)

# anova
anova(lm(parktime ~ likert, data = thesisdata))

# chi-square test EI TOIMI
chisq.test(table(thesisdata$walktime, thesisdata$likert))



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

# How time of day compared to parkspot
barplot(table(thesisdata$timeofday, thesisdata$parkspot), beside = T,
        cex.names = 0.7, 
        names = c("Side of the road", "lot", "garage", "reserved/private", 
                  "other"),
        legend.text = c("Weekday, rush hour", "Weekday, other than rush hour", 
                        "Weekend", "None of the above, no usual time"),
        args.legend = list(x = 25, y = 900, cex = 0.8),
        col = c("pink", "light blue", "red", "blue"))

# How parktime compared to timeofday
barplot(table(thesisdata$parkspot, thesisdata$timeofday), beside = T,
        cex.names = 0.7, 
        names = c("Weekday, rush hour", "Weekday, other than rush hour", 
                  "Weekend", "None of the above, no usual time"),
        legend.text = c("Side of the road", "lot", "garage", "reserved/private", 
                        "other"),
        args.legend = list(x = 24, y = 800, cex = 0.8),
        col = c("pink", "light blue", "red", "blue", "green"))



#### Visualise 2 ####
# Histograms
hist(thesisdata$likert)
hist(thesisdata$timeofday)
hist(thesisdata$parkspot)



# T-Test
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



t.test(walktime ~ likert, data = thesisdata)