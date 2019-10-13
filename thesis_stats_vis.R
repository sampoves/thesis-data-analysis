

# Master's thesis statistical tests and visualisation
#####################################################

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen

# Reference material for the tests
#https://en.wikipedia.org/wiki/One-way_analysis_of_variance
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
levels(thesisdata$parkspot) <- list("On the side of street" = 1,
                                    "Parking lot" = 2,
                                    "Parking garage" = 3,
                                    "Private or reserved" = 4,
                                    "other" = 5)

levels(thesisdata$likert) <- list("Extremely familiar" = 1,
                                  "Moderately familiar" = 2,
                                  "Somewhat familiar" = 3,
                                  "Slightly familiar" = 4,
                                  "Not at all familiar" = 5)

levels(thesisdata$timeofday) <- list("Weekday, rush hour" = 1,
                                     "Weekday, other than rush hour" = 2,
                                     "Weekend" = 3,
                                     "Can't specify, no usual time" = 4)

# Remove two columns "X" and "index"
thesisdata <- subset(thesisdata, select = -c(X, index))

# Timestampify
timefunc <- function(x) strptime(x, "%Y-%m-%d %H:%M:%S", tz = "Europe/Helsinki")
thesisdata["timestamp"] <- lapply(thesisdata["timestamp"], timefunc)



#### Descriptive statistics ####

# SPSS -> Analyze -> Compare means --> One-way anova 
# In "Options" select: Descriptive, Gomogeinity of variance test (Levene), 
# Brown-Forsythe

# One-way anova descriptives, describe()
# Order of columns in SPSS: "N", "Mean", "Std. Deviation", "Std. Error",
# "95 % Confidence Interval for Mean" ("Lower Bound", "Upper Bound"), "Minimum",
# "Maximum"
desc <- describe(walktime ~ timeofday, thesisdata[-c(1,2,3,4)])

### Std. Error
# clumsily calculate mean, so that we can preserve column names in the next
# phase
stder <- aggregate(walktime ~ timeofday, data = thesisdata,
                   FUN = function(x) c(mean = mean(x), "Std.Error" = std.error(x)))

# Remove column mean 
stder <- subset(stder[[2]], select = -mean)
desc <- cbind(desc, stder)

# Confidence intervals
confs <- aggregate(
        walktime ~ timeofday, data = thesisdata, 
        FUN = function(x) c("CI for mean, Lower Bound" = mean(x) - 2 * std.error(x), 
                            "CI for mean, Upper Bound" = mean(x) + 2 * std.error(x)))
confs <- confs[[2]]
desc <- cbind(desc, confs)

# reorder to SPSS order
desc <- desc[c("n", "Median", "Mean", "Std.Dev", "Std.Error", 
               "CI for mean, Lower Bound", "CI for mean, Upper Bound", "Min", 
               "Max", "25th", "75th", "Skewness", "Kurtosis", "NA")]

# Add total row. We will add total values for all columns in this inconvenient
# manner.
vect <- c()
vect[1] <- sapply(1, function(x) sum(desc[, x]))
vect[2] <- sapply(2, function(x) median(desc[, x]))
vect[3] <- sapply(3, function(x) mean(desc[, x]))
vect[4] <- sd(thesisdata$walktime)
vect[5] <- std.error(thesisdata$walktime)
vect[6] <- mean(thesisdata$walktime) - 2 * std.error(thesisdata$walktime)
vect[7] <- mean(thesisdata$walktime) + 2 * std.error(thesisdata$walktime)
vect[8] <- min(thesisdata$walktime)
vect[9] <- max(thesisdata$walktime)
vect[10] <- quantile(thesisdata$walktime)[2]
vect[11] <- quantile(thesisdata$walktime)[4]
vect[12] <- skewness(thesisdata$walktime)
vect[13] <- kurtosis(thesisdata$walktime)
vect[13] <- sapply(14, function(x) sum(is.na(desc[, x])))

# Add all values vector to desc, then name the new row and round all values in
# desc.
desc <- rbind(desc, vect)
row.names(desc)[5] <- "Total"
desc <- round(desc, 3)



#### Levene test ####
leveneTest(walktime ~ timeofday, thesisdata, center = mean) #car

#### One-way ANOVA ####
res.aov <- aov(walktime ~ timeofday, data = thesisdata)
summary(res.aov)

#### Brown-Forsythe test #### 
# Need to remove first four columns for this to work
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