

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
library(tcltk)



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







# Tcl tk is so hard
# Try a tcktk window solution to printing, highly experimental
desc <- describe(walktime ~ timeofday, thesisdata[-c(1, 2, 3, 4)])

tt <- tktoplevel()
txtWidget <- tktext(tt, height=75, width=200)
tkpack(txtWidget)
tkinsert(txtWidget, 
         "end", 
         paste("\n--------------------------\n# Descriptive Statistics #",
               "\n--------------------------\n",
               "If N is distributed somewhat equally, Levene test is not required.",
               "\n",
               desc),
         collapse="\n")



# 2
# THIS WAS HARD!

# https://tkdocs.com/tutorial/grid.html
# https://www.tutorialspoint.com/tcl-tk/tk_text_widget.htm
# https://stackoverflow.com/a/32724993/9455395
# https://stackoverflow.com/a/2398906/9455395
# https://www.tcl.tk/community/hobbs/tcl/capp/tkTable/tkTable.html
# https://stackoverflow.com/a/48361639/9455395
# https://stat.ethz.ch/pipermail/r-help/2003-July/036982.html
# https://www.tutorialspoint.com/tcl-tk/tk_widgets_overview.htm
# https://www.mishcon.com/assets/managed/docs/downloads/doc_2322/tkref803-bklt-ltr.pdf

require(tcltk)
tclRequire("Tktable")

toTclArray <- function(dsn, dig = 2) {
        
  # http://r.789695.n4.nabble.com/Tck-tk-help-td1837711.html
  # Converts Data Frame/Matrix to a Tcl Array for Use in Displaying Tables 
  # dsn is the data set name 
  # dig is the number of digits to round to
        
  tclarray1 <- tclArray() 
  
  # iterate rows
  for (row_id in 0:(dim(dsn)[1])) {
          
    # iterate columns
    for (col_id in 0:(dim(dsn)[2] - 1)) {
            
      # First row is set to column names to be used as labels 
      if (row_id == 0) {
              
        # col_id + 1 makes sure the first row is for rownames
        tclarray1[[row_id, col_id + 1]] <- colnames(dsn)[col_id + 1] # new +1!

      } else {
        tem <- dsn[row_id, col_id + 1]
        
        # Generate rownames as ordinary cells
        if(col_id == 0) {
          tclarray1[[row_id, col_id]] <- rownames(desc)[row_id]
        }
        
        # col_id + 1 make sure the first row is for rownames
        tclarray1[[row_id, col_id + 1]] <- ifelse(is.na(tem), ".", # new +1!
                                                  ifelse(is.numeric(tem),
                                                         round(tem, digits = dig),
                                                         as.character(tem)))
      }
    }
  }
  return (tclarray1) 
}

temptable <- toTclArray(desc)



root <- tktoplevel()
tktitle(tt) <- "jeejee"
frame <- tkframe(root, borderwidth = 5, relief = "sunken", width = 200, height = 500)
namelabel <- tklabel(root,
                     text = paste("--------------------------\n# Descriptive Statistics #",
                                  "\n--------------------------\n",
                                  "If N is distributed somewhat equally, Levene test is not required."))
table1 <- tkwidget(root, "table",
                   width = 4500,
                   height = 2200,
                   variable = temptable,
                   rows = dim(desc)[1] + 1,
                   cols = dim(desc)[2],
                   titlerows = 1,
                   titlecols = 1,
                   colstretchmode = "all",
                   state = "disabled", # prevent editing of data
                   selectmode = "extended",
                   resizeborders = "none", # prevent resizing
                   colwidth = 10)

# txtWidget <- tktext(root,
#                     height = 6, 
#                     width = 80,
#                     relief = "flat",
#                     background = "lightgrey")
# tkinsert(txtWidget,
#          "end",
#          paste("--------------------------\n# Descriptive Statistics #",
#                "\n--------------------------\n",
#                "If N is distributed somewhat equally, Levene test is not required."),
#          collapse="\n")

tkgrid(namelabel, 
       column = 0, 
       row = 0)

# tkgrid(txtWidget,
#        column = 0,
#        row = 0,
#        pady = 5,
#        padx = 5)

tkgrid(table1,
       column = 0,
       row = 1,
       pady = 20,
       padx = 30)

# Change color of cell
tcl(table1, "tag", "celltag", "OneOne", "1,1")
tcl(table1, "tag", "configure", "OneOne", background="red")

# try to resize specific column
#tcl(table1, "tag", "coltag", "col0", "1")
#tcl(table1, "tag", "configure", "col0", background="blue")

# try to resize specific column
tcl(table1, "tag", "coltag", "col0", "1")
tcl(table1, "tag", "configure", "col0", borderwidth=3)

# try to resize specific column
tcl(table1, "tag", "coltag", "col03", "1")
tcl(table1, "tag", "cget", "col03", "configure")
#tcl(table1, "tag", "configure", "col03", cget=3)



# KableExtra

library(knitr)
library(kableExtra)

desc %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                      "responsive"))



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