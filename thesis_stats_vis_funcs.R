

# Master's thesis statistical tests and visualisation
#### Stats and visualisation functions

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 27.11.2019

# Initialise
library(onewaytests)
library(car)
library(plotrix)
library(moments)



GetANOVA <- function(thisFormula, response, explanatory, inputdata, 
                     columnsToRemove) {
  
  # NB! Make note that this function is superseded by the ShinyApp described in
  # thesis_stats_vis.R.
  #
  # Get all parts of ANOVA we need in this thesis:
  #
  # Descriptives
  # Levene test
  # One-way ANOVA
  # Brown-Forsythe test
  #
  # Input:  thisFormula:  formula as formula object
  #         response:     column name as string (continuous variable)
  #         explanatory:  column name as string (ordinal variable)
  #         inputdata:    dataframe, inputdata in this thesis
  #         columnsToRemove: This determines which column indices are to be
  #                       removed from analyses

  
  #### Descriptive statistics ####
  
  # In SPSS, follow these steps to get equal results
  # SPSS -> Analyze -> Compare means --> One-way anova 
  # In "Options" select: Descriptive, Homogeinity of variance test (Levene), 
  # Brown-Forsythe
  
  # One-way anova descriptives, describe()
  # Order of columns in SPSS: "N", "Mean", "Std. Deviation", "Std. Error",
  # "95 % Confidence Interval for Mean" ("Lower Bound", "Upper Bound"), "Minimum",
  # "Maximum"
  desc <- describe(thisFormula, inputdata[-columnsToRemove])
  
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
  
  #### Levene test ####
  levene <- leveneTest(thisFormula, inputdata, center = mean) #car
  leveneVal <- levene[[3]][1]
  
  #### One-way ANOVA ####
  res.aov <- aov(thisFormula, data = inputdata)
  anovasummary <- summary(res.aov)
  
  #### Brown-Forsythe test #### 
  # Need to remove first four columns for this to work
  # not in use, we only need to run this in the printing section
  #brownf <- bf.test(thisFormula, data = inputdata[-columnsToRemove])
  
  

  #### Printing of results ####
  
  # Desc
  cat("\n--------------------------\n")
  cat("# Descriptive Statistics #")
  cat("\n--------------------------\n")
  print("If N is distributed somewhat equally, Levene test is not required.")
  cat("\n")
  print(desc)
  
  # Levene
  cat("\n\n------------------------------------\n")
  cat("# Test of Homogeneity of Variances #")
  cat("\n------------------------------------\n\n")
  
  if(leveneVal < 0.05){
    print(paste0("Levene test is under 0.05 (", 
                 round(leveneVal, 5), 
                 "), need to perform Brown-Forsythe"))
    cat("\n")
  }
  
  print(levene)
  
  # ANOVA
  cat("\n\n---------\n")
  cat("# ANOVA #")
  cat("\n---------\n\n")
  print(anovasummary)
  
  # Brown-Forsythe
  cat("\n\n-------------------------------------\n")
  cat("# Robust Tests of Equality of Means #")
  cat("\n-------------------------------------\n")
  bf.test(thisFormula, data = inputdata[-columnsToRemove])
}



SigTableToShiny <- function(sigTable, hasHeading) {
  
  # Use this function to show significance tables in Shiny. It will be useful
  # with Levene and ANOVA results. 
  
  # Due to the format of the significance table it is difficult to present it
  # in Shiny. The main functionality of this method is to make the significance
  # star available for the app.
  
  # Levene test dataframe requires transposing. Levene table
  # has an attribute heading while ANOVA doesn't. Use this.
  if (is.null(attributes(sigTable)$heading)) {
    # ANOVA
    res <- as.data.frame(do.call(rbind, sigTable))
  } else {
    # Levene
    res <- t(as.data.frame(do.call(rbind, sigTable)))
  }
  
  # Take into account that the table may have an attribute heading. Ask user
  # if this is the case
  if (hasHeading == FALSE){
    sigTablePosition = 2
  } else {
    sigTablePosition = 3
  }
  
  # Get the location of the signif.star
  signif_ncol <- ncol(read.table(textConnection(
    capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE))
  
  # get signif.star
  signif_star <- read.table(textConnection(
    capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE)[[signif_ncol]]
  
  # Detect if signif_star is something else than factor. If so, the function
  # has picked up a value from probability column and the current analysis is 
  # not significant. Change value to " ".
  if(!is.factor(signif_star)){
    signif_star <- " "
  }
  
  # repeated_na takes into account that the significance table may have more
  # rows than two.
  repeated_na <- rep("NA", nrow(res) - 1)
  signif_star <- c(as.character(signif_star), repeated_na)
  
  # bind column signif_star to result.
  res <- cbind.data.frame(res, signif_star)
  
  # Name rows. Try to detect differences in Levene and ANOVA summary tables.
  if(is.null(rownames(sigTable[[1]]))){
    # Levene
    rownames(res) <- rownames(sigTable)
  } else {
    # ANOVA
    rownames(res) <- rownames(sigTable[[1]])
  }
  
  return(res)
}