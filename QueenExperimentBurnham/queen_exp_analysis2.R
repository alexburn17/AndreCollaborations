# Metadata
# Author: P. Alexander Burnham
# Date: 10 August 2016

# Data Set: These data were collected in 2016 in New York state by Andre Burnham and the Hamilton College bee research team from two yards in NY owned by one beekeeper who volunteered for the study.

# Data Source: 2016 Hamilton Queen Origin Research Project

# Funding Source: Hamilton College undergraduate research grants

# Data Collection: Weights of colonies were obtained by summing the weights of individual hive bodies and supers for each colony at each yard (add more about protocols for other assays).

# Columns: (from left to right) FieldID: Id including number location and treatment, Time: Generic Time Steps (T1-T5), Origin (California and Local), Yard (1 or 2),Nosema (spores per bee) only three time steps,Varroa (mites per 300 bees),Brood (frames of brood in colony),NosemaPA (binary data for nosema),VarroaPA (binary data for mites),MassDay(days after requeening for mass),NosemaDay days after requeening for nosema),VarroaDay days after requeening for varroa),BroodDay days after requeening for brood),MassDate (dates sampled),NosemaDate(dates sampled),VarroaDate(dates sampled),BroodDate (dates sampled)

# Rows: Data points for all columns in order from each collection event

# Missing values: NA

#------------------------------------------------------------------------

# P. Alexander Burnham
# 27, December 2016
# Queen Experiment (Andre)
# Data Analysis (Figures and Stats) 

#------------------------------------------------------------------------
# Preliminaries:

# Clear memory of characters:
ls()
rm(list=ls())

# Set Working Directory: 
setwd("~/Desktop/QueenExperimentBurnham")

# Read in Data:
QueenDF <- read.table("2016QueensHam.csv", header=TRUE, sep = ",") 


# These dataframes are seperate becuase they do not follow that sampling pattern that the other four varaibles in the previous data frame do (i.e. 5 time steps)
PollenDF <- read.table("PollenQueens.csv", header=TRUE, sep = ",") 
VirusDF <- read.table("RNAVirus.csv", header=TRUE, sep = ",") 

#------------------------------------------------------------------------
# STATISTICS:

# MASS - use repeated measures anova to look at differences between local and california through time:
aov.out <- aov(Mass~(Origin * Time) + Error(Yard/Time), data=QueenDF)

# look at summary of model:  SIGNIFICANT
summary(aov.out)

#---

# NOSEMA - use repeated measures anova to look at differences between local and california through time: 
aov.out <- aov(Nosema ~ (Origin * Time) + Error(Yard/Time), data=QueenDF)

# look at summary of model: SIGNIFICANT
summary(aov.out)

#---

# VARROA - use repeated measures anova to look at differences between local and california through time:  
aov.out <- aov(Varroa ~ (Origin * Time) + Error(Yard/Time), data=QueenDF)

# look at summary of model:  NOT SIGNIFICANT
summary(aov.out)

#---

# BROOD - use repeated measures anova to look at differences between local and california through time:  
aov.out <- aov(Brood ~ (Origin * Time) + Error(Yard/Time), data=QueenDF)

# look at summary of model: SIGNIFICANT
summary(aov.out)

#------------------------------------------------------------------------
# FIGURES:

# load plyr for data manipulation and ggplot plotting package
library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)




#------------------------------------------------------------------------
# VARROA

# summary stats for plotting purposes:
VarroaSummary <- ddply(QueenDF, c("Origin", "VarroaDay"), summarise, 
                     n = length(Varroa),
                     mean = mean(Varroa, na.rm = TRUE),
                     sd = sd(Varroa, na.rm = TRUE),
                     se = sd / sqrt(n))

# remove time steps with missing data:
VarroaSummary <- VarroaSummary[-c(5,10),]
print(VarroaSummary)

library(ggplot2)
#Create varroa line graph in ggplot 
plot <- ggplot(data = VarroaSummary, 
               aes(x = VarroaDay, 
                   y = mean, 
                   group = Origin, 
                   colour = Origin)
) + geom_line(size=1.5) + geom_point(size=4) + scale_colour_manual(values = c("dodgerblue4", "black")) + labs(x = "Time (days after requeening)", y = "Varroa Load (mites/300 bees)") + coord_cartesian(ylim = c(0, 12)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = .8)) + scale_fill_brewer(palette = "Paired") + theme_minimal(base_size = 17)  









#Create plot in ggplot for varroa:
plot2 <- ggplot(data = VarroaSummary, 
               aes(x = VarroaDay, 
                   y = mean, 
                   group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (days after requeening)", y = "Varroa Load (mites/100 bees)") + coord_cartesian(ylim = c(0, 10), xlim = c(0,70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_minimal(base_size = 17) + theme(legend.position=c(.15, .75)) + scale_x_continuous(breaks=pretty_breaks(n=10)) 





