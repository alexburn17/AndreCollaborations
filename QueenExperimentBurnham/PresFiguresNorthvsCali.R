


# Preliminaries:

# Clear memory of characters:
ls()
rm(list=ls())

# Set Working Directory: 
setwd("~/AndreCollaborations/QueenExperimentBurnham")

# Read in Data:
QueenDF <- read.table("2016QueensHam.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE) 



# These dataframes are seperate becuase they do not follow that sampling 
# pattern that the other four varaibles in the previous data frame do (i.e.5
# time steps):
PollenDF <- read.table("PollenQueens.csv", header=TRUE, sep = ",") 
VirusDF <- read.table("RNAVirus.csv", header=TRUE, sep = ",") 

# load plyr for data manipulation and ggplot plotting package and other 
# related packages:
library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)





# summary stats for plotting purposes:
MassSummary <- ddply(QueenDF, c("Origin", "MassDay"), summarise, 
                     n = length(Mass),
                     mean = mean(Mass, na.rm = TRUE),
                     sd = sd(Mass, na.rm = TRUE),
                     se = sd / sqrt(n))

# remove time steps with missing data:
MassSummary <- MassSummary[-c(5,10),]
print(MassSummary)

#------------------------------------------------------------------------


#Create plot in ggplot for brood 
plot1 <- ggplot(data = MassSummary, 
                aes(x = MassDay, 
                    y = mean, 
                    group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (days after requeening)", y = "Colony Mass (lbs)") + coord_cartesian(ylim = c(2, 7), xlim = c(-10,70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + theme(legend.position=c(3, 3), panel.border = element_blank(), axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_text(margin=margin(0,20,0,0))) + scale_x_continuous(breaks=pretty_breaks(n=10)) 

#------------------------------------------------------------------------
# BROOD

# summary stats for plotting purposes:
BroodSummary <- ddply(QueenDF, c("Origin", "BroodDay"), summarise, 
                      n = length(Brood),
                      mean = mean(Brood, na.rm = TRUE),
                      sd = sd(Brood, na.rm = TRUE),
                      se = sd / sqrt(n))
print(BroodSummary)

#------------------------------------------------------------------------

#Create plot in ggplot for brood 
plot1 <- ggplot(data = BroodSummary, 
                aes(x = BroodDay, 
                    y = mean, 
                    group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (days after requeening)", y = "Frames of Brood") + coord_cartesian(ylim = c(2, 7), xlim = c(-10,70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + theme(legend.position=c(3, 3), panel.border = element_blank(), axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_text(margin=margin(0,20,0,0))) + scale_x_continuous(breaks=pretty_breaks(n=10)) 
#------------------------------------------------------------------------
