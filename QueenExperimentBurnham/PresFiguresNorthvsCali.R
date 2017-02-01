


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
plot <- ggplot(data = MassSummary, 
                aes(x = MassDay, 
                    y = mean, 
                    group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (days after requeening)", y = "Colony Mass (lbs)") + coord_cartesian(ylim = c(20,70), xlim = c(5,70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + theme(legend.position=c(3, 3), panel.border = element_blank(), axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_text(margin=margin(0,20,0,0))) + scale_x_continuous(breaks=pretty_breaks(n=10)) 

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



# summary stats for plotting purposes:
VarroaSummary <- ddply(QueenDF, c("Origin", "VarroaDay"), summarise, 
                       n = length(Varroa),
                       mean = mean(Varroa, na.rm = TRUE),
                       sd = sd(Varroa, na.rm = TRUE),
                       se = sd / sqrt(n))

# remove time steps with missing data:
VarroaSummary <- VarroaSummary[-c(5,10),]
print(VarroaSummary)

#------------------------------------------------------------------------

ggplot(data = VarroaSummary, 
       aes(x = VarroaDay, 
           y = mean, 
           group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (days after requeening)", y = "Varroa Load (mites/300 bees)") + coord_cartesian(ylim = c(0, 12), xlim = c(10,70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + theme(legend.position=c(.85, .85), panel.border = element_blank(), axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks=pretty_breaks(n=5)) + labs(linetype="Queen Origin")



NosemaSummary <- ddply(QueenDF, c("Origin", "NosemaDay"), summarise, 
                       n = length(Nosema),
                       mean = mean(Nosema, na.rm = TRUE),
                       sd = sd(Nosema, na.rm = TRUE),
                       se = sd / sqrt(n))

# remove time steps with missing data:
NosemaSummary <- NosemaSummary[-c(4,8),]
print(NosemaSummary)

ggplot(data = NosemaSummary, 
       aes(x = NosemaDay, 
           y = mean, 
           group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (days after requeening)", y = "Nosema Load (spores/bee)") + coord_cartesian(ylim = c(0, 4300000), xlim = c(10,70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + theme(legend.position=c(.15, .85), panel.border = element_blank(), axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks=pretty_breaks(n=5)) + labs(linetype="Queen Origin") 





PollenSummary <- ddply(PollenDF, c("Origin", "Week"), summarise, 
                       n = length(Pollen),
                       mean = mean(Pollen, na.rm = TRUE),
                       sd = sd(Pollen, na.rm = TRUE),
                       se = sd / sqrt(n))
print(PollenSummary)

#------------------------------------------------------------------------

ggplot(data = PollenSummary, 
       aes(x = Week, 
           y = mean, 
           group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time (weeks after requeening)", y = "Pollen Mass (grams)") + coord_cartesian(ylim = c(0, 40), xlim = c(1,9)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.1)) + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + theme(legend.position=c(.2, .85), panel.border = element_blank(), axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                                                                                                                                                                                                                                                                                                                                                                                                    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks=pretty_breaks(n=9)) + labs(linetype="Queen Origin")





# summary stats for plotting purposes:
VirusSummary <- ddply(VirusDF, c("Origin", "Virus", "Time"), summarise, 
                      n = length(IntensityMult),
                      mean = mean(IntensityMult, na.rm = TRUE),
                      sd = sd(IntensityMult, na.rm = TRUE),
                      se = sd / sqrt(n))

# split data frame by virus type:
splitVir <- split(VirusSummary, VirusSummary$Virus)


#------------------------------------------------------------------------
#DWV

colors <- c("white", "gray32")

p1 <- ggplot(splitVir$DWV, aes(x=Time, y=mean, fill=Origin)) + 
  geom_bar(stat="identity", col="black",  
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4,
                position=position_dodge(.9)) + labs(x="DWV", y = "Viral Load (band intensity)") + theme_classic(base_size = 17) + coord_cartesian(ylim = c(0, 4000000)) + scale_fill_manual(values=colors, name="Queen Origin", labels=c("California", "Local")) + theme(legend.position=c(.5, .9), panel.border = element_blank(),
                                                                                                                                                                                                                                                                         axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#------------------------------------------------------------------------
#BQCV

p2 <- ggplot(splitVir$BQCV, aes(x=Time, y=mean, fill=Origin)) + 
  geom_bar(stat="identity", col="black",  
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4,
                position=position_dodge(.9)) + labs(x="BQCV", y = NULL) + theme_classic(base_size = 17) + coord_cartesian(ylim = c(0, 4000000)) + scale_fill_manual(values=colors, name="Queen Origin", labels=c("California", "Local")) + theme(legend.position=c(3, 3),axis.text.y=element_blank(),
                                                                                                                                                                                                                                                 axis.ticks.y=element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + annotate("segment", x = .5, xend = 2.5, y = 3900000, yend = 3900000) + annotate("segment", x = .5, xend = 1.4, y = 1500000, yend = 1500000) + annotate("segment", x = 1.5, xend = 2.5, y = 3600000, yend = 3600000) + annotate(geom = "text", x = 1.5, y = 4000000, label = "***",cex = 6) + annotate(geom = "text", x = 1, y = 1650000, label = "ns",cex = 5) + annotate(geom = "text", x = 2, y = 3750000, label = "ns",cex = 5)

#------------------------------------------------------------------------
#IAPV

p3 <- ggplot(splitVir$IAPV, aes(x=Time, y=mean, fill=Origin)) + 
  geom_bar(stat="identity", col="black",  
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4,
                position=position_dodge(.9)) + labs(x="IAPV", y = NULL) + theme_classic(base_size = 17) + coord_cartesian(ylim = c(0, 4000000)) + scale_fill_manual(values=colors, name="Queen Origin", labels=c("California", "Local")) + theme(legend.position=c(3, 3),axis.text.y=element_blank(),
                                                                                                                                                                                                                                                 axis.ticks.y=element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + annotate("segment", x = .5, xend = 2.5, y = 3100000, yend = 3100000) + annotate("segment", x = .5, xend = 1.4, y = 2200000, yend = 2200000) + annotate("segment", x = 1.5, xend = 2.5, y = 2800000, yend = 2800000) + annotate(geom = "text", x = 1.5, y = 3250000, label = "ns",cex = 5) + annotate(geom = "text", x = 1, y = 2300000, label = "**",cex = 6) + annotate(geom = "text", x = 2, y = 2900000, label = "**",cex = 6)

# merge figures into 1 side by side figure:
grid.newpage()
grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))

