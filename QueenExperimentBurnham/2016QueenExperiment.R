# Analysis of Andre Burnham's study of Colony success due to Queen origin
# P. Alexander Burnham
# 10 Aug 2016
#
# Metadata 
# Author: P. Alexander Burnham
# Date: 10 August 2016
#
# Data Set: These data were collected in 2016 in New York state by Andre Burnham and the Hamilton College bee research team from two yards in NY owned by one beekeeper who volunteered for the study.
# Data Source: 2016 Hamilton Bee Research Project
# Funding Source: Hamilton College undergraduate research grants
#
# Data Collection: Weights of colonies were obtained by summing the weights of individual hive bodies and supers for each colony at each yard (add more about protocols for other assays).
#
# Columns: (from left to right) Field ID: Id including number location and treatment, Mass (biomass minus supers in lbs) Time (T1-T4), Origin (California and Local), Yard (1 or 2),Nosema (spores per bee) only two time steps (T1 and T3),Varroa (mites per 300 bees),Brood (frames of brood in colony),NosemaPA (binary data for nosema),VarroaPA (binary data for mites),
# Rows: Data points for all columns in order from each collection event
#
# Missing values: NA


#-------------------------------------------------------------------
#Preliminaries:

# Clear memory of characters
ls()
rm(list=ls())

# Set Working Directory 
setwd("~/Desktop/QueenExperimentBurnham")

# read in data
QueensDF <- read.table("2016QueensHam.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

# attatch column names to column data
attach(QueensDF)

# Head my dataframe to see what's going on
head(QueensDF)

# Call blue color palette for graphics
library(RColorBrewer)

#-------------------------------------------------------------------
# looking at colony mass through time

#subsetting by removing rows that have missing data:
QueensMassDF <- QueensDF[-c(95,98,102,104,105),]

# create summary and sd and se using plyr
library(plyr)
MassSummary <- ddply(QueensMassDF, c("Origin", "Time"), summarise, 
                  n = length(Mass),
                  mean = mean(Mass),
                  sd = sd(Mass),
                  se = sd / sqrt(n))

MassSummary$ulim <- MassSummary$mean + MassSummary$se
MassSummary$llim <- MassSummary$mean - MassSummary$se

MassSummary
#-------------------------------------------------------------------
# plot mass at differnt time points using bars
library(lattice)

#colors <- colorRampPalette(brewer.pal(12,"Greens"))(2)
colors <- c("black", "steelblue")

barchart(MassSummary$mean ~ MassSummary$Time,
         col=colors,
         groups = MassSummary$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,65),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), rectangles = TRUE, points = FALSE, text = c("California Colonies","Local Colonies")), #add legend
         xlab = list(label = "Time Steps", fontsize = 20),
         ylab = list(label = "Colony Mass (lbs)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = MassSummary$llim[subscripts]
           ul = MassSummary$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + MassSummary$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + MassSummary$err[subscripts], ul, 
                          #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           #panel.text(1.45, 16.5, "*", cex = 2.5, font = 3)
           panel.segments(as.numeric(x) + MassSummary$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + MassSummary$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + MassSummary$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + MassSummary$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })

#-------------------------------------------------------------------
# line plot showing colony mass through time between the two groups 

library(ggplot2)

print(MassSummary)

# remove generic time steps and add the correct days for correct time scale
x <- MassSummary[,-2]
times <- c(1,24,43,57,1,24,43,57)
x <- data.frame(times, x)

#Create plot in ggplot 
plot <- ggplot(data = x, 
               aes(x = times, 
                   y = mean, 
                   group = Origin, 
                   colour = Origin)
               ) + geom_line(size=1.5) + geom_point(size=4) + scale_colour_manual(values = c("dodgerblue4", "black")) + labs(x = "Time (days after requeening)", y = "Colony Mass (lbs)") + coord_cartesian(ylim = c(20, 70)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) 

# add a theme and add asterix for significance 
plot + scale_fill_brewer(palette = "Paired") + theme_minimal(base_size = 17) + annotate(geom = "text", x = 24, y = 44, label = "*",cex = 8) + annotate(geom = "text", x = 57, y = 66, label = "*",cex = 8) 

# trend lines and panel text:
# + geom_abline(intercept = 24.1950952, slope = 0.5457924) + geom_abline(intercept = 22.268523, slope = 0.342725) + annotate(geom = "text", x = 12, y = 68, label = "California Growth Rate = 0.33",cex = 5) + annotate(geom = "text", x = 12, y = 65, label = "Local Growth Rate = 0.54",cex = 5) + annotate(geom = "text", x = 12, y = 62, label = "p = 0.06",cex = 5)

#-------------------------------------------------------------------

# find slopes and intercepts of the queen groups growth
print(x)
Cal <- x[-c(1,2,3,4),]
Loc <- x[-c(5,6,7,8),]

r <- (lm(mean ~ times, data = Cal))
t <- (lm(mean ~ times, data = Loc))
summary(r)
summary(t)

#-------------------------------------------------------------------
# compare slopes:

pModel <- lm(Mass ~ Origin + Days, data=RegDataMass) # parallel regressions
sModel <- lm(Mass ~ Origin/Days, data=RegDataMass)   # separate regressions
summary(pModel)
summary(sModel)
anova(pModel, sModel)		# tests whether there are differences in slopes.
#p=0.06 means there is a trend in slope difference


#-------------------------------------------------------------------
# stats comparing treatemetns at each timepoint:

#Check for normality and log transform
hist(QueensMassDF$Mass)
hist(log(QueensMassDF$Mass))

#Subset data into 4 time steps (T1-T4)
QMassT1 <- QueensMassDF[c(1:40),]
QMassT2 <- QueensMassDF[c(41:75),]
QMassT3 <- QueensMassDF[c(76:106),]
QMassT4 <- QueensMassDF[c(107:132),]

# run 4 t-tests comparing california to local colony mass
ComparingQueensMassT1 <- aov(log(Mass)~Origin, data=QMassT1)
ComparingQueensMassT2 <- aov(log(Mass)~Origin, data=QMassT2)
ComparingQueensMassT3 <- aov(log(Mass)~Origin, data=QMassT3)
ComparingQueensMassT4 <- aov(log(Mass)~Origin, data=QMassT4)

#store the summaries in some variables
w <- summary(ComparingQueensMassT1)
x <- summary(ComparingQueensMassT2)
y <- summary(ComparingQueensMassT3)
z <- summary(ComparingQueensMassT4)

#-------------------------------------------------------------------
#looking at nosema:

head(QueensDF)

#subsetting by removing rows that have missing data:
QueensNosemaDF <- QueensDF[-c(35,(41:75)),]

times <- c(rep("T1",39),rep("T2",31),rep("T3",26))

QueensNosemaDF <- data.frame(QueensNosemaDF, times)

# create summary and sd and se using plyr
library(plyr)
NosemaSummary <- ddply(QueensNosemaDF, c("Origin", "NosemaDay"), summarise, 
                     n = length(Nosema),
                     mean = mean(Nosema),
                     sd = sd(Nosema),
                     se = sd / sqrt(n))

NosemaSummary$ulim <- NosemaSummary$mean + NosemaSummary$se
NosemaSummary$llim <- NosemaSummary$mean - NosemaSummary$se

NosemaSummary$err <- ifelse(NosemaSummary$NosemaDay == 0, -0.175, 0.175)

err <- c(-0.175,-0.175,-0.175,0.175,0.175,0.175)

NosemaSummary <- NosemaSummary[,-9]  
NosemaSummary <- data.frame(NosemaSummary ,err)
NosemaSummary 


#Create plot in ggplot 
plot <- ggplot(data = NosemaSummary, 
               aes(x = NosemaDay, 
                   y = mean, 
                   group = Origin, 
                   colour = Origin)
) + geom_line(size=1.5) + geom_point(size=4) + scale_colour_manual(values = c("dodgerblue4", "black")) + labs(x = "Time (days after requeening)", y = "Nosmea Load (spores/bee)") + coord_cartesian(ylim = c(0, 5000000), xlim = c(0,60)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.9)) 

# add a theme and add asterix for significance 
plot + scale_fill_brewer(palette = "Paired") + theme_minimal(base_size = 17) + annotate(geom = "text", x = 34, y = 4500000, label = "*",cex = 8) + annotate(geom = "text", x = 55, y = 2850000, label = "*",cex = 8) 

# trend lines and panel text 

#+ geom_abline(intercept =  1630704, slope = 20917) + geom_abline(intercept = 1691167.3, slope = -24800.3) 

#+ annotate(geom = "text", x = 12, y = 68, label = "California Growth Rate = 0.33",cex = 5) + annotate(geom = "text", x = 12, y = 65, label = "Local Growth Rate = 0.54",cex = 5) + annotate(geom = "text", x = 12, y = 62, label = "p = 0.06",cex = 5)
 
#-------------------------------------------------------------------
# stats comparing treatemetns at each timepoint:

#Check for normality and log transform
#hist(QueensDF$Nosema)
#hist(log(QueensDF$Nosema))

#Subset data into 3 time steps (T1-T4)
NosemaT1 <- QueensDF[c(1:40),]
NosemaT2 <- QueensDF[c(78:106),]
NosemaT3 <- QueensDF[c(107:132),]


# run 3 t-tests comparing california to local colony mass
ComparingQueensNosemaT1 <- aov(Nosema~Origin, data=NosemaT1)
ComparingQueensNosemaT2 <- aov(Nosema~Origin, data=NosemaT2)
ComparingQueensNosemaT3 <- aov(Nosema~Origin, data=NosemaT3)



# checking residuals for T1 for normality 
ComparingQueensNosemaT1.res = resid(ComparingQueensNosemaT1)

NosemaT1 <- NosemaT1[-40,]

plot(log(NosemaT1$Nosema), ComparingQueensNosemaT1.res) 
abline(0, 0)          


#store the summaries in some variables
w <- summary(ComparingQueensNosemaT1) #Not sig
x <- summary(ComparingQueensNosemaT2) #Sig
y <- summary(ComparingQueensNosemaT3) #Sig

#-------------------------------------------------------------------
# plot nosema through time using bar graphs

QueensNosemaDF$NosemaDayChar <- as.character(QueensNosemaDF$NosemaDay)

library(plyr)
NosemaSummary <- ddply(QueensNosemaDF, c("Origin", "NosemaDayChar"), summarise, 
                       n = length(Nosema),
                       mean = mean(Nosema),
                       sd = sd(Nosema),
                       se = sd / sqrt(n))

NosemaSummary$ulim <- NosemaSummary$mean + NosemaSummary$se
NosemaSummary$llim <- NosemaSummary$mean - NosemaSummary$se

NosemaSummary$err <- ifelse(NosemaSummary$NosemaDayChar == 0, -0.175, 0.175)

err <- c(-0.175,-0.175,-0.175,0.175,0.175,0.175)

NosemaSummary <- NosemaSummary[,-9]  
NosemaSummary <- data.frame(NosemaSummary ,err)
NosemaSummary 


library(lattice)

#colors <- colorRampPalette(brewer.pal(12,"Greens"))(2)
colors <- c("steelblue", "grey")

barchart(NosemaSummary$mean ~ NosemaSummary$NosemaDayChar,
         col=colors,
         groups = NosemaSummary$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,5000000),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), rectangles = TRUE, points = FALSE, text = c("California","Local")), #add legend
         xlab = list(label = "Time (days after requeening)", fontsize = 20),
         ylab = list(label = "Nosema Load (spores/100 bees)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = NosemaSummary$llim[subscripts]
           ul = NosemaSummary$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + NosemaSummary$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + NosemaSummary$err[subscripts], ul, 
                          #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           panel.text(2.05, 4500000, "*", cex = 1.9, font = 3)
           panel.text(3.05, 3100000, "*", cex = 1.9, font = 3)
           panel.segments(as.numeric(x) + NosemaSummary$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + NosemaSummary$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + NosemaSummary$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + NosemaSummary$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })


#--------------------------------------------------------------------------

# create summary and sd and se using plyr for varroa
library(plyr)

# Remove rows containing NAs in column Varroa
VarroaDF <- QueensDF[!is.na(QueensDF$Varroa),]

VarroaSummary <- ddply(VarroaDF, c("Origin", "VarroaDay"), summarise, 
                       n = length(Varroa),
                       mean = mean(Varroa),
                       sd = sd(Varroa),
                       se = sd / sqrt(n))

VarroaSummary$ulim <- VarroaSummary$mean + VarroaSummary$se
VarroaSummary$llim <- VarroaSummary$mean - VarroaSummary$se

VarroaSummary$err <- ifelse(VarroaSummary$VarroaDay == 0, -0.175, 0.175)

err <- c(-0.175,-0.175,-0.175,-0.175, 0.175,0.175,0.175, 0.175)

VarroaSummary <- VarroaSummary[,-9]  
VarroaSummary <- data.frame(VarroaSummary, err)
VarroaSummary

#----------------------------------------------------------------------------
#Create varroa line graph in ggplot 
plot <- ggplot(data = VarroaSummary, 
               aes(x = VarroaDay, 
                   y = mean, 
                   group = Origin, 
                   colour = Origin)
) + geom_line(size=1.5) + geom_point(size=4) + scale_colour_manual(values = c("dodgerblue4", "black")) + labs(x = "Time (days after requeening)", y = "Varroa Load (mites/300 bees)") + coord_cartesian(ylim = c(0, 12)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = .8)) 

# add a theme and add asterix for significance 
plot + scale_fill_brewer(palette = "Paired") + theme_minimal(base_size = 17)  

#----------------------------------------------------------------------------
#Create varroa var graph in Lattice:


#colors <- colorRampPalette(brewer.pal(12,"Greens"))(2)
colors <- c("grey", "blue")

barchart(VarroaSummary$mean ~ VarroaSummary$VarroaDay,
         col=colors,
         groups = VarroaSummary$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,13),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.9,0.9), rectangles = TRUE, points = FALSE, text = c("California","Local")), #add legend
         xlab = list(label = "Time (days after requeening)", fontsize = 20),
         ylab = list(label = "Varroa Load (mites/300 bees)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = VarroaSummary$llim[subscripts]
           ul = VarroaSummary$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + VarroaSummary$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + VarroaSummary$err[subscripts], ul, 
                          #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
          # panel.text(2.05, 4500000, "*", cex = 1.9, font = 3)
          # panel.text(3.05, 3100000, "*", cex = 1.9, font = 3)
           panel.segments(as.numeric(x) + VarroaSummary$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + VarroaSummary$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + VarroaSummary$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + VarroaSummary$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })


#----------------------------------------------------------------------------
#Pathogen Prevalence: Nosema, Varroa, IAPV, DWV, BQCV

colors <- colorRampPalette(brewer.pal(9,"Blues"))(5)
colors <- rev(colors)

# plot in ggplot using minamal theme with some graphical additions
plot1 <- ggplot(ForageSummary, aes(x = ForageSummary$Genus, 
                                   y = ForageSummary$mean)
) + geom_bar(stat = "identity",
             color="black", 
             size=0.6, 
             fill=c(colors),
             width=0.7) + geom_errorbar(aes(ymin=ForageSummary$mean-ForageSummary$se, ymax=ForageSummary$mean + ForageSummary$se), size=0.6, width=0.4, position=position_dodge(0.9))+labs(title="Floral Rejection by Genera in Bumble Bees", x="Flower Genera", y = "Rejection Streak") 


# add theme to graphics package and add letteres to denote where sig differences are based on tukey post hoc

plot1 + scale_fill_brewer(palette="Paired") + theme_minimal(base_size = 17)



#--------------------------------------------------
# pollen work making a bargraph

# read in data
PollenDF <- read.table("PollenQueens.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

PollenDF2 <- PollenDF[!is.na(PollenDF$Pollen),]

PollenDF2$WeekChar <- as.character(PollenDF2$Week)

PollenSummary <- ddply(PollenDF2, c("Origin", "WeekChar"), summarise, 
                       n = length(Pollen),
                       mean = mean(Pollen),
                       sd = sd(Pollen),
                       se = sd / sqrt(n))

PollenSummary$ulim <- PollenSummary$mean + PollenSummary$se
PollenSummary$llim <- PollenSummary$mean - PollenSummary$se

PollenSummary$err <- ifelse(PollenSummary$Week == 0, -0.175, 0.175)

err <- c(rep(-0.175, 9),rep(0.175, 9))

PollenSummary <- PollenSummary[,-9]  
PollenSummary <- data.frame(PollenSummary, err)
PollenSummary


colors <- c("dodgerblue4", "slategray3")

plot2 <- ggplot(PollenSummary, aes(x=WeekChar, y=mean, fill=Origin)) + 
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4,
                position=position_dodge(.9)) + labs(x="Time (weeks after requeening)", 
                                                    y = "Pollen Weight (grams)")

plot2 + theme_minimal(base_size = 17) + coord_cartesian(ylim = c(0, 40)) + scale_fill_manual(values=colors) + annotate(geom = "text", x = 5, y = 27, label = "*",cex = 6) 




#-------------------------------------------------------------------------


#colors <- colorRampPalette(brewer.pal(12,"Greens"))(2)
colors <- c("grey", "blue")

barchart(PollenSummary$mean ~ PollenSummary$WeekChar,
         col=colors,
         groups = PollenSummary$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,40),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), rectangles = TRUE, points = FALSE, text = c("California","Local"),title="Queen Origin", cex.title=1.39), #add legend
         xlab = list(label = "Time (weeks after reqeening)", fontsize = 20),
         ylab = list(label = "Pollen Weight (grams)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = PollenSummary$llim[subscripts]
           ul = PollenSummary$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + PollenSummary$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + PollenSummary$err[subscripts], ul, 
                          #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
            panel.text(5, 27, "*", cex = 1.5, font = 3)
           # panel.text(3.05, 3100000, "*", cex = 1.9, font = 3)
           panel.segments(as.numeric(x) + PollenSummary$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + PollenSummary$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + PollenSummary$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + PollenSummary$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })

#---------------------------------------------------------------------------

# Brood line graph

BroodDF <- PollenDF[!is.na(PollenDF$Brood),]

BroodSummary <- ddply(BroodDF, c("Origin", "BroodDays"), summarise, 
                       n = length(Brood),
                       mean = mean(Brood),
                       sd = sd(Brood),
                       se = sd / sqrt(n))

BroodSummary$ulim <- BroodSummary$mean + BroodSummary$se
BroodSummary$llim <- BroodSummary$mean - BroodSummary$se

BroodSummary$err <- ifelse(BroodSummary$BroodDays == 0, -0.175, 0.175)

err <- c(rep(-0.175, 5),rep(0.175, 5))

BroodSummary <- BroodSummary[,-9]  
BroodSummary <- data.frame(BroodSummary, err)
BroodSummary

#---------------------------------------------------
plot <- ggplot(data = BroodSummary, 
               aes(x = BroodDays, 
                   y = mean, 
                   group = Origin, 
                   colour = Origin)
) + geom_line(size=1.5) + geom_point(size=4) + scale_colour_manual(values = c("dodgerblue4", "black")) + labs(x = "Time (days after requeening)", y = "Frames of Brood/Hive") + coord_cartesian(ylim = c(2, 7)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 1)) 

# add a theme and add asterix for significance 
plot + scale_fill_brewer(palette = "Paired") + theme_minimal(base_size = 17) + annotate(geom = "text", x = 15, y = 5.2, label = "*",cex = 8) + annotate(geom = "text", x = 43, y = 6, label = "*",cex = 8) + annotate(geom = "text", x = 64, y = 6.7, label = "*",cex = 8) 


x<-split(BroodDF, BroodDF$BroodDays)

t.test(x$`15`$Brood~x$`15`$Origin)
t.test(x$`27`$Brood~x$`27`$Origin)
t.test(x$`43`$Brood~x$`43`$Origin)
t.test(x$`64`$Brood~x$`64`$Origin)

#--------------------------------------------------------------------

#Virus stuff

VirusDF <- read.table("RNAVirus.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)




VirusDF <- VirusDF[!is.na(VirusDF$Intensity),]

Virus1 <- split(VirusDF,VirusDF$Virus)
DWV <- Virus1$DWV
IAPV <- Virus1$IAPV

x <- split(DWV, DWV$Time)

t.test(x$T2$Intensity~x$T2$Origin)




t <- x$T2
s <- split(t, t$Origin)

hist(s$California$Intensity)
       hist(s$Local$Intensity)

y<-x$T2$Intensity
y

#-------------------------------------------------------------------------
#Use ddply to get summary data for RNA Viruses

library(dplyr)

VirusSummary <- ddply(DWV, c("Origin", "Time"), summarise, 
                      n = length(Intensity),
                      mean = mean(Intensity),
                      sd = sd(Intensity),
                      se = sd / sqrt(n))

VirusSummary$ulim <- VirusSummary$mean + VirusSummary$se
VirusSummary$llim <- VirusSummary$mean - VirusSummary$se

VirusSummary$err <- ifelse(VirusSummary$Time == 0, -0.175, 0.175)

err <- c(rep(-0.175, 2),rep(0.175, 2))

VirusSummary <- VirusSummary[,-9]  
VirusSummary <- data.frame(VirusSummary, err)


#-------------------------------------------------------------------

#colors <- colorRampPalette(brewer.pal(12,"Greens"))(2)
colors <- c("grey", "blue", "Black")

barchart(VirusSummary$mean ~ VirusSummary$Time,
         col=colors,
         groups = VirusSummary$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,4.5),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), rectangles = TRUE, points = FALSE, text = c("California","Local"),title="Queen Origin", cex.title=1.39), #add legend
         xlab = list(label = "Time (28 days apart)", fontsize = 20),
         ylab = list(label = "DWV Volume (Int)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = VirusSummary$llim[subscripts]
           ul = VirusSummary$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + VirusSummary$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + VirusSummary$err[subscripts], ul, 
                          #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           panel.text(5, 27, "*", cex = 1.5, font = 3)
           # panel.text(3.05, 3100000, "*", cex = 1.9, font = 3)
           panel.segments(as.numeric(x) + VirusSummary$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + VirusSummary$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + VirusSummary$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + VirusSummary$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })


#---------------------------------------------------------------------------
#IAPV



VirusDF <- VirusDF[!is.na(VirusDF$Intensity),]

Virus1 <- split(VirusDF,VirusDF$Virus)
IAPV <- Virus1$IAPV

z <- split(IAPV, IAPV$Time)

t.test(z$T2$Intensity~z$T2$Origin)
t.test(z$T1$Intensity~z$T1$Origin)

VirusSummary <- ddply(IAPV, c("Origin", "Time"), summarise, 
                      n = length(Intensity),
                      mean = mean(Intensity),
                      sd = sd(Intensity),
                      se = sd / sqrt(n))

VirusSummary$ulim <- VirusSummary$mean + VirusSummary$se
VirusSummary$llim <- VirusSummary$mean - VirusSummary$se

VirusSummary$err <- ifelse(VirusSummary$Time == 0, -0.175, 0.175)

err <- c(rep(-0.175, 2),rep(0.175, 2))

VirusSummary <- VirusSummary[,-9]  
VirusSummary <- data.frame(VirusSummary, err)
VirusSummary

#-------------------------------------------------------------------

colors <- c("grey38", "dodgerblue3")

barchart(VirusSummary$mean ~ VirusSummary$Time,
         col=colors,
         groups = VirusSummary$Origin,
         ylim=c(0,4.5),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), 
                         rectangles = TRUE, 
                         points = FALSE, 
                         text = c("California","Local"),
                         title="Queen Origin:", cex.title=1.36), 
         xlab = list(label = "Time (28 days apart)", fontsize = 20),
         ylab = list(label = "IAPV Volume (Int.)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = VirusSummary$llim[subscripts]
           ul = VirusSummary$ulim[subscripts]
           
           panel.segments(as.numeric(x) + VirusSummary$err[subscripts], ll, 
                          
                          as.numeric(x) + VirusSummary$err[subscripts], ul, 
                        
                          col = 'black', lwd = 1)   
           
           panel.text(1, 2.5, "*", cex = 1.9, font = 3)
           panel.text(2, 3.3, "*", cex = 1.9, font = 3)
           
           panel.segments(as.numeric(x) + VirusSummary$err[subscripts] - 0.1, ll,
                          as.numeric(x) + VirusSummary$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           
           panel.segments(as.numeric(x) + VirusSummary$err[subscripts] - 0.1, ul, 
                          as.numeric(x) + VirusSummary$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })

#---------------------------------------------------------------------
#Plot Virus in ggplot

colors <- c("dodgerblue4", "slategray3")

plot1 <- ggplot(VirusSummary, aes(x=Time, y=mean, fill=Origin)) + 
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4,
                position=position_dodge(.9)) + labs(x="Time (28 days apart)", 
                                                    y = "DWV Volume (Int)")

plot1 + theme_minimal(base_size = 17) + coord_cartesian(ylim = c(0, 4)) + scale_fill_manual(values=colors)

#Plot IAPV in ggplot

colors <- c("dodgerblue4", "slategray3")

plot2 <- ggplot(VirusSummary, aes(x=Time, y=mean, fill=Origin)) + 
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4,
                position=position_dodge(.9)) + labs(x="Time (28 days apart)", 
                                                    y = "IAPV Volume (Int)")

plot2 + theme_minimal(base_size = 17) + coord_cartesian(ylim = c(0, 3)) + scale_fill_manual(values=colors) + annotate(geom = "text", x = 1, y = 2.3, label = "*",cex = 8) + annotate(geom = "text", x = 2, y = 3, label = "*",cex = 8) 






