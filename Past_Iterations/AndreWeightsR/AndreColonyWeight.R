# Analysis of Andre Burnham's study of Colony success due to Queen origin
# P. Alexander Burnham
# 11 July 2016

# Metadata 
# Author: P. Alexander Burnham
# Date: 11 July 2016
#
# Data Set: These data were collected in 2016 in New York state by Andre Burnham and the Hamilton College bee research team.
# Data Source: 2016 Hamilton Bee Research Project
# Funding Source: Hamilton College undergraduate research grants
#
# Data Collection: Weights of colonies were obtained by summing the weights of individual hive bodies and supers for each colony at each yard.
#
# Columns: (from left to right) Field ID: Id including number location and treatment, Colony weights and T1, colony weights at T2, difference between two times (i.e. growth), origin (local or Californian queens) and yard (1 or 2)
# Rows: Data points for all columns in order from each collection event
# Missing values: NA



#-------------------------------------------------------------------
#Preliminaries:

# Clear memory of characters
ls()
rm(list=ls())

# Set Working Directory 
setwd("~/Desktop/AndreWeightsR")

# read in data
Andre_DF <- read.table("2016AndreWeightsData.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

# attatch column names to column data
attach(Andre_DF)

# Head my dataframe to see what's going on
head(Andre_DF)

# Call blue color palette for graphics
library(RColorBrewer)

#-------------------------------------------------------------------
# Summarize difference in weight by queen origin regardless of bee yard

library(plyr)
AndreDF2 <- ddply(Andre_DF, c("Origin"), summarise, 
                  n = length(WeightDif),
                  mean = mean(WeightDif),
                  sd = sd(WeightDif),
                  se = sd / sqrt(n))

AndreDF2$ulim <- AndreDF2$mean + AndreDF2$se
AndreDF2$llim <- AndreDF2$mean - AndreDF2$se

AndreDF2$err <- ifelse(AndreDF2$Origin == 0, -0.015, 0.015)
AndreDF2


#-------------------------------------------------------------------

# call package lattice and use barchart instead of base-R barplot
library(lattice)


barchart(AndreDF2$mean ~ AndreDF2$Origin,
         col=c("grey","blue"),
         #groups = AndreDF2$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,20),
         par.settings=list(superpose.polygon=list(col=c("grey","blue"))),
         main=paste("Colony Growth by Queen Origin"),
        # auto.key = list(corner=c(0.9,0.9), rectangles = TRUE, points = FALSE, text = c("Californian Queen","Local Queen")), #add legend
         xlab = list(label = "Queen Origin", fontsize = 20),
         ylab = list(label = "Colony Growth After 24 Days (lbs)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = AndreDF2$llim[subscripts]
           ul = AndreDF2$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + AndreDF2$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + AndreDF2$err[subscripts], ul, #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           panel.text(1.45, 16.5, "*", cex = 2.5, font = 3)
           panel.segments(as.numeric(x) + AndreDF2$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + AndreDF2$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + AndreDF2$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + AndreDF2$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })

# Run ANOVA (automatically t test if only 2 groups but good habit)
#-------------------------------------------------------------------

ComparingYards <- aov(WeightDif~Yard, data=Andre_DF) 
z <- summary(ComparingYards)
z

# p=0.0839

# insignifcant p value, OK to join yards together


ComparingQueens <- aov(WeightDif~Origin, data=Andre_DF) 
z <- summary(ComparingQueens)
z

# p=0.0115

# not as low a p value as in your exel worksheet but I found several mistakes there

#-------------------------------------------------------------------
# show california and local at two time steps (T1 and T2)

weights <- c(Andre_DF$WeightT1,Andre_DF$WeightT2)
Qorigin <- rep(Andre_DF$Origin, 2)
Qyard <- rep(Andre_DF$Yard, 2)
T1 <- rep("T1", 36)
T2 <- rep("T2", 36)
Time <- c(T1,T2)

DF2 <- data.frame(weights, Qorigin, Qyard, Time)

library(plyr)
AndreDF2 <- ddply(DF2, c("Qorigin", "Time"), summarise, 
                  n = length(weights),
                  mean = mean(weights),
                  sd = sd(weights),
                  se = sd / sqrt(n))

AndreDF2$ulim <- AndreDF2$mean + AndreDF2$se
AndreDF2$llim <- AndreDF2$mean - AndreDF2$se

AndreDF2$err <- ifelse(AndreDF2$Time == 0, -0.175, 0.175)

err <- c(-0.175,-0.175,0.175,0.175)

AndreDF2 <- AndreDF2[,-9]  
AndreDF2 <- data.frame(AndreDF2,err)
AndreDF2

#-------------------------------------------------------------------
# call package lattice and use barchart instead of base-R barplot
library(lattice)

colors<-colorRampPalette(brewer.pal(9,"Blues"))(2)

barchart(AndreDF2$mean ~ AndreDF2$Time,
         col=colors,
         groups = AndreDF2$Qorigin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,65),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), rectangles = TRUE, points = FALSE, text = c("Californian Queen","Local Queen")), #add legend
         xlab = list(label = "Time Steps (every 24 days)", fontsize = 20),
         ylab = list(label = "Colony Mass (lbs)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = AndreDF2$llim[subscripts]
           ul = AndreDF2$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + AndreDF2$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + AndreDF2$err[subscripts], ul, 
                          #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           #panel.text(1.45, 16.5, "*", cex = 2.5, font = 3)
           panel.segments(as.numeric(x) + AndreDF2$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + AndreDF2$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + AndreDF2$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + AndreDF2$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })
#-------------------------------------------------------------------
# calculate percent increase: To calculate the percentage increase: First: work out the difference (increase) between the two numbers you are comparing. Then: divide the increase by the original number and multiply the answer by 100. If your answer is a negative number then this is a percentage decrease.

Andre_DF
PercentIncrease <- (Andre_DF$WeightDif/Andre_DF$WeightT1)

Andre_DFP <- data.frame(Andre_DF, PercentIncrease)

# Summarize difference in percent increase by queen origin regardless of bee yard

library(plyr)
Andre <- ddply(Andre_DFP, c("Origin"), summarise, 
                  n = length(PercentIncrease),
                  mean = mean(PercentIncrease),
                  sd = sd(PercentIncrease),
                  se = sd / sqrt(n))

# ply R code for error bars:

Andre$ulim <- Andre$mean + Andre$se
Andre$llim <- Andre$mean - Andre$se

Andre$err <- ifelse(Andre$Origin == 0, -0.015, 0.015)


#---------------------------------------------------------------------------
# plot percent increase using ggplot2
library(ggplot2)
library(scales)
library(lattice)

plot <- ggplot(Andre, aes(x = Andre$Origin, y = Andre$mean)) + geom_bar(stat = "identity", color="black", size=0.8, fill=c("grey","blue"),width=0.7) + scale_y_continuous(labels = percent_format()) + geom_errorbar(aes(ymin=Andre$mean-Andre$se, ymax=Andre$mean+Andre$se), size=0.8, width=.1, position=position_dodge(.9)) + labs(title="Percent Increase by Queen Origin", x="Origins of Queen", y = "Percent Increase") 

panel.text(1.45, 16.5, "*", cex = 2.5, font = 3)
plot + scale_fill_brewer(palette="Paired") + theme_minimal(base_size = 17)
#---------------------------------------------------------------------------
# Restructuring data frame to include Nosema counts at both time steps

N <- Andre_DFP$Nosema2
O <- Andre_DFP$Origin
NO <- data.frame(N, O)
NO <- NO[-15,]
NO <- NO[-15,]
NO <- NO[-15,]
NO <- NO[-15,]


library(plyr)
And <- ddply(NO, c("O"), summarise, 
               n = length(N),
               mean = mean(N),
               sd = sd(N),
               se = sd / sqrt(n))


ComparingQueens2 <- aov(N~O, data=NO) 
z <- summary(ComparingQueens2)
z


#---------------------------------------------------------------------------


ID <- rep(Andre_DFP$FieldID,2)
NosemaCount <- c(Andre_DFP$Nosema1,Andre_DFP$Nosema2)
Origin <- rep(Andre_DFP$Origin, 2)
Yard <- rep(Andre_DFP$Yard,2 )
Weight <- c(Andre_DFP$WeightT1, Andre_DFP$WeightT2) 
Time <- c(rep("T1",36), rep("T2",36))

Andre1 <- data.frame(ID, NosemaCount, Origin, Yard, Weight, Time)

Andre1 <- Andre1[-51,]
Andre1 <- Andre1[-51,]
Andre1 <- Andre1[-51,]
Andre1 <- Andre1[-51,]

#---------------------------------------------------------------------------
# Aggregate data into summary stats for plot:

library(plyr)
AndreSum <- ddply(Andre1, c("Origin", "Time"), summarise, 
               n = length(NosemaCount),
               mean = mean(NosemaCount),
               sd = sd(NosemaCount),
               se = sd / sqrt(n))

# ply R code for error bars:

AndreSum$ulim <- AndreSum$mean + AndreSum$se
AndreSum$llim <- AndreSum$mean - AndreSum$se

AndreSum$err <- ifelse(AndreSum$Origin == 0, -0.175, 0.175)

err <- c(-0.175,-0.175,0.175,0.175)

AndreSum <- AndreSum[,-9]  
AndreSum <- data.frame(AndreSum,err)
AndreSum


#---------------------------------------------------------------------------
# plot mean nosema counts for both time steps by origin using ggplot2

library(lattice)

colors<-colorRampPalette(brewer.pal(9,"Blues"))(2)

barchart(AndreSum$mean ~ AndreSum$Time,
         col=colors,
         groups = AndreSum$Origin, #Be sure to add the groups argument to specify how to group the bars
         ylim=c(0,5500000),
         par.settings=list(superpose.polygon=list(col=colors)),
         #main=paste("Colony Mass Through Time"),
         auto.key = list(corner=c(0.1,0.9), rectangles = TRUE, points = FALSE, text = c("Local Queen","Californian Queen")), #Legend is Reversed:
         xlab = list(label = "Time Steps (every 24 days)", fontsize = 20),
         ylab = list(label = "Nosema Load (spores per 100 bees)", fontsize = 20), 
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25),
         panel = function(x, y, ..., subscripts) 
           
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = AndreSum$llim[subscripts]
           ul = AndreSum$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + AndreSum$err[subscripts], ll, #added AndreDF2$err[subscripts]
                          as.numeric(x) + AndreSum$err[subscripts], ul, #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           #panel.text(1.45, 16.5, "*", cex = 2.5, font = 3)
           panel.segments(as.numeric(x) + AndreSum$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + AndreSum$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + AndreSum$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + AndreSum$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)  
         })

#t-test between californian and local queens at time step T2

ComparingQueens1 <- aov(NosemaCount~Origin*Time, data=Andre1) 
z <- summary(ComparingQueens1)
z

#---------------------------------------------------------------------------
# linear regresssion of colony mass by nosema load

head(Andre_DFP)

GrowthGrid <- plot(x=Andre_DFP$WeightT1, 
     y=Andre_DFP$WeightT2,
     font.lab=2,
     ylim=c(20,90),
     xlim=c(20,65),
     pch=19,
     ylab="Colony Mass at T2 (lbs)",
     xlab="Colony Mass at T1 (lbs)",
     main=paste("Growth Consistancy Grid")
)

grid(col="grey")
# fit these data to a linear model and plot line of best fit (generate equation:
LineBF <- lm(WeightT2~WeightT1, data=Andre_DFP)
line<-abline(LineBF, col = "blue", lwd=3)

# Extract slope and intercept from linear model 

z <- summary(LineBF)
x <- names(z)
z$coefficients[c(1,2)]
z









