# This R script serves to help practice R programming
# Created by Zhengsi Chang
# Created on 9-30-2019
# Modified on 10-22-2019

# Log
#----------------------------------
#10-22-2019
# Add codes for plotting

#-------------------------------------
#11-12-2019
# Added ANOVA and t-test


#--------------------------------------

# Clean up work space
rm(list = ls()) # remove objects
graphics.off() # remove plots


# set working directory
setwd("C:/Users/zhengsi/Dropbox/Working/VWST") # Fill in your working directory

# Install libray
#install.packages("tidyverse")
# tidyverse includes a variety of packages
# 

# Import library
library(tidyverse)


#-------------------------------------
# power analysis
#install.packages("pwr")
#library(pwr)
#pwr.anova.test(k = 3,  f = 0.25, sig.level = 0.05, power = .80)


#-------------------------------------

# Import dataset
data <- read.csv("DataAll.csv")


# Check the dataset
str(data)

summary(data)

#-------------------------------------

# Check variables
data$Gender

data$gender <- factor(data$Gender, levels = c(0, 1))

data$gender







#--------------------------------------
# using dplyr for data manipulation


data$Gender.new <- "Female"
data$Gender.new[which(data$gender == 1)] <- "Male"

data$Gender.new

#--------------------------------------
# Create a new dataset for plotting and statistical analysis
# Pipleline 

str(data)

data.new <- data %>%
  filter(PriortoWCST != "Yes") %>%
  select(Condition, Age, Gender.new, CRT.1.RT, CRT.2.RT, PID) %>%
  mutate(Diff.RT = CRT.1.RT - CRT.2.RT)
  #arrange(desc())

# The following is the traditional way
#data.new.1 <- flter(data = data.new, Piror != "yes")
#data.new.2<- select(data = data.new.1, Condtion, Gender,RT...)
#data.new.3 <- mutate (data = data.new.2, DiffRt = RT1 - RT2)






#-----------------------------------
# Use ggplot2 to do plotting

# The first layer: an empty plate
ggplot(data = data.new)


# The second layer: x and y axis
ggplot(data = data.new, aes(x = Gender.new, y = CRT.1.RT ))


# The third layer: add dots
ggplot(data = data.new, aes(x = Gender.new, y = CRT.1.RT ))+
  geom_jitter()+
  geom_boxplot()


# The full version
# To make this plot, you need one categorical variable
# and a continuous variable
ggplot(data = data, aes(x = Gender.new, y = CRT.1.RT))+
  geom_point() +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Reaction time (in seconds)") +
  ggtitle("Gender difference in reaction time")+
  theme_classic()+
  scale_y_continuous(limits = c(0, 200))
  


# Histogram
ggplot(data.new, aes(x= Gender.new))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()



# Scatter plot - two continuous variables
ggplot(data = data.new, aes(x = CRT.2.RT, y = CRT.1.RT ))+
  geom_point()+
  geom_smooth(method = "lm")



# Two categorical and one continous variables
ggplot(data=data.new, aes(x= Gender.new, y= CRT.1.RT, fill= Condition)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.7)+
  xlab("Gender") +
  ylab("Reaction time (in seconds)") +
  ggtitle("Gender difference in reaction time")+
  theme_classic()+
  scale_y_continuous(limits = c(0, 200))+
  geom_boxplot() +
  geom_jitter()+
  theme(legend.position="top")
  



#-------------------------------------
#-------------------------------------
# This is for the next session of workshop
# Statistical analysis


#--------------------------------------------------
# Descriptive analysis

# Mean

mean(data$CRT.3.RT)

# Standard deviation

sd(data$CRT.3.RT)



#--------------------------------------------------
# t-test : between two groups


# Normality test
shapiro.test(data$p)
shapiro.test(data$nq)

# independent t-test
t.test(data$p, data$nq)



# dependent t-test
# assumption

data.new <- data %>%
  mutate(Diff.RT = CRT.1.RT - CRT.2.RT)

shapiro.test(data.new$Diff.RT)

ggplot(data.new, aes(x = Diff.RT))+
  geom_histogram(bins = 30)

# t-test
t.test(data.new$CRT.1.RT, data.new$CRT.2.RT, paired = T)


# If normality assumption was violated

#https://www.statmethods.net/stats/nonparametric.html



#--------------------------------------------------
# Between three or more groups

#http://talklab.psy.gla.ac.uk/r_training/anova/index.html

#http://www.sthda.com/english/wiki/one-way-anova-test-in-r


data <- data %>%
  filter(Age > 0) %>%
  mutate(vwstScore = p + nq - np - q)



library(car)

# Homogeneity of variance
leveneTest(vwstScore~ Condition, data = data)


# ANOVA
# One-way ANOAV (between subject)
fit <- aov(vwstScore ~ Condition, data = data)

summary(fit)




# Two-way ANOVA

fit <- aov(vwstScore ~ Condition * Diff, data = data)

summary(fit)

data.plot <- data %>%
group_by(Condition, Diff) %>% 
  summarise(vwstScore= mean(vwstScore))


ggplot(data.plot, aes(x = Condition, y = vwstScore, col= Diff))+
  geom_point()+
  geom_line(aes(group = Diff))





#---------------------------------------------------
# repeated measures ANOVA

# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

data.CRT.short <- data %>%
  select(PID, Age, CRT.1.RT, CRT.2.RT, CRT.3.RT)


# We need the function "melt" in the library reshape2 to
# convert the short form to long form
library(reshape2)

# short form to long form

#data.CRT.long <- gather(data.CRT.short, CRT, crtScore, CRT.1:CRT.3, factor_key =T)

data.CRT.long <- melt(data.CRT.short, id.vars = c("PID", "Age"))

colnames(data.CRT.long) <- c("PID", "Age", "CRT", "Reaction.Time")



#-----------------------------------------------------
# ANOVA 
fit <- aov( Reaction.Time ~ CRT + Error(PID), data = data.CRT.long)

summary(fit)


#------------------------------------------------------
# factorial ANOVA (one between subject variable: Age, on within subject variable: CRT)

fit <- aov(Reaction.Time ~ CRT * Age  + Error(PID/(CRT)), data = data.CRT.long)

summary(fit)


#------------------------------------------------------
# Two way repeated-measures ANOVA (two within subject)
fit <- aov(DV ~ IV1 * IV2 + Error(PID/(IV1 * IV2)), data = data)

summary(fit)



#-------------------------------------------------------

# simple regression

model <- lm( vwstScore ~ instrTime * Age * x * y * lb, data = data)
model

summary(model)

ggplot(data = data, aes(instrTime, vwstScore))+
  geom_point()+
  stat_smooth(method = lm)



#--------------------------------------
