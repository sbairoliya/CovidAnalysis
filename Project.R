###########################################################
# STAT 35500 - Project                                    #
# Analysis of COVID 19 across different States in the USA #
# By - Shivam Bairoliya                                   #      
# The .csv file should be in the same directory as the    #
# R project. It can be downloaded from                    #
# https://www.kaggle.com/nightranger77/covid19-state-data #
###########################################################

#Read the data from the CSV file
full_data = read.csv("COVID19_state.csv")
#Extract all the relevant columns
data = full_data[, c("State", "Tested", "Infected", "Deaths", "Pop.Density",
                     "Smoking.Rate", "Pollution", "Temperature")]

#Create a column for Case Fatality Ratio
data$Case.Fatality = round((data$Deaths/data$Infected)*100, 3)
#Create a column for Positivity Rate 
data$Positivity.Rate = round((data$Infected/data$Tested)*100, 3)
#Create a column for ICU Beds per 10000
data$ICU.Beds.10k = round((full_data$ICU.Beds/full_data$Population)*10000, 3)
#Create a column for Physicians per 10000
data$Physicians.10k = round((full_data$Physicians/full_data$Population)*10000, 3)
#Manually create a vector with Political Affiliations of the governor
data$Governors = c("R", "R", "R", "R", "D", "D", "D", "D", "D", "R", "R", "D", "R",
              "D", "R", "R", "D", "D", "D", "D", "R", "R", "D", "D", "R", "R", 
              "D", "R", "D", "R", "D", "D", "D", "D", "R", "R", "R", "D", "D", 
              "D", "R", "R", "R", "R", "R", "R", "D", "D", "R", "D", "R")

attach(data)
#Display the data being used
head(data)

#Adjust the graph window to fit all the labels
par(mar = c(6.5, 6.5, 2, 0.5), mgp = c(5, 1, 0))

#Mean and Standard Deviation for Total Tests
mean(Tested)
sd(Tested)
#The bar plot for Total Tests
barplot(Tested, names.arg = State, las = 2, cex.names = 0.70, 
        col = "light blue", ylab = "Tested", xlab = "State", 
        ylim = c(0,2e07), main = "Total Tests Conducted")

#Mean and Standard Deviation for total Infected
mean(Infected)
sd(Infected)
#The bar plot for Infected 
barplot(Infected, names.arg = State, las = 2, cex.names = 0.70, 
        col = "red", ylab = "Infected", xlab = "State", 
        ylim = c(0,1e06), main = "Total Infected")

#Mean and Standard Deviation for Total Deaths
mean(Deaths)
sd(Deaths)
#The bar plot for Deaths 
barplot(Deaths, names.arg = State, las = 2, cex.names = 0.70, 
        col = "black", ylab = "Deaths", xlab = "State", 
        ylim = c(0,28000), main = "Total Deaths")

#Mean and Standard Deviation for Population density
mean(Pop.Density)
sd(Pop.Density)
#The bar plot for population density
barplot(Pop.Density, names.arg = State, las = 2, cex.names = 0.70, 
        col = "turquoise3", ylab = "Population Density", xlab = "State", 
        ylim = c(0, 4000), main = "Population Density")

#Mean and Standard Deviation for Smoking Rate
mean(Smoking.Rate)
sd(Smoking.Rate)
#The bar plot for Smoking Rate
barplot(Smoking.Rate, names.arg = State, las = 2, cex.names = 0.70, 
        col = "gray50", ylab = "Smoking Rate", xlab = "State", 
        ylim = c(0, 27), main = "Percentage of Pouplation that Smokes")

#Mean and Standard Deviation for Pollution
mean(Pollution)
sd(Pollution)
#The bar plot for Pollution
barplot(Pollution, names.arg = State, las = 2, cex.names = 0.70, 
        col = "gray23", ylab = "Pollution", xlab = "State", 
       ylim = c(0, 13),  main = "Pollution in microns per cubic meter")

#Mean and Standard Deviation for Temperature
mean(Temperature)
sd(Temperature)
#The bar plot for Temperature
barplot(Temperature, names.arg = State, las = 2, cex.names = 0.70, 
        col = heat.colors(80), ylab = "Temperature", xlab = "State", 
        ylim = c(0, 75),  main = "Average Temperature in Farenheit")

#Mean and Standard Deviation for Case Fatality Ratio
mean(Case.Fatality)
sd(Case.Fatality)
#The bar plot for Case Fatality Ratio
barplot(Case.Fatality, names.arg = State, las = 2, cex.names = 0.70, 
        col = "gray10", ylab = "Case Fatality", xlab = "State", 
        ylim = c(0, 7), main = "Percentage of People who died vs People who 
        tested +ve for the virus")

#Mean and Standard Deviation for Positivity Rate
mean(Positivity.Rate)
sd(Positivity.Rate)
#The bar plot for Positivity Rate
barplot(Positivity.Rate, names.arg = State, las = 2, cex.names = 0.70, 
        col = "orangered", ylab = "Positivity Rate", xlab = "State", 
        ylim = c(0, 19), main = "Positivity Rate as percentage")

#Mean and Standard Deviation for ICU Beds per 10000 of population
mean(ICU.Beds.10k)
sd(ICU.Beds.10k)
#The bar plot for ICU Beds per 10000 of population
barplot(ICU.Beds.10k, names.arg = State, las = 2, cex.names = 0.70, 
        col = "orchid", ylab = "ICU beds per 10k population", xlab = "State", 
        ylim = c(0, 4.5), main = "Number of ICU beds per 10000 of the population")

#Mean and Standard Deviation for Physicians per 10000 of population
mean(Physicians.10k)
sd(Physicians.10k)
#The bar plot for Physicians per 10000 of population
barplot(Physicians.10k, names.arg = State, las = 2, cex.names = 0.70, 
        col = "pink", ylab = "Physicians per 10k population", xlab = "State", 
        main = "Number of Physicians per 10000 of the population")

#Number of Democrat Governors/Mayor
length(Governors[Governors == "D"])
#Number of Republican Governors
length(Governors[Governors == "R"])

#Reset the plot window
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))

#Adjust the graph window for multiple boxplots
par(mfrow = c(1, 3))
#Boxplot of Tested, Infected and Deaths
boxplot(Tested, col = "light blue", main = "Total Tested", ylab = "Tested", cex = 1.5)
boxplot(Infected, col = "red", main = "Total Tested Positive",
        ylab = "Tested +ve", cex = 1.5)
boxplot(Deaths, col ="gray20", main = "Total Deaths", ylab ="Deaths", cex = 1.5)

#Boxplot of Tested, Infected and Deaths 
boxplot(Pop.Density, col = "turquoise3", main = "Population Density",
        ylab = "Pouplation Density", cex = 1.5)
boxplot(Case.Fatality, col = "gray30", main = "Case Fatality",
        ylab = "Case Fatality", cex = 1.5)
boxplot(Positivity.Rate, col ="orangered", main = "Positivity Rate", 
        ylab ="Positivity Rate", cex = 1.5)

#Boxplot of Smoking Rate, Pollution, Temperature
boxplot(Smoking.Rate, col = "gray50", main = "Smoking Rate",
        ylab = "Smoking Rate", cex = 1.5)
boxplot(Pollution, col = "gray20", main = "Pollution",
        ylab = "Pollution", cex = 1.5)
boxplot(Temperature, col ="orangered3", main = "Temperature", 
        ylab ="Temperature", cex = 1.5)

#Adjust the window for two Boxplots
par(mfrow = c(1, 2))
#Boxplot of ICU Beds per 10k and Physicians per 10k
boxplot(ICU.Beds.10k, col = "orchid", main = "ICU Beds per 10k",
        ylab = "SICU Beds per 10k", cex = 1.5)
boxplot(Physicians.10k, col = "pink", main = "Physicians per 10k",
        ylab = "Physicians per 10k", cex = 1.5)

#Reset the graph window
par(mfrow = c(1, 1))

#Question 1
#Is there a significant difference between the mean of Positivity Rate of the
#Republican Controlled States and the Democratic Controlled States?

#Method used - two-sample independent unpaired t-test 

#Checking for assumptions
#qqplot of the republican states
Republican.Pos.Rate = Positivity.Rate[Governors == "R"]
qqnorm(Republican.Pos.Rate, col = "red", pch = 16, 
       main = "Normal Q-Q Plot of Republican States")
qqline(Republican.Pos.Rate, lwd = 3)
#qqplot of the democratic states
Democratic.Pos.Rate = Positivity.Rate[Governors == "D"]
qqnorm(Democratic.Pos.Rate, col = "blue", pch = 16,
       main = "Normal Q-Q Plot of Democratic States")
qqline(Democratic.Pos.Rate, lwd = 3)

#Performing the test
#Null hypothesis(H0) - There is no difference in means
#Alternate hypothesis - There is a difference in means

t.test(x = Republican.Pos.Rate, y = Democratic.Pos.Rate, 
       alternative = "two.sided", paired = FALSE)

#p-value < alpha Reject H0 and accept H1
#This indicates that the positivity rate of the Republican states is
#higher than the positivity rate of the Democratic states

#Question 2
#Is there a relationship between the case fatality ratio and the number of ICU 
#beds and Physicians relative to the population and is the interaction significant,
#if yes then how much of the variability is explained by these factors?

#Method used - Multiple Regression

#Making the linear Model
lmodel = lm(Case.Fatality~Physicians.10k*ICU.Beds.10k)

#Checking Assumptions
plot(lmodel)

#Analyzing the results and looking at the Anova table
summary(lmodel)
#p-value < alpha Reject H0 and accept H1
anova(lmodel)
#This is an indication that there is some correlation between ICU beds, 
#Physicians, and the interaction is significant. Although the slope cannot be
#interpreted easily we can see there is a relation.


#Question 3
#Is there a correlation between the Positivity rate the average Temperature 
#of the state? 

#Make a linear model
temperatureModel = lm(Positivity.Rate~Temperature)
#Check Assumptions
plot(temperatureModel)

#View a plot
plot(Positivity.Rate~Temperature, pch = 16, col = "orangered")

cor.test(Positivity.Rate, Temperature, alternative = "two.sided")
#p-value > alpha accept H0

#In the end, we can conclude that the Positivity Rate is related to the
#average temperature of the state. 
