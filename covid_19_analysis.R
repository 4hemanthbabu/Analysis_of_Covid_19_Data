library(ggplot2)
library(broom)
library(ggpubr)
library(matrixStats)
library(dplyr)
#Reading the File
file = read.csv('~/Downloads/covid_19_data.csv')
print(file)
#grouping the data
file_1= aggregate(list(Confirmed=file$Confirmed,Deaths=file$Deaths,Recovered=file$Recovered), by=list(Country.Region=file$Country.Region), FUN=sum)
print(file_1)
# first 10 values
print(head(file_1, 10))
# last 10 values
print(tail(file_1, 10))
# Column names of the data
print(colnames(file_1))
# dimension of the data
print(dim(file_1))
# information of the data
print(?file_1)
# summary of the data
print(summary(file_1))
# Column Values' Sum
Confrirmed_sum = sum(file_1[, 'Confirmed'])
print(Confrirmed_sum)
Death_sum = sum(file_1[, 'Deaths'])
print(Death_sum)
Recovered_sum = sum(file_1[, 'Recovered'])
print(Recovered_sum)
# Mean and Median Values of Column values'
confirmed_mean = mean(file_1[['Confirmed']])
print(confirmed_mean)
confirmed_median = median(file_1[['Confirmed']])
print(confirmed_median)
death_mean = mean(file_1[['Deaths']])
print(death_mean)
death_median = median(file_1[['Deaths']])
recovered_mean = mean(file_1[['Recovered']])
print(recovered_mean)
recovered_median = median(file[['Recovered']])
print(recovered_median)
# Minimum and Maximum Values in Columns
print(min(file_1$Confirmed))
print(max(file_1$Confirmed))
print(min(file_1$Deaths))
print(max(file_1$Deaths))
print(min(file_1$Recovered))
print(max(file_1$Recovered))
# QUARTILE Value of Columns
# Confirmed
print(quantile(file_1$Confirmed, 0.25))
print(quantile(file_1$Confirmed,0.50)) 
print(quantile(file_1$Confirmed, 0.75))
# Deaths
print(quantile(file_1$Deaths, 0.25))
print(quantile(file_1$Deaths,0.50)) 
print(quantile(file_1$Deaths, 0.75))
#Recovered
print(quantile(file_1$Recovered, 0.25))
print(quantile(file_1$Recovered,0.50)) 
print(quantile(file_1$Recovered, 0.75))
# Regression Analysis
# Visualization of Confirmed
hist(file_1$Confirmed)
# Visualization of Deaths
hist(file_1$Deaths)
# Realtionship Between Confirmed Cases and Deaths
plot(Deaths ~ Confirmed, data = file_1)
lr_1 = lm(Deaths ~ Confirmed, data = file_1)
print(lr_1)
summary(lr_1)
par(mfrow = c(2,2))
plot(lr_1)
graph_lr_1= ggplot(file_1, aes(x = Confirmed, y = Deaths)) + geom_point()
print(graph_lr_1)
graph_lr_1 = graph_lr_1 + stat_regline_equation(label.x = 2, label.y = 6)
print(graph_lr_1)
graph_lr_1 = graph_lr_1 + theme_bw() + labs(title = 'Confirmed_cases and deaths',x = 'Confirmed', y = 'Deaths')
print(graph_lr_1)
# Relationship Between Confirmed_Cases and Recoveries
hist(file_1$Recovered)
plot(Recovered ~ Confirmed, data = file_1)
lr_2 = lm(Recovered ~ Confirmed, data = file_1)
print(lr_2)
summary(lr_2)
par(mfrow = c(3,3))
plot(lr_2)
graph_lr_2= ggplot(file_1, aes(x = Confirmed, y = Recovered)) + geom_point()
print(graph_lr_2)
graph_lr_2 = graph_lr_2 + stat_regline_equation(label.x = 4, label.y = 6)
print(graph_lr_2)
graph_lr_2 = graph_lr_2 + theme_bw() + labs(title = 'Confirmed_cases and Recoveries',x = 'Confirmed_Cases', y = 'Recoveries')
print(graph_lr_1)
# Data Visualization
Countries = file_1$Country.Region
Confirmed = file_1$Confirmed
Deaths = file$Deaths
Recovered = file$Recovered
Countries = file_1$Country.Region
Confirmed = file_1$Confirmed
print(ggplot(file_1, aes(x=Countries, y=Confirmed)) + 
        geom_bar(stat = 'identity'))
print(ggplot(file_1, aes(x=Countries, y=Deaths)) + 
        geom_bar(stat = 'identity') +
        coord_flip())
print(ggplot(file_1, aes(x=Countries, y=Recovered)) +
        geom_segment( aes(x=Countries, xend=Countries, y=0, yend=Recovered), color='skyblue') +
        geom_point( color='blue', size=4, alpha=0.6) +
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()))