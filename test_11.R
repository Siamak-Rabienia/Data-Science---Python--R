rm(list=ls(all=TRUE))
library('tidyverse')
library('ggplot2') 
library('xlsx')
library('lme4')
library('nnet')
#library(stringr)
library(Matrix)
#library(dplyr)
#---------------------------------------------------------------------------------------------------
#Section 2
data1 <- read.csv(file.choose(),header=TRUE) ## Importing the Arrest_Data CSV file

save(data1,file='C:\\Users\\User\\Downloads\\Data Incubator2\\data1.RData')
load('C:\\Users\\User\\Downloads\\Data Incubator2\\data1.RData')

#---------------------------------------------------------------------------------------------------
#a) How many bookings of arrestees were made in 2018? 

mean(is.na(data1$ArrestDate)) # To check if there is any NA

class(data1$ArrestDate)
data1$ArrestDate_22 <- as.Date(data1$ArrestDate, format = "%m/%d/%Y")

Q <- which(data1$ArrestDate_22 >= "2018-01-01" )
temp <- data1[Q,]

Q <- which(temp$ArrestDate_22 < "2019-01-01")
temp <- temp[Q,]

unique(temp$ArrestDate_22)
min(temp$ArrestDate_22)
max(temp$ArrestDate_22)
NAzz <- is.na(temp$ArrestDate_22)
mean(NAzz)

temp$ArrestDate_33 <- format(as.Date(temp$ArrestDate_22, format="%m/%d/%Y"),"%Y")
unique(temp$ArrestDate_33)

t_s_e <-  temp %>% group_by(ArrestDate_33) %>%  
  summarise(countZ=n())
sprintf(t_s_e, fmt = '%#.10f')
# Answer: 30000


#---------------------------------------------------------------------------------------------------
#b) How many bookings of arrestees were made in the area with the most arrests in 2018?

t_s_e_1 <-  temp %>% group_by(AreaID) %>%  
  summarise(countZ=n())
MostArrest <-  which.max(t_s_e_1$countZ)
NumMostArrest <- t_s_e_1[MostArrest,2]
sprintf(NumMostArrest, fmt = '%#.10f')
# Answer: 4486

#---------------------------------------------------------------------------------------------------
#c) What is the 95% quantile of the age of the arrestee in 2018? 
#Only consider the following charge groups for your analysis:
#Vehicle Theft = 7
#Robbery = 3
#Burglary = 5
#Receive Stolen Property = 11

temp <- is.na(data1$ChargeGroupCode)
temp <- which(temp == FALSE)
temp <- data1[temp,] # No more NAs! :)
unique(temp$ChargeGroupCode)

# Now we filter those that don't have an entry (i.e., Make=='')
target <- c(3,5,7,11)
temp <- filter(temp,temp$ChargeGroupCode %in% target)
ANS <- quantile(temp$Age, prob = c(0.95)) 
sprintf(ANS, fmt = '%#.10f')

# Answer = 51.0000000000

#---------------------------------------------------------------------------------------------------
#d) There are differences between the average age of an arrestee for the various charge groups. 
#Are these differences statistically significant? For this question, calculate the Z-score 
#of the average age for each charge group. Report the largest absolute value among the calculated Z-scores.
#Only consider data for 2018
#Do not consider "Pre-Delinquency" and "Non-Criminal Detention" as these charge groups are reserved for minors
#Exclude any arrests where the charge group description is not known

data1$ArrestDate_22 <- as.Date(data1$ArrestDate, format = "%m/%d/%Y")

Q <- which(data1$ArrestDate_22 >= "2018-01-01" )
temp <- data1[Q,]

Q <- which(temp$ArrestDate_22 < "2019-01-01")
temp <- temp[Q,]

target <- c("Pre-Delinquency","Non-Criminal Detention")

temp <- filter(temp,temp$ChargeGroupCode %in% target)





unique(data1$Make) # There are some starange Makes, we first delete those entries that 
# have numbers, the reason is that it seems that in some cases instead of Make the operator entered the data instead
Q <- grepl("\\d", data1$Make)
temp <- data1[which(Q==FALSE),]
unique(temp$Make) 
# Now we filter those that don't have an entry (i.e., Make=='')
temp <- filter(temp,Make!='')
# Now we filter any of them containing a ' ', since no car make have an space in it 
Q <- grepl(" ", temp$Make)
temp <- temp[which(Q==FALSE),]
unique(temp$Make) 
# Now, to take care of cases like TOYT and TOYO (i.e., to make them the same thing), we truncate the string and 
# keep only the first 3 charachters
temp$Make <- substr(temp$Make, 1, 3)
# Now we filter any of them containing a '/', since no car make have a / in it
Q <- grepl("/|//|-", temp$Make)
temp$Make[which(Q==TRUE)]
temp <- temp[which(Q==FALSE),]
unique(temp$Make) 
# Now we filter any of them containing only one or two charachters since no Make car has one or two charachters
Q <- which(nchar(temp$Make)==3 | temp$Make=='VW')
temp <- temp[Q,]
unique(temp$Make)
# Now we filter any of them containing a "."
Q <- grepl(".", temp$Make,fixed=TRUE)
temp$Make[which(Q==TRUE)]
temp <- temp[which(Q==FALSE),]
unique(temp$Make)

t_s_d <-  temp %>% group_by(Make) %>%  
  summarise(mean_fine=n())

t_s_d$Make[which(t_s_d$mean_fine ==max(t_s_d$mean_fine) )] # asnwer is "HON"

ANS <- t_s_d$mean_fine[which(t_s_d$mean_fine ==max(t_s_d$mean_fine) )] 
sprintf(ANS, fmt = '%#.10f')

#---------------------------------------------------------------------------------------------------
#e) First, find the total number of citations given in each year between 2004 and 2014 (inclusive).
#Next, using linear regression, create a function that plots the total number of citations as a 
#function of the year. If you were to plot a line using this function, what would be the slope of 
#that line?

class(data1$NoticeDate)
data1$NoticeDate_22 <- as.Date(data1$NoticeDate, format = "%m/%d/%Y")
#Q <- which(data1$NoticeDate_22 >= "2004-01-01" && data1$NoticeDate_22 < "2015-01-01")
Q <- which(data1$NoticeDate_22 >= "2004-01-01" )
temp <- data1[Q,]

Q <- which(temp$NoticeDate_22 < "2015-01-01")
temp <- temp[Q,]

unique(temp$NoticeDate_22)
min(temp$NoticeDate_22)
max(temp$NoticeDate_22)
NAzz <- is.na(temp$NoticeDate_22)
mean(NAzz)

temp$NoticeDate_33 <- format(as.Date(temp$NoticeDate_22, format="%m/%d/%Y"),"%Y")
unique(temp$NoticeDate_33)

t_s_e <-  temp %>% group_by(NoticeDate_33) %>%  
  summarise(countZ=n())

t_s_e$NoticeDate_44 <- as.numeric(t_s_e$NoticeDate_33)

t_s_e$NoticeDate_55 <- t_s_e$NoticeDate_44 - 2004

ggplot(t_s_e,aes(x=NoticeDate_33,y=countZ))+ geom_point()+
  geom_smooth(method=lm, se=TRUE)

m1 <- lm(countZ~NoticeDate_55,data=t_s_e)
summary(m1)
sprintf(m1$coefficients[2], fmt = '%#.10f')

