setwd("C:/Users/Yashaswin/Downloads/cQuant")
library(dplyr)
library(assertive)
library(ggplot2)

#Task 1

#read CSV file
raw_data_set1 <- read.csv(file= "historicalPriceData_ERCOT_DA_Prices_2016.csv")
raw_data_set2 <- read.csv(file = "historicalPriceData_ERCOT_DA_Prices_2017.csv")
raw_data_set3 <- read.csv(file = "historicalPriceData_ERCOT_DA_Prices_2018.csv")
raw_data_set4 <- read.csv(file = "historicalPriceData_ERCOT_DA_Prices_2019.csv")

#Convert to dataframe

data_set1 <- data.frame(raw_data_set1)
data_set2 <- data.frame(raw_data_set2)
data_set3 <- data.frame(raw_data_set3)
data_set4 <- data.frame(raw_data_set4)

#merge all data frames
combined_data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data_set1, data_set2, data_set3, data_set4))

#Summary to check for data cleaning
summary(combined_data)

combined_data$Date <- as.Date(combined_data$Date)
combined_data$Date <- format(as.Date(combined_data$Date), "%Y-%m")
combined_data$Date <- as.factor(combined_data$Date)
combined_data$SettlementPoint <- as.character(combined_data$SettlementPoint)

str(combined_data$Date)

summary(combined_data)


check <- as.factor(combined_data$Date)
check2 <- format(as.Date(check), "%Y-%m")
check3 <- as.factor(check2)

#Task 2



average <- combined_data %>%  
            group_by(SettlementPoint, Date)%>%
            summarise(AveragePrice = mean(Price))

average$Date <- as.character(average$Date)

average_final <- average %>%
                 mutate(Year = substr(Date,0,4), Month = substr(Date,6,7))

average_final <- subset(average_final, select = c("SettlementPoint","Year","Month","AveragePrice"))
average_final

#Task3
#Write to CSV

write.csv(average_final,"C:\\Users\\Yashaswin\\Downloads\\cQuant\\AveragePriceByMonth.csv", row.names = FALSE)



#Task 4

combined_data_task4 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data_set1, data_set2, data_set3, data_set4))

#(combined_data_task4$Date)

hour <- substr(combined_data_task4$Date,12,13)


 
a <- as.character(combined_data_task4$SettlementPoint)
a_sub <- substr(a,0,2)

task4 <- cbind(combined_data_task4,a_sub)
task4 <- cbind(task4,hour)
task4$a_sub <- as.factor(task4$a_sub)
task4$hour <- as.numeric(task4$hour)
task4$Date <- as.Date(task4$Date)
summary(task4)



task4$year <- as.numeric(task4$year)
task4$SettlementPoint <- as.factor(task4$SettlementPoint)
task4

table4_temp$year <- as.numeric(table4_temp$year)
table4_temp$SettlementPoint <- as.factor(table4_temp$SettlementPoint)
#str(table4_temp$year)

table4_temp <- subset(table4_temp, select = c("Date", "SettlementPoint", "Price", "a_sub", "hour", "year"))

summary(table4_temp)

table4 <- table4_temp %>%
          filter(Price>0) %>%
          filter(a_sub == "HB") 
       

#Tedious!!! I'm sure I can come up with something more efficient!!

point1 <- table4 %>%
          filter(SettlementPoint == "HB_BUSAVG")
          

n1 <- length(point1$Price)
lreturn1 <- log(point1$Price[-1]/point1$Price[-n1])
point1 <- cbind(point1,c(NA,lreturn1))
names(point1) <- c("Date","SettlementPoint","Price","a_sub", "hour", "Year","HourlyVolatility")




point2 <- table4 %>%
  filter(SettlementPoint == "HB_HOUSTON")


n2 <- length(point2$Price)
lreturn2 <- log(point2$Price[-1]/point2$Price[-n2])
point2 <- cbind(point2,c(NA,lreturn2))
names(point2) <- c("Date","SettlementPoint","Price","a_sub", "hour", "Year","HourlyVolatility")



point3 <- table4 %>%
  filter(SettlementPoint == "HB_HUBAVG")


n3 <- length(point3$Price)
lreturn3 <- log(point3$Price[-1]/point3$Price[-n3])
point3 <- cbind(point3,c(NA,lreturn3))
names(point3) <- c("Date","SettlementPoint","Price","a_sub", "hour", "Year","HourlyVolatility")




point4 <- table4 %>%
  filter(SettlementPoint == "HB_NORTH")


n4 <- length(point4$Price)
lreturn4 <- log(point4$Price[-1]/point4$Price[-n4])
point4 <- cbind(point4,c(NA,lreturn4))
names(point4) <- c("Date","SettlementPoint","Price","a_sub", "hour", "Year","HourlyVolatility")




point5 <- table4 %>%
  filter(SettlementPoint == "HB_SOUTH")


n5 <- length(point5$Price)
lreturn5 <- log(point5$Price[-1]/point5$Price[-n5])
point5 <- cbind(point5,c(NA,lreturn5))
names(point5) <- c("Date","SettlementPoint","Price","a_sub", "hour", "Year","HourlyVolatility")



point6 <- table4 %>%
  filter(SettlementPoint == "HB_WEST")


n6 <- length(point6$Price)
lreturn6 <- log(point6$Price[-1]/point6$Price[-n6])
point6 <- cbind(point6,c(NA,lreturn6))
names(point6) <- c("Date","SettlementPoint","Price","a_sub", "hour", "Year","HourlyVolatility")

final <- rbind(point1,point2,point3,point4,point5,point6)
final_table <- subset(final, select = c("SettlementPoint", "Year","HourlyVolatility"))

#Write to CSV

write.csv(final_table,"C:\\Users\\Yashaswin\\Downloads\\cQuant\\HourlyVolatilityByYear.csv", row.names = FALSE)


#Task6

final_table1 <- final_table %>%
                group_by(Year)%>%
                filter(HourlyVolatility == max(HourlyVolatility)) %>%
                arrange(SettlementPoint,Year,HourlyVolatility)

write.csv(final_table,"C:\\Users\\Yashaswin\\Downloads\\cQuant\\MaxVolatilityByYear.csv", row.names = FALSE)


#Task 7

data1 <- table4_temp %>%
          filter(SettlementPoint == "HB_BUSAVG", year == 2016) %>%
          select(c("SettlementPoint","Price","hour"))
data1

n = length(data1$Price)

x <- vector()
for(i in 1:n)
  x[i] = data1$Price[i]
  
#x is price distribution per hour
#Ran out of time!


