                    # Germany - Cars Exploratory Data Analysis
#Dataset scraped from AutoScout24 with information about new and used cars.
#AutoScout24 is one of the largest Europe's car market for new and used cars. We've collected car data from 2011 to 2021.

library(readr) #to read rectangular data (like csv, tsv, and fwf).


#To read tha datset

cars<-read.csv("autoscout24-germany-dataset.csv") 


#To print head of the dataframe

head(cars)

# Attaching package:'dplyr'
library(dplyr) #used for easy and crisp data manipulation prior to modeling.

glimpse(cars)#shows overall structure of the data according to library dplyr.
str(cars)#str is a compact way to display the structure of an R object.

cars<-rename(cars,maker=make,fuel_type=fuel,transmission=gear,offer_type=offerType,horsepower=hp)
#Renamed some variables for better understanding in analysis.

sum(is.null(cars)) 
#null values not founded

sum(is.na(cars)) 
#founded 29 na values

summary(cars)
#using summary function founded 29 NA values in hp variable.

dim(cars)
#Dimension of cars is 46376 rows & 9 columns

# 1. Description of Individual Variables
#We have 9 variables which is plotted individually.

# 1.1. Price
summary(cars$price)#Shows min,max,mean,median,1st quarter,3rd quarter values of price.

range(cars$price)#Range function is used to know the minimum & maximum value of a variable.
#range of price is 1100 & 1199900.

diff(range(cars$price))#diff function is used for find the difference of range.
#The difference of price range is 1198800.

#Loading libraries for plotting.
library(ggplot2)
library(scales)
library(grid)
library(GGally)

table(cars$price)#shows the table of price.

glimpse(cars$price)#price has 46405 in count.

summary(cars$price)

ggplot(cars,aes(price))+
  stat_density(fill="deepskyblue4")+scale_x_log10(labels = scales::dollar_format(suffix = "$", prefix = ""))+
  labs(title="Cars Price",subtitle="In Dollars",caption="Germany Cars")+
  geom_vline(xintercept = mean(cars$price), size = 1, colour = 'red') +
  geom_vline(xintercept = median(cars$price), size = 1, colour = 'yellow')

#The minimum and maximum prices for a car are 1100 and 1199900 respectively and the difference in price range is 1198800.
#From the density plot, the mean and median of the price is 16572 and 10999 respectively.
#According to the plot, most of the cars were sold below 10,000 dollars.

#1.2. Year
table(cars$year)#shows the table of the year.
summary(cars$year)

ggplot(data = subset(cars, !is.na(year)),
       aes(x = year)) +
  geom_histogram(binwidth = 1, fill = 'deepskyblue4') +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  geom_vline(xintercept = mean(cars$year),
             size = 1, colour = 'red') +
  geom_vline(xintercept = median(cars$year),
             size = 1, colour = 'blue')
#From the plot, we can see that every year the cars sold similarly. 
#The interesting fact is that the mean and median are the same in 2016 with sales of 4266, hence proved that the graph is symmetric.
#From the observation, I can confirm that most of the cars were sold in 2016 with a count of 4286.
#Compared to other years, the year 2011 has very low sales with 4112.

#1.3. Transmission 
table(cars$transmission)#shows the table of the transmission.

cars$transmission[cars$transmission == "" | cars$transmission == " "] <- NA  # Replace blank & space by NA
#which(!complete.cases(cars$transmission))
cars$transmission<-as.factor(cars$transmission)
class(cars$transmission)

table(cars$transmission)

table(cars$transmission)
ggplot(data = cars, aes(x = transmission)) +
  geom_bar(fill = 'deepskyblue4') 
#Car with manual transmission sold the most except others.
#But the automatic transmission is not bad it is in the second place.
#Also there are 182 NA values, I am dropping it because it may reduce the accuracy of the data.

#1.4. Mileage
summary(cars$mileage)

ggplot(data = cars, aes(x =mileage )) +
  geom_histogram(binwidth = 10000, fill = 'deepskyblue4') +
  geom_vline(xintercept = mean(cars$mileage), size = 1, colour = 'red') +
  geom_vline(xintercept = median(cars$mileage), size = 1, colour = 'blue')
#This not symmetric around the mean anymore. 
#For a right skewed distribution, the mean is typically greater than the median. 
#Also notice that the tail of the distribution on the right hand (positive) side is longer than on the left hand side.

#1.5. Horsepower
table(cars$horsepower)
summary(cars$horsepower)

which(!complete.cases(cars$horsepower)) 
cars$horsepower[which(is.na(cars$horsepower))]<-mean(cars$horsepower,na.rm = TRUE)
sum(is.na(cars$horsepower))
#NA values not founded.

ggplot(data = subset(cars, !is.na(horsepower)),
       aes(x = horsepower)) +
  geom_histogram(binwidth = 50, fill = 'deepskyblue4') +
  scale_x_continuous(breaks = seq(0, 850, 20)) +
  geom_vline(xintercept = mean(cars$horsepower), size = 1, colour = 'brown') +
  geom_vline(xintercept = median(cars$horsepower), size = 1, colour = 'blue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Skewed right (positive) symmetric around the mean.
#Most of the cars horsepower lies in median range.

#1.6. Fuel Type
table(cars$fuel_type)

ggplot(data = cars, aes(x = fuel_type)) +
  geom_bar(fill = 'deepskyblue4') 
#The cars with gasoline have more sales compared with others.
#The diesel cars also have some contribution on sales.
#The electric cars on different segments also have future scope.

#1.7. Maker
table(cars$maker)

cars$maker<-factor(cars$maker)
class(cars$maker)
glimpse(cars$maker)
summary(cars$maker)

ggplot(data = cars, aes(x = maker)) +
  geom_bar(fill = 'deepskyblue4')+coord_flip()
#The Volkswagen has highest in the sales. Others maker's sales are fluctuating
#9ff,Brilliance,DAF,FISKER,Isuzu,Tazzari,Trucks-Lkw,Zhidou this maker's sold the lowest with 1 car.

#1.8. Offer_Type
table(cars$offer_type)

ggplot(data = cars, aes(x = offer_type)) +
  geom_bar(fill = 'deepskyblue4')
#Used cars having more sales on Germany.
#Cars with Pre-registeration and Demonstrated are approximately similar in sales.
#New car sales is very low with 13 cars.

#2.1 Price Vs Maker

ggplot(cars,aes(x = price, y = maker)) +
  geom_line()   

summary(cars$price)

subset(cars,cars$price==1199900)
subset(cars,cars$price==1100)
#Ferrai having more price compared to other because its is too costly.
#Ferrai has got maximun price value of "1199900 dollars" and Citroen has got minimum price value of "1100 dollars".

#2.2 Price Vs Fuel_type

ggplot(cars,aes(x=fuel_type, y=price)) + 
  geom_bar(stat="identity", width=.5, fill="deepskyblue4") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Fuel Vs Price", 
       caption="source: ") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

#For cars with diesel and gasoline fuel type gives more influence in the pricing.
#The influence of price in gasoline cars are higher than all fuel_type.

#2.3 Price Vs Transmission

ggplot(cars,aes(x=transmission, y=price)) + 
  geom_bar(stat="identity", width=.5, fill="deepskyblue4") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Transmission Vs Price") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))
#For cars with automatic and manual transmission gives more influence in the pricing.
#The influence of price in automatic cars are higher than other transmission.

#2.4 Price Vs Year

ggplot(cars,aes(x=year, y=price)) + 
  geom_bar(stat="identity", width=.5, fill="deepskyblue4") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Price Vs Year") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.5))
#Compared to other years, the year 2019 to 2021 the increase of price is higher in state.
#But the year 2020 has the highest price and the year 2011 has very low in price.

#2.5 Price Vs Horsepower

ggplot(data = cars, aes(x = horsepower, y = price)) + 
  geom_point(fill = "deepskyblue1",
             shape = 21, alpha = .15, size = 1, position = 'jitter') +
  geom_smooth()+labs(subtitle="Price Vs Horsepower")
#From the scatter plot we can see that price and horse power have some relation.

#2.6 Price Vs Offer_Type

ggplot(cars, aes(x=offer_type, y=price)) + 
  geom_bar(stat="identity", width=.5, fill="deepskyblue1") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Price Vs Offer_Type", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
# Used cars have some resale value in Germany.
#Price have some influence on used car models.

#2.7 Price Vs Mileage

ggplot(data = cars, aes(x = mileage, y = price)) + 
  geom_point(fill = "deepskyblue1",
             shape = 21, alpha = .15, size = 1, position = 'jitter') +
  geom_smooth()+labs(subtitle="Price Vs Mileage")
#From the scatter plot the trend line is become flat and there is no relation between price & mileage.

#2.8.1 Price vs. Power vs. Fuel Type

ggplot(data = subset(cars, fuel_type %in% c("Diesel", "Gasoline")),
       aes(x = horsepower, y = price, fill = fuel_type)) +
  geom_point(shape = 21, alpha = .15, size = 1, position = 'jitter') +
  scale_fill_brewer(type = 'qual',
                    guide = guide_legend(reverse = TRUE,
                                         override.aes = list(alpha = 1,
                                                             size = 2))) +
  geom_smooth(aes(colour = fuel_type))
#The fuel_type have some relation between horse power and price.
#The Gasoline car have more relation than diesel.

#2.8 Price vs. Horsepower vs. Fuel_Type vs. Transmission
#Let's try to plot these variables together:

ggplot(data = subset(cars, 
                     fuel_type %in% c("Diesel", "Gasoline") & !is.na(transmission)),
       aes(x = log(horsepower), y = price)) +
  geom_point(shape = 21, fill = 'deepskyblue1', 
             alpha = .05, position = 'jitter') +
  geom_smooth() +
  facet_grid(transmission ~ fuel_type)
#The car with gasoline and automatic transmission have relation with price and horsepower.
#Diesel cars also have some relation.

#-----------------------CONCLUSION-----------------------------------------------#
#AutoScout24 is one of the largest Europe's car market for new and used cars. We've collected car data from 2011 to 2021.

#The minimum and maximum prices for a car are 1100 and 1199900 respectively and the difference in price range is 1198800.
#From the density plot, the mean and median of the price is 16572 and 10999 respectively.
#According to the plot, most of the cars were sold below 10,000 dollars.

#We can see that every year the cars have similar sales and 2016 has more sales which equivalent to mean and median. 

#Car with manual transmission sold the most except others.

#The cars with gasoline,have more sales compared with others.

#The electric cars on different segments also have future scope.

#The Volkswagen has highest in the sales. Others maker's sales are fluctuating
#9ff,Brilliance,DAF,FISKER,Isuzu,Tazzari,Trucks-Lkw,Zhidou this maker's sold the lowest with 1 car.

#Used cars having more sales on Germany.
#Cars with Pre-registeration and Demonstrated are approximately similar in sales.
#New car sales is very low with 13 cars.

#Ferrai having more price compared to other because its is too costly.
#Ferrai has got maximun price value of "1199900 dollars" and Citroen has got minimum price value of "1100 dollars".

#For cars with diesel and gasoline fuel type gives more influence in the pricing.
#The influence of price in gasoline cars are higher than all fuel_type.

#For cars with automatic and manual transmission gives more influence in the pricing.
#The influence of price in automatic cars are higher than other transmission.

#Compared to other years, the year 2019 to 2021 the increase of price is higher in state.
#But the year 2020 has the highest price and the year 2011 has very low in price.

#From the scatter plot we can see that price and horse power have some relation.

#Used cars have some resale value in Germany.
#Price have some influence on used car models.

#From the scatter plot the trend line is become flat and there is no relation between price & mileage.

#The fuel_type have some relation between horse power and price.
#The Gasoline car have more relation than diesel.

#The car with gasoline and automatic transmission have relation with price and horsepower.
#Diesel cars also have some relation.

