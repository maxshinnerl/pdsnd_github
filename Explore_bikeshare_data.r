# R version (not python) (see README)

# load in files and ggplot

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')
library(ggplot2)

# head(ny)

# head(wash)

# head(chi)

# Your solution code goes here
# use na.omit to remove NA rows in sum/mean calculation

# save mean for each city's trip duration
washAvg = as.numeric(mean(na.omit(wash$Trip.Duration)))
chiAvg = as.numeric(mean(na.omit(chi$Trip.Duration)))
nyAvg = as.numeric(mean(na.omit(ny$Trip.Duration)))

# create new df with data for each city
res1 <- data.frame(
  cityName = c("Washington", "Chicago", "New York"),
  averageTime = c(washAvg/60, chiAvg/60, nyAvg/60)
  )

head(res1)


# graph each city's mean travel time separately to compare the three

qplot(x = cityName, y = averageTime, data = res1,
     xlab = "City Name",
     ylab = "Average Ride Duration (minutes)",
     main = "Average Ride Duration in Each City") +
  geom_bar(stat = "identity")

# extract month number from each ride's start time
# create new column with just month value

wash$Start.Time<-as.Date(wash$Start.Time)
wash$Start.Month <- format(as.Date(wash$Start.Time), "%m")

ny$Start.Time<-as.Date(ny$Start.Time)
ny$Start.Month <- format(as.Date(ny$Start.Time), "%m")

chi$Start.Time<-as.Date(chi$Start.Time)
chi$Start.Month <- format(as.Date(chi$Start.Time), "%m")

# holders store column with all the month numbers
holder1 <- wash$Start.Month
holder2 <- ny$Start.Month
holder3 <- chi$Start.Month

# append the months to the same column
test = append(holder1, holder2)
months = append(test, holder3)

# save as a data frame so we can graph it
res2 = as.data.frame(months)

summary(res2)

# graph the count of rides for each month to compare each month's count
qplot(x = months, data = subset(res2, !is.na(months)),
     xlab = 'Month Number',
     ylab = 'Number of Rides',
     main = 'Number of Rides per Month Across All Cities')

# Your solution code goes here
# gender count pie chart\
# only available from ny and chicago, not washington

# function to count each gender given a column of genders
# returns (numMale, numFemale)
gender.count = function(col){
    male = 0
    female = 0
    for (item in col){
        if (item == "Male"){
            male = male + 1
            }
        # not just "else" -> remove na's
        else if(item == "Female"){
            female = female + 1
        }
    }
    test = c(male, female)
    print(test)
    return(test)
}

# call it on data
nyGen = gender.count(ny$Gender)
chiGen = gender.count(chi$Gender)

# find total male rides and total female rides
mCount = nyGen[1] + chiGen[1]
fCount = nyGen[2] + chiGen[2]

# find total rides from male/female combined
totalMFriders = mCount + fCount
# print(totalMFriders)

# save data in data frame for easy graphing
res3 <- data.frame(
  gender = c("Male", "Female"),
  count = c(mCount, fCount)
  )
head(res3)

# store plot in variable
plot1 <- ggplot(aes(x="", y=count, fill=gender), data = res3)+
geom_bar(width = 1, stat = "identity")

# now make it a pie chart
pie <- plot1 + coord_polar("y", start=0)

# run it
pie + labs(title="Number of Rides with Male vs Female Customers",
        x ="Male: 42360 \nFemale: 13882" , y = "Total male/female riders: 56242")
        # would have liked to use previous variables here instead of magic numbers
        # couldn't get it to comply with labs() syntax unfortunately

system('python -m nbconvert Explore_bikeshare_data.ipynb')
