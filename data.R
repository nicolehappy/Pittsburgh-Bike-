library(plyr)

## load the data
filePath = 'D:/Study_Pitt/INFSCI2160_Data Mining/FinalProject/HealthyRideTripData2015Q3/'
Q3 = read.csv(sprintf('%s%s',filePath,'HealthyRideRentals2015Q3.csv'))
filePath = 'D:/Study_Pitt/INFSCI2160_Data Mining/FinalProject/'
dailyweather = read.csv(sprintf('%s%s',filePath,'Q3Q4_daily.csv'))

## check the empty value and omit the empty value
any(is.na(Q3))
Q3 <- na.omit(Q3)
any(is.na(dailyweather))

## select the starttime by month, day and hour
starttime <- Q3[,2:4]
stoptime <- Q3[,5:7]
## count the freq
starttime <- count(starttime,c("StartTimeMonth","StartTimeDay","StartTimeHour"))
stoptime <- count(stoptime,c("StopTimeMonth","StopTimeDay","StopTimeHour"))
## sum the freq by month and day
count_starttime_MD <- aggregate(freq ~ StartTimeMonth + StartTimeDay, data = starttime, sum)
count_stoptime_MD <- aggregate(freq ~ StopTimeMonth + StopTimeDay, data = stoptime, sum)

## select the dailyweather by month, day and holiday
daily_holiday <- dailywether[, c(1,2,5)]
## merge the daily_holiday with starttime and stoptime
m_startholiday <- merge(starttime, daily_holiday, by.x = c("StartTimeMonth","StartTimeDay"), by.y = c("Month","Day"))
m_stopholiday <- merge(stoptime, daily_holiday, by.x = c("StopTimeMonth","StopTimeDay"), by.y = c("Month","Day"))
## select the m_startholiday and m_stopholiday by holiday or not
m_startholiday_sp <- split(m_startholiday, m_startholiday$weekday)
m_startholiday_w <- m_startholiday_sp[[2]]
m_startholiday_h <- m_startholiday_sp[[1]]
m_stopholiday_sp <- split(m_stopholiday, m_stopholiday$weekday)
m_stopholiday_w <- m_stopholiday_sp[[2]]
m_stopholiday_h <- m_stopholiday_sp[[1]]
## sum the freq by hour and holiday or not
count_starttime_w = aggregate(freq ~ StartTimeHour, data = m_startholiday_w, sum)
count_starttime_h = aggregate(freq ~ StartTimeHour, data = m_startholiday_h, sum)
count_stoptime_w = aggregate(freq ~ StopTimeHour, data = m_stopholiday_w, sum)
count_stoptime_h = aggregate(freq ~ StopTimeHour, data = m_stopholiday_h, sum)
## merge the starttime and stoptime by month, day and holiday
m_startstoptime_MD <- merge(count_starttime_MD, count_stoptime_MD, by.x = c("StartTimeMonth","StartTimeDay"), by.y = c("StopTimeMonth","StopTimeDay"))
## merge the starttime and stoptime by hour(weekday and holiday)
m_startstoptime_w <- merge(count_starttime_w, count_stoptime_w, by.x = "StartTimeHour", by.y = "StopTimeHour")
m_startstoptime_h <- merge(count_starttime_h, count_stoptime_h, by.x = "StartTimeHour", by.y = "StopTimeHour")
## rename the column
m_startstoptime_MD <- rename(m_startstoptime_MD, c("freq.x"="inCount", "freq.y"="outCount"))
m_startstoptime_w <- rename(m_startstoptime_w, c("freq.x"="inCount", "freq.y"="outCount"))
m_startstoptime_h <- rename(m_startstoptime_h, c("freq.x"="inCount", "freq.y"="outCount"))
