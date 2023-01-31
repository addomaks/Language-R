library('tidyverse')
library('ggplot2')
library('heatmaply')


Delhi <- read.csv('D:\\Adnan\\Predictive Analytics Course\\Multivariate Analysis\\Project 1\\Data\\AirQualityIndia\\2015-2020\\delhi.csv\\DelhiAQI.csv')
Delhi_24H <- read.csv('D:\\Adnan\\Predictive Analytics Course\\Multivariate Analysis\\Project 1\\Data\\AirQualityIndia\\2015-2020\\delhi.csv\\DelhiAQI_24H.csv')

Delhi_24H %>% ggplot(aes(x=tempC, y = PM10)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
cor(Delhi$PM10, Delhi$tempC)
#With the above negative trend of -0.216062 shows that, decrease in temperature leads yo increase in  particular matter PM10.

Delhi_24H %>% ggplot(aes(x= PM10, y = PM2.5)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
cor(Delhi$PM10, Delhi$PM2.5)


Delhi_24H %>% ggplot(aes(x = PM2.5, y=CO)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
cor(Delhi$PM2.5, Delhi$humidity)


plot(x=Delhi_24H$tempC, y=Delhi_24H$CO)
abline(lm(tempC ~ CO,  data = Delhi_24H), col = 'blue')

PM <- lm(PM2.5~PM10, data = Delhi)
plot(x = Delhi_24H$PM2.5, y = Delhi_24H$PM10, abline(PM, col = 'red'))
coef(PM)

predict(PM, data.frame(PM10 = c(100)))



tempoo <-lm(tempC ~ PM10, data = Delhi_24H)
plot(x = Delhi_24H$tempC, y = Delhi_24H$PM10, abline(tempoo, col = 'red'))
coef(tempoo)


Delhi_24H %>% ggplot(aes(x=tempC, y = PM10)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
cor(Delhi_24H$tempC, Delhi_24H$PM10)


Delhi_24H %>% ggplot(aes(x=Year, y=PM10)) + geom_boxplot() + geom_jitter(color = 'black', size = 0.4, alpha=0.9)


Trial1_Model <- lm(tempC ~ PM10, data = Delhi)
coef(Trial1_Model)


Trial2_Model <- lm(tempC ~ PM10 + PM2.5 + CO, data = Delhi)
coef(Trial2_Model)
summary(Trial1_Model)
summary(Trial2_Model)


Full_ModelT <- lm(AQI_VALUE ~  PM10 + PM2.5 + SO2 + sunHour  + mintempC +  humidity + windspeedKmph + Hour, data = Delhi)
coef(Full_ModelT)
summary(Full_ModelT)

FullModelT2 <- lm(tempC ~ AQI_VALUE + Hour + sunHour +  humidity  + uvIndex + Main_Pollutant + windspeedKmph + mintempC, data = Delhi)
coef(FullModelT2)
summary(FullModelT2)

FullModelT3 <- lm(AQI_VALUE ~ mintempC + maxtempC + PM10 + PM2.5 + sunHour +  humidity  + uvIndex + Month, data = Delhi)
coef(FullModelT3)
summary(FullModelT3)


Delhi %>% ggplot(aes(x=tempC, y = AQI_VALUE)) + geom_point() + geom_smooth(method = 'lm', se=FALSE)
Delhi_24H %>% ggplot(aes(x=Main_Pollutant, y = tempC)) + geom_boxplot() + geom_jitter(color = 'red', size = 0.2, alpha = 0.5) + geom_smooth(method = 'lm', se = FALSE)


predict(FullModelT2, data.frame(AQI_VALUE = c(400, 100),
                                Hour = c(20, 13),
                                sunHour = c(10,14),
                                humidity = c(70,21),
                                uvIndex = c(5,8),
                                Main_Pollutant = c('PM10', 'PM10'),
                                windspeedKmph = c(17,7),
                                mintempC = c(22,33)))

tempp <- lm(tempC ~ Main_Pollutant + AQI_VALUE + sunHour, data = Delhi_24H)
coef(tempp)
temppResidual <- tempp$residuals
tempfitted <- tempp$fitted.values
scatter.smooth(temppResidual)
scatter.smooth(tempfitted)
summary(tempp)



FullModelT2Residual <- FullModelT2$residuals
FullModelT2Fitted <- FullModelT2$fitted.values
scatter.smooth(FullModelT2Residual)
scatter.smooth(FullModelT2Fitted)
plot(FullModelT2Fitted, FullModelT2Residual)
hist(FullModelT2Residual)
qqnorm(FullModelT2Residual)


Delhi %>% ggplot(aes(x = AQI_VALUE, y = sunHour)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
Delhi_24H %>% ggplot(aes(x = sunHour, y = AQI_VALUE)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
