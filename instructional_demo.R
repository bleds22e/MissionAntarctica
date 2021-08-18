# EKB
# Data exploration
# Aug 2021

# PACKAGES #
library(tidyverse)
library(forecast)
library(maps)
library(patchwork)

# TUCSON ####

# DATA #
# Average monthly temperature (Celcius) data from the Tucson Airport
# Downloaded from NASA: https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00023160&dt=1&ds=14

tucson <- read_csv("../../../Desktop/Tucson_Intl_Airport_WS.csv")

# let's take a look
glimpse(tucson)
# note the 999.9s in the data
# this is a common way of indicating that there is no data, or NA

# indicate that NA is represented by 999.9 in the data
tucson <- read_csv("../../../Desktop/Tucson_Intl_Airport_WS.csv",
                   na = "999.9")

glimpse(tucson)
head(tucson)
summary(tucson)

# plot the summer data
ggplot(data = tucson, aes(x = YEAR, y = `J-J-A`)) +
  geom_point() 

# good start, but let's make it better
ggplot(data = tucson, aes(x = YEAR, y = `J-J-A`)) +
  geom_point() +
  geom_line() +
  stat_smooth() +
  labs(
    title = "Mean Summer Temperature at Tucson Intl. Airport (1949-2021)",
    subtitle = "June, July, August",
    x = "Year",
    y = expression("Mean Summer Temp ("*degree*"C)")
  ) +
  theme_bw()
# clear upward trend in temperatures

# now let's extrapolate
# NOTE: I'm not suggesting that this is the best method for prediction!
# simply an example
temp <- ggplot(data = tucson, aes(x = YEAR, y = `J-J-A`)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 35) + # 35 wet-bulb is too much
  xlim(1945, 2200) +
  stat_smooth(method = 'lm', fullrange = TRUE) +
  labs(
    title = "Mean Summer Temperature at Tucson Intl. Airport (1949-2021)",
    subtitle = "June, July, August",
    x = "Year",
    y = expression("Mean Summer Temp ("*degree*"C)")
  ) +
  theme_bw()

# time-series analysis
summer_ts <- ts(tucson$`J-J-A`)
model <- auto.arima(summer_ts)
plot(forecast(model, h = 20))
plot(forecast(model, h = 200))
abline(h = 35)

# map
us_states <- map_data("state") %>% 
  rename(Latitude = lat, Longitude = long)
tucson_airport <- data.frame(
  Latitude = c(32.1314),
  Longitude = c(-110.9553)
)

map <- ggplot(us_states) +
  geom_polygon(aes(x = Longitude, y = Latitude, group = group), 
               fill = "white", color = "black") +
  coord_quickmap() + 
  geom_point(tucson_airport, mapping = aes(x = Longitude, y = Latitude), 
             color = "red", size = 8, shape = 18) +
  theme_bw()

final_plot <- map + temp
final_plot
