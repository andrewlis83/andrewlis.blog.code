# 2021-09-16-is-it-hot-in-here-or-is-it-just-me
# Author: andrewlis.blog

# Looking at historical weather patterns.

library(tidyverse)
library(here)
library(rvest)
library(leaflet)
library(htmlwidgets)

# Variable Names (h/t: http://rstudio-pubs-static.s3.amazonaws.com/515896_447eb78ecf96411ea12118beb7a3965f.html)
# Stn_Name Station Name
# Lat Latitude (North+, degrees)
# Long Longitude (West - , degrees)
# Prov Province
# Tm Mean Temperature (?C)
# DwTm Days without Valid Mean Temperature
# D Mean Temperature difference from Normal (1981-2010) (?C)
# Tx Highest Monthly Maximum Temperature (?C)
# DwTx Days without Valid Maximum Temperature
# Tn Lowest Monthly Minimum Temperature (?C)
# DwTn Days without Valid Minimum Temperature S Snowfall (cm)
# DwS Days without Valid Snowfall
# S%N Percent of Normal (1981-2010) Snowfall
# P Total Precipitation (mm)
# DwP Days without Valid Precipitation
# P%N Percent of Normal (1981-2010) Precipitation
# S_G Snow on the ground at the end of the month (cm)
# Pd Number of days with Precipitation 1.0 mm or more
# BS Bright Sunshine (hours)
# DwBS Days without Valid Bright Sunshine
# BS% Percent of Normal (1981-2010) Bright Sunshine
# HDD Degree Days below 18 ?C
# CDD Degree Days above 18 ?C
# Stn_No Climate station identifier (first 3 digits indicate drainage basin, last 4 characters are for sorting alphabetically).
# NA Not Available


# Data from:
# https://www.canada.ca/en/environment-climate-change/services/climate-change/canadian-centre-climate-services/display-download/advanced-tools.html
URL <- "https://dd.weather.gc.ca/climate/observations/monthly/csv/BC/"
pg <- read_html("https://dd.weather.gc.ca/climate/observations/monthly/csv/BC/")

f_list <- html_attr(html_nodes(pg, "a"), "href")
f_list <- f_list[grep('*M.csv', f_list)]

# Use this snippet to get a list of the date range for a specific station:
f_list[grep('1018621', f_list)] 


# First, pull all the stations that have data for 2021 in BC, to get a list
# of the available stations, and get sense of how complete the data is for each.

pull <- f_list[grep('*_2021', f_list)]
ext_l <- list()
for (p in pull) {
  print(p)
  ext_l[[p]] <- read_csv(paste0(URL, p), show_col_types = FALSE)
}

stations <- do.call(rbind, ext_l)
write.csv(stations, here('_data', 'stations_2021.csv'))



# Have a poke around at the weather stations on a map.

stn <- filter(stations, Month == 5)  #Arbitrarily picked a month that seems to have data for all stations.

m <- leaflet(stn) %>%
  addProviderTiles("OpenStreetMap") %>%
  addCircleMarkers(lng = stn$Longitude,
                   lat = stn$Latitude,
                   popup = paste(stn$`Station Name`, stn$`Climate ID`, sep = '\n'),
                   fillColor = "darkblue",
                   fillOpacity = .45,
                   radius = 4,
                   stroke = F)
m
saveWidget(m, here('_outputs', 'stationMap.html'))

# Some stations of interest...
# 1011500 Chemainus (missing about 10 years in the 80s)
# 1012055 Lake Cowichan (there's a level shift in the 2000s in some data...)
# 1012710 ESQUIMALT HARBOUR	
# 1015105 Metchosin
# 1015638 North Pender 
# 1016940 SAANICHTON CDA 
# 1018611 VICTORIA GONZALES 


# Pull all the historical data for a specific station, then collapse to one dataframe and save locally.

# Enter the station code in the line below:
pull <- f_list[grep('*1018611', f_list)]
ext_l <- list()
for (p in pull) {
  print(p)
  ext_l[[p]] <- read_csv(paste0(URL, p), show_col_types = FALSE)
}

weather_df <- do.call(rbind, ext_l)

# Save output, so that we don't have to pull it again in the future.
write.csv(weather_df, here('_data', 'VictoriaGonzales.csv'))


# Look at White Rock
weather_df <- read_csv(here('_data', 'WhiteRockCampbellScientific.csv')) %>% select(-1)

plot_dat <- weather_df %>%
  select(-c('Station Name', 'Province', 'Longitude' , 
            'Latitude', 'Climate ID')) %>%
  mutate(Date = as.Date(paste(Year, 
                              ifelse(nchar(Month)==1, paste0('0', Month), Month), 
                              '01', sep = '-'))) %>%
  select(-c('Year', 'Month')) %>%
  pivot_longer(!Date)

p <- ggplot(plot_dat, aes(x=Date, y=value, color = name)) +
  geom_point(alpha = 0.15) + 
  geom_smooth(method = 'loess', se = F) +
  facet_wrap('name', scales = 'free', ncol=4) +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust =0.5), legend.position = 'none') +
  labs(title = 'White Rock Campbell Scientific Weather Station',
       subtitle = 'Historical Weather Data',
       x = 'Date',
       y = '',
       caption = 'Source Data: Canadian Centre for Climate Services\nandrewlis.blog')

ggsave(here('_outputs', 'WhiteRockCampbellScientific.svg'), p, device = 'svg', width=8, height=11)



# Look at Saanichton
weather_df <- read_csv(here('_data', 'Saanichton.csv')) %>% select(-1)

weather_df[weather_df == '######'] <- NA
weather_df$`S%N` <- as.numeric(weather_df$`S%N`)

plot_dat <- weather_df %>%
  select(-c('Station Name', 'Province', 'Longitude' , 
            'Latitude', 'Climate ID')) %>%
  mutate(Date = as.Date(paste(Year, 
                              ifelse(nchar(Month)==1, paste0('0', Month), Month), 
                              '01', sep = '-'))) %>%
  select(-c('Year', 'Month')) %>%
  pivot_longer(!Date)

p <- ggplot(plot_dat, aes(x=Date, y=value, color = name)) +
  geom_point(alpha = 0.15) + 
  geom_smooth(method = 'loess', se = F) +
  facet_wrap('name', scales = 'free', ncol=4) +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust =0.5), legend.position = 'none') +
  labs(title = 'Saanichton CDA Weather Station',
       subtitle = 'Historical Weather Data',
       x = 'Date',
       y = '',
       caption = 'Source Data: Canadian Centre for Climate Services\nandrewlis.blog')

ggsave(here('_outputs', 'SaanichtonCDA.svg'), p, device = 'svg', width=8, height=11)



# Interesting to look at Heating and Cooling Degree day data..
hdd_cdd <- plot_dat %>% 
  filter(name %in% c('CDD', 'HDD'))

ggplot(hdd_cdd, aes(x=Date, y=value, color = name)) +
  geom_point(alpha = 0.15) + 
  geom_smooth(method = 'loess', se = F) +
  scale_x_date(limits = c(as.Date('1990-01-01'), as.Date('2022-01-01'))) +
  facet_wrap('name', scales = 'free')




