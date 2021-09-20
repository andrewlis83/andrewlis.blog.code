# 2021-09-19-visualizing-house-prices
# Author: andrewlis.blog

# This script pulls a zip file from the Canadian Real Estate Association (CREA)
# which contains 2 excel files, and then rips through the excel sheets to turn
# the data into a tidy(er) format that's more useful in the R environment.

library(tidyverse)
library(plyr)
library(here)
library(readxl)
options(scipen = 999)

# Sometimes the URL changes, the download link might need to be updated periodically.
# https://www.crea.ca/housing-market-stats/mls-home-price-index/hpi-tool/

# Save the zip folder locally.
download.file('https://www.crea.ca/wp-content/uploads/2019/06/MLS%C2%AE-HPI.zip', 
              destfile = here('CREA.zip'), mode = 'wb')

unzip(here("CREA.zip"), "Not Seasonally Adjusted.xlsx")
unzip(here("CREA.zip"), "Seasonally Adjusted.xlsx")


# Read in the excel files and condense down to a tidy format.

shtnms_nsa <- excel_sheets(here("Not Seasonally Adjusted.xlsx"))
shtnms_sa <- excel_sheets(here("Seasonally Adjusted.xlsx"))

# Start with NSA data...
crea_nsa <- list()
for (nm in shtnms_nsa) {
  crea_nsa[[nm]] <- read_excel(here("Not Seasonally Adjusted.xlsx"), nm)
}

# now unpack the SA data...
crea_sa <- list()
for (nm in shtnms_sa) {
  crea_sa[[nm]] <- read_excel(here("Seasonally Adjusted.xlsx"), nm)
}

# The names in each file are in different cases, and it'd be good to check if there's any areas
# not contained in both files. So, we'll fix the names and do a quick set difference check.

names(crea_sa) <- tolower(names(crea_sa))
names(crea_nsa) <- tolower(names(crea_nsa))

setdiff(names(crea_sa), names(crea_nsa))

# Cleanup
# Don't need the zip file, but will retain the unzipped excel files locally 
# in case we want to use them in the future for something.
unlink(here("CREA.zip"))
rm(shtnms_nsa, shtnms_sa, nm)

# Nice! Now we have 2 named lists containing all the excel sheets that were contained in those nasty Excel files.
# Might as well plot some data while we're at it.

# Quick and dirty plot comparing SA and NSA data...
x <- data.frame(date = crea_nsa$aggregate$Date,
                nsa = crea_nsa$victoria$Single_Family_Benchmark, 
                sa = crea_sa$victoria$Single_Family_Benchmark_SA) %>%
  pivot_longer(!date)

ggplot(x, aes(x = date, y = value, color = name)) +
  geom_line()

# But this is a cumbersome way to plot data, and ggplot likes data in long format, so it'd be
# nice to convert these data into something more plot-friendly.

# plyr's ldply function is a nice way to unlist a nested list of this sort.

crea_nsa_df <- ldply(crea_nsa, data.frame, .id = 'Region') %>%
  pivot_longer(!c(Date, Region))

crea_sa_df <- ldply(crea_sa, data.frame, .id = 'Region') %>%
  pivot_longer(!c(Date, Region))

# Let's plot out the values of all single detached homes across all regions
p <- ggplot(filter(crea_nsa_df,
              name == 'Single_Family_Benchmark'), 
       aes(x = as.Date(Date), y = value)) + 
  geom_line(color = 'darkblue') + 
  facet_wrap('Region', scales = 'free_y', ncol = 3) +
  labs(title = 'Benchmark Single Family Home Prices',
       subtitle = 'by CREA Region',
       x = 'Date',
       y = 'Price',
       caption = 'Source Data: Canadian Real Estate Association (CREA)\nandrewlis.blog') +
  geom_vline(xintercept = as.Date('2020-03-01'), linetype="dotted", color = "red")

ggsave(here('_outputs', 'sfhPrices.svg'), p, device = 'svg', width=8, height=15)


# what about apartments across all regions
p <- ggplot(filter(crea_nsa_df,
                   name %in% c('Apartment_Benchmark', 'Townhouse_Benchmark')), 
            aes(x = as.Date(Date), y = value, color = name)) + 
  geom_line() + 
  theme(legend.position = 'bottom') +
  facet_wrap('Region', scales = 'free_y', ncol = 3) +
  labs(title = 'Benchmark Apartment & Townhouse Prices',
       subtitle = 'by CREA Region',
       x = 'Date',
       y = 'Price',
       caption = 'Source Data: Canadian Real Estate Association (CREA)\nandrewlis.blog', 
       color = 'Dwelling Type:') +
  geom_vline(xintercept = as.Date('2020-03-01'), linetype="dotted", color = "red")


ggsave(here('_outputs', 'aptPrices.svg'), p, device = 'svg', width=8, height=15)







