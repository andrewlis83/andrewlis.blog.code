# 2021-09-19-visualizing-house-prices
# Author: andrewlis.blog

# This script pulls a zip file from the Canadian Real Estate Association (CREA)
# which contains 2 excel files, and then rips through the excel sheets to turn
# the data into a tidy(er) format that's more useful in the R environment. There
# is also some accompanying data viz.

library(tidyverse)
library(plyr)
library(here)
library(readxl)
library(plotly)
library(htmlwidgets)
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


# Now we have 2 named lists containing all the excel sheets that were contained in Excel files.
# Might as well do a quick and dirty plot to check that the data is OK.

# Quick and dirty plot comparing SA and NSA data...
x <- data.frame(date = crea_nsa$aggregate$Date,
                nsa = crea_nsa$victoria$Single_Family_Benchmark, 
                sa = crea_sa$victoria$Single_Family_Benchmark_SA) %>%
  pivot_longer(!date)

ggplot(x, aes(x = date, y = value, color = name)) +
  geom_line()

rm(x)

# ...but this is a cumbersome way to plot data, and ggplot likes data in long format, 
# so it'd be nice to convert these data into something more ggplot-friendly.

# plyr's ldply function is a nice way to unlist a nested list of this sort.

crea_nsa_df <- ldply(crea_nsa, data.frame, .id = 'Region') %>%
  pivot_longer(!c(Date, Region))

crea_sa_df <- ldply(crea_sa, data.frame, .id = 'Region') %>%
  pivot_longer(!c(Date, Region))

rm(crea_sa, crea_nsa)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                           DATA VIZ
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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



# PLOTLY HEAT MAP WITH DIFFERENCES BETWEEN BENCHMAKRK VALUES BETWEEN MARKETS

diff_mat_nsa_f <- function(SERIES) {
  
  df <- crea_nsa_df %>% 
    filter(Date == max(Date), 
           name == SERIES) %>%
    select(Region, value)
  
  dmat <- as.data.frame(outer(df$value, df$value, '-'))
  colnames(dmat) <- df$Region
  dmat$Region <- df$Region
  
  # If you want the upper diagonal only, uncomment the line below
  # dmat[lower.tri(dmat, diag = F)] <- NA
  
  p_df <- dmat %>%
    pivot_longer(!Region) %>%
    mutate(t = paste0("Diff: ", Region, ' - ', name, "\n", "Value: ", value))
  
  return(p_df)
  
}

# List of possible series to series to plot:
unique(crea_nsa_df$name)

# Plug desired SERIES in and generate plot
plot_dat <- diff_mat_nsa_f('Single_Family_Benchmark')

p <- ggplot(plot_dat, 
            aes(Region, 
                name, 
                fill = value, 
                text = t)) + 
  geom_tile(alpha = 0.85) +
  scale_x_discrete(limits = levels(plot_dat$Region)) +
  scale_y_discrete(limits = levels(plot_dat$Region)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_gradient(low="#14015e", high="#ffe3f1") +
  labs( x = NULL, y=NULL, fill = "Price Difference" )

p_dif <- ggplotly(p, tooltip='t') %>%
  layout(title = list(text = "Price Differential - Single Detached Homes", pad = list(t = 15)))

saveWidget(p_dif, here('_outputs', "p_dif.html"))
