# 2021-09-22-bees!
# Author: andrewlis.blog

library(tidyverse)
library(here)
library(gridExtra)
options(scipen = 999)

# Save the zip folder locally.
download.file('https://www150.statcan.gc.ca/n1/tbl/csv/32100353-eng.zip', 
              destfile = here('bees.zip'), mode = 'wb')

unzip(here("bees.zip"), "32100353.csv")
unlink(here('bees.zip'))

bees_df <- read_csv('32100353.csv')


# Loop through and plot every series by GEO, to have a look at all the data
for(e in unique(bees_df$Estimates)) {
  p <- bees_df %>%
    filter(Estimates == e) %>%
    ggplot(aes(x = REF_DATE, y = VALUE, color = GEO)) +
    geom_line() +
    facet_wrap('GEO', scales = 'free_y') +
    labs(title = e,
         caption = 'Statistics Canada Table 32-10-0353-01\nandrewlis.blog') +
    theme(legend.position = '')
  print(p)
  # ggsave(here('_outputs', paste0(e,'.svg')), p, device = 'svg')
  
}

# List possible estimates that can be plotted:
unique(bees_df$Estimates)


# Looking at value and amount of honey produced
p1 <- bees_df %>%
  filter(Estimates == "Value of natural honey, total [16133111]") %>%
  ggplot(aes(x = REF_DATE, y = VALUE, color = GEO)) +
  geom_line() +
  facet_wrap('GEO', scales = 'free_y') +
  scale_x_continuous(breaks = seq(1924, 2034, 10)) +
  labs(title = "Value of Natural Honey, Total",
       subtitle = 'Thousands of Dollars',
       x = 'Date',
       y = "Dollars (000's)",
       caption = 'Statistics Canada Table 32-10-0353-01\nandrewlis.blog') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = '') 

p2 <- bees_df %>%
  filter(Estimates == "Production of natural honey, total [16133111]") %>%
  ggplot(aes(x = REF_DATE, y = VALUE, color = GEO)) +
  geom_line() +
  facet_wrap('GEO', scales = 'free_y') +
  scale_x_continuous(breaks = seq(1924, 2034, 10)) +
  labs(title = "Production of Natural Honey, Total",
       subtitle = 'Thousands of Pounds',
       x = 'Date',
       y = "Pounds (000's)",
       caption = 'Statistics Canada Table 32-10-0353-01\nandrewlis.blog') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = '') 

p <- grid.arrange(p1, p2, ncol = 1)

p

ggsave(here('_outputs', 'ValProdHoney.svg'), p, device = 'svg', width = 7, height=12)


# Ontario has an interesting divergence:
p1 <- bees_df %>%
  filter(Estimates == "Value of natural honey, total [16133111]",
         GEO == 'Ontario') %>%
  ggplot(aes(x = REF_DATE, y = VALUE)) +
  geom_line(color = 'darkgreen', size=1, alpha=0.65) +
  scale_x_continuous(breaks = seq(1924, 2034, 10)) +
  labs(title = "Value of Natural Honey, Total - Ontario",
       subtitle = 'Thousands of Dollars',
       x = 'Date',
       y = "Dollars (000's)",
       caption = '\n') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = '') 

p2 <- bees_df %>%
  filter(Estimates == "Production of natural honey, total [16133111]", 
         GEO == 'Ontario') %>%
  ggplot(aes(x = REF_DATE, y = VALUE)) +
  geom_line(color = 'darkblue', size=1, alpha=0.65) +
  scale_x_continuous(breaks = seq(1924, 2034, 10)) +
  labs(title = "Production of Natural Honey, Total - Ontario",
       subtitle = 'Thousands of Pounds',
       x = 'Date',
       y = "Pounds (000's)",
       caption = 'Statistics Canada Table 32-10-0353-01\nandrewlis.blog') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = '') 

p <- grid.arrange(p1, p2, ncol = 1)

ggsave(here('_outputs', 'OntarioValProd.svg'), p, device = 'svg', height=7)


# Interesting... what's the value per pound produced?
x <- bees_df %>%
  filter(Estimates == 'Value of natural honey, total [16133111]') %>%
  select(REF_DATE, GEO, VALUE) %>%
  rename(ValHoney = VALUE)

y <- bees_df %>%
  filter(Estimates == 'Production of natural honey, total [16133111]') %>%
  select(REF_DATE, GEO, VALUE) %>%
  rename(Production = VALUE)

xy <- merge(x, y, by = c('REF_DATE', 'GEO')) %>%
  mutate(ValperPound = ValHoney / Production) 

p <- xy %>%
  ggplot(aes(x = REF_DATE, y = ValperPound, color = GEO)) +
  geom_line() +
  facet_wrap('GEO', scales = 'free_y') +
  scale_x_continuous(breaks = seq(1924, 2034, 10)) +
  labs(title = "Value of Natural Honey, per Pound",
       subtitle = 'Dollars',
       x = 'Date',
       y = "Dollars",
       caption = 'Statistics Canada Table 32-10-0353-01\nandrewlis.blog') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = '') 
p

ggsave(here('_outputs', 'ValNatHoneyPerPound.svg'), p, device = 'svg', height=6)


# Beekeeper Bubble?
p <- bees_df %>%
  filter(Estimates == "Beekeepers") %>%
  ggplot(aes(x = REF_DATE, y = VALUE, color = GEO)) +
  geom_line() +
  facet_wrap('GEO', scales = 'free_y') +
  scale_x_continuous(breaks = seq(1924, 2034, 10)) +
  labs(title = "Beekeepers in Canada",
       subtitle = 'by Province',
       x = 'Date',
       y = "Beekeepers",
       caption = 'Statistics Canada Table 32-10-0353-01\nandrewlis.blog') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = '') 

ggsave(here('_outputs', 'BeekeepersByProvince.svg'), p, device = 'svg', height=6)


