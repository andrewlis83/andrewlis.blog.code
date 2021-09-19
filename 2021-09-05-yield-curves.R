# 2021-09-05-yield-curves
# Author: andrewlis.blog


# Script to pull and plot Yield Curves for Canada
library(tidyverse)
library(here)
library(plotly)
library(htmlwidgets)
library(gganimate)
library(gifski)
library(transformr)


# Pull Bond Yield Data for Canada
download.file("https://www150.statcan.gc.ca/n1/en/tbl/csv/10100139-eng.zip?st=NQRjQ_4E", 
              destfile = here('_data', 'data.zip'), mode="wb")

CADBY_df <- as.data.frame(read.csv(unz(here('_data', "data.zip"), "10100139.csv")))

# Fix weird characters in first column name
colnames(CADBY_df)[1] <- "REF_DATE"


# Pull CAD Bond Yields into a grouped data frame for plotting
bond_dat <- CADBY_df %>%
  select(c('REF_DATE', 'Financial.market.statistics', 'VALUE')) %>%
  filter(Financial.market.statistics %in% 
           c('Government of Canada benchmark bond yields, 2 year',
             'Government of Canada benchmark bond yields, 3 year',
             'Government of Canada benchmark bond yields, 5 year',
             'Government of Canada benchmark bond yields, 7 year',
             'Government of Canada benchmark bond yields, 10 year',
             'Government of Canada benchmark bond yields, long term')) %>%
  rename(Date = REF_DATE, Series = Financial.market.statistics, value = VALUE) %>%
  # mutate(Date = as.POSIXlt(Date, format="%Y-%m-%d"),
  mutate(Date = as.Date(Date, format="%Y-%m-%d"),
         Series = 
           case_when(Series == 'Government of Canada benchmark bond yields, 2 year' ~ 'GC_2_Year',
                     Series == 'Government of Canada benchmark bond yields, 3 year' ~ 'GC_3_Year',
                     Series == 'Government of Canada benchmark bond yields, 5 year' ~ 'GC_5_Year',
                     Series == 'Government of Canada benchmark bond yields, 7 year' ~ 'GC_7_Year',
                     Series == 'Government of Canada benchmark bond yields, 10 year' ~ 'GC_10_Year',
                     Series == 'Government of Canada benchmark bond yields, long term' ~ 'GC_LT'),
         value = replace(value, value == 0, NA)) %>%
  filter(!(weekdays(as.Date('Date', format="%Y-%m-%d")) %in% c('Saturday','Sunday'))) %>%
  drop_na()


p_Yields <- ggplot(bond_dat, 
       aes(x=as.Date(Date), y=value, color=Series, group = Series)) + 
  geom_line() + 
  labs(title = 'Government of Canada Benchmark Bond Yields',
       # subtitle = 'by Maturity',
       caption = 'Source Data: StatCan Table 10-10-0139\nandrewlis.blog',
       x = 'Date',
       y = 'Yield') +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +  
  scale_x_date(date_breaks="1 years", date_labels = "%Y")

p_Yields

# ggplotly(p_Yields) 

ggsave(here('_outputs', 'p_Yields.svg'), p_Yields, device = 'svg')

# Save plotly output as html widget
# saveWidget(as_widget(ggplotly(p_Yields)), "p_Yields.html")


# Make a 2s and 10s plot...
twosandtens_dat <- bond_dat %>%
  filter(Series %in% c('GC_2_Year', 'GC_10_Year')) %>%
  pivot_wider(names_from = 'Series') %>%
  mutate(twos_tens = GC_10_Year - GC_2_Year) %>%
  drop_na() %>%
  select(c(Date, twos_tens))


# Plot
p_2s_10s <- ggplot(twosandtens_dat, 
       aes(x=as.Date(Date), y=twos_tens)) + 
  geom_line(color = 'darkblue', size = 0.75, alpha = 0.65) + 
  labs(title = "Government of Canada Benchmark Bond Yields - 2's and 10's",
       # subtitle = 'by Maturity',
       caption = 'Source Data: StatCan Table 10-10-0139\nandrewlis.blog',
       x = 'Date',
       y = 'Spread') +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +  
  scale_x_date(date_breaks="1 years", date_labels = "%Y") + 
  scale_y_continuous(limits = c(-1, 3)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")

p_2s_10s

# ggplotly(p_2s_10s) 
# saveWidget(as_widget(ggplotly(p_2s_10s)), "p_2s_10s.html")



# Pure plotly implementation of the chart above:

p_2s_10s <- plot_ly(twosandtens_dat, x = ~as.Date(Date), y = ~twos_tens, type = 'scatter', mode = 'lines', 
        line = list(color = 'rgba(12, 0, 143, 0.55)', width = 2)) %>% 
  layout(title = list(text = "Government of Canada Benchmark Bond Yields - 2's and 10's", x =0 ),
         xaxis = list(title = 'Date'),
         yaxis = list (title = 'Spread'))

  # add_segments(x = "1991-09-01", xend = "2021-08-01", y = 0, yend = 0, line = list(color = 'red', width = 2, dash = 'dot') )

saveWidget(as_widget(p_2s_10s), "p_2s_10s.html")






# Plot Animated Yield curves

yc_dat <- bond_dat %>%
  filter(Date >= '2019-03-01' & Date <= '2021-01-01') 

yc_dat$Series <- factor(yc_dat$Series, 
                        levels = c('GC_2_Year', 'GC_3_Year', 'GC_5_Year', 'GC_7_Year', 'GC_10_Year', 'GC_LT'))

a <- ggplot(yc_dat, 
       aes(x = Series, y = value, group = as.factor(Date))) + 
  geom_path(alpha = 0.75, size = 1, color = 'darkblue',) +
  # scale_colour_viridis_c() +
  theme_minimal() + 
  theme(legend.position = 'none', text = element_text(size = 13)) +
  labs(title = 'Government of Canada Bond Yield Curves',
       subtitle = 'Date: {frame_time}',
       caption = 'Source Data: StatCan Table 10-10-0139\nandrewlis.blog',
       x = 'Maturity',
       y = 'Yield') +
  transition_time(Date) +
  # shadow_mark(colour = 'darkblue', alpha = 0.1, size = 0.15)
  shadow_trail(distance = 0.01, colour = 'darkblue', alpha = 0.1, size = 0.15)


a_a <- animate(a, nframes = length(unique(yc_dat$Date)), 
               dev = "png", 
               fps = 25, 
               type = "cairo-png",
               antialias = "subpixel",
               width = 800, height = 600,
               renderer = gifski_renderer())

anim_save(here('_outputs', "myAnimation_shadowTrail.gif"), a_a)


