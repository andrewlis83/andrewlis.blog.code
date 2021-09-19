# 2021-08-05-an-economist-at-the-liquor-store
# Author: andrewlis.blog

# A look at BC Liquor Store historical pricing open data
library(tidyverse)
library(here)
library(data.table)
library(gt)
library(plotly)
library(htmlwidgets)


# Download and read in the data from BC Open Data
download.file('https://catalogue.data.gov.bc.ca/dataset/e43be180-7511-4e6f-84d3-ad6c9f5c3e2b/resource/79cfd06f-8f3e-41b6-a411-4b843ee39236/download/bc_liquor_store_product_price_list_apr_2021.csv', here('bcl_df.csv'))
bcl_df <- fread(here('bcl_df.csv'))

# Quick peek at the data
head(bcl_df)
summary(bcl_df)

# What's the cheapest/most expensive products?
bcl_df %>% slice_max(order_by = PRODUCT_PRICE, n =25)
bcl_df %>% slice_min(order_by = PRODUCT_PRICE, n =25)

# Create some new variables
bcl_df <- bcl_df %>%
  mutate(TtlAlcPerContainer = PRODUCT_LITRES_PER_CONTAINER*1000*(PRODUCT_ALCOHOL_PERCENT/100),
         PricePerContainer = PRODUCT_PRICE / PRD_CONTAINER_PER_SELL_UNIT, 
         PricePerML = PricePerContainer / (PRODUCT_LITRES_PER_CONTAINER*1000),
         PricePerMLalc = PricePerContainer / TtlAlcPerContainer)

# What gives you the most/least booze per dollar?
# Note: Have to filter out products that contain zero alcohol (i.e. non-alcoholic beer/cider, etc.)

best <- bcl_df %>% 
  filter(PRODUCT_ALCOHOL_PERCENT > 1) %>%
  select(c(ITEM_CATEGORY_NAME, PRODUCT_LONG_NAME, PRODUCT_PRICE, PricePerMLalc)) %>%
  slice_min(order_by = PricePerMLalc, n =25)

p_best <- ggplot(best, aes(x = reorder(PRODUCT_LONG_NAME, -PricePerMLalc), y = PricePerMLalc, fill = ITEM_CATEGORY_NAME)) +
  geom_col() +
  coord_flip() + 
  theme_minimal() +
  labs(title = 'Best Bang for Your Buck - Top 25 Products',
       subtitle = 'BC Liquor Store Products',
       caption = 'Source Data: BC Liquor Store Product Price List - Historical Prices\nandrewlis.blog',
       x = '',
       y = 'Price Per Milliliter of Alcohol',
       fill = 'Product Category')

worst <- bcl_df %>% 
  filter(PRODUCT_ALCOHOL_PERCENT > 1) %>%
  select(c(ITEM_CATEGORY_NAME, PRODUCT_LONG_NAME, PRODUCT_PRICE, PricePerMLalc)) %>%
  slice_max(order_by = PricePerMLalc, n =25)

p_worst <- ggplot(worst, aes(x = reorder(PRODUCT_LONG_NAME, PricePerMLalc), y = PricePerMLalc, fill = ITEM_CATEGORY_NAME)) +
  geom_col() +
  coord_flip() + 
  theme_minimal() +
  labs(title = 'Worst Bang for Your Buck - Top 25 Products',
       subtitle = 'BC Liquor Store Products',
       caption = 'Source Data: BC Liquor Store Product Price List - Historical Prices\nandrewlis.blog',
       x = '',
       y = 'Price Per Milliliter of Alcohol',
       fill = 'Product Category')

ggsave(here('_outputs', 'p_best.svg'), p_best, device = svg)
ggsave(here('_outputs', 'p_worst.svg'), p_worst, device = svg)


# If you want the data as tables, can use gt package...
# gtsave(gt(worst), 'worst.html')
# gtsave(gt(best), 'best.html')


# Looking at cheap cooking wine

cookingwine <- bcl_df %>% 
  filter(ITEM_CATEGORY_NAME == "Wine",
         PRODUCT_LITRES_PER_CONTAINER <=1) %>%
  select(c(PRODUCT_COUNTRY_ORIGIN_NAME, PRODUCT_LONG_NAME, PRODUCT_PRICE, PRODUCT_LITRES_PER_CONTAINER, PricePerML)) %>%
  slice_min(order_by = PricePerML, n =25)
  
p_cookingwine <- ggplot(cookingwine, 
                        aes(x = reorder(PRODUCT_LONG_NAME, PricePerML), y = PricePerML, fill = PRODUCT_COUNTRY_ORIGIN_NAME)) +
  geom_col() +
  coord_flip() + 
  theme_minimal() +
  labs(title = 'Best Value Cooking Wines - Top 25 Products',
       subtitle = 'BC Liquor Store Products',
       caption = 'Source Data: BC Liquor Store Product Price List - Historical Prices\nandrewlis.blog',
       x = '',
       y = 'Price Per Milliliter',
       fill = 'Country of Origin')


ggsave(here('_outputs', 'p_cookingwine.svg'), p_cookingwine, device = svg, width = 12, height = 8)

# Again, if you want the data as a table...
# gt(cookingwine)
# gtsave(cookingwine, 'cookingwine.html')

ggplot(filter(bcl_df,
              ITEM_CATEGORY_NAME  == 'Wine'),
       aes(x=reorder(PRODUCT_COUNTRY_ORIGIN_NAME, PricePerContainer), 
           y=PricePerContainer, 
           color = PRODUCT_COUNTRY_ORIGIN_NAME)) +
  geom_boxplot(outlier.alpha = 0.15) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(x = '',
       y = 'Price Per Bottle',
       title = 'Distributions of Wine Prices per Bottle',
       subtitle = 'by Country of Origin',
       caption = 'Source Data: BC Liquor Store Product Price List - Historical Prices\nandrewlis.blog' )


# Looking at wine price distributions (with Plotly, to make the data more interactive)
p_winedist <- plot_ly(
  filter(bcl_df, ITEM_CATEGORY_NAME  == 'Wine'),
  x = ~ PricePerContainer,
  color = ~ reorder(PRODUCT_COUNTRY_ORIGIN_NAME, PricePerContainer),
  type = "box"
) %>%
  layout(
    title = list(text = 'Distributions of Wine Prices per Bottle, by Country of Origin'),
    xaxis = list(title = 'Price Per Container'),
    showlegend = FALSE
  )

saveWidget(p_winedist, "p_winedist.html")



# Create a bubble plot with drop down filters to let users explore the data themselves

p_bubble <- bcl_df %>%
  plot_ly(
    x = ~ PRODUCT_ALCOHOL_PERCENT,
    y = ~ PricePerContainer,
    color = ~ ITEM_CATEGORY_NAME,
    hoverinfo = 'text',
    text = ~ paste(
      'Product:',
      PRODUCT_LONG_NAME,
      '<br>Country:',
      PRODUCT_COUNTRY_ORIGIN_NAME,
      '<br>Sub-category:',
      ITEM_SUBCATEGORY_NAME,
      '<br>Containers Per Sell Unit:',
      PRD_CONTAINER_PER_SELL_UNIT,
      '<br>Liters Per Container:',
      round(PRODUCT_LITRES_PER_CONTAINER, 2),
      '<br>Product Price:',
      round(PRODUCT_PRICE, 2),
      '<br>Price Per Container:',
      round(PricePerContainer, 2),
      '<br>Price Per ML:',
      round(PricePerML, 2),
      '<br>Price Per ML of Alcohol:',
      round(PricePerMLalc, 2),
      '<br>ML Alcohol Per Container:',
      round(TtlAlcPerContainer, 2)
    )
  ) %>%
  add_trace(type = 'scatter',
            mode = 'markers',
            name = ~ ITEM_CATEGORY_NAME) %>%
  layout(
    title = list(text = "BC Liquor Store Catalogue Data Explorer"),
    xaxis = list(title = 'Product Alcohol %'),
    yaxis = list(title = 'Price Per Container'),
    showlegend = FALSE,
    updatemenus = list(list(
      type = "list",
      label = 'Category',
      x = -0.1,
      buttons = list(
        list(
          method = "restyle",
          args = list('visible', c(T, T, T, T, T)),
          label = "All"
        ),
        list(
          method = "restyle",
          args = list('visible', c(T, F, F, F, F)),
          label = "Beer"
        ),
        list(
          method = "restyle",
          args = list('visible', c(F, T, F, F, F)),
          label = "General Merchandise"
        ),
        list(
          method = "restyle",
          args = list('visible', c(F, F, T, F, F)),
          label = "Refreshment Beverages"
        ),
        list(
          method = "restyle",
          args = list('visible', c(F, F, F, T, F)),
          label = "Spirits"
        ),
        list(
          method = "restyle",
          args = list('visible', c(F, F, F, F, T)),
          label = "Wine"
        )
      )
    ))
  )


saveWidget(p_bubble, "p_bubble.html")












