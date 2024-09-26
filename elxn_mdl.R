# 2024-09-25-election-projection
# author: andrewlis.blog

# Super simple "swing" election prediction model
library(tidyverse)

# DL Data -----------------------------------------------------------------

# Grab historical election data from OpenDataBC
df_bc <- read_csv('https://catalogue.data.gov.bc.ca/dataset/44914a35-de9a-4830-ac48-870001ef8935/resource/fb40239e-b718-4a79-b18f-7a62139d9792/download/provincial_voting_results.csv')

# Cache data to disk
write_rds(df_bc, 'df_bc.rds', compress='xz', compression=9L)



# Analyze -----------------------------------------------------------------

# Read data from cache 
df_bc <- read_rds('df_bc.rds')


# Plot of 2020 Election, for Reference
df_bc %>% 
  filter(
    EVENT_NAME == '2020 General Election', 
    VOTE_CATEGORY=='Valid',
    AFFILIATION %in% c('BC NDP', 'BC Liberal Party', 'BC Green Party', 'Conservative')) %>% 
  group_by(ED_NAME) %>% 
  mutate(
    ttl_votes = sum(VOTES_CONSIDERED, na.rm = T)
  ) %>% 
  group_by(ED_NAME, AFFILIATION) %>% 
  reframe(
    vote_share = sum(VOTES_CONSIDERED / ttl_votes, na.rm = T)
  ) %>% 
  
  ggplot(aes(x=reorder(AFFILIATION, vote_share), y=vote_share, fill=AFFILIATION)) +
  geom_col(alpha=1) +
  facet_wrap('ED_NAME') +
  coord_flip() +
  scale_fill_manual(values = c('darkgreen', 'darkred', 'darkorange', 'darkblue')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.major.y = element_blank(), 
        strip.background = element_rect('white')) +
  labs(
    title = '2020 Election Vote Share, by BC Electoral District',
    subtitle = 'Major Political Parties Only',
    fill=NULL,
    x=NULL,
    y=NULL
  )



# Calculate 2020 Popular Vote ---------------------------------------------

pop_vote_2020 <-
  df_bc %>%
  filter(
    EVENT_NAME == '2020 General Election',
    VOTE_CATEGORY == 'Valid',
    AFFILIATION %in% c('BC NDP', 'BC Liberal Party', 'BC Green Party', 'Conservative')
  ) %>%
  mutate(ttl_votes = sum(VOTES_CONSIDERED)) %>%
  group_by(AFFILIATION) %>%
  reframe(vote_share = sum(VOTES_CONSIDERED, na.rm = T) / ttl_votes) %>% 
  distinct()

# Quick plot
pop_vote_2020 %>% 
  ggplot(aes(x=reorder(AFFILIATION, vote_share), y=vote_share, fill=AFFILIATION)) +
  geom_col(alpha=1) +
  geom_text(aes(label = scales::percent(vote_share), hjust = -0.5)) + 
  coord_flip() +
  scale_fill_manual(values = c('darkgreen', 'darkred', 'darkorange', 'darkblue')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = 'none', panel.grid = element_blank()) +
  labs(
    title = '2020 BC Election, Share of Popular Vote',
    subtitle = 'Major Political Parties Only',
    fill=NULL,
    x=NULL,
    y=NULL
  )


# Projected 2024 Popular Vote, as of latest Poll
# https://www.mainstreetresearch.ca/
# https://338canada.com/bc/

# Hard Code 2024 Polling 
pop_vote_2024 <- data.frame(
  AFFILIATION = c('BC NDP', 'BC Liberal Party', 'BC Green Party', 'Conservative'),
  vote_share = c(0.419, 0.0, 0.093, 0.441) # Mainstreet Poll, Sept. 25 2024
  # vote_share = c(0.44, 0.0, 0.11, 0.44) # 338 Canada Poll, Sept. 25 2024
)

# Quick plot
pop_vote_2024 %>% 
  ggplot(aes(x=reorder(AFFILIATION, vote_share), y=vote_share, fill=AFFILIATION)) +
  geom_col(alpha=1) +
  geom_text(aes(label = scales::percent(vote_share), hjust = -0.5)) + 
  coord_flip() +
  scale_fill_manual(values = c('darkgreen', 'darkred', 'darkorange', 'darkblue')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = 'none', panel.grid = element_blank()) +
  labs(
    title = '2024 BC Election, Share of Popular Vote',
    subtitle = 'Major Political Parties Only',
    fill=NULL,
    x=NULL,
    y=NULL
  )



# Distribute Swing to Electoral Districts ---------------------------------


# First need to merge forme BC Liberal / BC United vote share with conservative, as 
# BC Liberal / BC United collapsed. It's a big assumption, but there's no other
# easy alternative here.

pop_vote_2020_adj <-
  pop_vote_2020 %>% 
  pivot_wider(names_from = AFFILIATION, values_from = vote_share) %>% 
  mutate(
    `BC Conservatives` = sum(`BC Liberal Party`, Conservative)
  ) %>% 
  select(-c(`BC Liberal Party`, Conservative)) %>% 
  pivot_longer(names_to = 'AFFILIATION', values_to = 'vote_share', cols=1:3)
  

# Modify party names in pop vote 2024 and drop BC Liberals
pop_vote_2024_adj <- data.frame(
  AFFILIATION = c('BC NDP', 'BC Green Party', 'BC Conservatives'),
  vote_share = c(0.419, 0.093, 0.441)
)

# Calculate the swing

df_swing <- left_join(pop_vote_2024_adj, pop_vote_2020_adj, by='AFFILIATION') %>% 
  mutate(swing=vote_share.x - vote_share.y)



# Project 2024 Results ----------------------------------------------------


# Generate Projection by Riding, by uniformly applying swing

df_projection <- 
  df_bc %>% 
  filter(
    EVENT_NAME == '2020 General Election', 
    VOTE_CATEGORY=='Valid',
    AFFILIATION %in% c('BC NDP', 'BC Liberal Party', 'BC Green Party', 'Conservative')) %>% 
  group_by(ED_NAME) %>% 
  mutate(
    ttl_votes = sum(VOTES_CONSIDERED, na.rm = T)
  ) %>% 
  group_by(ED_NAME, AFFILIATION) %>% 
  reframe(
    vote_share = sum(VOTES_CONSIDERED, na.rm = T) / ttl_votes
  ) %>% 
  distinct() %>% 
  pivot_wider(names_from = AFFILIATION, values_from = vote_share) %>% 
  
  # Shift former Liberal/BC United Share over to Conservatives
  mutate(
    `BC Conservatives` = ifelse(is.na(`BC Liberal Party`), 0, `BC Liberal Party`) + ifelse(is.na(Conservative), 0, Conservative) 
  ) %>% 
  select(-c(`BC Liberal Party`, Conservative)) %>% 
  
  pivot_longer(!ED_NAME, names_to = 'AFFILIATION', values_to = 'vote_share') %>% 
  left_join(., df_swing, by='AFFILIATION') %>% 
  mutate(
    projected_vote = ifelse(vote_share+swing<0, 0, vote_share+swing)
  ) %>% 
  select(c(1, 2, projected_vote))



# Plot of 2024 Projection by Electoral District

df_projection %>% 
  ggplot(aes(x=reorder(AFFILIATION, projected_vote), y=projected_vote, fill=AFFILIATION)) +
  geom_col(alpha=1) +
  facet_wrap('ED_NAME') +
  coord_flip() +
  scale_fill_manual(values = c('darkblue','darkgreen', 'darkorange')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = 'none', panel.grid = element_blank()) +
  labs(
    title = 'Projected 2024 Election Vote Share, by BC Electoral District',
    subtitle = 'Major Political Parties Only',
    fill=NULL,
    x=NULL,
    y=NULL
  )




# Seat Projection ---------------------------------------------------------

df_projection %>% 
  group_by(ED_NAME) %>% 
  mutate(seat_winner = max(projected_vote, na.rm = T)) %>% 
  filter(projected_vote==seat_winner) %>% 
  group_by(AFFILIATION) %>% 
  reframe(
    projected_seats = n()
  ) %>% 
  ggplot(aes(x=reorder(AFFILIATION, projected_seats), y=projected_seats, fill=AFFILIATION)) +
  geom_col(alpha=1) +
  geom_hline(yintercept = 45, color='darkred', linetype='dashed') +
  geom_text(aes(label = projected_seats), color='black', hjust = -0.5) + 
  scale_y_continuous(limits = c(0, 50)) +
  coord_flip() +
  scale_fill_manual(values = c('darkblue','darkgreen', 'darkorange')) +
  # theme_classic() +
  theme(legend.position = 'none', panel.grid = element_blank()) +
  labs(
    title = 'Projected 2024 Election Seats, by BC Electoral District',
    subtitle = 'Major Political Parties Only',
    fill=NULL,
    x=NULL,
    y=NULL
  )



















