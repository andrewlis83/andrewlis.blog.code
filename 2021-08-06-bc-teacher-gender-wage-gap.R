# 2021-08-06-bc-teacher-gender-wage-gap
# Author: andrewlis.blog

# Looking at gender wage gap in teachers
library(tidyverse)
library(data.table)
library(here)
options(scipen = 999)

# Pull the data

download.file('https://catalogue.data.gov.bc.ca/dataset/76a16627-90a2-4d94-a8d0-dfe2b7d0a87f/resource/dde120f1-4f33-4338-a8fc-90f99f1516a2/download/teacherstatistics20162017.csv', 
              destfile = here('teacherstatistics20162017.csv'), mode="wb")

edu_df <- fread(here('teacherstatistics20162017.csv'))


# Provincial level wage gap for teachers
p <- ggplot(
  filter(
    edu_df,
    `DISTRICT NUMBER` == 'Prov',
    `Educators CATEGORY`  == "Teachers",
    GENDER != 'All'
  ) ,
  aes(
    x = `SCHOOL YEAR`,
    y = as.numeric(`BASE SALARY`),
    color = GENDER,
    group = GENDER
  )
) +
  geom_line() +
  labs(title = 'Gender Wage Gap in Base Salary',
       subtitle = 'BC Teachers',
       caption = 'Source Data: BC Open Data\nandrewlis.blog',
       x = 'School Year',
       y = 'Annual Base Salary',
       color = 'Gender') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p

ggsave('_outputs\\ProvWageGap.svg', p, device = 'svg')



# Faceting by School District (Teachers)
p <- ggplot(
  filter(
    edu_df,
    `DISTRICT NUMBER` != 'Prov',
    `Educators CATEGORY`  == "Teachers",
    GENDER != 'All'
  ) ,
  aes(
    x = `SCHOOL YEAR`,
    y = as.numeric(`BASE SALARY`),
    color = GENDER,
    group = GENDER
  )
) +
  geom_line() +
  labs(title = 'Gender Wage Gap in Base Salary',
       subtitle = 'BC Teachers, by School District',
       caption = 'Source Data: BC Open Data\nandrewlis.blog',
       x = 'School Year',
       y = 'Annual Base Salary',
       color = 'Gender') +
  theme_minimal() +
  facet_wrap('`DISTRICT NUMBER`', scales = 'free', ncol = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p

ggsave('_outputs\\WageGap_Teachers_SD.svg', p, device = 'svg', width=15, height = 45)



# Breaking out by teachers, admin, and educators at provincial level
p <- ggplot(
  filter(
    edu_df,
    `DISTRICT NUMBER` == 'Prov',
    # `Educators CATEGORY`  == "Teachers",
    GENDER != 'All'
  ) ,
  aes(
    x = `SCHOOL YEAR`,
    y = as.numeric(`BASE SALARY`),
    color = GENDER,
    group = GENDER
  )
) +
  geom_line() +
  labs(title = 'Gender Wage Gap in Base Salary',
       subtitle = 'by Educator Category',
       caption = 'Source Data: BC Open Data\nandrewlis.blog',
       x = 'School Year',
       y = 'Annual Base Salary',
       color = 'Gender') +
  theme_minimal() +
  facet_wrap('`Educators CATEGORY`', scales = 'free_y', ncol = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p

ggsave('_outputs\\ProvWageGap_EduCat.svg', p, device = 'svg', width=8, height = 10)



# Looking at average age for teachers, and base salary at provincial level
p <- ggplot(
  filter(
    edu_df,
    # `DISTRICT NUMBER` == 'Prov',
    `Educators CATEGORY`  == 'Teachers',
    `AVERAGE AGE` != 'MSR',
    # `SCHOOL YEAR`  %in% c('2016/2017'),
    GENDER != 'All'
  ) ,
  aes(
    x = as.numeric(`AVERAGE AGE`),
    y = as.numeric(`BASE SALARY`),
    color = GENDER,
    group = GENDER
  )
) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = 'lm', se = F) +
  labs(title = 'BC Teacher Gender Wage Gap in Base Salary',
       subtitle = 'by Average Age and Sex',
       caption = 'Source Data: BC Open Data\nandrewlis.blog',
       x = 'Average Age',
       y = 'Annual Base Salary',
       color = 'Gender') +
  theme_minimal() +
  scale_x_continuous(breaks = seq(31, 55, 1)) +
  facet_wrap('`SCHOOL YEAR`', ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p

ggsave('_outputs\\ProvWageGap_AvgAge.svg', p, device = 'svg', width=10, height = 35)


# Looking at average experience for teachers, and base salary at provincial level
p <- ggplot(
  filter(
    edu_df,
    # `DISTRICT NUMBER` == 'Prov',
    `Educators CATEGORY`  == 'Teachers',
    # `SCHOOL YEAR`  %in% c('2016/2017'),
    GENDER != 'All'
  ) ,
  aes(
    x = as.numeric(`AVERAGE EXPERIENCE`),
    y = as.numeric(`BASE SALARY`),
    color = GENDER,
    group = GENDER
  )
) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = 'lm', se = F) +
  labs(title = 'BC Teacher Gender Wage Gap in Base Salary',
       subtitle = 'by Average Experience and Sex',
       caption = 'Source Data: BC Open Data\nandrewlis.blog',
       x = 'Average Experience',
       y = 'Annual Base Salary',
       color = 'Gender') +
  theme_minimal() +
  facet_wrap('`SCHOOL YEAR`',  ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p

ggsave('_outputs\\ProvWageGap_AvgExp.svg', p, device = 'svg', width=10, height = 35)





