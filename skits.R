library(tidyverse)
library(httr)
library(rvest)
library(extrafont)




#font_import()
loadfonts(device = 'win')

people <- data.frame(
  person = c("eric_trump","ivanka_trump","don_trump_jr","donald_trump",
             "melania_trump","hunter_biden","joe_biden","jill_biden"),
  url = c("http://www.snlarchives.net/Impressions/?2341",
          'http://www.snlarchives.net/Impressions/?2342',
          'http://www.snlarchives.net/Impressions/?2340',
          'http://www.snlarchives.net/Impressions/?729',
          'http://www.snlarchives.net/Impressions/?2343',
          'http://www.snlarchives.net/Impressions/?4052',
          'http://www.snlarchives.net/Impressions/?924',
          'http://www.snlarchives.net/Impressions/?4075'
          )
)

df_cons <- data.frame()

for(i in 1:nrow(people)) {
  data <-read_html(people[i,2])
  
  data_cln <- data %>%
    html_elements('#section_3') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    parse_number() %>%
    substr(1,8) %>%
    as.Date(format = "%Y%m%d") %>%
    as.data.frame() %>%
    rename("date" = '.') %>%
    mutate(person = people[i,1])
  
  df_cons <- bind_rows(df_cons,data_cln)
}

trump_inaug <- as.Date("2017-01-20")
biden_inaug <- as.Date("2021-01-20")

days <- Sys.Date() - biden_inaug

df_comp <- df_cons %>%
  mutate(admin = case_when(
    str_detect(person,"trump") == TRUE ~ 'trump',
    str_detect(person,"biden") == TRUE ~ 'biden',
    TRUE ~ 'error'
  )) %>%
  mutate(inaug_date = as.Date(case_when(
    str_detect(person,"trump") == TRUE ~ "2017-01-20",
    str_detect(person,"biden") == TRUE ~ "2021-01-20",
    TRUE ~ "1970-01-01"
  ))) %>%
  mutate(day_diff = date - inaug_date)


df_rec <- df_comp %>%
  filter(day_diff > -365*1.5 & day_diff <= days)

ggplot(df_rec) +
  geom_point(aes(y = person, x = day_diff,group=admin))

df_slim <- df_rec %>%
  filter(person %in% c("donald_trump","don_trump_jr","joe_biden","hunter_biden")) %>%
  group_by(person) %>%
  mutate(person = as.factor(person)) %>%
  arrange(person,day_diff) %>%
  mutate(app_num = row_number())

levels(df_slim$person) <-  c("Donald Trump Jr","Donald Trump","Hunter Biden","Joe Biden")


fct_reorder(df_slim$person,
            c("Donald Trump Jr","Donald Trump","Hunter Biden","Joe Biden"),
            c("Donald Trump","Donald Trump Jr","Joe Biden","Hunter Biden"))

ggplot(df_slim) +
  geom_point(aes(y = app_num, x = day_diff, color = admin), size = 2) +
  geom_ribbon(aes(ymax = app_num, ymin = 0, x = day_diff, group = person, fill = admin), alpha = .2) +
  facet_wrap(~person) +
  theme_minimal() +
  scale_color_manual(breaks = c("trump","biden"), values = c("red","blue")) +
  scale_fill_manual(breaks = c("trump","biden"), values = c('red',"blue")) +
  labs(
    title = "Saturday Night Live Impressions",
    x = "Days From Presidential Inauguration",
    y = "Show Appearances"
  ) + 
  theme(
    legend.position = 'none',
    text = element_text(family = "Bahnschrift", size = 16),
    plot.background = element_rect(fill = 'white')
  )

ggsave('snl.png', dpi = 320, width = 10, height = 8)







df_slim1 <- df_rec %>%
  filter(person %in% c("melania_trump","jill_biden")) %>%
  group_by(person) %>%
  mutate(person = as.factor(person)) %>%
  arrange(person,day_diff) %>%
  mutate(app_num = row_number())

levels(df_slim1$person) <-  c("Jill Biden","Melania Trump")


ggplot(df_slim1) +
  geom_point(aes(y = app_num, x = day_diff, color = admin), size = 2) +
  geom_ribbon(aes(ymax = app_num, ymin = 0, x = day_diff, group = person, fill = admin), alpha = .2) +
  facet_wrap(~person) +
  theme_minimal() +
  scale_color_manual(breaks = c("trump","biden"), values = c("red","blue")) +
  scale_fill_manual(breaks = c("trump","biden"), values = c('red',"blue")) +
  labs(
    title = "Saturday Night Live Impressions",
    x = "Days From Presidential Inauguration",
    y = "Show Appearances"
  ) + 
  theme(
    legend.position = 'none',
    text = element_text(family = "Bahnschrift", size = 16),
    plot.background = element_rect(fill = 'white')
  )

ggsave('snl.png', dpi = 320, width = 10, height = 8)

