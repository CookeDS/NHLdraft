library(tidyverse)
library(scales)
library(ggdark)

#probability of playing in 100+ NHL Games by overall pick

df1 %>%
  filter(year >= 1995,
         year <= 2015, 
         overall_pick == 1) %>%
  select(overall_pick, player)


pd1 <- df1 %>%
  select(year, overall_pick, games_played, position) %>%
  filter(year >= 1995, year <= 2015, 
         position != "G")

pd2 <- pd1 %>%
  filter(games_played >= 100, 
         overall_pick <= 100) %>%
  group_by(overall_pick) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(-overall_pick))


pd3 <- pd1 %>% 
  filter(overall_pick <= 100) %>%
  group_by(overall_pick) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(-overall_pick))

pd4 <- right_join(pd2, pd3, by = "overall_pick") %>%
  rename(over_100 = n.x,
         total = n.y) %>%
  mutate(pct_over_100 = over_100/total *100) %>%
  mutate("pct_over_100" = round(pct_over_100, 1))
  
#visualization 1

pd4 %>%
  ggplot(aes(x = overall_pick, y = pct_over_100)) +
  geom_line() +
  geom_smooth()


# >= 200 games played

pd2b <- pd1 %>%
  filter(games_played >= 200, 
         overall_pick <= 100) %>%
  group_by(overall_pick) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(-overall_pick))


pd4b <- right_join(pd2b, pd3, by = "overall_pick") %>%
  rename(over_200 = n.x,
         total = n.y) %>%
  mutate(pct_over_200 = over_200/total *100) %>%
  mutate("pct_over_200" = round(pct_over_200, 1))

#visualization 2

pd4b %>%
  ggplot(aes(x = overall_pick, y = pct_over_200)) +
  geom_line() +
  geom_smooth()

# >= 50 games played

pd2c <- pd1 %>%
  filter(games_played >= 50, 
         overall_pick <= 100) %>%
  group_by(overall_pick) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(-overall_pick))


pd4c <- right_join(pd2c, pd3, by = "overall_pick") %>%
  rename(over_50 = n.x,
         total = n.y) %>%
  mutate(pct_over_50 = over_50/total *100) %>%
  mutate("pct_over_50" = round(pct_over_50, 1))

#visualization 3

pd4c %>%
  ggplot(aes(x = overall_pick, y = pct_over_50)) +
  geom_point() +
  geom_smooth()

# >= 10 games played

pd2d <- pd1 %>%
  filter(games_played >= 10, 
         overall_pick <= 100) %>%
  group_by(overall_pick) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(-overall_pick))


pd4d <- right_join(pd2d, pd3, by = "overall_pick") %>%
  rename(over_10 = n.x,
         total = n.y) %>%
  mutate(pct_over_10 = over_10/total *100) %>%
  mutate("pct_over_10" = round(pct_over_10, 1))

#visualization 4

pd4d %>%
  ggplot(aes(x = overall_pick, y = pct_over_10)) +
  geom_smooth()


#Create master table

pd5 <- bind_cols(pd4b, pct_over_100 = pd4$pct_over_100)
pd5 <- bind_cols(pd5, pct_over_50 = pd4c$pct_over_50)
pd5 <- bind_cols(pd5, pct_over_10 = pd4d$pct_over_10)

pd5 <- pd5 %>%
  select(overall_pick, pct_over_200, pct_over_100, pct_over_50, pct_over_10)
  
#Visualization 

install.packages("ggdark")
library(ggdark)

glimpse(pd5)

 pd5 %>%
  ggplot(aes(x = overall_pick)) +
  geom_smooth(aes(y = pct_over_200, color = "d"), 
              se = FALSE, 
              size = 1.1) +
  geom_smooth(aes(y = pct_over_100, color = "c"),
              se = FALSE,
              size = 1.1) +
  geom_smooth(aes(y = pct_over_50, color = "b"),
              se = FALSE,
              size = 1.1) +
  geom_smooth(aes(y = pct_over_10, color = "a"),
              se = FALSE,
              size = 1.1) +
  labs(x = "Overall Pick",
       y = "", 
       title = "Probability of Games Played by Overall Pick", 
       subtitle = "Draft Years (1995-2015), Excluding Goalies",
       color = "Games Played",
       caption = "Source: https://www.hockey-reference.com/draft") +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     expand = expansion(0),
                     labels = label_percent(scale = 1)) +
  scale_x_continuous(expand = expansion(0)) +
  scale_color_manual(labels = c('10+', '50+', '100+', '200+'), values = c('dodgerblue', 'darkorchid1', 'orange', 'seagreen1')) +
  dark_theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray34"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"))




















