# (Top 100 picks) Which position has the highest % of players that play 200+ games between year 1995-2015? (5 - year intervals)

df25b <- df1 %>%
  filter(year >= 1995, year <= 2014, overall_pick <= 100) %>%
  select(year, position, games_played, overall_pick) 

df25b[is.na(df25b)] <- 0

df25b <- df25b %>%
  filter(position != "C/RW", 
         position != "C/LW",
         position != "C/W",
         position != "W",
         position != "L/RW", 
         position != "C RW",
         position != "",
         position != "F",
         position != "Centr", 
         position != "C; LW", 
         position != "LW/C")


df_95_99b <- df25b %>%
  filter(year >= 1995, year <= 1999) %>%
  mutate("1995_1999" = year) %>%
  mutate(year = NULL)


df_00_04b <- df25b %>%
  filter(year >= 2000, year <= 2004) %>%
  mutate("2000_2004" = year) %>%
  mutate(year = NULL)

df_05_09b <- df25b %>%
  filter(year >= 2005, year <= 2009) %>%
  mutate("2005_2009" = year) %>%
  mutate(year = NULL)

df_10_14b <- df25b %>%
  filter(year >= 2010, year <= 2014) %>%
  mutate("2010_2014" = year) %>%
  mutate(year = NULL)


#Template for creating final dataframes


df26b <- df_00_04b %>%
  filter(games_played >= 200) %>%
  count(position) %>%
  arrange(desc(position))

df27b <- df_00_04b %>%
  filter(games_played <= 200) %>%
  count(position) %>%
  arrange(desc(position))


df2000_2004b <- full_join(df26b, df27b, by = "position")


df2000_2004b <- df2000_2004b %>%
  rename("over_200" = n.x, 
         "less_200" = n.y) %>%
  mutate(total = over_200+less_200) %>%
  mutate("pct_over_200_2000" = over_200/total) %>%
  mutate("pct_over_200_2000" = round(pct_over_200_2000, 2)) %>%
  arrange(desc(position))

df2000_2004b <- df2000_2004b %>% 
  select(position, pct_over_200_2000)

#2005-2009

df28b <- df_05_09b %>%
  filter(games_played >= 200) %>%
  count(position) %>%
  arrange(desc(position))

df29b <- df_05_09b %>%
  filter(games_played <= 200) %>%
  count(position) %>%
  arrange(desc(position))


df2005_2009b <- full_join(df28b, df29b, by = "position")


df2005_2009b <- df2005_2009b %>%
  rename("over_200" = n.x, 
         "less_200" = n.y) %>%
  mutate(total = over_200+less_200) %>%
  mutate("pct_over_200_2005" = over_200/total) %>%
  mutate("pct_over_200_2005" = round(pct_over_200_2005, 2)) %>%
  arrange(desc(position))

df2005_2009b <- df2005_2009b %>% 
  select(position, pct_over_200_2005)

#2010-2014

df30b <- df_10_14b %>%
  filter(games_played >= 200) %>%
  count(position) %>%
  arrange(desc(position))

df31b <- df_10_14b %>%
  filter(games_played <= 200) %>%
  count(position) %>%
  arrange(desc(position))


df2010_2014b <- full_join(df30b, df31b, by = "position")


df2010_2014b <- df2010_2014b %>%
  rename("over_200" = n.x, 
         "less_200" = n.y) %>%
  mutate(total = over_200+less_200) %>%
  mutate("pct_over_200_2010" = over_200/total) %>%
  mutate("pct_over_200_2010" = round(pct_over_200_2010, 2)) %>%
  arrange(desc(position))

df2010_2014b <- df2010_2014b %>% 
  select(position, pct_over_200_2010)

#1995-1999
df32b <- df_95_99b %>%
  filter(games_played >= 200) %>%
  count(position) %>%
  arrange(desc(position))

df33b <- df_95_99b %>%
  filter(games_played <= 200) %>%
  count(position) %>%
  arrange(desc(position))


df1995_1999b <- full_join(df32b, df33b, by = "position")


df1995_1999b <- df1995_1999b %>%
  rename("over_200" = n.x, 
         "less_200" = n.y) %>%
  mutate(total = over_200+less_200) %>%
  mutate("pct_over_200_1995" = over_200/total) %>%
  mutate("pct_over_200_1995" = round(pct_over_200_1995, 2)) %>%
  arrange(desc(position))

df1995_1999b <- df1995_1999b %>% 
  select(position, pct_over_200_1995)

#Create final dataframe

df_2000_2005b <- full_join(df2000_2004b, df2005_2009b, by ="position")

df_2000_2005_2010b <- full_join(df_2000_2005b, df2010_2014b, by = "position")

df_1995_2000_2005_2010b <- full_join(df_2000_2005_2010b, df1995_1999b, by = "position")

df_1995_2000_2005_2010b <- pivot_longer(df_1995_2000_2005_2010b, cols = 2:5, names_to = "interval", values_to = "pct_over_200")

df_1995_2000_2005_2010b


df_1995_2000_2005_2010b %>%
  ggplot(aes(x = position, y = pct_over_200, fill = interval)) +
  geom_col(width = .6, 
           alpha = .9,
           position = position_dodge(width = .6)) +
  scale_y_continuous(breaks = seq(0, 0.6, .1),
                     limits = c(0, 0.7),
                     expand = expansion(0),
                     labels = label_percent(scale = 100)) +
  scale_fill_manual(labels = c("1995-2000", "2000-2004", "2005-2009", "2010-2014"),
                    values = c("#93C5FD", "#3B82F6", "#1D4ED8", "#1E3A8A"), 
                    name = NULL) +
  scale_x_discrete(limits = c("C", "LW", "RW", "D", "G"),
                   labels = c('Center', 'Left-Wing', 'Right-Wing', 'Defense', 'Goalie')) +
  labs(title = "Percentage of draft picks that have played 200+ NHL games by position",
       subtitle = "  Selected from top 100 picks (1995-2014)",
       x = "", 
       y = "",
       caption = "Source: https://www.hockey-reference.com/draft") +
  theme(
    plot.margin = unit(rep(2, 4), "cm"),
    legend.position = "bottom",
    plot.title = element_text(size = 15, face = "bold",
                              margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 15,
                                 lineheight = 1.1,
                                 margin = margin(b = 30)),
    plot.caption = element_text(size = 13, color = "grey20",
                                margin = margin(t = 30),
                                hjust = 0),
    axis.text = element_text(size = 14, color = "black"),
    axis.ticks = element_blank(),
    axis.line.x = element_line(color = "grey50"),
    legend.text = element_text(size = 14, margin = margin(r = 12)),
    legend.margin = margin(t = 15),
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey90")
  )

#Done!