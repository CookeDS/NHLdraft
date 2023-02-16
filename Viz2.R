library(tidyverse)
library(ggtext)
library(scales)

#Question: Graph top countries by players with 200+ NHL Games (1990-2015)


ts1 <- df1 %>%
  filter(year >= 1995 & year <= 2020)

ts1 <- ts1 %>% mutate(id = row_number()) %>%
  relocate(id)


#Demographic distribution of players with more than 200 games played 

ts2 <- ts1 %>%
  select(id, year, nationality, games_played) 


ts2 %>%
  group_by(nationality) %>%
  count() %>%
  arrange(desc(n))

ts3 <- ts2 %>%
  filter(nationality == "CA" | 
         nationality == "US" |
         nationality == "SE" |
         nationality == "CZ" |
         nationality == "RU" | 
         nationality == "FI")

#set NA to zero 

ts3[is.na(ts3)] <- 0


ts4 <- ts3 %>%
  group_by(year, nationality) %>%
  count() %>%
  ungroup()

ts5 <- pivot_wider(ts4, names_from = nationality, values_from = n) 

ts6 <- ts5 %>%
 mutate(total = select(., CA:US) %>% rowSums(na.rm = TRUE))

#Do the same for "other" countries

ts3b <- ts2 %>%
  filter(nationality != "CA" , 
         nationality != "US" ,
         nationality != "SE" ,
         nationality != "CZ" ,
         nationality != "RU" , 
         nationality != "FI", 
         nationality != "")

ts4b <- ts3b %>%
  group_by(year, nationality) %>%
  count() %>%
  ungroup()

ts5b <- pivot_wider(ts4b, names_from = nationality, values_from = n)

ts5b[is.na(ts5b)] <- 0

ts6b <- ts5b %>%
  mutate(total = select(., AT:UZ) %>% rowSums(na.rm = TRUE)) 

ts7b <- ts6b %>%
  select(year, total)

#Combine data sets

ts7 <- right_join(ts6, ts7b, by = "year")

ts7 <- ts7 %>%
  mutate(total = total.x+total.y) %>% 
  mutate(total.x = NULL, total.y = NULL) 

ts8 <- ts7 %>%
  mutate(Canada = (CA/total) *100,
         Czechia = CZ/total *100,
         Finland = FI/total *100,
         Russia = RU/total *100,
         Sweden = SE/total *100,
         USA = US/total *100) 


ts9 <- ts8 %>%
  select(year, Canada, Finland, Russia, Sweden, USA)


ts10 <- pivot_longer(ts9, cols=2:6, names_to = "country", values_to = "pct")


#Visualization 

labels1 <- ts10 %>% 
  filter(year == min(year), 
         country == "Canada")

labels2 <- ts10 %>% 
  filter(year == min(year), 
         country == "Russia")

labels3 <- ts10 %>% 
  filter(year == min(year), 
         country == "USA")

labels4 <- ts10 %>% 
  filter(year == min(year), 
         country == "Sweden")

labels5 <- ts10 %>% 
  filter(year == min(year), 
         country == "Finland")

labels1 <- ts10 %>% 
  filter(year == min(year), 
         country == "Canada")

fake_grid <- tibble(
  x = c(rep(1995, 7)),
  xend = c(rep(2020, 7)),
  y = seq(10, 70, 10),
  yend = y
)

ts10 %>% 
  ggplot(aes(x = year,
             y = pct,
             color = country)) +
  geom_line(size = 1.1, 
            show.legend = FALSE) +
  geom_segment(
    data = fake_grid,
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = "dotted",
    color = "grey50"
  ) +
  geom_text(
    data = labels1,
    aes(label = country),
    size = 5.5,
    hjust = 1,
    nudge_y = 1.3,
    nudge_x = -.3,
    show.legend = FALSE
  ) +
  geom_text(
    data = labels2,
    aes(label = country),
    size = 5.5,
    hjust = 1,
    nudge_y = 1.9,
    nudge_x = -.3,
    show.legend = FALSE
  ) +
  geom_text(
    data = labels3,
    aes(label = country),
    size = 5.5,
    hjust = 1,
    nudge_y = 1.6,
    nudge_x = -.3,
    show.legend = FALSE
  ) +
  geom_text(
    data = labels4,
    aes(label = country),
    size = 5.5,
    hjust = 1,
    nudge_y = -1.6,
    nudge_x = -.3,
    show.legend = FALSE
  )+
  geom_text(
    data = labels5,
    aes(label = country),
    size = 5.5,
    hjust = 1,
    nudge_x = -.3,
    show.legend = FALSE
  ) +
  geom_line(size = 1.1,
            show.legend = FALSE) +
  annotate(
    geom = "label",
    x = 2020,
    y = 70,
    size = 5,
    label = "Percentage share of\ndraft pool",
    hjust = 1,
    label.size = 0,
    ) +
  scale_y_continuous(position = "right",
                     limits = c(0, 75),
                     name = NULL,
                     labels = label_percent(scale = 1),
                     breaks = seq(10, 70, 10)) +
  scale_x_continuous(limits = c(1992, 2020),
                     breaks = seq(1995, 2020, 5),
                     expand = expansion(0),
                     name = NULL) +
  scale_color_manual(values = c("firebrick1","blue3", "darkred", "orange2", "#16A34A")) +
  labs(title = paste("Canadian NHL prospects have consistently accounted for the largest share of <br />",
             "drafted prospects. The **<span style='color: #16A34A'>USA</span>**",
             "has narrowed the gap significantly since the mid 1990s, <br />",
             "while other notable prospect countries continue to lag behind."),
    y = "",
    caption = "Source: https://www.hockey-reference.com/draft",
    x = ""
  ) +
  theme(
    plot.margin = unit(rep(1.4, 4), "cm"),
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 15, color = "black"),
    plot.title = element_markdown(size = 19, lineheight = 1.1,
                                  color = "black",
                                  margin = margin(b = 15)),
    plot.caption = element_text(size = 13, color = "grey20",
                                margin = margin(t = 20)),
    axis.line = element_blank(),
  ) 






  
