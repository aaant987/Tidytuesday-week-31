library(tidyverse)
library(hrbrthemes)
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

df <- olympics

df <- df %>%
  filter(sport == "Athletics")

s <- glimpse(unique(df$event))
s <- as.data.frame(s)
s

df <- df %>%
  drop_na(weight) %>%
  drop_na(height) %>%
  filter(season == "Summer")
df

df <- df %>%
  group_by(year, event) %>%
  summarise(mean_height = mean(height),
            mean_weight = mean(weight))

df <- df %>%
  mutate(mean_height = mean_height / 100)

df <- df %>%
  mutate(imc = mean_weight / mean_height^2)

ggplot(df, aes(x = year, y = imc)) +
  geom_line() +
  facet_wrap(~event, scales = "free_y") +
  scale_y_continuous() +
  scale_x_continuous(limits=c(1896, 2016), breaks = seq(1896, 2016, by = 25)) +
  theme(text = element_text(size=7.5))




olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

df <- olympics

df <- df %>%
  filter(sport == "Athletics")

s <- glimpse(unique(df$event))
s <- as.data.frame(s)
s

df <- df %>%
  filter(event == c("Athletics Men's 1,500 metres",
                     "Athletics Men's 10,000 metres",
                       "Athletics Men's 100 metres", 
                       "Athletics Men's 110 metres Hurdles",
                       "Athletics Men's 20 kilometres Walk",
                       "Athletics Men's 200 metres",
                       "Athletics Men's 3,000 metres Steeplechase",
                       "Athletics Men's 4 x 100 metres Relay",
                       "Athletics Men's 4 x 400 metres Relay",
                       "Athletics Men's 400 metres Relay",
                       "Athletics Men's 400 metres Hurdles",
                       "Athletics Men's 5,000 metres",
                       "Athletics Men's 50 kilometres Walk",
                       "Athletics Men's 800 metres",
                       "Athletics Men's Decathlon",
                       "Athletics Men's Discus Throw",
                       "Athletics Men's Hammer Throw",
                       "Athletics Men's High Jump",
                       "Athletics Men's Javelin Throw",
                       "Athletics Men's Long Jump",
                       "Athletics Men's Marathon",
                       "Athletics Men's Pole Vault",
                       "Athletics Men's Shot Put",
                     "Athletics Men's Triple Jump"))
df

df <- df %>%
  drop_na(weight) %>%
  drop_na(height)

df

df <- df %>%
  group_by(year, event) %>%
  summarise(mean_height = mean(height),
            mean_weight = mean(weight))

df <- df %>%
  mutate(mean_height = mean_height / 100)

df <- df %>%
  mutate(imc = mean_weight / mean_height^2)


df$event[df$event == "Athletics Men's 3,000 metres Steeplechase"] <- "Athletics Men's 3,000 m Steeplechase"

scalefree <-  ggplot(df, aes(x = year, y = imc)) +
  geom_line(color = "firebrick", size = 1.5) +
  facet_wrap(~event, scales = "free_y") +
  scale_y_continuous() +
  scale_x_continuous(limits=c(1896, 2016), breaks = seq(1896, 2016, by = 24)) +
  labs(title = "HISTORICAL EVOLUTION OF THE BODY MASS INDEX FOR ATHLETICS EVENTS IN OLYMPIC GAMES",
       caption = "Source: Kaggle | #Tidytuesday Week 31 | @dataR_amateur") +
  ylab("Body Mass Index") +
  xlab("") +
  theme(text = element_text(size=10, face = "bold.italic"),
        plot.background = element_rect(fill = "firebrick"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "firebrick"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"))

scalefree

scalefree + ggsave("jjoofreescale_.png", width = 13, height = 8.5, dpi = 500)

normalscale <-  ggplot(df, aes(x = year, y = imc)) +
  geom_line(color = "firebrick", size = 1.5) +
  facet_wrap(~event) +
  scale_y_continuous() +
  scale_x_continuous(limits=c(1896, 2016), breaks = seq(1896, 2016, by = 24)) +
  labs(title = "HISTORICAL EVOLUTION OF THE BODY MASS INDEX FOR ATHLETICS EVENTS IN OLYMPIC GAMES",
       caption = "Source: Kaggle | #Tidytuesday Week 31 | @dataR_amateur") +
  ylab("Body Mass Index") +
  xlab("") +
  theme(text = element_text(size=10, face = "bold.italic"),
        plot.background = element_rect(fill = "firebrick"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "firebrick"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"))

normalscale

normalscale + ggsave("jjoonormal_.png", width = 13, height = 8.5, dpi = 500)




