library(tidyverse)
library(ggthemes)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 16)
    )
}

bitcoin <- read_csv(url("https://raw.githubusercontent.com/tejseth/bullish/master/bitcoin_monthly.csv"))

bitcoin <- bitcoin %>% 
  rename(month = month...1,
         year = month...3) %>%
  mutate(change = change*100,
         label = paste0(change, "%"))

bitcoin %>%
  filter(year >= 2014) %>%
  ggplot(aes(x = month, y = year, fill = change)) +
  geom_tile(aes(fill =change)) +
  scale_fill_brewer(palette = "RdYlGn") +
  geom_text(aes(label = label, color = "white"), size = 5) +
  scale_color_identity() +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Month",
       y = "Year",
       title = "Bitcoin's Monthly Percent Change, 2014-2021") +
  theme(panel.grid.major = element_line(size = 0.1))
ggsave('bitcoin-1.png', width = 15, height = 10, dpi = "retina")

bitcoin %>%
  filter(year >= 2014 & year <= 2020) %>%
  ggplot(aes(x = year, y = change)) +
  geom_bar(aes(fill = ifelse(change < 0, "darkred", "darkgreen")), stat = "identity") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  facet_wrap(~month) +
  labs(x = "Year",
       y = "Bitcoin Monthly Change (%)",
       title = "Bitcoin Change by Year and Month") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(strip.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = -60, size = 12))
ggsave('bitcoin-2.png', width = 15, height = 10, dpi = 'retina')

sept <- bitcoin %>%
  filter(month == 9)

sept %>%
  ggplot(aes(x = year, y = change)) +
  geom_bar(aes(fill = ifelse(change < 0, "darkred", "darkgreen")), stat = "identity") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  geom_text(aes(y = ifelse(change > 0, change + 1, change - 1), label = label), size = 5) +
  labs(x = "Year",
       y = "Bitcoin Percent Change",
       title = "Bitcoin's Percent Change in September by Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
  theme(panel.grid.major.x = element_line(size = 0.1))
ggsave('bitcoin-3.png', width = 15, height = 10, dpi = "retina")
