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

stocks <- read_csv(url("https://raw.githubusercontent.com/tejseth/bullish/master/stocks-21.csv"))

stocks %>%
  ggplot(aes(x = fct_reorder(symbol, percent), y = percent)) +
  geom_bar(aes(fill = percent), stat = "identity", color = "black") +
  theme_reach() +
  geom_text(aes(y = percent + 2, label = paste0("+", percent, "%")), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Stock",
       y = "Percent Gain in 2021",
       title = "Best Performing Mega Cap* Stocks so far in 2021",
       caption = "*$200B+ market cap") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(panel.grid.major.x = element_line(size = 0.1),
        plot.caption = element_text(size = 14))
ggsave('mega-cap-21.png', width = 15, height = 10, dpi = "retina")
