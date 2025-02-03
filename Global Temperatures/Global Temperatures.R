library(tidyverse)
library(readxl)
library(showtext)

# https://climate.nasa.gov/vital-signs/global-temperature/?intent=121
# https://www.nytimes.com/2020/04/23/learning/whats-going-on-in-this-graph-global-temperature-change.html

font_add_google("Libre Franklin", "franklin")
showtext.auto()
showtext.opts(dpi = 300)

temps <- read_excel("Global Temperatures/Land-Ocean Temperature Index (C).xlsx",
                    col_names = TRUE)

temps <- temps |> 
  rename(
    year = Year,
    avg_temp = No_Smoothing,
    loess = `Lowess(5)`
  ) |> 
  filter(year <= 2019)

temps |> 
  ggplot(aes(x = year, y = avg_temp)) +
  geom_col(aes(fill = avg_temp > 0),
           width = 0.8,
           show.legend = FALSE) +
  annotate(geom = "segment",
           x = 2018.75, xend = 2018.75, y = 0.985, yend = 1.05,
           color = "grey70", size = 0.4) +
  annotate(geom = "text",
           x = 2016.75, y = 1.07, label = "2019",
           size = 7, size.unit = "pt",
           color = "black", fontface = "bold", family = "franklin") +
  annotate(geom = "text", 
           x = 1939, y = -0.25, label = "-0.25°",
           size = 8.5, size.unit = "pt",
           color = "#8a8a8a") +
  annotate(geom = "text",
           x = 1974, y = 0.25, label = "+0.25°",
           size = 8.5, size.unit = "pt",
           color = "#8a8a8a") +
  annotate(geom = "text",
           x = 1992, y = 0.5, label = "+0.50°",
           size = 8.5, size.unit = "pt",
           color = "#8a8a8a") +
  annotate(geom = "text",
           x = 2008, y = 0.76, label = "+0.75°C",
           size = 8.5, size.unit = "pt",
           color = "#8a8a8a") +
  annotate(geom = "text",
           x = 1880, y = 0.76, label = "Change in global surface temperature\ncompared to the long-term average",
           size = 10, size.unit = "pt",
           color = "black", hjust = 0,
           fontface = "bold", lineheight = 1) +
  geom_segment(aes(x = 1879, xend = 2020, y = 0, yend = 0), size = 0.3) +
  geom_segment(aes(x = 1880, xend = 1934, y = -0.25, yend = -0.25), colour = "white") +
  geom_segment(aes(x = 1979, xend = 2019, y = 0.25, yend = 0.25), colour = "white") +
  geom_segment(aes(x = 1997, xend = 2019, y = 0.5, yend = 0.5), colour = "white") +
  geom_segment(aes(x = 2013, xend = 2019, y = 0.75, yend = 0.75), colour = "white") +
  scale_fill_manual(values = c("TRUE" = "#e3762c", "FALSE" = "#2a8cb0"))+
  scale_x_continuous(breaks = seq(1880, 2000, by = 20)) +
  scale_y_continuous(breaks = seq(-0.25, 0.75, by = 0.25)) +
  coord_cartesian(xlim = c(1880, 2019)) +
  theme_classic() +
  theme(
    text = element_text(family = "franklin"),
    plot.margin = margin(t = 0, r = 0, b = 20, l = 0),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(5, "pt"),
    axis.ticks.x = element_line(size = 0.5, color = "#8a8a8a"),
    axis.text.x = element_text(size = 7, color = "#8a8a8a")
  )

ggsave("Change in global surface temperature.png", width = 6, height = 4.34)
