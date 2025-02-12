# https://www.cbpp.org/research/house-gop-a-better-way-tax-cuts-would-overwhelmingly-benefit-top-1-percent-while-sharply
# https://www.urban.org/sites/default/files/publication/89706/2001229-an-analysis-of-the-house-gop-tax-plan_1.pdf

library(tidyverse)
library(readxl)
library(snakecase)
library(showtext)


font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Roboto", "Roboto")
showtext.auto()

gop_tax_cuts <- read_excel("GOP Tax Cuts 2016/gop_tax_cuts_2016.xlsx",
                           col_names = TRUE)

names(gop_tax_cuts) <- to_snake_case(names(gop_tax_cuts))

gop_tax_cuts <- gop_tax_cuts |> 
  select(income_percentile = expanded_cash_income_percentile, 3) |> 
  filter(!income_percentile %in% c('Top quintile', 'All', 'Top 0.1 percent')) |> 
  mutate(income_percentile = as.factor(income_percentile))

gop_tax_cuts$income_percentile <- factor(gop_tax_cuts$income_percentile,
                                         levels = c("Lowest quintile", "Second quintile", "Middle quintile",
                                                    "Fourth quintile", "80–90", "90–95", "95–99", "Top 1 percent"))

gop_tax_cuts |> 
  ggplot(aes(x = income_percentile , y = share_of_total_federal_tax_change)) +
  geom_col(aes(fill = share_of_total_federal_tax_change > 99),
           width = 0.75) +
  geom_hline(yintercept = 0, size = 0.75) +
  geom_text(aes(label = scales::percent(share_of_total_federal_tax_change/100)),
            vjust = if_else(gop_tax_cuts$share_of_total_federal_tax_change > 0, -0.5, 1.25),
            size = 13, colour = "grey25", family = "Roboto Condensed") +
  annotate(geom = "rect",
           xmin = 4.6, xmax = 8.4, ymin = -44, ymax = -56,
           fill = "grey75", alpha = 0.3) +
  annotate(geom = "text", x = 6.5, y = -49.25,
           size = 10.5, color = "grey50",
           label = "Top quintile", family = "Roboto") +
  annotate(geom = "segment",
           x = -Inf, xend = Inf, y = -120, yend = -120,
           size = 0.25, colour = "black") +
  annotate(geom = "text", x = 6.1, y = -128,
           size = 8, color = "grey50",
           label = "CENTER ON BUDGET AND POLICY PRIORITIES | CBPP.ORG", family = "Roboto") +
  scale_fill_manual(values = c("TRUE" = "#ce696d", "FALSE" = "#f6cd9c")) +
  scale_x_discrete(labels = c("Lowest\nquintile", "Second\nquintile", "Middle\nquintile",
                              "Fourth\nquintile", "80–90th\npercent", "90–95th\npercent", "95–99th\npercent", "Top 1\npercent")) +
  coord_cartesian(ylim = c(-10,100), clip = "off") +
  labs(
    title = "Overwhelming Share of House Republican Tax\nCuts Goes to Top 1%",
    subtitle = "Share of total federal tax cut by income group, 2025",
    caption = "Note: The total federal tax change under the House GOP plan would be a net tax cut, but\nsome income groups are expected to receive tax increases. Those groups are shown with a\nnegative share of the total change, while groups expected to receive tax cuts are shown with\na positive share.\nSource: Table 5 from James R. Nunns, et al., \"An Analysis of the House GOP Tax Plan,\"\nTax Policy Center."
  ) +
  theme_classic() +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 55, l = 10),
    plot.title = element_text(size = 46, face = "bold", family = "Roboto", lineheight = 0.3),
    plot.subtitle = element_text(size = 36, colour = "grey25", family = "Roboto Condensed"),
    plot.caption = element_text(size = 25.5, colour = "grey40", family = "Roboto",
                                lineheight = 0.335, hjust = 0, vjust = -16.5),
    plot.caption.position = "plot",
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 34, colour = "grey50", family = "Roboto Condensed",
                               lineheight = 0.3, margin = margin(t = 5)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )


ggsave("House GOP Tax Cuts 2016.png", width = 4.9, height = 5)
