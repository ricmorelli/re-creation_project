library(tidyverse)
library(snakecase)
library(readxl)
library(showtext)

font_add_google("Libre Franklin", "franklin")
showtext_auto()

online_study <- read_excel("Online Study 2025/online_study_data_CHLOE7.xlsx")

names(online_study) <- to_snake_case(names(online_study))

online_study <- online_study |> 
  pivot_longer(
    cols = -c(study_type),
    names_to = "student_type",
    values_to = "value"
  ) |> 
  mutate(
    across(where(is.character), as.factor),
    student_type = fct_relevel(student_type, "graduate_students", "adult_undergraduate", "traditional_aged_undergraduate"),
    study_type = fct_relevel(study_type, "Fully online, little or no on-campus", "Majority online, some on campus", "Balance", "Majority on campus, some onine", "Fully on-campus, little or no online")
    )

online_study |> 
  ggplot(aes(x = student_type, y = value, fill = study_type)) +
  geom_bar(stat = "identity",
           width = 0.9,
           show.legend = FALSE
           ) +
  annotate(geom = "text", size = 3, color = "white",
           x = c(2.95, 1.9), y = c(0.0425, 0.0175), label = c("4%","2"),
           hjust = 1, vjust = 0, lineheight = 0.9,
           fontface = "bold", family = "franklin") +
  annotate(geom = "text", size = 3, color = "black",
           x = c(2.95, 1.9, 0.9), y = c(0.1125, 0.11, 0.11), label = c("44","21","15"),
           hjust = 1, vjust = 0, lineheight = 0.95,
           fontface = "bold", family = "franklin") +
  annotate(geom = "text", size = 3, color = "grey30",
           x = c(2.95, 2.95, 1.9, 1.9, 0.9, 0.9), y = c(0.605, 0.9025, 0.42, 0.76, 0.335, 0.67), label = c("40","9","36","32","33","34"),
           hjust = 1, vjust = 0, lineheight = 0.95,
           fontface = "plain", family = "franklin") +
  annotate(geom = "text", size = 3, color = "black",
           x = c(2.95, 1.9, 0.9), y = c(0.9875, 0.9875, 0.9875), label = c("2","9","17"),
           hjust = 1, vjust = 0, lineheight = 0.95,
           fontface = "plain", family = "franklin") +
  annotate(geom = "text", size = 3, lineheight = 0.95,
           x = 3.75, y = 0.03, label = "Fully on-campus,\nlittle or no online",
           hjust = 1, vjust = 0,
           fontface = "bold", family = "franklin") +
  annotate(geom = "text", size = 3, lineheight = 0.95,
           x = 3.75, y = 0.085, label = "Majority on-campus,\nsome online",
           hjust = 0, vjust = 0,
           fontface = "bold", family = "franklin") +
  annotate(geom = "text", size = 3, lineheight = 0.95, color = "grey30",
           x = 3.75, y = 0.61, label = "A balance of\ncampus/online",
           hjust = 1, vjust = 0,
           fontface = "plain", family = "franklin") +
  annotate(geom = "text", size = 3, lineheight = 0.95, color = "grey30",
           x = 3.75, y = 0.9, label = "Majority online,\nsome on campus",
           hjust = 1, vjust = 0,
           fontface = "plain", family = "franklin") +
  annotate(geom = "text", size = 3, lineheight = 0.95, color = "grey30",
         x = 3.75, y = 1, label = "Fully\nonline",
         hjust = 1, vjust = 0,
         fontface = "plain", family = "franklin") +
  annotate(geom = "segment", linetype = "dotted",
           x = 3.45, xend = 3.7,
           y = c(0.02, 0.095, 0.59, 0.895, 0.98), yend = c(0.02, 0.095, 0.59, 0.895, 0.98)) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("#ee9f46","#fdd0a6","#daebf4","#a7d0e4","#2787a6")) +
  scale_x_discrete(labels = c("traditional_aged_undergraduate" = "For traditional-aged\nundergraduates",
                              "adult_undergraduate" = "For adult undergraduates",
                              "graduate_students" = "For graduate students")) +
  labs(
    title = "What College Classes May Look Like in 2025",
    subtitle = "How 271 chief online officers at colleges expect students nationwide to experience their classes.",
    caption = "Sources: Changing Landscape of Online Education (CHOLE) report by Quality Matters and Eduventures Research\nNote: Some totals do not add up to 100 because of rounding or unanswered questions."
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Libre Franklin"),
    plot.title = element_text(size = 14, face = "bold", vjust = 8),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey30", size = 9, vjust = 13),
    plot.caption = element_text(color = "grey30", size = 7, hjust = 0, margin = margin(b = -5), lineheight = 1),
    plot.caption.position = "plot",
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey30", size = 9, lineheight = 0.8, margin = margin(r = -10)),
    plot.margin = margin(t = 30, r = 0, b = 10, l = 10)
  )

ggsave("Online Study 2025.png", width = 6.5, height = 2.88)

