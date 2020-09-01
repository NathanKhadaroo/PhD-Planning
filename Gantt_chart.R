#Code inspired by <https://stats.andrewheiss.com/misc/gantt.html>


# Loading libraries -------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lubridate) # Make Dealing with Dates a Little Easier
library(scales) # Scale Functions for Visualization
library(Cairo) # R Graphics Device using Cairo Graphics Library
library(patchwork) # The Composer of Plots
library(RColorBrewer) # ColorBrewer Palettes


# Specifying tasks --------------------------------------------------------

tasks <- tribble(
  ~Start,       ~End,         ~Project,          ~Task,
  "12-08-2020", "01-08-2023", "Whole PhD", "PhD",
  "01-08-2020", "01-09-2020", "Reading", "Preliminary literature review",
  "01-09-2020", "01-04-2021", "Training", "Elective modules",
  "01-09-2020", "01-01-2021", "Review", "Systematic review training",
  "01-01-2021", "30-08-2021", "Review", "Systematic review writing",
  "02-09-2021", "01-01-2022", "Simulation Study", "Simulation Study training",
  "01-01-2022", "30-08-2022", "Simulation Study", "Simulation Study writing",
  "01-01-2022", "01-04-2022", "Other", "Internship",
  "02-09-2022", "01-02-2023", "Reading", "Final literature review",
  "01-02-2023", "15-06-2023", "Writing", "Final Write-Up") %>%
  mutate(Start = dmy(Start),
         End = dmy(End)) %>%
  gather(date.type, task.date, -c(Project, Task)) %>%
  arrange(date.type, task.date) %>%
  mutate(Task = factor(Task, levels = rev(unique(Task)), ordered = TRUE))

# Specifying years --------------------------------------------------------

tasks_y2 <- tasks %>%
  filter(task.date < "2021-09-01",
         Task != "PhD")

tasks_y3 <- tasks %>%
  filter(task.date > "2021-09-01",
         task.date < "2022-09-01",
         Task != "PhD")

tasks_y4 <- tasks %>%
  filter(task.date > "2022-09-01",
         task.date < "2023-09-01",
         Task != "PhD")

# Making Gantt function ---------------------------------------------------

Gantt <- function(x, Task, task.date, Project) {
    ggplot(x, aes(x = Task,
                  y = task.date,
                  colour = Project)) +
    scale_fill_brewer(palette="Set2") +
    scale_color_brewer(palette="Set2") +
    geom_line(size = 6) +
    geom_vline(xintercept = seq(length(tasks$Task) + 0.5 - 1, 0, by = -1),
               colour = "grey80",
               linetype = "dotted") +
    guides(colour = guide_legend(title = NULL)) +
    labs(x = NULL, y = NULL) +
    coord_flip() +
    scale_y_date(date_breaks = "1 month",
                 labels = date_format("%b ‘%y")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  }


# Creating plots ----------------------------------------------------------

# Whole PhD

whole <- Gantt(tasks) +
  ggtitle("Whole PhD! :")

# By year

y2 <- Gantt(tasks_y2) +
  ggtitle("Second Year:")

y3 <- Gantt(tasks_y3) +
  ggtitle("Third Year:")
  
y4 <- Gantt(tasks_y4) +
  ggtitle("Final Year:")

charts <- whole / (y2 + y3 + y4) +
  plot_annotation(title = "PhD plan (so far!)") &
  theme(text = element_text("mono"))


# Save plot as high resolution PNG (the secret is 'type = "cairo", dpi = 300')

ggsave(charts, filename = "Gantt_chart.png",
       width = 12, height = 6.5, units = "in", type = "cairo", dpi = 300)

