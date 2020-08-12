#Code from <https://stats.andrewheiss.com/misc/gantt.html>


# Loading libraries -------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)
library(Cairo)


# Specifying tasks --------------------------------------------------------

tasks <- tribble(
  ~Start,       ~End,         ~Project,          ~Task,
  "12-08-2020", "01-08-2023", "Whole PhD", "PhD",
  "01-08-2020", "01-09-2020", "Reading", "Preliminary litterature review",
  "01-09-2020", "01-04-2021", "Training", "Elective modules",
  "01-01-2021", "01-05-2021", "Writing", "Master's Dissertation",
  "01-01-2021", "01-04-2021", "Other", "Internship",
  "01-02-2023", "01-04-2023", "Reading", "Final litterature review",
  "01-04-2023", "15-06-2023", "Writing", "Final Write-Up") %>%
  mutate(Start = dmy(Start),
         End = dmy(End)) %>%
  gather(date.type, task.date, -c(Project, Task)) %>%
  arrange(date.type, task.date) %>%
  mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))

# Calculate where to put the dotted lines ---------------------------------

x.breaks <- seq(length(tasks$Task) + 0.5 - 1, 0, by=-1)

# Build plot --------------------------------------------------------------

timeline <- ggplot(tasks, aes(x=Task, y=task.date, colour=Project)) + 
  geom_line(size=6) + 
  geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") + 
  guides(colour=guide_legend(title=NULL)) +
  labs(x=NULL, y=NULL) + coord_flip() +
  scale_y_date(date_breaks="1 month", labels=date_format("%b ‘%y")) +
  theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1),legend.position="bottom")

timeline



# Save plot as PDF with embedded fonts (the secret is "device=cairo_pdf")
ggsave(timeline, filename="timeline.pdf",
       width=6.5, height=6.5, units="in", device=cairo_pdf)

# Save plot as high resolution PNG (the secret is 'type="cairo", dpi=300')
ggsave(timeline, filename="timeline.png",
       width=10, height=6.5, units="in", type="cairo", dpi=300)
