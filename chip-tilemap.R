library(tidyverse)
library(forcats)
library(geofacet)
library(gridExtra)

source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_theme_windows.R')

urban_colors <- c("#062635", "#12719e", "#46abdb", "#a2d4ec", "#cfe8f3")


chip <- read_csv("data/chip-enrollment.csv")

# geofacet      
chip <- chip %>%
  mutate(x = 1, 
         y = 1) %>%
  arrange(desc(`CHIP Enrollment`)) %>%
  mutate(enrollment_group = c(rep("Group 1", 11), 
                              rep("Group 2", 10), 
                              rep("Group 3", 10), 
                              rep("Group 4", 10), 
                              rep("Group 5", 10)))


chip_map <- chip %>%
  ggplot(aes(x, y, fill = enrollment_group)) +
    geom_tile() +
    scale_fill_manual(values = urban_colors) +  
    geom_text(aes(label = state_abbreviation), color = "white") +
    facet_geo(~state_abbreviation, grid = "us_state_grid1") +
    labs(x = NULL,
         y = NULL) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(0L, "pt"))

# bar plot
chip_bar_plot <- chip %>%
  ggplot(aes(x = fct_reorder(state_abbreviation, `CHIP Enrollment`, .desc = TRUE), y = `CHIP Enrollment`, fill = enrollment_group)) +
    geom_bar(stat = "identity") +
  scale_fill_manual(values = urban_colors) +
  scale_y_continuous(expand = c(0, 0)) +
    labs(x = NULL,
         y = NULL) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size = 6L),
        legend.position = "none")

chip_plots <- grid.arrange(chip_map, chip_bar_plot)

ggsave(file = "output/chip-tilemap.png", chip_plots, width = 8.5, height = 9)
ggsave(file = "output/chip-tilemap.svg", chip_plots, width = 8.5, height = 9)


