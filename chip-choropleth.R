library(tidyverse)
library(forcats)
library(geofacet)
library(gridExtra)
library(rgdal)
library(broom)
library(tigris)
library(fiftystater)

source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_theme_windows.R')

urban_colors <- c("#cfe8f3", "#a2d4ec", "#46abdb", "#12719e", "#062635")

chip <- read_csv("data/chip-enrollment.csv")

# Create data frame with vector boundaries
data("fifty_states")

# geofacet      
chip <- chip %>%
  mutate(state = tolower(State)) %>%
  mutate(x = 1, 
         y = 1) %>%
  arrange(`CHIP Enrollment`) %>%
  mutate(enrollment_group = c(rep("Group 1", 11), 
                              rep("Group 2", 10), 
                              rep("Group 3", 10), 
                              rep("Group 4", 10), 
                              rep("Group 5", 10)))

# Plot!
chip_map <- ggplot(chip, aes(map_id = state)) +
  geom_map(aes(fill = enrollment_group), map = fifty_states, color = "#5c5859", size = 0.1) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  scale_fill_manual(values = urban_colors) +  
  coord_map() +
  labs(title = "State CHIP Enrollment")  +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))


# bar plot
chip_bar_plot <- chip %>%
  ggplot(aes(x = fct_reorder(state_abbreviation, `CHIP Enrollment`), y = `CHIP Enrollment`, fill = enrollment_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = urban_colors) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL,
       y = NULL,
       caption = "Urban Institute") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size = 7L),
        legend.position = "none")

chip_plots <- grid.arrange(chip_map, chip_bar_plot, ncol = 1, heights = c(2, 1))

ggsave(file = "output/chip-choropleth.png", chip_plots, width = 8.5, height = 9)
ggsave(file = "output/chip-choropleth.svg", chip_plots, width = 8.5, height = 9)
