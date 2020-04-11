library(tidyverse)
library(patchwork)
library(extrafont)
windowsFonts(Papyrus = windowsFont('Papyrus'))

# Function to create circle data --------------------------------------------
# based on  the algebraic equation ( x - h )^2 + ( y - k )^2 = r^2 -----------
circle <- function(radius, x, center) {
  y = sqrt(radius^2 - (x - center[1])^2) + center[2]
  data.frame(x = x, y1 = y, y2 = -y)
}

data <- circle(radius = 3, x = seq(-3,3, by = 0.01), center = c(0,0))%>% 
  pivot_longer(cols = -1, names_to = 'group', values_to = 'y')

# Unexpected ------------------------------------------------------------
unexpected <- 
  data %>% 
  ggplot()+
  geom_path(aes(x,y), alpha = 0.8, size = 0.3) +
  coord_equal() +
  theme_void()

# Expected circle ----------------------------------------------------------
# suplied the grouping variable
expected_circle <- 
  data %>% 
  ggplot()+
  geom_path(aes(x,y, group = group, color = group), size = 4, show.legend = F) +
  labs(caption = "github: @johnmutiso\nCIRCLE{mathart}") +
  coord_equal() +
  theme_void()+
  theme(plot.caption = element_text(family = 'Papyrus', size = 14))

# final graphic
plot <- unexpected|expected_circle +
  plot_annotation()

ggsave(plot = plot, path = './graphic/', dpi = 500, device = 'png', 
       filename = 'art_2.png', width = 20, height = 10)
