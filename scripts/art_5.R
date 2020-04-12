library(tidyverse)
library(extrafont)
windowsFonts(Papyrus=windowsFont('Papyrus'))
y <- seq(0,180, 0.1)

data <- data.frame(x = seq(0,180, 0.1))

seq1 <- seq(0,3, 0.15)

for(i in seq1){
  col1 <- paste0('y1_',i)
  col2 <- paste0('y2_',i)
  data[,col1] <- cos(y) + i
  data[,col2] <- sin(y) + i
  data[,col1] <- -cos(y-i) + i
  data[,col2] <- -sin(y-i) + i
}

mydata <- 
  data %>% 
  pivot_longer(cols = -1, values_to = 'y', names_to = 'group')

plot <- 
  ggplot(mydata) +
  geom_path(aes(x, y, group = group), alpha = 0.7, color = '#f8fc00', size = 0.5) +
  expand_limits(y = c(-7,-1)) +
  labs(caption = '@johnmutiso\nmathart') + 
  coord_polar() +
  theme_void() +
  theme(plot.background = element_rect(fill = 'black'),
        plot.caption = element_text(family = 'Papyrus', size = 14, color = '#f8fc00'))


# Save ---------------------------------------------------------------
ggsave(plot = plot, path = './graphic/', dpi = 500, device = 'png', 
       filename = 'art_5.png', width = 12, height = 12)


