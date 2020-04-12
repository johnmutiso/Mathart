library(tidyverse)
library(extrafont)
windowsFonts(Papyrus=windowsFont('Papyrus'))
y <- seq(0,180, 0.1)

data <- data.frame(x = seq(0,180, 0.1))

seq1 <- seq(0,3, 0.01)

for(i in seq1){
  col <- paste0('y_',i)
  data[,col] <- cos(y) + i
}

mydata <- 
  data %>% 
  pivot_longer(cols = -1, values_to = 'y', names_to = 'group')
  
plot <- 
ggplot(mydata) +
  geom_path(aes(x, y, group = group), alpha = 0.4, color = '#f8fc00') +
  expand_limits(y = c(-1.5,-1)) +
  labs(caption = '@johnmutiso\nmathart') + 
  coord_polar() +
  theme_void() +
  theme(plot.background = element_rect(fill = 'black'),
        plot.caption = element_text(family = 'Papyrus', size = 12, color = '#f8fc00'))


# Save ---------------------------------------------------------------
ggsave(plot = plot, path = './graphic/', dpi = 400, device = 'png', 
       filename = 'art_4.png', width = 9, height = 8)

  
