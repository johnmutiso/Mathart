library(tidyverse)
library(extrafont)
windowsFonts(Papyrus=windowsFont('Papyrus'))

# creating data
data1 <- data.frame() #for the pyramid art

for(i in seq(0,2, 0.06)) {
  # pyramid data
  data1 <- tribble(
    ~x,~y,~group,
    0,0,i,
    0,2,i,
    i,0.5,i,
    0,0,i,
    -i,0.5,i,
    0,2,i
    
  ) %>% 
    bind_rows(data1)
}

data2 <- data.frame() # for the aetobatus-like art
for(i in seq(-2.5,2, 0.07)) {
  # aetobatus  data
  data2 <- tribble(
    ~x,~y,~group,
    0,i,i,
    0,2,i,
    i,1,i,
    0,i,i,
    -i,1,i,
    0,2,i
    
  ) %>% 
    bind_rows(data2)
}

# Pyramid --------------------------------------------------------------------
plot1 <-
  ggplot(data1) +
  geom_polygon(aes(x,y, group = group, col = group), 
               lty = 3, size = 0.3, alpha = 0.05, show.legend = F) +
  scale_color_continuous(type = 'viridis') +
  labs(caption = '@johnmutiso\nmathart') + 
  theme_void() +
  theme(plot.background = element_rect(fill = '#0a2d11'),
        plot.caption = element_text(family = 'Papyrus', size = 12, color = '#f8fc00'))

# Aetobactus - like ------------------------------------------------------------
plot2 <- 
  ggplot(data2) +
  geom_polygon(aes(x,y, group = group, col = group), 
               lty = 3, size = 0.3, alpha = 0.3, show.legend = F) +
  scale_color_continuous(type = 'viridis') +
  labs(caption = '@johnmutiso\nmathart') + 
  theme_void() +
  theme(plot.background = element_rect(fill = '#0a2d11'),
        plot.caption = element_text(family = 'Papyrus', size = 12, color = '#f8fc00'))

# Save -------------------------------------------------------------------------------------
ggsave(plot = plot1, path = './graphic/', dpi = 500, device = 'png', 
       filename = 'art_7a.png', width = 6, height = 7) #plot 1

ggsave(plot = plot2, path = './graphic/', dpi = 500, device = 'png', 
       filename = 'art_7b.png', width = 5, height = 7) #plot2


