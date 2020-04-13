library(tidyverse)
library(extrafont)
windowsFonts(Papyrus=windowsFont('Papyrus')) #load windows font Papyrus
y <- seq(-10,10,0.01) #Define y var
x_ <- seq(-10,10,0.01) 
x <- x_**2#define x var

mydata <- data.frame(y = seq(-10,10,0.01))

#iterating the x variable
line_seq <- seq(0,200,1)

for(i in line_seq){
  col1 <- paste0('x1_', i)
  col2 <- paste0('x2_', i)
  mydata[,col1] <- x + i
  mydata[,col2] <- -x + i
}

# data ---------------------------------------------------------------- 
mydata <- 
  mydata %>% 
  pivot_longer(cols = -1, names_to = 'group', values_to = 'x')

# plot -----------------------------------------------------------------
plot <- 
  mydata %>% 
    ggplot() +
    geom_path(aes(x,y, group=group), size = 0.2) +
    labs(caption = '@johnmutiso\nmathart') + 
    theme_void() +
    theme(plot.caption = element_text(family = 'Papyrus', size = 12),
          plot.background = element_blank())
# coloured plot
n_groups <- mydata %>% distinct(group) %>% nrow()
colorpalette <- grDevices::colorRampPalette(brewer.pal(8, 'Set1'))

plot2 <- 
  mydata %>% 
  mutate(group = factor(group)) %>%
  ggplot() +
  geom_path(aes(x,y, group=group, col = group), 
            alpha =0.9, size = 0.7, show.legend = F) +
  scale_colour_manual(values = colorpalette(n_groups)) +
  labs(caption = '@johnmutiso\nmathart') + 
  theme_void() +
  theme(plot.caption = element_text(family = 'Papyrus', size = 12),
        plot.background = element_blank())

# Save ---------------------------------------------------------------
ggsave(plot = plot, path = './graphic/', dpi = 400, device = 'png', 
       filename = 'art_3.png', width = 8, height = 8)

