library(tidyverse)

data1 <- data.frame(x=1:1000)

# Creating the Dataset ------------------------------
# sequences
seq1 <- seq(1,20,0.1)
seq2 <- seq(-20,20,0.1)

for(i in seq2) {

  fun1 <- function(x){
    y = (x**i)
    return(y)
  }
  
  col <- paste0('y_',i)
  data1[,col] <- map_dbl(1:1000, fun1)
}

# mathart plot ------------------------------------------
plot <- data1 %>% pivot_longer(cols = c(-1)) %>%
  ggplot() +
  geom_line(aes(x = log10(x),y = sin(log10(value)), group = name), 
            alpha = 0.3, size=0.5, col = '#f4f6fa') +
  coord_polar(direction = -1)+
  labs(caption = 'Github: @johnmutiso\nmathart')+
  theme_void() +
  theme(plot.background = element_rect(fill = '#008080', colour = '#f4f6fa', linetype = 7),
        text = element_text(color = '#f4f6fa', size = 20, family = 'mono'))

# Saving the plot -----------------------------------------
ggsave(filename = 'art_1.jpeg',
       device = ,
       path = './graphic/',
       plot = plot,
       dpi = 400,
       width = 15,
       height = 15)
