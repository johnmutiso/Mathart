library(tidyverse); library(extrafont)
windowsFonts(Papyrus = windowsFont('Papyrus'))

# Function to create circle data --------------------------------------------
# based on  the algebraic equation ( x - h )^2 + ( y - k )^2 = r^2 -----------
circle <- function(radius, x, center=c(0,0)) {
  y = sqrt(radius^2 - (x - center[1])^2) + center[2]
  data.frame(x = x, y1 = y, y2 = -y)
}

# circle centres ------------------------------------------------------
circle_data <- circle(radius = 6, x = seq(-4,4, by = 0.03), center = c(0,0))%>% 
  pivot_longer(cols = -1, names_to = 'group', values_to = 'y') %>% 
  drop_na(.)



newdata <- data.frame()
radius <- 3

#' Replicating circles around the [circle_data] circle
for(i in 1:nrow(circle_centres)) {
  temp <- circle_centres[i,] 
  
  center <- c(temp[[1]],temp[[2]])
  
  x_values <- seq(center[1]-radius, center[1]+radius, by = 0.01)
  
  data_ <- circle(radius = radius,
                  center = center,
                  x = x_values) %>% 
    mutate(group = i)
    
  newdata <- newdata %>% bind_rows(data_)
    
}


newdata2 <- 
  newdata %>% 
  pivot_longer(cols = -c(1,4), names_to = 'y_groups', values_to = 'y') %>% 
  mutate(y = -y) %>% 
  drop_na(.)

#art ---------------------------

art_8 <-  
newdata2 %>% 
  ggplot()+
  geom_point(aes(x,y, group = group, color = group), size = 0.1, show.legend = F, alpha = 0.5) +
  labs(caption = "github: @johnmutiso\n{mathart}") +
  coord_equal() +
  theme_void()+
  theme(plot.caption = element_text(family = 'Papyrus', size = 10))


# final graphic
ggsave(plot = art_8, path = './graphic/', dpi = 500, device = 'png', 
       filename = 'art_8.png', width = 6, height = 6)