library(tidyverse)
BR = tibble(Best_Response = c(rep("Player 1", 3), rep("Player 2", 4)), 
            p = c(1,1,0,0,2/3,2/3,1), 
            q = c(0,1,1,0,0,1,1))

NE = tibble(p = c(2/3, 1),
            q = c(1,1))
img <- png::readPNG("Identifying_All_NE_2x2/Game1.png")


ggplot(data = BR, aes(x = q, y = p, color = Best_Response))+
  ggpubr::background_image(img) +
  geom_path(data = NE, aes(x = q, y = p), color = "darkblue", size = 6) +
  geom_path(size = 2)+
  scale_x_continuous(position = "top")+
  theme(text = element_text(size = 30))+
  labs(color = "Best Response") +
  #ggtitle("Best Response Functions for Player 1 and Player 2")+
  scale_color_viridis_d(option = "C", end = .9) +
  coord_cartesian(xlim = c(-1,2), ylim = c(-.5,2)) +
  theme_void()
  #scale_color_manual(name = "Best Response", values = c("Player 1" = rgb(1,0,0), "Player 2" = rgb(0,0,.75)))
  #geom_point(aes(x = .75, y = .25), size = 1.5, color = "black")+
  #annotate("text", x = .62, y = .23, size = 5, label = "Nash Equilibrium")

#test in github
