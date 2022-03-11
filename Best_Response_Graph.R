library("tidyverse")
BR = tibble(Best_Response = c(rep("Player 1", 4), rep("Player 2", 4)), 
            p = c(1,1,0,0,1,.25,.25,0), 
            q = c(1,.75,.75,0,0,0,1,1))

ggplot(data = BR, aes(x = q, y = p, color = Best_Response))+
  geom_path(size = 1.5)+
  scale_x_reverse(position = "top")+
  theme(text = element_text(size = 20))+
  ggtitle("Best Response Functions for Player 1 and Player 2")+
  scale_color_manual(name = "Best Response", values = c("Player 1" = rgb(1,0,0), "Player 2" = rgb(0,0,.75)))+
  geom_point(aes(x = .75, y = .25, size = 1.5), color = "black")+
  guides(size = FALSE) +
  annotate("text", x = .62, y = .23, size = 5, label = "Nash Equilibrium")
