game = MakeGame(2,2)
tabledata = MakeTableData(2,2,game)
game

ggplot()+
  geom_segment(aes(x = seq(0,2), y = 0, xend = seq(0,2), yend = -2))+
  geom_segment(aes(x = 0, y = seq(0,-2), xend = 2, yend = seq(0,-2)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  coord_cartesian(xlim =c(-.3, 2), ylim = c(-2,.3))

ggplot()+
  geom_segment(aes(x = seq(0,2), y = 0, xend = seq(0,2), yend = -2))+
  geom_segment(aes(x = 0, y = seq(0,-2), xend = 2, yend = seq(0,-2)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  annotate("text", x = -.4, y = -.5, size = 10, label = "p", color = "red") +
  annotate("text", x = -.4, y = -1.5, size = 10, label = "(1-p)", color = "red") +
  annotate("text", x = 1.5, y = .4, size = 10, label = "q", color = "blue") +
  annotate("text", x = .5, y = .4, size = 10, label = "(1-q)", color = "blue") +
  coord_cartesian(xlim =c(-.5, 2), ylim = c(-2,.5))


ggplot(data = BR, aes(x = q, y = p, color = Best_Response))+
  geom_path(size = 1.5)+
  scale_x_continuous(position = "top")+
  ggtitle("Best Response Functions for Player 1 and Player 2")+
  scale_color_manual(name = "Best Response", values = c("Player 1" = rgb(1,0,0), "Player 2" = rgb(0,0,.75)))+
  geom_point(aes(x = .75, y = .25, size = 1.5), color = "black")+
  guides(size = FALSE) +
  annotate("text", x = .62, y = .23, size = 5, label = "Nash Equilibrium") +
  labs(x = expression(atop("p", paste("s1                  s2"))), y = expression(atop("p", paste("s1                  s2")))) +
  theme(text = element_text(size = 20),
        axis.title.x = element_text(colour = "blue"),
        axis.title.y = element_text(colour = "red"))


