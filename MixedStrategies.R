game = MakeGame(2,2)
tabledata = MakeTableData(2,2,game)


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
  coord_cartesian(xlim =c(-.3, 2), ylim = c(-2,.3)) +
  annotate("path",
           x=.25+.1*cos(seq(0,2*pi,length.out=100)),
           y=-.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x=1.25+.1*cos(seq(0,2*pi,length.out=100)),
           y=-1.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x=.75+.1*cos(seq(0,2*pi,length.out=100)),
           y=-.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue") + 
  annotate("path",
           x=1.75+.1*cos(seq(0,2*pi,length.out=100)),
           y=-1.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue")


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
  coord_cartesian(xlim =c(-.5, 2), ylim = c(-2,.5))  +
  annotate("path",
           x=.25+.1*cos(seq(0,2*pi,length.out=100)),
           y=-.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x=1.25+.1*cos(seq(0,2*pi,length.out=100)),
           y=-1.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x=.75+.1*cos(seq(0,2*pi,length.out=100)),
           y=-.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue") + 
  annotate("path",
           x=1.75+.1*cos(seq(0,2*pi,length.out=100)),
           y=-1.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue")


tabledataexpand = tabledata
tabledataexpand[[1]] = c(-.2, -.2, -.2, .5, 1.5, 2.5)
tabledataexpand[[2]] = c(-.5, -1.5, -2.5, .2, .2, .2)
tabledataexpand[[3]] = c("s1", "p*s1+(1-p)*s2", "s2", "s1", "q*s1+(1-p)*s2", "s2")
tabledataexpand[[4]] = c("red", "red", "red", "blue", "blue", "blue")
tabledataexpand[[5]] = c(0.25, 0.25, 2.25, 2.25, 0.75, 0.75, 2.75, 2.75)
tabledataexpand[[6]] = c(-0.75, -2.75, -0.75, -2.75, -0.25, -2.25, -0.25, -2.25)


ggplot()+
  geom_segment(aes(x = seq(0,3), y = 0, xend = seq(0,3), yend = -3))+
  geom_segment(aes(x = 0, y = seq(0,-3), xend = 3, yend = seq(0,-3)))+
  annotate("text", x = tabledataexpand[[1]], y = tabledataexpand[[2]], size = 10, label = tabledataexpand[[3]], color = tabledataexpand[[4]])+
  annotate("text", x = tabledataexpand[[5]], y = tabledataexpand[[6]], size = 10, label = tabledataexpand[[7]], color = tabledataexpand[[8]])+
  theme_void()+
  coord_cartesian(xlim =c(-.5, 3), ylim = c(-3,.5))  +
  annotate("path",
           x = .25+.1*cos(seq(0,2*pi,length.out=100)),
           y = -.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x = 2.25+.1*cos(seq(0,2*pi,length.out=100)),
           y = -2.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x = .75+.1*cos(seq(0,2*pi,length.out=100)),
           y = -.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue") + 
  annotate("path",
           x = 2.75+.1*cos(seq(0,2*pi,length.out=100)),
           y = -2.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue") +
  # annotate("path",
  #          x = c(.35, 1.25, 1.25, 2.15),
  #          y = c(-.75, -.75, -2.75, -2.75),
  #          color = "red",
  #          size = 1.5) +
  # annotate("path",
  #          x = c(.75, .75, 2.75, 2.75),
  #          y = c(-.35, -1.75, -1.75, -2.15),
  #          color = "blue",
  #          size = 1.5)
  annotate("path",
           x = c(.5, 1.5, 1.5, 2.5),
           y = c(-.5, -.5, -2.5, -2.5),
           color = "red",
           size = 1.5) +
  annotate("path",
           x = c(.5, .5, 2.5, 2.5),
           y = c(-.5, -1.5, -1.5, -2.5),
           color = "blue",
           size = 1.5)


ggplot()+
  geom_segment(aes(x = seq(0,3), y = 0, xend = seq(0,3), yend = -3))+
  geom_segment(aes(x = 0, y = seq(0,-3), xend = 3, yend = seq(0,-3)))+
  annotate("text", x = tabledataexpand[[1]], y = tabledataexpand[[2]], size = 10, label = tabledataexpand[[3]], color = tabledataexpand[[4]])+
  annotate("text", x = tabledataexpand[[5]], y = tabledataexpand[[6]], size = 10, label = tabledataexpand[[7]], color = tabledataexpand[[8]])+
  theme_void()+
  coord_cartesian(xlim =c(-.5, 3), ylim = c(-3,.5))  +
  annotate("path",
           x = .25+.1*cos(seq(0,2*pi,length.out=100)),
           y = -.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x = 2.25+.1*cos(seq(0,2*pi,length.out=100)),
           y = -2.75+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "red") + 
  annotate("path",
           x = .75+.1*cos(seq(0,2*pi,length.out=100)),
           y = -.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue") + 
  annotate("path",
           x = 2.75+.1*cos(seq(0,2*pi,length.out=100)),
           y = -2.25+.1*sin(seq(0,2*pi,length.out=100)),
           size = 1.5,
           color = "blue") +
  # annotate("path",
  #          x = c(.35, 1.25, 1.25, 2.15),
  #          y = c(-.75, -.75, -2.75, -2.75),
  #          color = "red",
  #          size = 1.5) +
  # annotate("path",
  #          x = c(.75, .75, 2.75, 2.75),
  #          y = c(-.35, -1.75, -1.75, -2.15),
  #          color = "blue",
  #          size = 1.5)
  annotate("path",
           x = c(.5, 1.5, 1.5, 2.5),
           y = c(-.5, -.5, -2.5, -2.5),
           color = "red",
           size = 1.5) +
  annotate("path",
           x = c(.5, .5, 2.5, 2.5),
           y = c(-.5, -1.5, -1.5, -2.5),
           color = "blue",
           size = 1.5) +
  annotate("point",
           x = c(.5, 1.5, 2.5),
           y = c(-.5, -1.5, -2.5),
           color = "black",
           size = 5) +
  annotate("text",
           x = c(.5, 1.5, 2.5),
           y = c(-.5, -1.5, -2.5),
           label = "Nash Equilibrium",
           color = "black",
           hjust = 1.1,
           vjust = -1,
           size = 4)


ggplot(data = BR, aes(x = q, y = p, color = Best_Response))+
  geom_path(size = 1.5)+
  scale_x_continuous(position = "top")+
  ggtitle("Best Response Functions for Player 1 and Player 2")+
  scale_color_manual(name = "Best Response", values = c("Player 1" = rgb(1,0,0), "Player 2" = rgb(0,0,.75)))+
  geom_point(aes(x = .75, y = .25, size = 1.5), color = "black")+
  guides(size = "none") +
  annotate("text", x = .62, y = .23, size = 5, label = "Nash Equilibrium") +
  labs(x = "q", y = "p") +
  geom_label(x = 0, y = 1, label = "(s1,s1)", show.legend = FALSE, color = "black") +
  geom_label(x = 1, y = 1, label = "(s1,s2)", show.legend = FALSE, color = "black") +
  geom_label(x = 0, y = 0, label = "(s2,s1)", show.legend = FALSE, color = "black") +
  geom_label(x = 1, y = 0, label = "(s2,s2)", show.legend = FALSE, color = "black") +
  theme(text = element_text(size = 20),
        axis.title.x = element_text(colour = "blue"),
        axis.title.y = element_text(colour = "red"))


