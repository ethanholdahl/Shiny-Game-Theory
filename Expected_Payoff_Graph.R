library("tidyverse")
#Class Example
Payoff = tibble(Strat = c(rep("Up", 4), rep("Middle", 4), rep("Down", 4)), 
            ExpectedPayoff = c(8,38/7,38/7,2,5,38/7,17/3,6,3,17/3,17/3,7), 
            q = c(1,4/7,4/7,0,1,4/7,1/3,0,1,1/3,1/3,0),
            type = as.factor(c(1,1,2,2,2,1,1,2,2,2,1,1)))
Payoff$Strat <- factor(Payoff$Strat, levels = c("Up", "Middle", "Down"))

#HW4Q5
Payoff = tibble(Strat = c(rep("Up", 4), rep("Middle", 4), rep("Down", 4)), 
                ExpectedPayoff = c(7,11/2,41/8,4,9,11/2,11/2,2,2,41/8,41/8,7), 
                q = c(1,1/2,3/8,0,1,1/2,1/2,0,1,3/8,3/8,0),
                type = as.factor(c(2,1,1,2,1,1,2,2,2,2,1,1)))
Payoff$Strat <- factor(Payoff$Strat, levels = c("Up", "Middle", "Down"))

#MidtermReview
Payoff = tibble(Strat = c(rep("Up", 2), rep("Middle", 2), rep("Down", 2)), 
                ExpectedPayoff = c(6,2,4,7,1,8), 
                q = c(1,0,1,0,1,0),
                type = as.factor(c(1,1,1,1,1,1)))
Payoff$Strat <- factor(Payoff$Strat, levels = c("Up", "Middle", "Down"))

##Quiz4
#Q1
Payoff = tibble(Strat = c(rep("Most Risk", 4), rep("Moderate Risk", 4), rep("Ol' Reliable", 4)), 
                ExpectedPayoff = c(10,6,6,0,8,6,17/4,3,5,17/4,17/4,4), 
                q = c(1,3/5,3/5,0,1,3/5,1/4,0,1,1/4,1/4,0),
                type = as.factor(c(1,1,2,2,2,1,1,2,2,2,1,1)))
Payoff$Strat <- factor(Payoff$Strat, levels = c("Most Risk", "Moderate Risk", "Ol' Reliable"))
#EC2
Payoff = tibble(Strat = c(rep("Most Risk", 4), rep("Moderate Risk", 4), rep("Ol' Reliable", 4), rep("Insurance-Low", 4), rep("Insurance-High", 4)), 
                ExpectedPayoff = c(10,6,6,0,8,6,17/4,3,5,17/4,17/4,4,3,17/4,17/4,14/3,3,6,6,21/2), 
                q = c(1,3/5,3/5,0,1,3/5,1/4,0,1,1/4,1/4,0,1,1/4,1/4,0,1,3/5,3/5,0),
                type = as.factor(c(1,1,2,2,2,1,1,2,2,2,2,2,2,2,1,1,2,2,1,1)))
Payoff$Strat <- factor(Payoff$Strat, levels = c("Most Risk", "Moderate Risk", "Ol' Reliable", "Insurance-High", "Insurance-Low"))


#Take Home
Payoff = tibble(Strat = c(rep("Left", 2), rep("Up", 2), rep("Down", 2)), 
                ExpectedPayoff = c(7,6,10,4,4,8), 
                q = c(1,0,1,0,1,0),
                type = as.factor(c(1,1,1,1,1,1)))
Payoff$Strat <- factor(Payoff$Strat, levels = c("Left", "Up", "Down"))


ggplot(data = Payoff, aes(x = q, y = ExpectedPayoff, color = Strat, linetype = type))+
  geom_path(size = 1.5)+
  scale_x_reverse(position = "top")+
  theme(text = element_text(size = 20))+
  scale_color_manual(name = "Strategy", values = c(rgb(1,.5,0), rgb(0.5,.2,.5), rgb(0,.5,0), rgb(.3,.5,1), rgb(.1,0,1)))+
  scale_linetype_manual(name = "Best Response", values = c(1, 2), labels = c("Yes", "No")) +
  geom_point(aes(x = 3/5, y = 6, size = 1.5), color = "black", stroke = 3)+
  geom_point(aes(x = 1/4, y = 17/4, size = 1.5), color = "black", stroke = 3)+
  geom_point(aes(x = 1/4, y = 17/4, size = 1.5), color = "white", stroke = 1)+
  #geom_point(aes(x = 4/10, y = 64/10, size = 1.5), color = "black")+
  guides(size = FALSE)+ 
  ggtitle("Expected Payoffs for Player 1's Strategies given Player 2's Choice of q")+
  ylab("Expected Payoff")

