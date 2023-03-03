library("tidyverse")
##### Class Materials #####

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

##### Code for Generic #####

n_strategies = 2
payoff_1_strat = c(1,2)
payoff_2_strat = c(2,0)
payoff_difference = payoff_2_strat - payoff_1_strat
p = caracas::symbol('p')
payoff_expected = list()
for(i in 1:n_strategies){
  payoff_expected[[i]] = payoff_1_strat[i] + p * payoff_difference[i]
}
first_vector = rep(NA, (n_strategies^2-n_strategies)/2)
second_vector = rep(NA, (n_strategies^2-n_strategies)/2)
p_value_vector = rep(NaN, (n_strategies^2-n_strategies)/2)
i = 0
for(first in 1:(n_strategies-1)){
  for(second in (first+1):n_strategies){
    i = i+1
    result = caracas::solve_sys(payoff_expected[[first]], payoff_expected[[second]], p)
    t = try(result[[1]], silent = TRUE)
    if (inherits(t, "try-error")){
      p_value = NA 
    } else {
      p_value = caracas::as_expr(result[[1]]$p)
    } 
    first_vector[i] = first
    second_vector[i] = second
    p_value_vector[i] = p_value
  }
}
possible_intersections = tibble(first = first_vector, second = second_vector, p_value = p_value_vector)
possible_intersections = possible_intersections %>%
  mutate(inside_range = p_value>=0 & p_value <=1)
possible_intersections

#identify all unique intersections within the bounds of 0 to 1 and arrange them by p-value from smallest to greatest 
critical_points = possible_intersections %>%
  filter(inside_range == TRUE) %>%
  select(p_value) %>%
  arrange(p_value) %>%
  unique() %>%
  mutate(intersection = TRUE)

#add midpoints
midpoints = dim(critical_points)[1]-1
if(midpoints > 0){
  for(i in 1:midpoints){
    midpoint_p_value = (critical_points$p_value[i] + critical_points$p_value[i+1])/2
    critical_points = critical_points %>%
      add_row(!!! c(p_value = midpoint_p_value, intersection = FALSE))
  }
}

#check if endpoints are intersections and add midpoints
if(!0%in%critical_points$p_value){
  critical_points = critical_points %>%
    add_row(!!! c(p_value = 0, intersection = FALSE))
}
if(!1%in%critical_points$p_value){
  critical_points = critical_points %>%
    add_row(!!! c(p_value = 1, intersection = FALSE))
}
critical_points = critical_points %>%
  arrange(p_value)

critical_points

for(strategy in 1:n_strategies){
  strategy_payoffs = rep(NA, dim(critical_points)[1])
  for(i in 1:dim(critical_points)[1]){
    t = try(caracas::as_expr(caracas::subs(payoff_expected[[strategy]], p, critical_points$p_value[i])), silent = TRUE)
    if (inherits(t, "try-error")){
      #constant
      strategy_payoffs[i] = caracas::as_expr(payoff_expected[[strategy]])
    } else {
      strategy_payoffs[i] = t
    } 
  }
  critical_points = critical_points %>%
    add_column(strategy_payoffs)
  colnames(critical_points)[2 + strategy] = paste0("strategy_payoffs_", strategy)
}

critical_points = critical_points %>%
  pivot_longer(!c(p_value,intersection), names_to = "strategy", names_prefix = "strategy_payoffs_", values_to = "payoff")
critical_points = critical_points %>%
  group_by(p_value) %>%
  mutate(BR = ifelse(payoff == max(payoff), TRUE, FALSE))
as.factor(critical_points$strategy)

best_response = critical_points %>%
  filter(BR == TRUE) 
best_response

BRlist = list()
p_vals = unique(best_response$p_value)
for(i in 1:length(p_vals)){
  BRlist[[i]] = list(p_value = p_vals[i], strategy = best_response$strategy[best_response$p_value==p_vals[i]], payoff = best_response$payoff[best_response$p_value==p_vals[i]][1])
}
BRlist

p_value = rep(NA, length(BRlist))
payoff = rep(NA, length(BRlist))
strategies = c()
for(i in 1:length(BRlist)){
  p_value[i] = BRlist[[i]]$p_value
  payoff[i] = BRlist[[i]]$payoff
  strategies = c(strategies, BRlist[[i]]$strategy)
}
upper_envelope = tibble(p_value, payoff)
upper_envelope

if(length(unique(strategies)) == 2){
  best_response = best_response %>% arrange(strategy) %>% arrange(p_value)
  if(dim(best_response)[1] == 4){
    # Z shape. Make sure strategies are together
    #Else, L shape. No need to fix
    if(best_response$strategy[1] != best_response$strategy[2]){
      best_response = best_response %>% arrange(strategy) %>% arrange(desc(p_value))
    }
  }
  best_response = best_response %>% 
    mutate(q_value = ifelse(strategy == "1", 0, 1)) %>%
    select(q_value, p_value, payoff, intersection)
}



#add intersection points
inside_intersections = possible_intersections %>%
  filter(inside_range == TRUE) %>%
  select(p_value, first) %>%
  arrange(p_value) %>%
  unique() 
payoff = rep(NA, dim(inside_intersections)[1])
for(i in 1:dim(inside_intersections)[1]){
  t = try(caracas::as_expr(caracas::subs(payoff_expected[[inside_intersections$first[i]]], p, inside_intersections$p_value[i])), silent = TRUE)
  if (inherits(t, "try-error")){
    #constant
    payoff[i] = caracas::as_expr(payoff_expected[[inside_intersections$first[i]]])
  } else {
    payoff[i] = t
  } 
}
inside_intersections = inside_intersections %>%
  add_column(payoff)

#rename strategies
critical_points$strategy = sub("1", "Left", critical_points$strategy) 
critical_points$strategy = sub("2", "Right", critical_points$strategy) 
#critical_points$strategy = sub("1", "Up", critical_points$strategy) 
#critical_points$strategy = sub("2", "Down", critical_points$strategy) 


#1000*700
ggplot(data = critical_points) +
  #geom_path(data = upper_envelope, aes(x = p_value, y = payoff), alpha = .5, size = 7, color = "gray") +
  geom_path(aes(x = p_value, y = payoff, color = strategy), size = 2) +
  geom_point(data = inside_intersections, aes(x = p_value, y = payoff), size = 6) + 
  scale_color_viridis_d(option = "C", end = .9) + 
  labs(y = "Expected Payoff", x = "p", color = "Strategy") +
  #labs(y = "Expected Payoff", x = "q", color = "Strategy") +
  theme(text = element_text(size = 30))


