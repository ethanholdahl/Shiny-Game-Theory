
## Load and install the packages
library("tidyverse", "shiny")
theme_set(theme_minimal())


# Define UI for the application 
navbarPage(
  title = "Game Theory Tools by Ethan Holdahl",
  tabPanel(
    "Normal Form Games",
    h2("Instructions"),
    helpText(
      "Hello my EC327 students. The final on Monday will heavily feature normal form
      games and test your ability to find all the Nash Equilibria (both pure and mixed) in those games.
      I've created this application to help give you additional practice in solving mixed strategy games if you desire it.
      To practice, simply generate a game, solve it, then use the buttons to reveal the solutions."
    ),
    
    helpText(
      "Recall: When solving normal form games there are 4 large steps we want to follow:"
    ),
    
    helpText(
      "1. Use Iterated Elimination of Dominated Strategies to make the game as small as possible without removing any equilibria from the game"
    ),
    
    helpText("2. Use Best Response Analysis (The order of 1 and 2 may be flipped)"),
    
    helpText("3. Identify any pure strategy Nash Equilibria"),
    
    helpText("4. Identify any mixed strategy Nash Equilibria"),
    
    helpText(
      "On the sidebar on the left you will be able to select how large of a game you want to generate. The payoffs shown in the generated game are randomly sampled from a list of integers from -10 to 20 with replacement.
      After selecting the size of your game you may select the IEDS, Best Response, Pure NE, and All NE buttons to reveal the strategies dominated, the best reponse correspondance, list of pure strategy Nash Equilibria, and list of all Nash Equilibria in the game."
    ),
    helpText(
      "Note: You may select a game as large as a 9x9, however, currently the application will only report mixed strategy Nash Equilibria from Nx2 or 2xN games (when N can be any size). Recall, on the final I will not ask you to solve anything that can't be reduced down to atleast a 2x3 or 3x2 game."
    ),
    helpText("More features will be added thoughout the weekend as soon as I'm able to code them."),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
                      sidebarPanel(
                        style = "position:fixed;width:inherit;",
                        sliderInput(
                          "S1",
                          label = "Number of Strategies for Player 1:",
                          min = 2,
                          max = 9,
                          step = 1,
                          value = 3
                        ),
                        sliderInput(
                          "S2",
                          label = "Number of Strategies for Player 2:",
                          min = 2,
                          max = 9,
                          step = 1,
                          value = 2
                        ),
                        actionButton(
                          "IEDS",
                          "IEDS"
                        ),
                        actionButton(
                          "BR",
                          "Best Response"
                        ),
                        actionButton(
                          "PureNE",
                          "Pure NE"
                        ),
                        actionButton(
                          "allNE",
                          "All NE"
                        )
                      ),
                      
                      
                      # Show a plot of the generated distribution
                      mainPanel(plotOutput("GameTable"), br(), br(), br(), textOutput("ieds"), br(), textOutput("br"), br(), textOutput("pureNE"), br(), textOutput("allNE")
                      )))
)
