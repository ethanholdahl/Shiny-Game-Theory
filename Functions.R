makegame = function(S1, S2) {
  #This function creates a 3-dimensional array meant to represent payoffs in a 2 player S1 by S2 normal form game.
  
  #Name strategies
  player1strats = paste("row", 1:S1, sep = "")
  player2strats = paste("col", 1:S2, sep = "")
  players = c("Player 1 Payoffs", "Player 2 Payoffs")
  
  #Randomly generate payoffs, create payoff array (game table).
  payoffs = sample(-10:20, (S1 * S2 * 2), replace = TRUE)
  game = array(
    payoffs,
    dim = c(S1, S2, 2),
    dimnames = list(player1strats, player2strats, players)
  )
  return(game)
}


IEDS = function(game) {
  #This function completes the IEDS proccess using only pure strategies for a given game starting with player 1.
  #The input of the function should be a S1 by S2 by 2 numerical array representing payoffs for 2 players in a normal form game where player 1 has S1 strategies and player 2 has S2 strategies.
  #An output of the makegame function is an intended input for this function
  #The output of this function is a list of the dominated strategies and the strategies they were dominated by.
  
  #Initiate list of dominated strategies and the strategies that dominate them. 777 used as a filler to avoid null indexing errors later on.
  dominated = list(c(777), c(777))
  dominators = list(c(777), c(777))
  
  #extract the number of strategies for each player (S1 and S2) from the game
  S1 = dim(game)[1]
  S2 = dim(game)[2]
  
  #create variable to track if new eliminations are made
  finish = FALSE
  
  #Conditional iteration control flow
  while (finish == FALSE) {
    #Switch finish variable to TRUE (will be reassigned to false if eliminations are made in the current iteration)
    finish = TRUE
    
    #EDS for player 1
    for (s1 in 1:(S1 - 1)) {
      #Go to next strategy if current one already dominated
      if (s1 %in% dominated[[1]]) {
        next
      }
      for (s2 in (s1 + 1):S1) {
        #Go to next strategy if current one already dominated
        if (s2 %in% dominated[[1]]) {
          next
        }
        #check if s1 dominates s2
        if (all(game[s1,-dominated[[2]], 1] > game[s2,-dominated[[2]], 1])) {
          #record domination
          dominated[[1]] = c(dominated[[1]], s2)
          dominators[[1]] = c(dominators[[1]], s1)
          #Set finish to false
          finish = FALSE
        }
        #check if s2 dominates s1
        if (all(game[s1,-dominated[[2]], 1] < game[s2,-dominated[[2]], 1])) {
          #record domination
          dominated[[1]] = c(dominated[[1]], s1)
          dominators[[1]] = c(dominators[[1]], s2)
          #Set finish to false
          finish = FALSE
          #go to new s1 if s1 dominated
          break
        }
      }
    }
    
    #EDS for player 2
    for (s1 in 1:(S2 - 1)) {
      #Go to next strategy if current one already dominated
      if (s1 %in% dominated[[2]]) {
        next
      }
      for (s2 in (s1 + 1):S2) {
        #Go to next strategy if current one already dominated
        if (s2 %in% dominated[[2]]) {
          next
        }
        #check if s1 dominates s2
        if (all(game[-dominated[[1]], s1, 2] > game[-dominated[[1]], s2, 2])) {
          #record domination
          dominated[[2]] = c(dominated[[2]], s2)
          dominators[[2]] = c(dominators[[2]], s1)
          #Set finish to false
          finish = FALSE
        }
        #check if s2 dominates s1
        if (all(game[-dominated[[1]], s1, 2] < game[-dominated[[1]], s2, 2])) {
          #record domination
          dominated[[2]] = c(dominated[[2]], s1)
          dominators[[2]] = c(dominators[[2]], s2)
          #Set finish to false
          finish = FALSE
          #go to new s1 if s1 dominated
          break
        }
      }
    }
    
    #input spacer between elimintions (In case I want to animate the iterated process)
    if (finish == FALSE) {
      dominated[[1]] = c(dominated[[1]], 888)
      dominators[[1]] = c(dominators[[1]], 888)
      dominated[[2]] = c(dominated[[2]], 888)
      dominators[[2]] = c(dominators[[2]], 888)
    }
  }
  returnlist = list(dominated, dominators)
  names(returnlist) = c("dominated", "dominators")
  return(returnlist)
}


BRA = function(game, dominated) {
  #This function preforms best response analysis on the remaining strategies in the game.
  #It's inputs are the game being analyzed and the strategies dominated by IEDS
  #The output of this function are lists of the best response correspondance for each player as well as lists of remaining strategies and lists of eliminated strategies.
  
  #retrieve the list of strategies available to each player
  S1 = 1:dim(game)[1]
  S2 = 1:dim(game)[2]
  
  #create list of eliminated strategies by IEDS
  elim1 = dominated[[1]][dominated[[1]] < 100]
  elim2 = dominated[[2]][dominated[[2]] < 100]
  
  #identify strategies remaining after IEDS
  remain1 = S1[!S1 %in% elim1]
  remain2 = S2[!S2 %in% elim2]
  
  #Player 1 BRs
  #initiate BR list
  p1BRs = list()
  
  for (s in remain2) {
    brval = max(game[remain1, s, 1])
    p1BRs[[s]] = remain1[brval == game[remain1, s, 1]]
  }
  
  #Player 2 BRs
  #initiate BR list
  p2BRs = list()
  
  for (s in remain1) {
    brval = max(game[s, remain2, 2])
    p2BRs[[s]] = remain2[brval == game[s, remain2, 2]]
  }
  returnlist = list(p1BRs, p2BRs, remain1, remain2, elim1, elim2)
  names(returnlist) = c("p1BRs", "p2BRs", "remain1", "remain2", "elim1", "elim2")
  return(returnlist)
}


FindPureNE = function(game, p1BRs, p2BRs, remain2) {
  #This function finds and returns a list of all pure strategy NE in a game
  #Inputs are the original game, best response correspondences, and a list of remaining strategies for player 2.
  
  #create empty list for Nash Eeuilibrias
  NE = list()
  for (s in remain2) {
    for (strats1 in p1BRs[[s]]) {
      for (strats2 in p2BRs[[strats1]]) {
        #Check if best response to the best response to s is indeed s (a Nash Equilibrium).
        if (strats2 == s) {
          #Pure strategy NE reached at strats1, strats2.
          #Create vector form strategy for player 1 and player 2
          s1 = 1:dim(game)[1]
          s2 = 1:dim(game)[2]
          #set other strategies = 0
          s1[s1[!(s1 %in% strats1)]] = 0
          s2[s2[!(s2 %in% strats2)]] = 0
          #set NE pure strat = 1
          s1[strats1] = 1
          s2[strats2] = 1
          #Add to list of NEs
          NE[[length(NE) + 1]] = list(s1, s2)
        }
      }
    }
  }
  return(NE)
}


Find2x2MixedNE = function(game, remain1, remain2) {
  #This function finds and returns a list of the 2x2 mixed strategy Nash Equilibrium from this 2x2 game (if it exists)
  
  #in 2x2: check to make sure no player has a strictly dominant strategy
  if (!all(game[remain1, remain2[1], 2] > game[remain1, remain2[2], 2]) &
      !all(game[remain1, remain2[2], 2] > game[remain1, remain2[1], 2]) &
      !all(game[remain1[1], remain2, 1] > game[remain1[2], remain2, 1]) &
      !all(game[remain1[2], remain2, 1] > game[remain1[1], remain2, 1])) {
    #calculate mix player 2 needs to play to make player 1 indifferent
    remaingame1 = game[remain1, remain2, 1]
    p = (remaingame1[2, 2] - remaingame1[1, 2]) / (remaingame1[1, 1] + remaingame1[2, 2] -
                                                     remaingame1[1, 2] - remaingame1[2, 1])
    
    #calculate mix player 1 needs to play to make player 2 indifferent
    remaingame2 = game[remain1, remain2, 2]
    q = (remaingame2[2, 2] - remaingame2[2, 1]) / (remaingame2[1, 1] + remaingame2[2, 2] -
                                                     remaingame2[1, 2] - remaingame2[2, 1])
    #Mixed strategy NE reached.
    #Create vector form strategy for player 1 and player 2
    s1 = 1:dim(game)[1]
    s2 = 1:dim(game)[2]
    #set other strategies = 0
    s1[s1[!(s1 %in% remain1)]] = 0
    s2[s2[!(s2 %in% remain2)]] = 0
    
    #check for any NaNs (the case where one player is always indifferent) and correct by substituting "P And/Or Q"
    #assign probabilities to remaining pure strategies
    if (is.nan(q)) {
      s1[remain1][1] = "q"
      s1[remain1][2] = "1 - q"
    } else {
      s1[remain1][1] = q
      s1[remain1][2] = 1 - q
    }
    if (is.nan(p)) {
      s2[remain2][1] = "p"
      s2[remain2][2] = "1 - p"
    } else {
      s2[remain2][1] = p
      s2[remain2][2] = 1 - p
    }
    
    #return the mixed NE as a list
    return(list(s1, s2))
  }
}


FindNx2MixedNE = function(game, remain1, remain2, p1BRs, p2BRs) {
  #This function finds and returns a list of the 2x2 mixed strategy Nash Equilibrium from this Nx2 or 2xN game (if it exists)
  #Possible m>2 x 2 mixed strategy Nash equilibria (if they exist) are linear combinations of these equilibria.
  
  #in 2xN games
  MixedNE = list()
  if (length(remain1) == 2 & length(remain2) > 2) {
    #Player 2 with more than 2 strategies remaining.
    n = length(remain2)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (!all(p1BRs[[i]] == p1BRs[[j]]) |
            (length(p1BRs[[i]]) + length(p1BRs[[j]])) == 4) {
          #find the mixed NE in all 2x2 games that don't share the same BR
          Possible = Find2x2MixedNE(game, remain1, c(remain2[i], remain2[j]))
          #Check to see if the mixed NE in the 2x2 game is also a mixed NE in the 2xN game
          PossiblePayoffs = colSums(game[remain1, , 2] * Possible[[1]])
          BR = which(PossiblePayoffs == max(PossiblePayoffs))
          if (remain2[i] %in% BR & remain2[j] %in% BR) {
            #If true, then  the 2x2 NE is a NE in the 2xN game
            MixedNE[[length(MixedNE) + 1]] = Possible
          }
        }
      }
    }
  }
  
  #in Nx2 games
  if (length(remain1) > 2 & length(remain2) == 2) {
    #Player 1 with more than 2 strategies remaining.
    n = length(remain1)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (!all(p2BRs[[i]] == p2BRs[[j]]) |
            (length(p2BRs[[i]]) + length(p2BRs[[j]])) == 4) {
          #find the mixed NE in all 2x2 games that don't share the same BR
          Possible = Find2x2MixedNE(game, c(remain1[i], remain1[j]), remain2)
          #Check to see if the mixed NE in the 2x2 game is also a mixed NE in the Nx2 game
          PossiblePayoffs = rowSums(game[, remain2, 1] * Possible[[2]])
          BR = which(PossiblePayoffs == max(PossiblePayoffs))
          if (remain1[i] %in% BR & remain1[j] %in% BR) {
            #If true, then  the 2x2 NE is a NE in the 2xN game
            MixedNE[[length(MixedNE) + 1]] = Possible
          }
        }
      }
    }
  }
  return(MixedNE)
}

