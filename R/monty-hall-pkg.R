#' @title
#' Create a new Monty Hall Problem game.
#'
#' @description
#' `create_game()` generates a new game setup for the Monty Hall Problem,
#' consisting of three doors with two goats and one car behind them.
#'
#' @details
#' This function simulates the initial setup of the Monty Hall Problem game.
#' It randomly assigns a car to one of the three doors and goats to the other two.
#' This setup is used as the basis for playing through the game scenario.
#'
#' The Monty Hall Problem is based on a game show scenario where a contestant
#' must choose between three doors, behind one of which is a car (the prize)
#' and behind the other two are goats. After the contestant chooses a door,
#' the host (who knows what's behind each door) opens another door to reveal
#' a goat, and then offers the contestant the chance to switch their choice
#' to the remaining unopened door.
#'
#' @return
#' A character vector of length 3, where each element represents the contents
#' behind each door. The vector will contain two "goat" elements and one "car" element,
#' randomly ordered.
#'
#' @examples
#' # Generate a new game setup
#' create_game()
#'
#' # Generate multiple game setups
#' replicate(5, create_game())
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}

#' @title
#' Select a door in the Monty Hall game
#'
#' @description
#' `select_door()` simulates the contestant's initial door selection in the Monty Hall Problem.
#'
#' @details
#' This function randomly selects one of the three doors, mimicking a contestant's
#' initial choice in the Monty Hall Problem. The selection is made without any
#' knowledge of what's behind the doors, simulating the blind choice a real
#' contestant would make.
#'
#' @return
#' An integer between 1 and 3, representing the selected door number.
#'
#' @examples
#' # Select a door
#' select_door()
#'
#' # Make multiple selections
#' replicate(5, select_door())
#'
#' @export
select_door <- function()
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

#' @title
#' Open a goat door in the Monty Hall game
#'
#' @description
#' `open_goat_door()` simulates the host opening a door to reveal a goat in the Monty Hall Problem.
#'
#' @details
#' This function mimics the action of the game show host, who always opens a door
#' that contains a goat and is not the door chosen by the contestant. If the
#' contestant initially chose a door with a goat, the host will open the other
#' door with a goat. If the contestant chose the door with the car, the host
#' randomly selects one of the two goat doors to open.
#'
#' @param game A character vector of length 3 representing the current game state,
#'   as created by the `create_game()` function.
#' @param a.pick An integer between 1 and 3 representing the contestant's initial choice,
#'   as returned by the `select_door()` function.
#'
#' @return
#' An integer between 1 and 3, representing the door number opened by the host.
#'
#' @examples
#' # Create a game and make a selection
#' game <- create_game()
#' pick <- select_door()
#' 
#' # Open a goat door
#' open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function(game, a.pick)
{
  doors <- c(1,2,3)
  if(game[a.pick] == "car")
  { 
    goat.doors <- doors[game != "car"] 
    opened.door <- sample(goat.doors, size=1)
  }
  if(game[a.pick] == "goat")
  { 
    opened.door <- doors[game != "car" & doors != a.pick] 
  }
  return(opened.door)
}

#' @title
#' Change door selection in the Monty Hall game
#'
#' @description
#' `change_door()` determines the final door selection based on the contestant's decision to stay or switch.
#'
#' @details
#' This function simulates the contestant's final decision in the Monty Hall Problem.
#' After the host opens a door revealing a goat, the contestant can either stick with
#' their original choice (stay) or switch to the other unopened door. This function
#' determines which door is the final selection based on this decision.
#'
#' @param stay A logical value. If TRUE, the contestant stays with their initial pick.
#'   If FALSE, they switch to the other unopened door.
#' @param opened.door An integer between 1 and 3 representing the door opened by the host,
#'   as returned by the `open_goat_door()` function.
#' @param a.pick An integer between 1 and 3 representing the contestant's initial choice,
#'   as returned by the `select_door()` function.
#'
#' @return
#' An integer between 1 and 3, representing the final door selection.
#'
#' @examples
#' # Simulate staying with the initial choice
#' change_door(stay = TRUE, opened.door = 2, a.pick = 1)
#'
#' # Simulate switching to the other unopened door
#' change_door(stay = FALSE, opened.door = 2, a.pick = 1)
#'
#' @export
change_door <- function(stay=T, opened.door, a.pick)
{
  doors <- c(1,2,3) 
  if(stay)
  {
    final.pick <- a.pick
  }
  if(!stay)
  {
    final.pick <- doors[doors != opened.door & doors != a.pick] 
  }
  return(final.pick)
}

#' @title
#' Determine the winner in the Monty Hall game
#'
#' @description
#' `determine_winner()` checks if the final door selection results in a win or loss.
#'
#' @details
#' This function compares the final door selection with the game setup to determine
#' if the contestant has won (selected the door with the car) or lost (selected a
#' door with a goat). It represents the final step in the Monty Hall Problem simulation.
#'
#' @param final.pick An integer between 1 and 3 representing the final door selection,
#'   as returned by the `change_door()` function.
#' @param game A character vector of length 3 representing the game state,
#'   as created by the `create_game()` function.
#'
#' @return
#' A character string, either "WIN" if the final pick reveals a car, or "LOSE" if it reveals a goat.
#'
#' @examples
#' # Create a game
#' game <- create_game()
#' 
#' # Determine outcome for different picks
#' determine_winner(1, game)
#' determine_winner(2, game)
#' determine_winner(3, game)
#'
#' @export
determine_winner <- function(final.pick, game)
{
  if(game[final.pick] == "car")
  {
    return("WIN")
  }
  if(game[final.pick] == "goat")
  {
    return("LOSE")
  }
}

#' @title
#' Play one complete Monty Hall game
#'
#' @description
#' `play_game()` simulates one complete game of the Monty Hall problem,
#' including both stay and switch strategies.
#'
#' @details
#' This function orchestrates a complete play-through of the Monty Hall Problem.
#' It creates a new game setup, simulates the contestant's initial door selection,
#' the host opening a goat door, and then determines the outcome for both strategies:
#' staying with the initial selection and switching to the other unopened door.
#' This allows for a direct comparison of the two strategies in a single game instance.
#'
#' @return
#' A data frame with two rows and two columns:
#' - 'strategy': a character vector with values "stay" and "switch"
#' - 'outcome': a character vector with the corresponding game outcomes ("WIN" or "LOSE")
#'
#' @examples
#' # Play one game
#' play_game()
#'
#' # Play multiple games and view results
#' results <- replicate(100, play_game(), simplify = FALSE)
#' do.call(rbind, results)
#'
#' @export
play_game <- function()
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)
  
  final.pick.stay <- change_door(stay=T, opened.door, first.pick)
  final.pick.switch <- change_door(stay=F, opened.door, first.pick)
  
  outcome.stay <- determine_winner(final.pick.stay, new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame(strategy, outcome, stringsAsFactors=F)
  return(game.results)
}

#' @title
#' Play multiple Monty Hall games
#'
#' @description
#' `play_n_games()` simulates multiple games of the Monty Hall problem
#' and returns the aggregate results.
#'
#' @details
#' This function runs a specified number of Monty Hall game simulations,
#' collects the results, and provides a summary of the outcomes for both
#' the stay and switch strategies. It's useful for demonstrating the long-term
#' probabilities of winning with each strategy.
#'
#' @param n An integer specifying the number of games to simulate. Default is 100.
#'
#' @return
#' A data frame containing the results of all simulated games. Each row represents
#' one game strategy (stay or switch) for one simulation. The function also prints
#' a table showing the proportion of wins for each strategy.
#'
#' @examples
#' # Run 1000 game simulations
#' results <- play_n_games(1000)
#' 
#' # View the first few results
#' head(results)
#'
#' # Calculate overall win percentages
#' table(results$strategy, results$outcome) / nrow(results) * 100
#'
#' @export
play_n_games <- function(n=100)
{
  library(dplyr)
  results.list <- list()
  loop.count <- 1
  
  for(i in 1:n)
  {
    game.outcome <- play_game()
    results.list[[loop.count]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows(results.list)
  
  table(results.df) %>% 
    prop.table(margin=1) %>%
    round(2) %>% 
    print()
  
  return(results.df)
}