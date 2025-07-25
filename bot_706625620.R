# Random bot
# Your bot must be a function with a single argument called board.
# You may NOT use any additional arguments. In particular, if your bot needs to
# know whether it is player one or player 2 it has to figure it out itself based
# on the board.
# Arguments: 
#   board: a 6x7 matrix containing only integers 0, 1, or 2
#    0 means a field is not taken
#    1 means this is player 1s piece
#    2 means this is player 2s piece
# Return: A single integer between 1 and 7. The column that you want to drop 
# your piece into.
bot_706625620 <- function(board) {
  # paste any helper functions like check_winner() here
  # then write your actual code
  # make sure the output is a single integer between 1 and 7
  
  # initialize_board <- function() {}
  
  # display_board <- function(board) {}
  
  # check_winner <- function(board, player) {}
  
  # helper function: check player number 1 or 2
  get_player_num <- function(board) {
    count_1 <- sum(board == 1)
    count_2 <- sum(board == 2)
    if (count_1 == count_2)
      return(1)
    return(2)
  }
  
  #
  win_move <- function(board, player) {
    for (i in 1:6) {
      for (j in 1:7) {
        if (board[i, j] != player) {
          next
        }
        # vertical connect
        if (i < 5 && i != 1 && sum(board[i:(i + 2), j] == player) == 3 && board[i - 1, j] == 0) {
          return(j)
        }
        # horizontal connect
        if (j < 5 && sum(board[i, j:(j + 3)] == player) == 3 && sum(board[i, j:(j + 3)] == 0) == 1) {
          winning_pt <- j - 1 + which(board[i, j:(j + 3)] == 0)
          if (i > 1 || board[i - 1, winning_pt] != 0)
            return(winning_pt)
        }
      }
    }
    return(0)
  }
  
  # attempt to block opponent from winning
  block <- function(board, player, opp) {
    for (i in 1:6) {
      for (j in 1:7) {
        if (board[i, j] != opp) {
          next
        }
        # block vertical connect
        if (i < 5 && i != 1 && sum(board[i:(i + 2), j] == opp) == 3 && board[i - 1, j] == 0) {
            return(j)
        }
        # block horizontal connect
        if (j < 5 && sum(board[i, j:(j + 3)] == opp) == 3 && sum(board[i, j:(j + 3)] == 0) == 1) {
          winning_pt <- j - 1 + which(board[i, j:(j + 3)] == 0)
          if (i > 1 || board[i - 1, winning_pt] != 0)
            return(winning_pt)
        }
          
        # else if (j < 5 && i < 4 &&
        #          board[i + 1, j + 1] == opp &&
        #          board[i + 2, j + 2] == opp &&
        #          board[i + 3, j + 3] == opp)
        #   return()
        # else if (j > 3 && i < 4 &&
        #          (board[i + 1, j - 1] == opp &&
        #          board[i + 2, j - 2] == opp &&
        #          board[i + 3, j - 3] == opp))
        #   return()
      }
    }
    return(0)
  }
  
  # get player and opponent's numbers
  player <- get_player_num(board)
  
  if (player == 1) {
    opp <- 2
  } else {
    opp <- 1
  }
  
  # take the center at the start of the game
  if (sum(board == 0) >= 41)
    return(3)
  
  # Attempt to block opponent
  col <- block(board, player, opp)
  
  if (col != 0)
    return(col)
  
  col <- win_move(board, player)
  if (col != 0)
    return(col)
  
  return(sample(1:7, size = 1))
  # display_board(board)
  
}

# DO NOT MODIFY
# Initialize the board
# the board is a 6x7 matrix containing only integers 0, 1, or 2
#   0 means a field is not taken
#   1 means this is player 1s piece
#   2 means this is player 2s piece
# the board starts out empty (all 0s)
initialize_board <- function() {
  matrix(0, nrow = 6, ncol = 7)
}

# DO NOT MODIFY
# Display the board in the console
# Argument:
#   board: a 6x7 matrix containing integers 0, 1, 2
# Return:
#  The same board
display_board <- function(board) {
  # feel free to add a nice plot here
  # this is totally optional and you will receive no credit for it
  print(board)
}

# DO NOT MODIFY
# Drop a piece into a column
# this is a function with arguments:
#   board: a 6x7 matrix containing integers 0, 1, 2
#   column: an integer between 1 and 7, the column that a piece is dropped into
#   player: integer 1 or 2, the player that dropped the piece
# this function returns the board with correct piece (1 or 2) dropped down into
# the correct column all the way down to the correct row.
# Notice that the columns fills up from row 6, then row 5, ..., and are full when 
# row 1 contains a piece.
# Return: the updated board
drop_piece <- function(board, column, player) {
  if (!(column %in% 1:7)) stop("column needs to be between 1 and 7.")
  # start checking in row 6, then row 5, ..., until row 1
  for (row in nrow(board):1) {
    # if this slot is still available
    if (board[row, column] == 0) {
      # fill it with the appropriate piece (1 or 2)
      board[row, column] <- player
      # output the updated board
      return(board)
    }
  }
  # throw an error message if the column is full already
  stop("Column is full!")
}

## MODIFY THIS IF YOU WANT TO
# Check for win condition
# Does a player have four in a row on the board? Check in vertical, horizontal,
# diagonal up, and diagonal down directions.
# arguments:
# board: a 6x7 matrix containing integers 0, 1, 2
# player: integer 1 or 2, the player for which to check if they won
# Return: logical (TRUE or FALSE) does this player have 4 in a row on the board?
check_winner <- function(board, player) {
  # code redacted, implement it yourself if you want to use it
  # this function has to return a single logical
  for (i in 1:6) {
    for (j in 1:7) {
      if (board[i, j] != player) {
        next
      }
      if (i < 4 && all(board[(i + 1):(i+3), j] == player)) {
        return(TRUE)
      }
      else if (j < 5 && all(board[i, (j + 1):(j+3)] == player))
        return(TRUE)
      else if (j < 5 && i < 4 && 
               board[i + 1, j + 1] == player &&
               board[i + 2, j + 2] == player &&
               board[i + 3, j + 3] == player)
        return(TRUE)
      else if (j > 3 && i < 4 &&
               board[i + 1, j - 1] == player &&
               board[i + 2, j - 2] == player &&
               board[i + 3, j - 3] == player)
        return(TRUE)
    }
  }
  return(FALSE)
}

# Main game loop
# This function implements a Connect Four game where a human player competes against a bot.
# Arguments:
# player_1_human: Logical (TRUE/FALSE). If TRUE, the human player is Player 1.
# Otherwise, the bot is Player 1.
# Returns:
# A list containing the winner of the game ("Human", "Bot", or "Draw!") and the
# total number of turns played.
play_connect_four <- function(player_1_human = TRUE) {
  board <- initialize_board() # Initialize the empty game board
  player_1_turn <- TRUE # Track whose turn it is (Player 1 starts)
  winner <- "undecided" # Track the game status
  n_turns <- 0 # Count the number of turns
  # play until someone wins or the board is filled up resulting in a draw
  while (winner == "undecided") {
    display_board(board) # Display the current board state
    n_turns <- n_turns + 1 # Increment the turn counter
    # Determine the current player
    if ((player_1_human & n_turns %% 2 == 1) |
        (!player_1_human & n_turns %% 2 == 0)) {
      current_player <- "Human"
    } else {
      current_player <- "bot"
    }
    # and what piece they are using
    if (player_1_turn) {
      piece <- 1
    } else {
      piece <- 2
    }
    if (current_player == "Human") {
      # human's turn
      cat("Your turn player ", piece, "! Choose a column (1-7): ")
      column <- -1
      # make sure the human player selects a valid column
      while (column < 1 || column > 7 || board[1, column] != 0) {
        column <- as.integer(readline())
        if (column < 1 || column > 7 || board[1, column] != 0) {
          cat("Invalid column. Try again.\n")
        }
      }
      board <- drop_piece(board = board, column = column, player = piece)
      if (check_winner(board = board, player = piece)) {
        display_board(board)
        cat("Congratulations! You win!\n")
        winner <- "Human"
      }
    } else {
      # bot's turn
      cat("Bot (Player ", piece,") is thinking...\n")
      Sys.sleep(1)
      # LATER bot_minimax() WILL GO THERE
      column <- bot_706625620(board) # YOUR UID NUMBER GOES HERE
      board <- drop_piece(board = board, column = column, player = piece)
      if (check_winner(board = board, player = piece)) {
        display_board(board)
        cat("Bot (Player ", piece,") wins! Better luck next time.\n")
        winner <- "Bot"
      }
    }
    # Check for draw
    if (!any(board == 0)) {
      display_board(board)
      cat("It's a draw!\n")
      winner <- "Draw!"
    }
    # Alternate turns
    player_1_turn <- !player_1_turn
  }
  # Return the game results
  list(winner = winner,
       n_turns = n_turns)
}
play_connect_four(player_1_human = TRUE)
