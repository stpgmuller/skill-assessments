## MiniProject: Tic-Tac-Toe

# Display the tic-tac-toe board
display_board <- function(board) {
  cat("\n")
  for (i in 1:3) {
    cat(paste(board[i, 1:3], collapse = " | "), "\n")
    if (i < 3) cat("---------\n")
  }
  cat("\n")
}

# Check if a player has won
check_win <- function(board, player) {
  # Check rows
  for (i in 1:3) {
    if (all(board[i, ] == player))
      return(TRUE)
  }
  
  # Check columns
  for (i in 1:3) {
    if (all(board[, i] == player))
      return(TRUE)
  }
  
  # Check diagonals
  if (all(diag(board) == player) || all(diag(board[, 3:1]) == player))
    return(TRUE)
  
  return(FALSE)
}

# Check if the board is full
check_full <- function(board) {
  return(all(board != ""))
}

# Function to make the computer's move
make_computer_move <- function(board, computer) {
  available_moves <- which(board == "")
  move <- sample(available_moves, 1)
  board[move] <- computer
  return(board)
}

# Function to validate and process the player's move
process_player_move <- function(board, player) {
  valid_move <- FALSE
  while (!valid_move) {
    cat("Enter your move (1-9): ")
    move <- as.integer(readLines(con = con, n = 1))
    if (is.na(move) || !is.numeric(move) || !move %in% 1:9) {
      cat("Invalid move. Please enter a number between 1 and 9.\n")
    } else {
      move <- as.integer(move)
      if (board[move] != "") {
        cat("Invalid move. That cell is already occupied.\n")
      } else {
        board[move] <- player
        valid_move <- TRUE
      }
    }
  }
  return(board)
}

# Function to start a new game
play_game <- function() {
  board <- matrix("", nrow = 3, ncol = 3)
  players <- c("X", "O")
  turn <- 1
  
  while (TRUE) {
    display_board(board)
    player <- players[turn]
    
    if (player == "X") {
      board <- process_player_move(board, player)
    } else {
      cat("It's the computer's turn...\n")
      Sys.sleep(1)
      board <- make_computer_move(board, player)
    }
    
    if (check_win(board, player)) {
      display_board(board)
      if (player == "X") {
        cat("Congratulations! You win!\n")
      } else {
        cat("Bad luck, you lost. The computer wins.\n")
      }
      break
    }
    
    if (check_full(board)) {
      display_board(board)
      cat("It's a tie!\n")
      break
    }
    
    turn <- 3 - turn  # Switch players
  }
}

# Determine the input source
if (interactive()) {
  con <- stdin()
} else {
  con <- file("stdin", open = "rt")
}

# Prompt for player's symbol
cat("X or O? ")
symbol <- readLines(con = con, n = 1)

# Start the game
play_game()
