
#################################################################
###                                                            ##
### 2048 by Broccolito                                         ##
### Graphics are not complete but indicative                   ##    
### Type "End" in terminal to quit at any time during the game ##
### Enjoy!                                                     ##   
###                                                            ##
#################################################################

main = function(){
  
  cat("


██████╗  ██████╗ ██╗  ██╗ █████╗ 
╚════██╗██╔═████╗██║  ██║██╔══██╗
 █████╔╝██║██╔██║███████║╚█████╔╝
██╔═══╝ ████╔╝██║╚════██║██╔══██╗
███████╗╚██████╔╝     ██║╚█████╔╝
╚══════╝ ╚═════╝      ╚═╝ ╚════╝ 
                                 

")
  
  #Installing Graphic Package Raster
  if(!require("raster")){
    install.packages(raster)
  }
  library(raster)
  
  initialize_board = function(){
    board <<- matrix(rep(0,4),4,4)
  }
  update_chessboard = function(){
    chess_board = raster(xmn = 0, xmx = 4,
                         ymn = 0, ymx = 4, 
                         nrows = 4, ncols = 4)
    chess_board[] <- board
    plot(chess_board, xaxt = "n",
         yaxt = "n", ann = FALSE,
         main = "2048")
  }
  
  add_number = function(){
    if(max(board) <= 512){
      board[sample(which(board == 0), 1)] <<- 2
    }else{
      board[sample(which(board == 0), 1)] <<- sample(c(2,4), 1)
    }
  }
  
  go_down = function(){
    for(i in 1:4){
      if(sum(board[,i]) != 0){
        #Combine identical numbers
        if(all(board[4,i] == board[3,i], board[4,i] != 0)){
          board[4,i] <<- board[4,i] + board[3,i]
          board[3,i] <<- 0
        }
        if(all(board[3,i] == board[2,i], board[3,i] != 0)){
          board[3,i] <<- board[3,i] + board[2,i]
          board[2,i] <<- 0
        }
        if(all(board[2,i] == board[1,i], board[2,i] != 0)){
          board[2,i] <<- board[2,i] + board[1,i]
          board[1,i] <<- 0
        }
        #Get rid of all the zeros in between numbers
        suppressWarnings({
          while(!any(
            all(which(board[,i] != 0) == c(4)),
            all(which(board[,i] != 0) == c(3,4)),
            all(which(board[,i] != 0) == c(2,3,4)),
            all(which(board[,i] != 0) == c(1,2,3,4))
          )){
            for(j in 3:1){
              if(board[j+1,i] == 0){
                board[j+1,i] <<- board[j,i]
                board[j,i] <<- 0
              }
            }
          }
        })
      }
    }
  }
  
  go_up = function(){
    board <<- board[ncol(board):1,]
    go_down()
    board <<- board[ncol(board):1,]
  }
  
  go_left = function(){
    board <<- t(board)[ncol(board):1,]
    go_down()
    board <<- t(board[nrow(board):1,])
  }
  
  go_right = function(){
    board <<- t(board[nrow(board):1,])
    go_down()
    board <<- t(board)[ncol(board):1,]
  }
  
  #The game is over if no moves can change the board
  is_over = function(){
    temp_board = board
    go_up()
    if(all(board == temp_board)){
      board <<- temp_board
      return(FALSE)
    }
    board = temp_board
    go_down()
    if(all(board == temp_board)){
      board <<- temp_board
      return(FALSE)
    }
    board = temp_board
    go_left()
    if(all(board == temp_board)){
      board <<- temp_board
      return(FALSE)
    }
    board = temp_board
    go_right()
    if(all(board == temp_board)){
      board <<- temp_board
      return(FALSE)
    }
    board <<- temp_board
    return(TRUE)
  }
  
  #Game starting routine
  initialize_board()
  while(!is_over()){
    add_number()
    update_chessboard()
    instruction = "
    #################
    ##   Up:    W  ##
    ##  Down:   S  ##
    ##  Left:   A  ##
    ##  Right:  D  ##
    #################
    "
    cat(instruction)
    choice = as.character(readline("Please choose a direction to move:  "))
    cat("\n")
    while(!any(choice == c("w", "a", "s", "d", "W", "A", "S", "D","end","End", "END"))){
      cat(instruction)
      choice = as.character(readline("Please choose a direction to move:  "))
      cat("\n")
    }
    if(any(choice == c("W", "w"))){
      go_up()
    }
    if(any(choice == c("S", "s"))){
      go_down()
    }
    if(any(choice == c("A", "a"))){
      go_left()
    }
    if(any(choice == c("D", "d"))){
      go_right()
    }
    if(any(choice == c("end", "End", "END"))){
      cat("\n Game Terminated \n")
      break
    }
    cat("\n Game Terminated \n")
  }
}

#Start the Game when the program is executed
main()

