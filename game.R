# global parameters, should not be changed 
NUM_SIMULATION <- 10000
UID <- 2017310711
UID_Reverse <- 1170137102
# make the original board object
make_board <- function(){
    X <- 10
    Y <- X
    C1 <- X * Y
    # dictionary object, key is ladder start
    ladders <- c(38,14,31,42,44,67,84,91,100)
    names(ladders) <- c(1,4,9,21,36,51,28,71,80)
    chutess <- c(6,11,26,19,60,53,24,73,75,78)
    names(chutess) <- c(16,49,47,62,64,56,87,92,95,98)
    board <- list(ladders=ladders, chutess=chutess, X=X, Y=Y, C=C1)
    board
}

get_coordinate <- function(X, num){
    # get the center coordinate of the point
    x_offset <- (num - 1)%%X
    y_offset <- (num - 1)%/%X
    y_value <- y_offset + 0.5
    if(y_offset %% 2 == 0)
        x_value <- x_offset + 0.5
    else
        x_value <- X - (x_offset + 0.5)
    c(x_value, y_value)
}

draw_arrow <- function(X, list_obj, color){
    x0_list <- c()
    y0_list <- c()
    x1_list <- c()
    y1_list <- c()
    for(i in names(list_obj)){
        xy0 <- as.numeric(i)
        xy1 <- list_obj[[i]]
        xy0 <- get_coordinate(X, xy0)
        xy1 <- get_coordinate(X, xy1)
        x0_list <- c(x0_list, xy0[[1]])
        y0_list <- c(y0_list, xy0[[2]])
        x1_list <- c(x1_list, xy1[[1]])
        y1_list <- c(y1_list, xy1[[2]])        
    }
    arrows(x0_list, y0_list, x1_list, y1_list, col=color)
}

show_board <- function(board){
    plot.new()
    plot.window(xlim=c(0,board$X), ylim=c(0,board$Y))
    box()
    par(xaxp=c(0,board$X,board$X))
    par(yaxp=c(0,board$Y,board$Y))
    axis(1)
    axis(2,panel.first=grid())
    draw_arrow(board$X, board$ladders, '#FFAC33')
    draw_arrow(board$X, board$chutess, '#FF0000')
}

play_cl <- function(board, n_players=1, spinner){
    inner_spinner <- spinner
    if(length(spinner) == 1)
        inner_spinner <- 1:spinner
    current_pos <- numeric(n_players)
    sixth_pos <- numeric(n_players)
    num_turns <- numeric(n_players)
    chutes <- numeric(n_players)
    ladders <- numeric(n_players)
    game_end <- FALSE
    while(!game_end){ # represent one turn of all players
        for(i in 1:n_players){
            move_step <- sample(inner_spinner, 1)
            num_turns[[i]] <- num_turns[[i]] + 1
            if(current_pos[[i]] + move_step < board$C)
                current_pos[[i]] <- current_pos[[i]] + move_step            
            else if(current_pos[[i]] + move_step == board$C){
                current_pos[[i]] <- board$C
                game_end <- TRUE
                winner <- i
                break
            }
            pos_str <- toString(current_pos[[i]])
            # check the ladders
            if(pos_str %in% names(board$ladders)){
                current_pos[[i]] <- board$ladders[[pos_str]]
                ladders[[i]] <- ladders[[i]] + 1
                if(current_pos[[i]] == board$C){
                    game_end <- TRUE
                    winner <- i
                    break
                }                
            }
            # check the chutes
            else if(pos_str %in% names(board$chutess)){
                current_pos[[i]] <- board$chutess[[pos_str]]
                chutes[[i]] <- chutes[[i]] + 1             
            } 
            # store the 6th turn pos results      
            if(num_turns[[i]] == 6){
                sixth_pos[[i]] <- current_pos[[i]]
            }                                 
        }
    }
    output_list <- list()
    output_list$winner <- winner
    output_list$turns <- num_turns
    output_list$chutes <- chutes
    output_list$ladders <- ladders
    output_list$current_pos <- current_pos
    output_list$sixth_pos <- sixth_pos
    output_list
}

get_minimum_turns <- function(results){
    min_turns <- Inf
    for(i in 1:NUM_SIMULATION){
        min_turns_tmp <- min(results[[i]]$turns)
        if(min_turns_tmp < min_turns)
            min_turns <- min_turns_tmp
    }
    min_turns
}

get_win_proportions <- function(results){
    proportions <- numeric(length(results[[1]]$turns))
    for(i in 1:NUM_SIMULATION){
        winner <- results[[i]]$winner
        proportions[[winner]] <- proportions[[winner]] + 1 
    }
    proportions <- proportions / NUM_SIMULATION
    proportions
}

get_proportions_games_end_in_turns <- function(results, turn_num){
    game_num <- 0
    for(i in 1:NUM_SIMULATION){
        min_turns_tmp <- min(results[[i]]$turns)
        if(min_turns_tmp == turn_num)
            game_num <- game_num + 1
    }
    game_num/NUM_SIMULATION
}

is_capatable_win <- function(board, current_pos, spinner){
    # spinner should be a vector of numeric
    for(i in spinner){
        if(current_pos + i == board$C)
            return(TRUE)
    }
    # check if a ladder exists whose terminal is at C
    has_special_ladder <- FALSE
    for(i in names(board$ladders)){
        if(board$ladders[[i]] == board$C){
            has_special_ladder <- TRUE
            special_ladder_start_pos <- as.numeric(i)
            break
        }
    }
    if(has_special_ladder == FALSE)
        return(FALSE)
    if(current_pos > special_ladder_start_pos)
        return(FALSE)
    for(i in spinner){
        if(current_pos + i == special_ladder_start_pos)
            return(TRUE)
    }
    return(FALSE) 
}

get_close_proportions <- function(board, results, spinner){
    game_num <- 0
    for(i in 1:NUM_SIMULATION){
        for(j in results[[i]]$current_pos){
            if(j == board$C)
                next
            if(is_capatable_win(board, j, spinner)){
                game_num <- game_num + 1
                break
            }
        }
    }
    game_num/NUM_SIMULATION
}

get_maximum_turns <- function(results){
    max_turns <- 0
    for(i in 1:NUM_SIMULATION){
        max_turns_tmp <- min(results[[i]]$turns)
        if(max_turns_tmp > max_turns)
            max_turns <- max_turns_tmp
    }
    max_turns
}

get_most_likely_pos <- function(results){
    n_player <- length(results[[1]]$turns)
    pos <- numeric(n_player)    
    for(i in 1:n_player){
        # collect pos for i-th player
        pos_list <- c()
        for(j in 1:NUM_SIMULATION){
            pos_list <- c(pos_list, results[[j]]$sixth_pos[[i]])
        }        
        pos[[i]] <- as.numeric(names(which.max(table(pos_list))))
    }
    pos
}
get_simulation_results <- function(board, num_player, spinner){
    results <- list()
    for(i in 1:NUM_SIMULATION){
        results[[i]] <- play_cl(board, num_player, spinner)
    }
    output_list <- list()
    # a)
    output_list$min_turns <- get_minimum_turns(results)
    # b)
    output_list$win_proportions <- get_win_proportions(results)
    # c)
    output_list$min_proportions <- get_proportions_games_end_in_turns(results, output_list$min_turns)
    # d)
    output_list$close_proportions <- get_close_proportions(board, results, spinner)
    # e)
    output_list$max_turns <- get_maximum_turns(results)
    # f)
    output_list$sixth_pos <- get_most_likely_pos(results)
    # original simulation results
    output_list$results <- results

    output_list
}

standard_game_1 <- function(board, spinner=6){
    # using list of list to store elements
    set.seed(UID)    
    num_player <- 1
    get_simulation_results(board, num_player, spinner)    
}

standard_game_2 <- function(board, spinner=6){
    # using list of list to store elements
    set.seed(UID_Reverse)    
    num_player <- 2
    get_simulation_results(board, num_player, spinner)    
}

standard_game_3 <- function(board, spinner=6){
    # using list of list to store elements
    set.seed(sample(1e4, 1))    
    num_player <- 3
    get_simulation_results(board, num_player, spinner)    
}

calculate_proportion_less <- function(results, num){
    num <- 0
    for(i in 1:NUM_SIMULATION){
        total_turn <- sum(results[[i]]$turns)
        if(total_turn < num)
            num <- num + 1
    }
    num/NUM_SIMULATION
}

game_routine <- function(board, spinner=6){
    result_1 <- standard_game_1(board, spinner)
    result_2 <- standard_game_2(board, spinner)
    result_3 <- standard_game_3(board, spinner)
    prop <- calculate_proportion_less(result_3$results, result_1$max_turns)
    game_result <- list()
    game_result$result_1 <- result_1
    game_result$result_2 <- result_2
    game_result$result_3 <- result_3
    game_result$prop <- prop
    game_result
}

get_turn_list <- function(results){
    turns <- c()
    for(i in 1:NUM_SIMULATION){
        turns <- c(turns, max(results[[i]]$turns))
    }
    turns
}

plot_distribution_turns <- function(game_result){
    results_1_turn <- get_turn_list(game_result$result_1$results)        
    results_2_turn <- get_turn_list(game_result$result_2$results)        
    results_3_turn <- get_turn_list(game_result$result_3$results)      
    plot.new()  
    plot(table(results_3_turn), type='l', col='#FF8000', xlab='turns', ylab='times')
    lines(table(results_1_turn), type='l', col='#FF0000')
    lines(table(results_2_turn), type='l', col='#0000FF')
    legend('topright', legend=c('one player', 'two players', 'three players'), col=c('#FF0000', '#0000FF', '#FF8000'), lwd=1)
}

is_valid_board <- function(board){
  if('chutess' %in% names(board) == FALSE)
    return(FALSE)
  if(toString(board$C) %in% names(board$chutess))
    return(FALSE)
  return(TRUE)  
}

make_random_board <- function(n_rows=10, n_cols=10, n_chutess=10, n_ladders=9){
  X <- n_rows
  Y <- n_cols
  C1 <- X * Y
  sample_list <- 1:C1
  board <- list(X=X,Y=Y,C=C1)
  while(is_valid_board(board) == FALSE){
    # dictionary object, key is ladder start
    sample_result <- sample(sample_list, 2*(n_chutess + n_ladders))
    chutess <- c()
    for(i in 1:n_chutess){
      start <- sample_result[[2*i-1]]
      end <- sample_result[[2*i]]
      if(start > end){
        start <- sample_result[[2*i]]
        end <- sample_result[[2*i-1]]
      }
      chutess[[i]] <- start
      names(chutess)[[i]] <- end
    }
    ladders <- c()
    sample_result <- sample_result[(2*n_chutess+1):length(sample_result)]
    for(i in 1:n_ladders){
      start <- sample_result[[2*i-1]]
      end <- sample_result[[2*i]]
      if(start > end){
        start <- sample_result[[2*i]]
        end <- sample_result[[2*i-1]]
      }
      ladders[[i]] <- end
      names(ladders)[[i]] <- start
    }
    board$ladders=ladders
    board$chutess=chutess
  }
  board  
}


# system.time(standard_game_routine())
