# global parameters, should not be changed 
X = 10
Y = X
C = X * Y
NUM_SIMULATION = 10000
UID = 2017310711
# use S3 class to create the board object
make_board = function(){
}
# dictionary object, key is ladder start

ladders = c(38,14,31,42,44,67,84,91,100)
names(ladders) = c(1,4,9,21,36,51,28,71,80)
chuntes = c(6,11,26,19,60,53,24,73,75,78)
names(chuntes) = c(16,49,47,62,64,56,87,92,95,98)
board = list(ladders=ladders, chuntes=chuntes, X=X,Y=Y,C=C)

get_coordinate = function(num){
    # get the center coordinate of the point
    x_offset = (num - 1)%%X
    y_offset = (num - 1)%/%X
    y_value = y_offset + 0.5
    if(y_offset %% 2 == 0)
        x_value = x_offset + 0.5
    else
        x_value = 10 - (x_offset + 0.5)
    c(x_value, y_value)
}
draw_arrow = function(list_obj, color){
    x0_list = c()
    y0_list = c()
    x1_list = c()
    y1_list = c()
    for(i in names(list_obj)){
        xy0 = as.numeric(i)
        xy1 = list_obj[[i]]
        xy0 = get_coordinate(xy0)
        xy1 = get_coordinate(xy1)
        x0_list = c(x0_list, xy0[[1]])
        y0_list = c(y0_list, xy0[[2]])
        x1_list = c(x1_list, xy1[[1]])
        y1_list = c(y1_list, xy1[[2]])        
    }
    arrows(x0_list, y0_list, x1_list, y1_list, col=color)
}
show_board = function(board){
    plot.new()
    plot.window(xlim=c(0,X), ylim=c(0,Y))
    box()
    par(xaxp=c(0,10,10))
    par(yaxp=c(0,10,10))
    axis(1)
    axis(2,panel.first=grid())
    draw_arrow(board$ladders, '#FFAC33')
    draw_arrow(board$chuntes, '#FF0000')
}
play_cl = function(n_players=1, spinner){
    inner_spinner = spinner
    if(length(spinner) == 1)
        inner_spinner = 1:spinner
    current_pos = numeric(n_players)
    num_turns = numeric(n_players)
    chutes = numeric(n_players)
    ladders = numeric(n_players)
    game_end = FALSE
    while(!game_end){ # represent one turn of all players
        for(i in 1:n_players){
            move_step = sample(inner_spinner, 1)
            num_turns[[i]] = num_turns[[i]] + 1
            if(current_pos[[i]] + move_step < C)
                current_pos[[i]] = current_pos[[i]] + move_step            
            else if(current_pos[[i]] + move_step == C){
                game_end = TRUE
                winner = i
                break
            }
            pos_str = toString(current_pos[[i]])
            # check the ladders
            if(pos_str %in% names(board$ladders)){
                current_pos[[i]] = board$ladders[[pos_str]]
                ladders[[i]] = ladders[[i]] + 1
                if(current_pos[[i]] == C){
                    game_end = TRUE
                    winner = i
                    break
                }                
            }
            # check the chutes
            else if(pos_str %in% names(board$chuntes)){
                current_pos[[i]] = board$chuntes[[pos_str]]
                chutes[[i]] = chutes[[i]] + 1             
            }                                        
        }
    }
    output_list = list()
    output_list$winner = winner
    output_list$turns = num_turns
    output_list$chutes = chutes
    output_list$ladders = ladders
    output_list$current_pos = current_pos
}
get_minimum_turns = function(results){
    min_turns = Inf
    for(i in 1:NUM_SIMULATION){
        min_turns_tmp = min(results[[i]]$turns)
        if(min_turns_tmp < min_turns)
            min_turns = min_turns_tmp
    }
    min_turns
}
get_win_proportions = function(results){
    proportions = numeric(length(results[[1]]$turns))
    for(i in 1:NUM_SIMULATION){
        winner = results[[i]]$winner
        proportions[[winner]] = proportions[[winner]] + 1 
    }
    proportions = proportions / NUM_SIMULATION
    proportions
}
get_proportions_games_end_in_turns = function(turn_num){
    game_num = 0
    for(i in 1:NUM_SIMULATION){
        min_turns_tmp = min(results[[i]]$turns)
        if(min_turns_tmp == turn_num)
            game_num = game_num + 1
    }
    game_num/NUM_SIMULATION
}
is_capatable_win = function(board, current_pos, spinner){
    # spinner should be a vector of numeric
    for(i in spinner){
        if(current_pos + i == C)
            return(TRUE)
    }
    # check if a ladder exists whose terminal is at C
    has_special_ladder = FALSE
    for(i in names(board$ladders)){
        if(board$ladders[[i]] == C){
            has_special_ladder = TRUE
            special_ladder_start_pos = as.numeric(i)
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
get_close_proportions = function(results, spinner){
    game_num = 0
    for(i in 1:NUM_SIMULATION){
        for(j in results$current_pos){
            if(j == C)
                continue
            if(is_capatable_win(board, j, spinner)){
                game_num = game_num + 1
                break
            }
        }
    }
    game_num/NUM_SIMULATION
}
standard_game_1 = function(){
    # using list of list to store elements
    set.seed(UID)
    results = list()
    spinner = 1:6
    for(i in 1:NUM_SIMULATION){
        results[[i]] = play_cl(1, spinner)
    }
    output_list = list()
    # a)
    output_list$min_turns = get_minimum_turns(results)
    # b)
    output_list$win_proportions = get_win_proportions(results)
    # c)
    output_list$min_proportions = get_proportions_games_end_in_turns(output_list$min_turns)
    # d)
    output_list$close_proportions = get_close_proportions(results, spinner)

    output_list
}
