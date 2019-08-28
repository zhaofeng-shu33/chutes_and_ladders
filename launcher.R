# get the results, omit the plotting routine
print_game_result <- function(game_result){
   print(sprintf('a) %f', game_result$min_turns))
   print('b)')
   print(game_result$win_proportions)
   print(sprintf('c) %f', game_result$min_proportions))
   print(sprintf('d) %f', game_result$close_proportions))
   print(sprintf('e) %f', game_result$max_turns))
   print('f)')
   print(game_result$sixth_pos)  
}

print_game_routine_result <- function(game_routine_result){
   print('game 1:')
   print_game_result(game_routine_result$result_1)
   print('game 2:')
   print_game_result(game_routine_result$result_2)
   print('game 3:')
   print_game_result(game_routine_result$result_3)
   print(sprintf('proportion less %f', game_routine_result$prop))
}

source('./game.R')

standard_board <- make_board()
standard_game_results <- game_routine(standard_board)
print('standard game result:')
print_game_routine_result(standard_game_results)

custom_board <- make_random_board(11, 13, 13, 11)
custom_game_results <- game_routine(custom_board, c(1,2,3,5,7,11,13))
print('custom game result:')
print_game_routine_result(custom_game_results)
