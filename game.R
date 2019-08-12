# global parameters, should not be changed 
X = 10
Y = X
C = X * Y
# use S3 class to create the board object
# dictionary object, key is ladder start
ladders = c(38,14,31,42,44,67,84,91,100)
names(ladders) = c(1,4,9,21,36,51,28,71,80)
chuntes = c(16)
names(chuntes) = c(6)
board = list(ladders=ladders, chuntes=chuntes)

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
show_board = function(board){
    plot.new()
    plot.window(xlim=c(0,X), ylim=c(0,Y))
    box()
    # draw arrow one by one
}