# global parameters, should not be changed 
X = 10
Y = X
C = X * Y
# use S3 class to create the board object
# dictionary object, key is ladder start
ladders = c(38,14,31,42,44,67,84,91,100)
names(ladders) = c(1,4,9,21,36,51,28,71,80)
chuntes = c(6,11,26,19,60,53,24,73,75,78)
names(chuntes) = c(16,49,47,62,64,56,87,92,95,98)
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
    par(xaxp=c(0,10,10))
    par(yaxp=c(0,10,10))
    axis(1)
    axis(2,panel.first=grid())
    x0_list = c()
    y0_list = c()
    x1_list = c()
    y1_list = c()
    for(i in names(board$ladders)){
        xy0 = as.numeric(i)
        xy1 = ladders[[i]]
        xy0 = get_coordinate(xy0)
        xy1 = get_coordinate(xy1)
        x0_list = c(x0_list, xy0[[1]])
        y0_list = c(y0_list, xy0[[2]])
        x1_list = c(x1_list, xy1[[1]])
        y1_list = c(y1_list, xy1[[2]])        
    }
    arrows(x0_list, y0_list, x1_list, y1_list, col='#FFAC33')
    # draw arrow one by one
}