#directions (d)
#   1 for right
#   2 for up
#   3 for left
#   4 for down
# grid is represented with matrix notation (y first and x)
xgrid = 5
ygrid = 5
pathl = 10

grid = array(0,c(ygrid,xgrid,4,pathl))
for (l in  1:2) {
  if (l == 1) { # initializing grid with 1-step counts
    grid[,1:xgrid-1,1,l] = 1
    grid[2:ygrid,,2,l] = 1
    grid[,2:xgrid,3,l] = 1
    grid[1:ygrid-1,,4,l] = 1
  } else {
    for (y in 1:ygrid) {
      for (x in 1:xgrid) {
        if (y>1) {
          grid[y,x,1,l] = grid[y,x+1,1,l-1] + grid[y,x+1,2,l-1] + grid[y,x+1,4,l-1]
          grid[y,x,2,l] = grid[y-1,x,1,l-1] + grid[y-1,x,2,l-1] + grid[y-1,x,3,l-1]
          grid[y,x,4,l] = grid[y+1,x,1,l-1] + grid[y+1,x,3,l-1] + grid[y+1,x,4,l-1]
        }
        if (y<ygrid) 
          movedown = grid[y-1,x,1,l-1] + grid[y-1,x,3,l-1] + grid[y-1,x,4,l-1]
        else
          movedown= 0
        if (x>1)
          moveleft = grid[y,x-1,2,l-1] + grid[y,x-1,3,l-1] + grid[y,x-1,4,l-1]
        else
          moveleft = 0
        if (x<xgrid)
          moveright = grid[y,x-1,1,l-1] + grid[y,x-1,2,l-1] + grid[y,x-1,4,l-1]
        else
          moveright = 0
        grid[y,x,1,l] = moveright + moveup + movedown
        grid[y,x,2,l] = moveright + moveup + moveleft
        grid[y,x,3,1] = moveup + moveleft + movedown
        grid[y,x,4,1] = moveright + moveleft + movedown
      }
    }
  }
}