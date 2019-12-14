#########################
# Advent Of Code 2019  #
# 13 Decembre         #
######################

require(tidyr)
require(dplyr)
source("intCode.R")

#########
# Part 1
#########

library(readr)
X13DecInput <- read_csv("13DecInput.txt", 
                        col_names = FALSE)

X13DecInput %>% gather ->input
input13dec<-input$value

res<-intCode(input13dec,c())

l<-length(res$output)/3
res$output[3*(1:l)]->tilesTypes
sum(tilesTypes==2)
# 213



#########
# Part 2
#########

# Put a quarter into the arcade game
input13dec[1]<-2
# Initialisation
game<-intCode(input13dec,c())

# Plot the grid
require(ggformula)
show<-function(res){
  score<<-grille %>% filter(x==-1 & y==0)  
  print(score$tile[length(score$tile)])
  grille %>% filter(x!=-1 | y!=0) %>%  
    gf_tile(gformula = (-y)~x,fill=~as.factor(tile)) %>% 
    gf_refine(scale_fill_manual(values = c("white", "black","green","blue","red"),guide=FALSE)) %>%
    gf_theme(theme_void())  #->> graph
  # graph
}

# Send instruction(s)
play<-function(res,moves){
  moves<- ifelse (moves %in% 4:6, moves-5, moves)
  game<<-intCode(
    value = res$vecteur,
    input = moves,
    currentIndex = res$currentIndex,
    relativeBase = res$relativeBase,
    output = res$output
  )
  l<-length(game$output)/3
  game$output[3*(1:l)]->tilesTypes
  game$output[3*(1:l)-2]->x
  game$output[3*(1:l)-1]->y
  grille<<-data.frame(x=x,y=y,tile=tilesTypes)
  score<<-grille %>% filter(x==-1 & y==0)  
  print(score$tile[length(score$tile)])
  if(game$over==99) print("game over!")
}

show(game)
graph
score

# To play with keyboard :
# 4 : left
# 5: stay
# 6 : right

humanPlay<-function(){
  move<-0
  while(game$over==3 & move!=9){
    move<-scan(n = 1)
    if (move==9) break
    if (move %in% 4:6) move <- (move-5)
    play(game,move)
    show(game)
    if(game$over==99) print("game over!")
  }
  score
}
game

# Test
# play(game,4)
# play(game,5)
# play(game,6)
# show(game)


###########################

# Initialisation
game<-intCode(input13dec,c())

l<-length(game$output)/3
game$output[3*(1:l)]->tilesTypes
game$output[3*(1:l)-2]->x
game$output[3*(1:l)-1]->y
grille<-data.frame(x=x,y=y,tile=tilesTypes)

# bottom row : y=22
# Borders: x=0 and x=35
grille %>% filter(tile==1) %>% group_by(x) %>% count %>% arrange(-n)

# Strategy

# Guessing where the ball is aiming to
# If going down: precise calculation of where it reaches row 21
# If going up: just its next absciss
expFall<-function(){
  grille %>% filter(tile==4) %>% filter(row_number()==n()-1)->prevBall
  grille %>% filter(tile==4) %>% filter(row_number()==n())->ball
  ballMove<-ball-prevBall
  if(ballMove$y==-1) {
    expFall<-ball$x + ballMove$x * (21-ball$y)
    if(expFall<1) expFall<-expFall + 2*(1-expFall)
    if(expFall>34) expFall<-expFall - 2*(expFall-34)
  }
  if(ballMove$y==+1) {
    expFall<-ball$x + ballMove$x
  }
  expFall
}
expFall()

# Pick a move : aiming for the expecting absciss of fall
pickMove<-function(){
  (grille %>% filter(tile==3) %>% filter(row_number()==n()))$x->xRaq
  target<-expFall()
  grille %>% filter(tile==4) %>% filter(row_number()==n())->ball
  if(ball$y==21 & ball$x==xRaq) return(5)
  case_when(
    xRaq<target ~ 6,
    xRaq>target ~ 4,
    xRaq==target ~ 5
  )
}
pickMove()

# Let's play now !

# First move
# (by hand in order to initialise global variables)
play(game,5)
show(game)

# Full game
twoPowers<-2^(1:50)
nbMoves<-1
while(game$over != 99){
  play(game,pickMove())
  nbMoves<-nbMoves+1
  # if(nbMoves %in% twoPowers){
  #   readline(prompt="Press [enter] to continue")
  # }
}

# score : 11441
# nbMoves : 5742
show(game)
