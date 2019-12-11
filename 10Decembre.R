#########################
# Advent Of Code 2019  #
# 10 Decembre         #
######################

############
# Part 1
############
require(FRACTION)
# Import du jeu de données

X10DecInput <- read_csv("10DecInputTest.txt", 
                        col_names = FALSE)
View(X10DecInput)

X10DecInput <- read_csv("10DecInput.txt", 
                        col_names = FALSE)
View(X10DecInput)

Asteroids<-data.frame(x=0,y=0)
n<-nrow(X10DecInput)

# Table des asteroides
# Attention: inversion des x et des y par rapport à l'énoncé.
# Pas malin mais pas grave.
for(k in 1:n){
  line<-unlist(strsplit(X10DecInput$X1[k],split = "")) 
  for (l in 1:n){
    if(line[l]=="#"){
      newline<-data.frame(x=k,y=l)
      Asteroids<-Asteroids %>% bind_rows(newline)
    }
  }
}
Asteroids %>% head
Asteroids %>% filter(row_number()>1) -> Asteroids
# Table des couples d'asteroides
target<-Asteroids
colnames(target)<-c("xt","yt")
Asteroids$key<-1
target$key<-1
couples<-full_join(Asteroids,target,by="key")

# Too long: for each couple of asteroid, check if any other asteroid is in between.
# Pretty slow execution...----

# couples %>% mutate(
#   dx=xt-x,
#   dy=yt-y,
#   visible=TRUE
# ) ->couples

# masks<-function(xO,yO,x,y,xt,yt){
#   if (xO<x & xO<xt | xO>x & xO>xt 
#       | yO<y & yO<yt | yO>y & yO>yt) return(FALSE)
#   if ((xO==x & yO==y) | (xO==xt & yO==yt)) return(FALSE)
#   if ((xO==x & yO==y) | (xO==xt & yO==yt)) return(FALSE)
#   if ((xO==x & xO==xt) | (yO==y & yO==yt)) return(TRUE)
#   if ((xO==x & xO!=xt) | (xO==xt & xO!=x)) return(FALSE)
#   (yt-yO)/(xt-xO)==(yt-y)/(xt-x)
# }
# 
# masks(4,4,1,1,7,7)
# masks(4,4,1,2,7,7)
# masks(4,4,4,2,7,7)
# masks(4,4,4,2,4,7)
# masks(4,4,4,5,4,7)

# visible<-function(k){
#   info<-couples[k,]
#   if(info$dx!=0 & info$dy!=0 & gcd(info$dx,info$dy)==1) return(TRUE)
#   for(j in 1:nrow(Asteroids)){
#     if (masks(Asteroids[j,1],Asteroids[j,2],info[1],info[2],info[4],info[5])) {
#       return(FALSE)
#     }
#   }
#   return(TRUE)
#   # visible1(info[1],info[2],info[4],info[5])
# }

# couples %>% ungroup %>%  mutate(row_=row_number())->couples
# couples %>% rowwise %>%  mutate(visible=visible(row_))-> couples

# -----
# Better approach: list all the possible blocking spots, then check if they are occupied

blockingPositions<-function(xs,ys,xt,yt){
  xs<-as.double(xs)
  xt<-as.double(xt)
  ys<-as.double(ys)
  yt<-as.double(yt)
  # print("-------------------")
  # print(c(xs,xt,ys,yt))
  dx<-xt-xs
  dy<-yt-ys
  pgcd<-abs(gcd(dx,dy))
  if( pgcd!=0 ) {
    stepX<-(dx/pgcd)
    stepY<-(dy/pgcd)
  } 
  x<-xs
  y<-ys
  if(dx==0){ 
    x<-xs
    y<-ys:yt
  }
  if(dy==0){ 
    x<-xs:xt
    y<-ys
  }
  if(dx!=0 & dy!=0 & pgcd!=1){
    x<-seq(from=xs,to=xt,by=stepX)
    y<-seq(from=ys,to=yt,by=stepY)
  }
  result<-data.frame(x=x,y=y)
  result %>% filter(!(x==xs & y==ys) & !(x==xt & y==yt))
}

blockingPositions(3,13,5,21)
blockingPositions(1,2,7,1)

visible1<-function(xs,ys,xt,yt){
  blockingPositions<-blockingPositions(xs,ys,xt,yt)
  blocks<-inner_join(blockingPositions,Asteroids,by=c("x","y"))
  nrow(blocks)==0
}

visible1(2,1,2,6)
visible1(2,4,2,6)

couples %>% rowwise %>% mutate(visible=visible1(x,y,xt,yt))-> couples

couples %>% group_by(x,y) %>% 
  summarise(nb=sum(visible)-1) %>% ungroup -> result

table(result$nb)
result %>% filter(nb==max(nb))

# 263

############
# Part 2
############

stationX<-30
stationY<-24

Asteroids <- Asteroids %>% rowwise() %>% 
  mutate(rotation=ifelse(visible1(stationX,stationY,x,y),1,0))
table(Asteroids$rotation)
# Pas besoin de calculer les rotations ultérieures, 
# le 200è astéroïde sera atteint dans la première rotation

quadrant<-function(x,y){
  case_when(
    x==stationX & y==stationY ~0,
    x<stationX & y==stationY ~1,
    x<stationX & y>stationY ~2,
    x==stationX & y>stationY ~3,
    x>stationX & y>stationY ~4,
    x>stationX & y==stationY ~5,
    x>stationX & y<stationY ~6,
    x==stationX & y<stationY ~7,
    x<stationX & y<stationY ~8,
  )
}
Asteroids <- Asteroids %>% rowwise() %>% 
  mutate(quadrant=quadrant(x,y))

Asteroids %>% group_by(rotation, quadrant) %>% count 
table(Asteroids$quadrant)

slope<-function(x,y){
  (stationX-x)/(stationY-y)
}

Asteroids %>% filter(rotation==1 & quadrant>0) %>%
  group_by(quadrant) %>%
  count %>% 
  ungroup %>% 
  mutate(tot=cumsum(n))
# La solution est atteinte lors de la première rotation, dans le cadrant 8.
# La 100è dans ce cadrant par pente croissante. 
# (On a écarté le cadrant 0 qui est l'astéroide où se trouve le laser)

Asteroids %>% 
  ungroup %>% 
  filter(rotation==1 & quadrant==8) %>% 
  mutate(slope=slope(x,y)) %>% 
  arrange(slope) %>% 
  filter(row_number()==100)
  
# Attention, x et y sont inversés et commencent à 1 au lieu de 0
# 1110
