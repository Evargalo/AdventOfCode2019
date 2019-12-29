#########################
# Advent Of Code 2019  #
# 15 Decembre         #
######################

require(tidyr)
require(dplyr)
require(purrr)
require(ggformula)

library(readr)
X15DecembreInput <- read_delim("15DecInput.txt", 
                               ",", escape_double = FALSE, col_names = FALSE)
X15DecembreInput %>% gather ->input
input15dec<-input$value

source("intCode.R")

#########
# Part 1
#########

# Draw map
map<-data.frame(x=0,y=0,t=0)
# t=0 : wall
# t=1 : empty
# t=2 : goal
# Robot position:
x<-0
y<-0
t<-0
# north (1), south (2), west (3), and east (4)


currentIndex<-0
relativeBase<-0
inputVect<-input15dec

favDirs<-1:4

pick<-function(){
  if(length(favDirs)==0) favDirs<<-1:4
  if(length(favDirs)==1) {dir<-favDirs} else {dir<-sample(favDirs, 1)}
  xx<<-case_when(
    dir==3 ~ x-1,
    dir==4 ~ x+1,
    TRUE ~ x
  )
  yy<<-case_when(
    dir==1 ~ y+1,
    dir==2 ~ y-1,
    TRUE ~ y
  )
  dir
}
history<-data.frame(x=x,y=x,dir=-1,t=-1)
nsteps<-0
#map<-map %>% select(x,y,t)
while(
#  t!=2 & 
  nsteps<100000){
  nsteps<-nsteps+1
  history<-history %>% bind_rows(data.frame(x=x,y=x,dir=dir))
  dir<-pick()
  intCode(value = inputVect,
          input=dir,
          currentIndex = currentIndex, 
          relativeBase = relativeBase, 
          output = c())->res
  t<-res$output
  currentIndex<-res$currentIndex
  relativeBase<-res$relativeBase
  inputVect<-res$vecteur
  
  if (t==0){
    #if(nrow(map %>% filter(x==xx,y==yy,t==0))==0){
    new<-data.frame(x=xx,y=yy,t=0)
    map<-bind_rows(map,new)
    #}
    favDirs<-favDirs[favDirs!=dir]
  }
  if (t==1){
    #if(nrow(map %>% filter(x==xx,y==yy,t==1))==0){
    new<-data.frame(x=xx,y=yy,t=1)
    map<-bind_rows(map,new)
    #}
    x<-xx
    y<-yy
    # favDirs<-case_when(
    #   dir==1 ~ c(1,3,4),
    #   dir==2 ~ c(2,3,4),
    #   dir==3 ~ c(1,2,4),
    #   dir==4 ~ c(1,2,3)
    # )
  }
  if (t==2){
    new<-data.frame(x=xx,y=yy,t=2)
    map<-bind_rows(map,new)
    x<-xx
    y<-yy
  }
  map<-map %>% group_by(x,y) %>% filter(row_number() == n()) %>% ungroup ->map
  # Coups de pouce-----
  #  if(x%in%(3:5) & y==-6) favDirs<-c(4)
  # if(x%in%(-7:-6) & y==10) favDirs<-c(3)
  # if(x==2 & y%in%(-4:4)) favDirs<-c(2)
  # 
  if(x==-4 & y==-12) favDirs<-c(4)
  #-----
}

# Starting point
map$t[map$x==0 & map$y==0]<-3
# Robot
map$t[map$t==5]<-1
map$t[map$x==x & map$y==y]<-5
# Draw map
map %>% 
  gf_tile(y~x,fill = ~ as.factor(t)) %>% 
  gf_theme(theme = theme_void())

list(map=map,x=x,y=y,inputVect=inputVect,relativeBase=relativeBase,currentIndex=currentIndex) -> mapSave1
map<-mapSave1$map
x<-mapSave1$x
y<-mapSave1$y
inputVect<-mapSave1$inputVect
relativeBase<-mapSave1$relativeBase
currentIndex<-mapSave1$currentIndex

# saveRDS(mapSave1,file = "mapMaze15December")

map$dist<-(-1)
map$dist[map$t==3]<-0
table(map$dist)
curDist<-0
#curDist<-59
while(map$dist[map$t==2]==-1 & curDist<400){
  map %>% filter(dist==curDist) %>% select(x,y,dist) -> df
  for(k in  1:nrow(df)){
    x<-df$x[k]
    y<-df$y[k]
    map$dist[map$t %in% c(1,2,5) & map$dist==-1 & 
                (  (map$x==x & map$y==y+1)
                   | (map$x==x & map$y==y-1)
                   | (map$y==y & map$x==x-1)
                   | (map$y==y & map$x==x+1)
                )
              ]<-curDist+1
  }
  curDist<-curDist+1
}
#########
# Part 2
#########


map$oXdist<-(-1)
map$oXdist[map$t==2]<-0
table(map$oXdist)
curDist<-0
#curDist<-59
while(curDist<520){
  map %>% filter(oXdist==curDist) %>% select(x,y) -> df
  for(k in  1:nrow(df)){
    x<-df$x[k]
    y<-df$y[k]
    map$oXdist[map$t %in% c(1,2,5) & map$oXdist==-1 & 
               (  (map$x==x & map$y==y+1)
                  | (map$x==x & map$y==y-1)
                  | (map$y==y & map$x==x-1)
                  | (map$y==y & map$x==x+1)
               )
             ]<-curDist+1
  }
  curDist<-curDist+1
}

#384
