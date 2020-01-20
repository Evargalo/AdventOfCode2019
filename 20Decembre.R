options(max.print = 40)

require(dplyr)
require(stringr)
require(tidyr)
require(purrr)
require(purrrlyr)
require(ggformula)


library(readr)
input <- read_csv("20DecInputDoubleDoors.txt", 
                  col_names = FALSE, col_types = cols(X1 = col_character()))

input[30:40,]

Maze<-data.frame(x=c(),y=c(),t=c(),stringsAsFactors = FALSE)

n<-nrow(input)
p<-nchar(input[1,])


innerDoors <- c("(",toupper(letters))
outerDoors <- c(")",letters)
doors <- c(innerDoors,outerDoors)
endpoints <- c("@", "$")
doorsAndEndpoints <- c(doors, "@", "$")


# Data
for(k in (1:n)){
  line<-input[k,]
  for (l in 1:p){
    newline<-data.frame(x=l,y=k,t=substr(line,l,l),stringsAsFactors = FALSE)
    Maze<-Maze %>% bind_rows(newline)
  }
}

Maze<-Maze %>% filter (t!=" ")

table(Maze$t)

# Get rid of dead-ends
Maze %>% summarise(max(x))
Maze %>% summarise(max(y))
nbDiesesAside<-function(xx,yy){
  Maze %>% filter(abs(xx-x)+abs(yy-y)==1 & t=='#') %>% count %>% unlist
}
Maze %>% rowwise %>% mutate(nbD=nbDiesesAside(x,y)) %>% ungroup %>% group_by(nbD) %>% count
prevDiese<-0
curDiese<-Maze %>% filter(t=='#') %>% count %>% unlist
while(curDiese>prevDiese){
  Maze %>% rowwise %>% mutate(t=case_when(
    x==1 | y==1 | x==105 | y==101 | t=='#' ~ t,
    nbDiesesAside(x,y) > 2 & t=='.' ~ '#',
    TRUE ~ t)) %>% ungroup -> Maze
  prevDiese<-curDiese
  curDiese<-Maze %>% filter(t=='#') %>% count %>% unlist
}
Maze<-Maze %>% filter (t != "#" & t!=" ")

saveRDS(Maze,"Maze20decembre.RDS")

Maze %>% rowwise %>% mutate(tt=case_when(t %in% innerDoors ~ 1,
                                         t %in% outerDoors ~ 2,
                                         TRUE ~3)) %>%  ungroup %>% group_by(tt) %>% count


Maze %>% rowwise %>% mutate(tt=case_when(t %in% innerDoors ~ 1,
                                         t %in% outerDoors ~ 2,
                                         TRUE ~3)) %>%  ungroup %>%
  gf_tile(y~x,fill=(~as.factor(tt))) %>% gf_theme(theme_void())

# Segments

segments<-data.frame(d1=c(),d2=c(),length=c(),deltaLevel=c())

countDist<-function(x,y,t,k,dist){
  x->xx;  y->yy; t->tt; dist->dd
  copyMaze<<-copyMaze %>% mutate(dist=ifelse(abs(xx-x)+abs(yy-y)==1 & dist==-1,dd+1,dist))
}

addSegments<-function(start,issues){
  newseg<-data.frame(d1=start,d2=issues$t,length=issues$dist,deltaLevel=(issues$t %in% innerDoors - issues$t %in% outerDoors))
  segments<<-segments %>% bind_rows(newseg)
}

for (start in doorsAndEndpoints){
  copyMaze<<-Maze
  curDist<-0
  copyMaze<<-copyMaze %>% mutate(dist=ifelse(t==start,curDist,-1))
  newlines<-copyMaze %>% filter(dist==curDist)
  while(newlines %>% nrow >0){
    curDist<-curDist+1
    pmap(newlines,countDist)
    newlines<-copyMaze %>% filter(dist==curDist)
  }
  issues<-copyMaze %>% filter(t %in% doorsAndEndpoints & dist>0)
  addSegments(start,issues)
}

segments %>% filter(d2=="(" | d1 == ")")

segments %>% group_by(deltaLevel) %>% count
segments %>% group_by(d2) %>% count %>% arrange(-n)

segments %>% mutate(deltaLevel=case_when(
  d2 %in% innerDoors ~ 1,
  d2 %in% outerDoors ~ -1,
  d2 %in% c("$","@") ~ 0
)) -> segments


require(devtools)
# install_github(repo="skranz/dplyrExtras")
require(dplyrExtras)

paths<-segments %>% filter(d1!="$" & d2 !="@")
toUpper<-function(char){
  case_when(
    char %in% letters ~ toupper(char),
    char==")" ~ "(",
    TRUE ~ char
  )
}
toLower<-function(char){
  case_when(
    char %in% LETTERS ~ tolower(char),
    char=="(" ~ ")",
    TRUE ~ char
  )
}
passDoor<-function(char){
  case_when(
    char %in% innerDoors ~ toLower(char),
    char %in% outerDoors ~ toUpper(char),
    TRUE ~ char
  )
}

gluPaths<-function(d1,d2,length,deltaLevel){
  d1->dd1; d2->dd2; length->ll; deltaLevel->dL
  paths %>% mutate_rows(d2==passDoor(dd1) & d1!=dd2,
                        length=length+ll+1,deltaLevel=deltaLevel+dL) %>% 
    mutate_rows(d2==passDoor(dd1) & d1!=dd2,
                d2=dd2) ->> paths
}

paths %>% group_by(d1) %>% count %>% as.data.frame() -> d1Counts
uniqueD1<-d1Counts %>% filter(n==1 & d1!="@") %>% select(d1)
uniqueD1<-uniqueD1$d1
k<-0
while(length(uniqueD1)>0 & k<30){
  pmap(paths %>% filter(d1 %in% uniqueD1), gluPaths)
  paths %>% filter(d1 %in% c(passDoor(paths$d2),"@"))->paths
  uniqueD1<-paths %>% group_by(d1) %>% count %>% as.data.frame() %>% filter(n==1 & d1!="@") %>% select(d1)
  uniqueD1<-uniqueD1$d1
  print(k)
  print(length(uniqueD1))
  print(nrow(paths))
  k<-k+1
}


paths %>% group_by(deltaLevel) %>% count


buildPaths<-function(d1,d2,length,deltaLevel){
  d1->dd1; d2->dd2; length->ll; deltaLevel->dL
  if(dd2 == "$") return(data.frame(d1=dd1,d2=dd2,length=ll,deltaLevel=dL))
  paths %>% filter(d1==passDoor(dd2)) %>% 
    mutate(length=length+ll+1,
           deltaLevel=deltaLevel+dL,
           d1=paste0(dd1,dd2,d1)) -> newPaths
  # ifelse(nrow(newPaths)>0,
  #        newPaths,
  #        data.frame(d1=c(),d2=c(),length=c(),deltaLevel=c())
  #        )
  newPaths
}


longPaths<-paths %>% filter(d1=="@")

k<-0
while(nrow(longPaths %>% filter(d2=="$" & deltaLevel==0))==0 & k<20){
  longPaths<-longPaths %>% filter(deltaLevel>=0)
  longPaths<-pmap_df(longPaths,buildPaths)
  k<-k+1
  print(k)
  print(nrow(longPaths))
}

# buildPaths("@","T",116,3)

longPaths %>% filter(d2=="$" & deltaLevel==0) %>% arrange(length)
# 5094: faux...
# 5214: Juste !
