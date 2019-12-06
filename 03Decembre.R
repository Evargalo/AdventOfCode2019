# 03Decembre
library(readr)
X03DecInput_Wire1 <- read_csv("03DecInput_Wire1.txt", 
                              col_names = FALSE)

X03DecInput_Wire2 <- read_csv("03DecInput_Wire2.txt", 
                              col_names = FALSE)

X03DecInput_Wire1 %>% gather -> wire1
substr(wire1$value,1,1) -> wire1$key
substr(wire1$value,2,nchar(wire1$value)) -> wire1$value
as.numeric(wire1$value)-> wire1$value

X03DecInput_Wire2 %>% gather -> wire2
substr(wire2$value,1,1) -> wire2$key
substr(wire2$value,2,nchar(wire2$value)) -> wire2$value
as.numeric(wire2$value)-> wire2$value

manhattanDist<-function(point) {abs(point$X)+abs(point$Y)}
manhattanDist(data.frame(X=11,Y=-5))

path1<-c()
for (k in 1:301){
  path1<-c(path1,rep(wire1$key[k],wire1$value[k]))
}
path1

path2<-c()
for (k in 1:301){
  path2<-c(path2,rep(wire2$key[k],wire2$value[k]))
}
path2

pathWire1<-data.frame(X=rep(0,length(path1)),Y=rep(0,length(path1)),path=path1)
pathWire2<-data.frame(X=rep(0,length(path2)),Y=rep(0,length(path2)),path=path2)

pathWire1 %>% mutate(dX=((path=="R")-(path=="L"))) -> pathWire1
pathWire1 %>% mutate(dY=((path=="U")-(path=="D"))) -> pathWire1
pathWire1$X<-cumsum(pathWire1$dX)                     
pathWire1$Y<-cumsum(pathWire1$dY)  
pathWire1 %>% mutate(steps1=row_number()) ->pathWire1

pathWire2 %>% mutate(dX=((path=="R")-(path=="L"))) -> pathWire2
pathWire2 %>% mutate(dY=((path=="U")-(path=="D"))) -> pathWire2
pathWire2$X<-cumsum(pathWire2$dX)                     
pathWire2$Y<-cumsum(pathWire2$dY)  
pathWire2 %>% mutate(steps2=row_number()) ->pathWire2

intersects<-inner_join(pathWire1,pathWire2,by=c("X","Y"))
intersects %>% mutate(distOrigine=abs(X)+abs(Y)) %>% 
  arrange(distOrigine)

# Part2
intersects %>% mutate(distOrigine=abs(X)+abs(Y)) %>% 
  mutate(totalSteps=steps1+steps2) %>% 
  arrange(totalSteps)

