# 06decembre

########
# Part 1
########
library(readr)
orbs <- read_delim("06DecInput.txt", 
                   ")", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)
colnames(orbs)<-c("CEN","ORB")
orbs$order<-(-1)
orbs<-bind_rows(orbs,data.frame(CEN="",ORB="COM",order=0))
table(orbs$order)
k=0
while(any(orbs$order==-1)){
  newCenter<-orbs$ORB[orbs$order==k]
  k<-k+1;
  orbs$order[orbs$CEN %in% newCenter]<-k  
}
sum(orbs$order)

########
# Part 2
########

orbs$distToYOU<-(-1)
orbs$distToYOU[orbs$ORB=="YOU"]<-0
table(orbs$distToYOU)
k<-0
while(any(orbs$distToYOU==-1)){
  oldCenter<-orbs$CEN[orbs$distToYOU==k]
  newCenter<-c(orbs$CEN[orbs$ORB %in% oldCenter],
               orbs$ORB[orbs$CEN %in% oldCenter])
  k<-k+1;
  orbs$distToYOU[orbs$distToYOU==-1 & orbs$CEN %in% newCenter ]<-k  
}
orbs$distToYOU[orbs$ORB=="SAN"]

###############################
# Recursive version for part 1
# (longer execution time, but more codegolfy)
###############################
orbs <- read_delim("06DecInput.txt", 
                   ")", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)
colnames(orbs)<-c("CEN","ORB")
orbs<-bind_rows(orbs,data.frame(CEN="",ORB="COM"))

calcOrder<-function(planet){
  if (planet=="COM") return(0)
  cen<-orbs$CEN[orbs$ORB==planet]
  1+calcOrder(cen)
}
orbs %>% rowwise() %>% mutate(order = calcOrder(ORB)) ->orbs
sum(orbs$order)

