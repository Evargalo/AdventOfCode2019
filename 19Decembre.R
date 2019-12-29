#########################
# Advent Of Code 2019  #
# 19 Decembre         #
######################

require(dplyr)
require(stringr)
require(tidyr)
require(purrr)
require(purrrlyr)
require(ggformula)

source("intCode.R")

library(readr)
X19DecembreInput <- read_delim("19DecInput.txt", delim= ",",
                               escape_double = FALSE, col_names = FALSE)

(X19DecembreInput %>% gather)$value->inputVect

###########
# Part 1
###########

l<-50
intCode(inputVect,c(0,0))

grid<-data.frame(x=rep(0:l-1,times=l),y=rep(0:l-1,each=l),t=0)

calcBeam<-function(x,y){
  intCode(inputVect,c(x,y))$output
}

calcBeam(7,3)
calcBeam(0,0)

grid %>% rowwise %>%  mutate(t=calcBeam(x,y)) -> grid

sum(grid$t)

# 215

grid %>% gf_tile(y~x,fill=~as.factor(t))
grid %>% gf_tile(y~x,color =~as.factor(t))

###########
# Part 2
###########

xMin<-500
yMin<-500

# 100 per 100
trouve<-FALSE
while(!trouve){
  xMin<-xMin+100
  while(!calcBeam(xMin,yMin)){
    yMin<-yMin+15
  }
  print(c("xMin=",xMin," yMin=",yMin))
  if(calcBeam(xMin,yMin+99)){
    yMin2<-yMin
    while(!calcBeam(xMin+99,yMin2)){
      yMin2<-yMin2+10
    }
    print(c("xMin=",xMin," yMin2=",yMin2))
    trouve<-calcBeam(xMin,yMin2+99)
  }
}

# One solution:
calcBeam(11300,12755)
calcBeam(11300,12854)
calcBeam(11399,12755)

# "xMin="   "8100"    " yMin2=" "9175" 
# [1] "xMin="   "5100"    " yMin2=" "5820"  
calcBeam(5100,5820)
calcBeam(5100,5919)
calcBeam(5199,5820) 
# [1] "xMin="   "900"     " yMin2=" "1120"
calcBeam(900,1120)
calcBeam(900,1219)
calcBeam(999,1120) 

xMin<-500
yMin<-500

# 10 per 10
trouve<-FALSE
while(!trouve){
  xMin<-xMin+10
  while(!calcBeam(xMin,yMin)){
    yMin<-yMin+15
  }
  print(c("xMin=",xMin," yMin=",yMin))
  if(calcBeam(xMin,yMin+99)){
    yMin2<-yMin
    while(!calcBeam(xMin+99,yMin2)){
      yMin2<-yMin2+10
    }
    print(c("xMin=",xMin," yMin2=",yMin2))
    trouve<-calcBeam(xMin,yMin2+99)
  }
}

# [1] "xMin="   "780"     " yMin2=" "985"  

xMin<-750
yMin<-850
calcBeam(750,850)

# 1 by 1
trouve<-FALSE
while(!trouve){
  xMin<-xMin+1
  while(!calcBeam(xMin,yMin)){
    yMin<-yMin+1
  }
  print(c("xMin=",xMin," yMin=",yMin))
  if(calcBeam(xMin,yMin+99)){
    yMin2<-yMin
    while(!calcBeam(xMin+99,yMin2)){
      yMin2<-yMin2+1
    }
    print(c("xMin=",xMin," yMin2=",yMin2))
    trouve<-calcBeam(xMin,yMin2+99)
  }
}

# x=772 ; y=975
