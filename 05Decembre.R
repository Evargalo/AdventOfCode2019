# 5 décembre

library(readr)
X05DecInput <- read_csv("05DecembreInput.txt", 
                        col_names = FALSE)
View(X05DecInput)
install.packages("dplyr")
install.packages("tidyr")
require(dplyr)
require(tidyr)
X05DecInput %>% gather ->input
inputvect<-input$value

# TODO : replace if() by case-when()
process<-function(value){
  input<-1
  output<-c()
  currentIndex<-1
  action<-0
  while (action!=99){
    instruction<-value[currentIndex]
    action<-instruction%%100
    param1<-(instruction%/%100) %%10
    param2<-(instruction%/%1000) %%10
    param3<-(instruction%/%10000) %%10
    
    if (param1==0){
      val1<-value[value[currentIndex+1]+1]
    }
    if (param1==1){
      val1<-value[currentIndex+1]
    }
    if (param2==0){
      val2<-value[value[currentIndex+2]+1]
    }
    if (param2==1){
      val2<-value[currentIndex+2]
    }
    pos<-value[currentIndex+3]+1
    
    print(c(instruction,action,val1,val2,pos,currentIndex)) 
    
    if (action==1) {
      value[pos]<-val1+val2
      currentIndex<-currentIndex+4
    }
    if (action==2){ 
      value[pos]<-val1*val2
      currentIndex<-currentIndex+4
    }
    if (action==3){ 
      pos<-value[currentIndex+1]+1
      value[pos]<-input
      currentIndex<-currentIndex+2
    } 
    if (action==4){
      output<-c(output,val1)
      currentIndex<-currentIndex+2
    }
    if (!action%in%1:4){break}
  }
  list(value,output)
}

values<-c(1002,4,3,4,33)
process(values)

inputvect[1:15]
process(inputvect)

# Part 2

process<-function(value,input){
  output<-c()
  currentIndex<-1
  action<-0
  count<-0
  print(c("instruction","action","val1","val2","pos","currentIndex")) 
  while (action!=99 & count<10){
    # count<-count+1
    instruction<-value[currentIndex]
    action<-instruction%%100
    param1<-(instruction%/%100) %%10
    param2<-(instruction%/%1000) %%10
    param3<-(instruction%/%10000) %%10
    
    if (param1==0){
      val1<-value[value[currentIndex+1]+1]
    }
    if (param1==1){
      val1<-value[currentIndex+1]
    }
    if (param2==0){
      val2<-value[value[currentIndex+2]+1]
    }
    if (param2==1){
      val2<-value[currentIndex+2]
    }
    pos<-value[currentIndex+3]+1
    
    print(c(instruction,action,val1,val2,pos,currentIndex)) 
    
    if (action==1) {
      value[pos]<-val1+val2
      currentIndex<-currentIndex+4
    }
    if (action==2){ 
      value[pos]<-val1*val2
      currentIndex<-currentIndex+4
    }
    if (action==3){ 
      pos<-value[currentIndex+1]+1
      value[pos]<-input
      currentIndex<-currentIndex+2
    } 
    if (action==4){
      output<-c(output,val1)
      currentIndex<-currentIndex+2
    }
    if (action==5){
      currentIndex<-ifelse(val1!=0,val2+1,currentIndex+3)
    }
    if (action==6){
      currentIndex<-ifelse(val1==0,val2+1,currentIndex+3)
    }
    if (action==7){
      value[pos]<-ifelse(val1<val2,1,0)
      currentIndex<-currentIndex+4
    }
    if (action==8){
      value[pos]<-ifelse(val1==val2,1,0)
      currentIndex<-currentIndex+4
    }
    if (!(action%in%1:8)){break}
  }
  list(vecteur=value,output=output)
}

test<-c(3,9,8,9,10,9,4,9,99,-1,8)
test<-c(3,9,7,9,10,9,4,9,99,-1,8)
test<-c(3,3,1108,-1,8,3,4,3,99)
test<-c(3,3,1107,-1,8,3,4,3,99)
test<-c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
process(test,12)
process(inputvect,5)


