#########################
# Advent Of Code 2019  #
# 9 Decembre          #
######################

options(digits=18)

############
# Part 1
############

# Import du jeu de données
X09DecInput <- read_csv("09DecInput.txt", 
                        col_names = FALSE)
View(X09DecInput)
X09DecInput %>% gather ->input
input9dec<-input$value

# IntCode computer
process<-function(value,input){
  value<-as.double(value)
  output<-c()
  log<-c()
  currentIndex<-1
  action<-0
  lastInput<-0
  relativeBase<-0
  print(c("instruction","action","val1","val2","pos","currentIndex")) 
  while (action!=99){
    instruction<-value[currentIndex]
    #-----
    # print("--------------------------------------------")
    # print("instruction")
    # print(instruction)
    # print("currentIndex")
    # print(currentIndex)
    # print("relativeBase")
    # print(relativeBase)
    # print("value")
    # print(value)
    # print("output")
    # print(output)
    #-----
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
    if (param1==2){
      val1<-value[value[currentIndex+1]+1+relativeBase]
    }
    if (param2==0){
      val2<-value[value[currentIndex+2]+1]
    }
    if (param2==1){
      val2<-value[currentIndex+2]
    }
    if (param2==2){
      val2<-value[value[currentIndex+2]+1+relativeBase]
    }
    pos<-value[currentIndex+3]+1
    if (param3==2){
      pos<-value[currentIndex+3]+1+relativeBase
    }
    
    # Deals with out-of-bounds values
    val1<-ifelse(is.na(val1),0,val1)
    val2<-ifelse(is.na(val2),0,val2)
    pos<-ifelse(is.na(pos),0,pos)
    
    # print(c(instruction,action,val1,val2,pos,currentIndex)) 
    log<-c(log,action)
    
    if (action==1) {
      value[pos]<-val1+val2
      currentIndex<-currentIndex+4
    }
    if (action==2){ 
      value[pos]<-val1*val2
      currentIndex<-currentIndex+4
    }
    if (action==3){ 
      pos<-ifelse(param1==2,
        value[currentIndex+1]+1+relativeBase,
        value[currentIndex+1]+1)
      lastInput<-lastInput+1
      value[pos]<-input[lastInput]
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
    if (action==9){
      relativeBase<-relativeBase+val1
      currentIndex<-currentIndex+2
    }
    if (!(action%in%1:9)){break}
  }
  # list(vecteur=value,log=log,output=output)
  output
}

test<-c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
test<-c(1102,34915192,34915192,7,4,7,99,0)
test<-c(104,1125899906842624,99)
process(test,c())

process(input9dec,1)

############
# Part 2
############
# 2-3 minutes d'exécution
# 1001 instructions !
process(input9dec,2)
