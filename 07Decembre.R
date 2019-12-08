#########################
# Advent Of Code 2019  #
# 7 Decembre          #
######################

############
# Part 1
############

# Import du jeu de données
X07DecInput <- read_csv("07DecembreInput.txt", 
                        col_names = FALSE)
View(X07DecInput)
X07DecInput %>% gather ->input
input7dec<-input$value

#############################################################################
# Travail d'un amplificateur à partir d'un programme et d'un vecteur d'input
# Paramètres:
# value : vecteur du programme de l'amplificateur
# input : vecteur des input
#############################################################################
process<-function(value,input){
  output<-c()
  log<-c()
  currentIndex<-1
  action<-0
  count<-0
  lastInput<-0
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
      pos<-value[currentIndex+1]+1
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
    if (!(action%in%1:8)){break}
  }
  list(vecteur=value,output=output,log=log)
  #output
}

#############################################################################
# Travail en série de 5 amplificateurs
# Paramètres:
# inputvect : vecteur du programme identique de chaque amplificateur
# phase : vecteur des phase pour l'initialisation des amplificateurs
#############################################################################
fiveAmp<-function(inputvect,phase){
  
  inputvectTemp<-inputvect
  process(inputvectTemp,c(phase[1],0)) ->amp1
  
  inputvectTemp<-inputvect
  process(inputvectTemp,c(phase[2],amp1$output[length(amp1$output)])) ->amp2
  
  inputvectTemp<-inputvect
  process(inputvectTemp,c(phase[3],amp2$output[length(amp2$output)])) ->amp3
  
  inputvectTemp<-inputvect
  process(inputvectTemp,c(phase[4],amp3$output[length(amp3$output)])) ->amp4
  
  inputvectTemp<-inputvect
  process(inputvectTemp,c(phase[5],amp4$output[length(amp4$output)])) ->amp5
  
  amp5$output[[length(amp5$output)]]
}

# Jeux tests----
fiveAmp(c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0),4:0)

fiveAmp(c(3,23,3,24,1002,24,10,24,1002,23,-1,23,
          101,5,23,23,1,24,23,23,4,23,99,0,0),0:4)

fiveAmp(c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
          1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0),c(1,0,4,3,2))
inputvect<-c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
phase<-c(1,0,4,3,2)

fiveAmp(inputvect,phase)

# Recherche du phasage optimal pour le jeu de données de l'exercice----
require(combinat)
permn(5,fun = function(x) x-1) ->perms
test<-data.frame(perms)
test[1,] %>% gather  %>% mutate(value=0)     -> test         

for(k in 1:120){
  test$value[k]<-fiveAmp(input7dec,perms[[k]])
}

test %>% mutate(value=unlist(value)) %>% arrange(value)
# 914828 


#########
# Part 2
#########

###################################################################
# Fonction proc
# Paramètres:
# signal: transmis d'amplificateur en amplificateur (output en input)
# currentAmp: indice (entre 1 et 5) de l'amplificateur en train de travailler
# amp matrice des programmes de chacun des 5 amplificateurs
# ampCurrentIndex: vecteur des index de la prochaine instruction de chaque amplificateur
# over: booléen signalant qu'on a aboutit à une instruction 99
#
# S'arrête et renvoie une liste comprenant l'output:
# * si on atteint l'instruction 99
# * ou si on atteint l'instruction 3 pour la deuxième fois (attente d'un nouvel input)
###########################################################################################
proc<-function(signal,currentAmp,amp,ampCurrentIndex,over){
  output<-c()
  currentIndex<-ampCurrentIndex[currentAmp]
  value<-amp[currentAmp,]
  action<-0
  nbInput<-0
  while (action!=99 & action!=98){
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
      nbInput<-nbInput+1
      if(nbInput>1) {action<-98}
      else{   value[pos]<-signal
              currentIndex<-currentIndex+2 }
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
  currentIndex->ampCurrentIndex[currentAmp]
  value->amp[currentAmp,]
  if (action==99) over<-TRUE
  list(output=output,amp=amp,ampCurrentIndex=ampCurrentIndex,over=over)
}

# Jeux test----
testVect<-c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
            27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
phase<-9:5

testVect<-c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
            -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
            53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
phase<-c(9,7,8,5,6)
inputVect<-testVect

##########################################################
# Fonction principale fiveAmpLoop
# Paramètres
# inputVect : vecteur du programme pour les amplificateurs
# phase : vecteurs des phases pour l'initialisation
# 
# Sortie: signal émis quand les amplificateurs en terminent
###########################################################
fiveAmpLoop<-function(inputVect,phase){
  amp=matrix(data = rep(inputVect,5),nrow = 5,byrow = TRUE)
  ampCurrentIndex=rep(1,5)
  signal<-0
  over<-FALSE
  for(currentAmp in 1:5){
    proc(phase[currentAmp],currentAmp,amp,ampCurrentIndex,over)->resp
    amp<-resp$amp
    ampCurrentIndex<-resp$ampCurrentIndex
  }
  currentAmp<-1
  while(!over){
    print(c("currentAmp:",currentAmp, "signal:" ,signal))
    proc(signal,currentAmp,amp,ampCurrentIndex,over) ->resp
    amp<-resp$amp
    ampCurrentIndex<-resp$ampCurrentIndex
    resp$output->signal
    resp$over & currentAmp==5->over
    currentAmp<-(currentAmp%%5)+1
  }
  signal
}

fiveAmpLoop(testVect,phase)

# Phasage optimal ----
permn(5,fun = function(x) x+4) ->perms
test<-data.frame(perms)
test[1,] %>% gather  %>% mutate(value=0)     -> test         

for(k in 1:120){
  test$value[k]<-fiveAmpLoop(input7dec,perms[[k]])
}

test %>% mutate(value=unlist(value)) %>% arrange(value)
#17956613
