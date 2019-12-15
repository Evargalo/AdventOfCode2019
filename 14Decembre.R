#########################
# Advent Of Code 2019  #
# 14 Decembre         #
######################

require(tidyr)
require(dplyr)
require(purrr)
options(digits=20)

library(readr)
X14DecembreInput <- read_delim("14DecembreInput.txt", 
                               ";", escape_double = FALSE, col_names = FALSE)
X14DecembreInputTest <- read_delim("14DecembreInputTest.txt", 
                                   ";", escape_double = FALSE, col_names = FALSE)

# strsplit(X14DecembreInputTest$X1,"=>") ->input
strsplit(X14DecembreInput$X1,"=>") ->input
length(input)
input %>% unlist -> vect
vect[2*(1:length(input))]->product
strsplit(product," ")->product2
(product2 %>% unlist) [3*(1:length(input))] ->prodType
(product2 %>% unlist) [3*(1:length(input))-1] ->prodQtt
vect[2*(1:length(input))-1]->ingr

reactions<-data.frame(ingr,prodType,prodQtt,stringsAsFactors = FALSE)
reactions %>% mutate(prodQtt=as.integer(prodQtt)) -> reactions

# Only one way to get each kind of output
reactions %>% group_by(prodType) %>% count %>% arrange(-n)
reactions %>% filter(prodType=="FUEL")

calcNeeds<-function(prodQtt=1,prodType="FUEL"){
  need<-prodType
  
  nbProd<-(reactions %>% filter(prodType==need))$prodQtt
  nbReact<-prodQtt %/% nbProd+((prodQtt %% nbProd) != 0)
  
  ingr<-strsplit((reactions %>% filter(prodType==need))$ingr,",")[[1]]
  nbIngr<-length(ingr)
  ingr<-strsplit(ingr," ")  %>% unlist
  ingr<-ingr[ingr!=""]
  prodType<-ingr[2*(1:nbIngr)]
  prodQtt<-nbReact*as.integer(ingr[2*(1:nbIngr)-1])
  data.frame(prodQtt,prodType,stringsAsFactors = FALSE) 
  
}

calcNeeds(1,"FUEL")

calcLevel<-function(ingr,prodType,prodQtt){
  calcNeeds(1,prodType)$prodType->ingrVect
  if(all(ingrVect %in% infoProducts$prodType)){
    level<-1+max((infoProducts$prodLevel)[infoProducts$prodType %in% ingrVect])
    return(data.frame(prodLevel=level,prodType=prodType,stringsAsFactors = FALSE))
  }
  return(data.frame(prodLevel=c(),prodType=c(),stringsAsFactors = FALSE))
}
#pmap_df(reactions %>% filter(prodType=="PSHF"),calcLevel)

infoProducts<-data.frame(prodLevel=0,prodType="ORE",stringsAsFactors = FALSE) 

while(nrow(infoProducts)<=nrow(reactions)){
  pmap_df(reactions %>% filter(!(prodType %in% infoProducts$prodType)),calcLevel) %>% 
    bind_rows(infoProducts ) -> infoProducts
}

#########
# Part 1
#########

needs<-data.frame(prodQtt=1,prodType="FUEL",stringsAsFactors = FALSE)
nsteps<-0
while(any(needs$prodType!="ORE") & nsteps<100){
  prodList<-(infoProducts %>% 
               filter(prodType %in% needs$prodType) %>% 
               filter(prodLevel==max(prodLevel)))$prodType
  pmap_df(needs %>% filter(prodType%in%prodList),calcNeeds) %>% 
    bind_rows(needs %>% filter(!(prodType%in%prodList))) -> needs
  needs %>% group_by(prodType) %>% summarise(prodQtt=sum(prodQtt)) -> needs
  nsteps<-nsteps+1
}

needs 

#########
# Part 2
#########

# Strategy 2

# Simply by try and error and dichotomy !

needs<-data.frame(prodQtt=1935265,prodType="FUEL",stringsAsFactors = FALSE)
nsteps<-0
while(any(needs$prodType!="ORE") & nsteps<100){
  prodList<-(infoProducts %>% 
               filter(prodType %in% needs$prodType) %>% 
               filter(prodLevel==max(prodLevel)))$prodType
  pmap_df(needs %>% filter(prodType%in%prodList),calcNeeds) %>% 
    bind_rows(needs %>% filter(!(prodType%in%prodList))) -> needs
  needs %>% group_by(prodType) %>% summarise(prodQtt=sum(prodQtt)) -> needs
  nsteps<-nsteps+1
}

needs 

results<-data.frame(x=1935266,ore=1000000392262)
newResult<-data.frame(x=1935265,ore=999999660128)
results<-results %>% bind_rows(newResult)
results %>% arrange(x)

# 1935265

# Done in 20 lines !
# After a day stuck the strategY 1 and 750+ lines produced...
# ... that lead to an overestimation of about 150 units of FUEL
# Whoever can find the mistake is welcome ! 
# (no need to go further than line 360)

# Strategy 1

# Calculates the extra product built at each step, 
# store them in a reserve, 
# then use them for new production...
# How stupid to waste a day like that when Strategy 2 worked in 5 minutes !

# lower bounds
1000000000000 %/% 892207
1000000000000 %/% 13312

reserve<-data.frame(prodQttReserve=0,prodType="FUEL",stringsAsFactors = FALSE)
# prodFilled<-c()

calcNeeds2<-function(prodQtt=1,prodType="FUEL"){
  need<-prodType
  nbProd<-(reactions %>% filter(prodType==need))$prodQtt
  nbReact<-prodQtt %/% nbProd+((prodQtt %% nbProd) != 0)
  # Fill reserve
  if(nbReact>0 & (prodQtt %% nbProd) != 0){
    reserve<<-reserve %>% 
      bind_rows(data.frame(prodQttReserve=nbReact*nbProd - prodQtt,prodType=need,stringsAsFactors = FALSE)) %>% 
      group_by(prodType) %>% summarise(prodQttReserve=sum(prodQttReserve))
  }
  
  ingr<-strsplit((reactions %>% filter(prodType==need))$ingr,",")[[1]]
  nbIngr<-length(ingr)
  ingr<-strsplit(ingr," ")  %>% unlist
  ingr<-ingr[ingr!=""]
  prodType<-ingr[2*(1:nbIngr)]
  prodQtt<-nbReact*as.integer(ingr[2*(1:nbIngr)-1])
  data.frame(prodQtt,prodType,stringsAsFactors = FALSE) 
}

calcNeeds2(1,"FUEL")

makeFuel<-function(lim,totalNeed=0,k=0){
  # for(k in 1:qtt){
  #   if(k%%100==0) {
  #     print(k)
  #     readline(prompt="Press [enter] to continue")
  #   }
  while(totalNeed<lim){
    k<-k+1
    
    needs<-data.frame(prodQtt=1,prodType="FUEL",stringsAsFactors = FALSE)
    nsteps<-0
    while(any(needs$prodType!="ORE") & nsteps<100){
      
      # Use reserve
      needs %>% 
        filter(prodQtt > 0) %>% 
        inner_join(reserve,by="prodType") %>% 
        filter(!is.na(prodQttReserve)) -> dt2
      if(nrow(dt2)>0){
        dt2  %>%  rowwise() %>% 
          mutate(conso=min(prodQtt,prodQttReserve)) -> dt2
        for(prod in (dt2 %>% filter(conso>0))$prodType){
          conso<-dt2$conso[dt2$prodType==prod]
          needs$prodQtt[needs$prodType==prod] <- (needs$prodQtt[needs$prodType==prod] - conso)
          reserve2<-reserve
          reserve2$prodQttReserve[reserve2$prodType==prod]<-reserve2$prodQttReserve[reserve2$prodType==prod] - conso
          reserve2->>reserve
        }
      }
      
      prodList<-(infoProducts %>% 
                   filter(prodType %in% needs$prodType) %>% 
                   filter(prodLevel==max(prodLevel)))$prodType
      pmap_df(needs %>% filter(prodType%in%prodList & prodQtt > 0),calcNeeds2) %>% 
        bind_rows(needs %>% filter(!(prodType%in%prodList))) -> needs
      needs %>% group_by(prodType) %>% summarise(prodQtt=sum(prodQtt)) -> needs
      
      nsteps<-nsteps+1
    }
    if(nrow(needs)>0) totalNeed<-totalNeed+needs$prodQtt
  }
  list(totalNeed=totalNeed,k=k)
}

# 
# makeFuel(1)
makeFuel(1000000)
# Trop lent !

makeFuel2<-function(lim,totalNeed=0,k=0,reserveMax=100){
  while(totalNeed<lim & any(reserve$prodQttReserve>reserveMax)){
    k<-k+1
    needs<-data.frame(prodQtt=1,prodType="FUEL",stringsAsFactors = FALSE)
    nsteps<-0
    while(any(needs$prodType!="ORE") & nsteps<100){
      
      # Use reserve
      needs %>% 
        filter(prodQtt > 0) %>% 
        inner_join(reserve,by="prodType") %>% 
        filter(!is.na(prodQttReserve)) -> dt2
      if(nrow(dt2)>0){
        dt2  %>%  rowwise() %>% 
          mutate(conso=min(prodQtt,prodQttReserve)) -> dt2
        for(prod in (dt2 %>% filter(conso>0))$prodType){
          conso<-dt2$conso[dt2$prodType==prod]
          needs$prodQtt[needs$prodType==prod] <- (needs$prodQtt[needs$prodType==prod] - conso)
          reserve2<-reserve
          reserve2$prodQttReserve[reserve2$prodType==prod]<-reserve2$prodQttReserve[reserve2$prodType==prod] - conso
          reserve2->>reserve
        }
      }
      prodList<-(infoProducts %>% 
                   filter(prodType %in% needs$prodType) %>% 
                   filter(prodLevel==max(prodLevel)))$prodType
      pmap_df(needs %>% filter(prodType%in%prodList & prodQtt > 0),calcNeeds2) %>% 
        bind_rows(needs %>% filter(!(prodType%in%prodList))) -> needs
      needs %>% group_by(prodType) %>% summarise(prodQtt=sum(prodQtt)) -> needs
      
      nsteps<-nsteps+1
    }
    if(nrow(needs[needs$prodType=="ORE",])>0) totalNeed<-totalNeed+needs$prodQtt[needs$prodType=="ORE"]
  }
  list(totalNeed=totalNeed,k=k)
}

makeFuel3<-function(lim,totalNeed=0,k=0,klim=100){
  klim<-klim+k
  while(totalNeed<lim & k<klim){
    k<-k+1
    needs<-data.frame(prodQtt=1,prodType="FUEL",stringsAsFactors = FALSE)
    nsteps<-0
    
    # Use reserve
    needs %>% 
      filter(prodQtt > 0) %>% 
      inner_join(reserve,by="prodType") %>% 
      filter(!is.na(prodQttReserve)) -> dt2
    if(nrow(dt2)>0){
      dt2  %>%  rowwise() %>% 
        mutate(conso=min(prodQtt,prodQttReserve)) -> dt2
      for(prod in (dt2 %>% filter(conso>0))$prodType){
        conso<-dt2$conso[dt2$prodType==prod]
        needs$prodQtt[needs$prodType==prod] <- (needs$prodQtt[needs$prodType==prod] - conso)
        reserve2<-reserve
        reserve2$prodQttReserve[reserve2$prodType==prod]<-reserve2$prodQttReserve[reserve2$prodType==prod] - conso
        reserve2->>reserve
      }
    }
    while(any(needs$prodType!="ORE") & nsteps<100){
      prodList<-(infoProducts %>% 
                   filter(prodType %in% needs$prodType) %>% 
                   filter(prodLevel==max(prodLevel)))$prodType
      pmap_df(needs %>% filter(prodType%in%prodList & prodQtt > 0),calcNeeds2) %>% 
        bind_rows(needs %>% filter(!(prodType%in%prodList))) -> needs
      needs %>% group_by(prodType) %>% summarise(prodQtt=sum(prodQtt)) -> needs
      
      nsteps<-nsteps+1
    }
    if(nrow(needs[needs$prodType=="ORE",])>0) totalNeed<-totalNeed+needs$prodQtt[needs$prodType=="ORE"]
  }
  list(totalNeed=totalNeed,k=k)
}


reserve<-data.frame(prodQttReserve=0,prodType="FUEL",stringsAsFactors = FALSE)
emptyReserve<-reserve %>% filter(prodType!="FUEL")

##############################
# 
# Exo

lim<-1000^4
reserve<-emptyReserve

res<-makeFuel3(lim = lim, klim = 2)
k<-res$k
ore<-res$totalNeed
nb_reserves<-1
resName<-paste0("reserve_",nb_reserves)
assign(x = resName,value = reserve)
result<-data.frame(k=k,ore=ore,reserve=resName,stringsAsFactors = FALSE)

while(ore<lim/2){
  
  res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
  k<-res$k
  ore<-res$totalNeed
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(x = newResName,value = reserve)
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
  k<-k*2
  ore<-ore*2
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*2) )
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
}

result

for(j in seq(nb_reserves-1,6,by=-2)){
  
  while(ore<lim-result[j,2]){
    k<-k+result[j,1]
    ore<-ore+result[j,2]
    nb_reserves<-nb_reserves+1
    newResName<-paste0("reserve_",nb_reserves)
    assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*10) )
    newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
    result<-bind_rows(result,newResult)
    reserve<-get(newResName)
    resName<-newResName
    
    res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 15+j)
    k<-res$k
    ore<-res$totalNeed
    nb_reserves<-nb_reserves+1
    newResName<-paste0("reserve_",nb_reserves)
    assign(x = newResName,value = reserve)
    newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
    result<-bind_rows(result,newResult)
    reserve<-get(newResName)
    resName<-newResName
  }
}

result

k<-result[48,1]
ore<-result[48,2]
reserve<-reserve_48
makeFuel(lim = lim,totalNeed = ore,k = k)

# $totalNeed
# [1] 1000000131323
# 
# $k
# [1] 1935408
# 
# Too high ! 

# makeFuel(lim=lim)






# With cooked functions makeFuel:
# $totalNeed
# [1] 1000000131323
# 
# $k
# [1] 1935408
# 
# Too high ! (not so far...)

############
# A la main
############
makeFuel3(lim = lim, klim = 100)
k<-100
ore<-51772765
reserve->reserve_1
result<-data.frame(k=k,ore=ore,reserve="reserve_1",stringsAsFactors = FALSE)

k<-k*100
ore<-ore*100
reserve_1%>% mutate(prodQttReserve=prodQttReserve*100) ->reserve_2
newResult<-data.frame(k=k,ore=ore,reserve="reserve_2",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
reserve<-reserve_2

makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
k<-10042
ore<-5189338267
reserve->reserve_3
newResult<-data.frame(k=k,ore=ore,reserve="reserve_3",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)

k<-k*100
ore<-ore*100
reserve_3%>% mutate(prodQttReserve=prodQttReserve*100) ->reserve_4
newResult<-data.frame(k=k,ore=ore,reserve="reserve_4",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
reserve<-reserve_4

makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
k<-1004494
ore<-519047510851
reserve->reserve_5
newResult<-data.frame(k=k,ore=ore,reserve="reserve_5",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)

lim-ore


k<-k+90*result[3,1]
ore<-ore+90*result[3,2]
reserve_3%>% mutate(prodQttReserve=prodQttReserve*90 +reserve_5$prodQttReserve) ->reserve_6
newResult<-data.frame(k=k,ore=ore,reserve="reserve_6",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
reserve<-reserve_6

makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
k<-1908544
ore<-986192624429
reserve->reserve_7
newResult<-data.frame(k=k,ore=ore,reserve="reserve_7",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
lim-ore


k<-k+2*result[3,1]
ore<-ore+2*result[3,2]
reserve_3%>% mutate(prodQttReserve=prodQttReserve*2 +reserve_7$prodQttReserve) ->reserve_8
newResult<-data.frame(k=k,ore=ore,reserve="reserve_8",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
reserve<-reserve_8


makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
k<-1928634
ore<-996573549022
reserve->reserve_9
newResult<-data.frame(k=k,ore=ore,reserve="reserve_9",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
(lim-ore)/result[1,2]


k<-k+60*result[1,1]
ore<-ore+60*result[1,2]
reserve_1%>% mutate(prodQttReserve=prodQttReserve*60 +reserve_9$prodQttReserve) ->reserve_10
newResult<-data.frame(k=k,ore=ore,reserve="reserve_10",stringsAsFactors = FALSE)
result<-bind_rows(result,newResult)
reserve<-reserve_10

makeFuel(lim = lim,totalNeed = ore,k = k)

# k<-1935265
# ore<-1000000764258


k<-1935266
ore<-1000000392262

# 1935266 -1
# Incorrect but why ??

# Different steps -> different results !
















###################

res<-makeFuel3(lim = lim, klim = 100)
k<-res$k
ore<-res$totalNeed
nb_reserves<-1
resName<-paste0("reserve_",nb_reserves)
assign(x = resName,value = reserve)
result<-data.frame(k=k,ore=ore,reserve=resName,stringsAsFactors = FALSE)

while(ore<lim/10){
  k<-k*10
  ore<-ore*10
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*10) )
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
  res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
  k<-res$k
  ore<-res$totalNeed
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(x = newResName,value = reserve)
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
}

result

while(ore<lim-result[7,2]){
  k<-k+result[7,1]
  ore<-ore+result[7,2]
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*10) )
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
  res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
  k<-res$k
  ore<-res$totalNeed
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(x = newResName,value = reserve)
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
}

result
reserve

while(ore<lim-result[5,2]){
  k<-k+result[5,1]
  ore<-ore+result[5,2]
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*10) )
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
  res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
  k<-res$k
  ore<-res$totalNeed
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(x = newResName,value = reserve)
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
}

result
reserve


while(ore<lim-result[3,2]){
  k<-k+result[3,1]
  ore<-ore+result[3,2]
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*10) )
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
  res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 10)
  k<-res$k
  ore<-res$totalNeed
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(x = newResName,value = reserve)
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
}

result
reserve

makeFuel(lim = lim,totalNeed = ore,k = k)

# $totalNeed
# [1] 1000000151079
# 
# $k
# [1] 1935372

# 1935371 too high!!

###############################
# Pour le test
###############################


lim<-1000^4
reserve<-emptyReserve

res<-makeFuel3(lim = lim, klim = 10)
k<-res$k
ore<-res$totalNeed
nb_reserves<-1
resName<-paste0("reserve_",nb_reserves)
assign(x = resName,value = reserve)
result<-data.frame(k=k,ore=ore,reserve=resName,stringsAsFactors = FALSE)

while(ore<lim/2){
  
  res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 40)
  k<-res$k
  ore<-res$totalNeed
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(x = newResName,value = reserve)
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
  k<-k*2
  ore<-ore*2
  nb_reserves<-nb_reserves+1
  newResName<-paste0("reserve_",nb_reserves)
  assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*2) )
  newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
  result<-bind_rows(result,newResult)
  reserve<-get(newResName)
  resName<-newResName
  
}

result

for(j in seq(nb_reserves,7,by=-2)){
  
  while(ore<lim-result[j,2]){
    k<-k+result[j,1]
    ore<-ore+result[j,2]
    nb_reserves<-nb_reserves+1
    newResName<-paste0("reserve_",nb_reserves)
    assign(newResName,get(resName)%>% mutate(prodQttReserve=prodQttReserve*10) )
    newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
    result<-bind_rows(result,newResult)
    reserve<-get(newResName)
    resName<-newResName
    
    res<-makeFuel2(lim = lim,totalNeed = ore,k = k,reserveMax = 15+j)
    k<-res$k
    ore<-res$totalNeed
    nb_reserves<-nb_reserves+1
    newResName<-paste0("reserve_",nb_reserves)
    assign(x = newResName,value = reserve)
    newResult<-data.frame(k=k,ore=ore,reserve=newResName,stringsAsFactors = FALSE)
    result<-bind_rows(result,newResult)
    reserve<-get(newResName)
    resName<-newResName
  }
}

result

k<-result[56,1]
ore<-result[56,2]
reserve<-reserve_56
makeFuel(lim = lim,totalNeed = ore,k = k)

reserve

# $totalNeed
# [1] 1000000003038
# 
# $k
# [1] 82892774

# 20 too high !!


k<-result[55,1]
ore<-result[55,2]
reserve<-reserve_55
makeFuel(lim = lim,totalNeed = ore,k = k)

reserve

# $totalNeed
# [1] 1000000001279
# 
# $k
# [1] 82892773

# 19 too high !!



k<-result[50,1]
ore<-result[50,2]
reserve<-reserve_50
makeFuel(lim = lim,totalNeed = ore,k = k)

reserve

# $totalNeed
# [1] 1000000001279
# 
# $k
# [1] 82892773

# 19 too high !!


# $totalNeed
# [1] 1000000004760
# 
# $k
# [1] 82892769

# 15 too high !!

















# 83
# 1002344

needs 
reserve-> reserveTest_1
reserveTest_1 %>% mutate(prodQttReserve=prodQttReserve*100) ->reserveTest_2
#FastForward
result<-data.frame(k=83,ore=1002344,reserve="reserveTest_1")
newResult<-data.frame(k=8300,ore=100234400,reserve="reserveTest_2")
result<-bind_rows(result,newResult)
k<-8300
ore<-100234400
reserve<-reserveTest_2
makeFuel(lim = ore+5000000,totalNeed = ore,k = k)


# 83041
# 1002552471

reserve->reserveTest_3
k<-83041
ore<-1002552471
newResult<-data.frame(k=k,ore=ore,reserve="reserveTest_3")
result<-bind_rows(result,newResult)


makeFuel2(lim = ore*2,totalNeed = ore,k = k)

# 83219
# 1004685321


reserve->reserveTest_4
k<-83219
ore<-1004685321
newResult<-data.frame(k=k,ore=ore,reserve="reserveTest_4")
result<-bind_rows(result,newResult)


makeFuel3(lim = ore*2,totalNeed = ore,k = k,klim = 100)

# 83319
# 1005883428

reserve->reserveTest_5
k<-83319
ore<-1005883428
newResult<-data.frame(k=k,ore=ore,reserve="reserveTest_5")
result<-bind_rows(result,newResult)

1000000/1006
reserveTest_5 %>% mutate(prodQttReserve=prodQttReserve*994) ->reserveTest_6

ore<-ore*994
k<-k*994
newResult<-data.frame(k=k,ore=ore,reserve="reserveTest_6")
result<-bind_rows(result,newResult)

reserve <- reserveTest_6



makeFuel3(lim = 1000000000000,totalNeed = ore,k = k,klim = 994)


1000000000000-ore
result$ore

ore<-ore+result$ore[5]
k<-k+result$k[5]
reserve <- reserveTest_6
reserve$prodQttReserve<-reserve$prodQttReserve+reserveTest_5$prodQttReserve

makeFuel2(lim = 1000000000000,totalNeed = ore,k = k)


reserve->reserveTest_7
k<-82802924
ore<-999661966770
newResult<-data.frame(k=k,ore=ore,reserve="reserveTest_7")
result<-bind_rows(result,newResult)


ore<-ore+3*result$ore[2]
k<-k+3*result$k[2]
reserve <- reserveTest_7
reserve$prodQttReserve<-reserve$prodQttReserve+3*reserveTest_2$prodQttReserve

makeFuel2(lim = 1000000000000,totalNeed = ore,k = k)

reserve->reserveTest_8
k<-82829832
ore<-999986586069
newResult<-data.frame(k=k,ore=ore,reserve="reserveTest_8")
result<-bind_rows(result,newResult)

makeFuel(lim = 1000000000000,totalNeed = ore,k = k)
reserve
