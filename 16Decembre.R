#########################
# Advent Of Code 2019  #
# 16 Decembre         #
######################

options(digits = 20)

require(dplyr)
require(stringr)
require(tidyr)
require(purrr)
require(ggformula)

input<-"59749012692497360857047467554796667139266971954850723493205431123165826080869880148759586023048633544809012786925668551639957826693687972765764376332866751104713940802555266397289142675218037547096634248307260671899871548313611756525442595930474639845266351381011380170903695947253040751698337167594183713030695951696818950161554630424143678657203441568388368475123279525515262317457621810448095399401556723919452322199989580496608898965260381604485810760050252737127446449982328185199195622241930968611065497781555948187157418339240689789877555108205275435831320988202725926797165070246910002881692028525921087207954194585166525017611171911807360959"
input %>% strsplit("") %>% unlist %>% as.integer->inputVect

l<-length(inputVect)

#########
# Part 1
#########

################
#
# Matrices !
#
################

# 12345678
# l<-8

buildPattern<-function(rn){
  rep(c(0,1,0,-1),each=rn,times=l%/%4+1)->lv
  lv[2:(l+1)]
}
buildPattern(7)

M<-matrix(data = 0, nrow = l, ncol = l)
for (k in 1:l){
  M[k,]<-buildPattern(k)
}
M[1:10,1:10]

V<-inputVect
for(k in 1:100){
  V <- M %*% V %>% abs %% 10
}

V[1:8]
# 4 2 2 0 5 9 8 6

#########
# Part 2
#########

offset<-as.integer(substr(input,1,7))

# offset%/%650
# 
# M[105:130,105:130]

# Partir de la fin !

l<-length(inputVect)
V<- inputVect

############################################

W<-rep(V,808)
Y<-W[(808*650):1]

for(k in 1:100){
  Y<- cumsum(Y) %% 10
}
X<-Y[(808*650):1]
X[101:108]
# 01327020 too low...

offset
650*10000-650*808+100
X[102:109]
# 13270205
























# Code like a fool...
#########
# Part 1
#########
# Ca suffit pour Part1 mais même pas en rêve pour Part2!

buildPattern<-function(rn){
  rep(c(0,1,0,-1),each=rn,times=l%/%4+1)->lv
  paste0(lv[2:(l+1)],"",collapse = ",")
}
stringToVect<-function(s){
  s %>% strsplit(",") %>% unlist %>% as.numeric
}
buildPattern(4)
stringToVect("1,2,3,4,5")

vect<-inputVect

phase<-function(vect){
  l<<-length(vect)
  data.frame(vect) %>% gather -> df
  df <- df %>% mutate(k=row_number()) %>% 
    rowwise %>% mutate(pattern=buildPattern(k))  %>% 
    mutate(prod=as.character(sum(stringToVect(pattern)*vect))) %>% 
    mutate(lastDigit=substr(prod,nchar(prod),nchar(prod)))
  as.integer(df$lastDigit)
}
phase(1:8)
sum(stringToVect(df$pattern)*vect)
df %>% filter(k %in% 1:3) %>% rowwise %>% stringToVect(pattern)
stringToVect(df$pattern[1])*vect
vect[651]
# Test ok
inputVect2<-1:8
for(k in 1:4){
  inputVect2<-phase(inputVect2)
}

test<-"80871224585914546619083218645595"
test<-"69317163492948606335995924319873"
test %>% strsplit("") %>% unlist %>% as.integer->testVect
for(k in 1:100){
  testVect<-phase(testVect)
  print(k)
}
testVect[1:8]
# Parfait...

# Exo
for(k in 1:100){
  inputVect<-phase(inputVect)
  print(k)
  if(k%%10==0) {
    print(inputVect)
    readline(prompt="Press [enter] to continue")
  }
}
inputVect[1:8]

# 42205986


#########
# Part 2
#########
# Let's stop being a fool

