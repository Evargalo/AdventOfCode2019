#########################
# Advent Of Code 2019  #
# 12 Decembre         #
######################

require(tidyr)
require(dplyr)

#test
pos<-data.frame(moon=c("C","E","G","I"),
                x=c(-1,2,4,3),
                y=c(0,-10,-8,5),
                z=c(2,-7,8,-1))
#exo
pos<-data.frame(moon=c("C","E","G","I"),
                x=c(-8,-11,8,-2),
                y=c(-18,-14,-3,-16),
                z=c(6,4,-10,1))

vel<-data.frame(moon=c("C","E","G","I"),x=0,y=0,z=0)

gravity<-function(pos){
  pos$key=1
  doubleTable<-full_join(pos,pos,by="key") %>% filter(moon.x != moon.y)
  doubleTable %>% mutate(
    dx=(x.y>x.x) - (x.x>x.y),
    dy=(y.y>y.x) - (y.x>y.y),
    dz=(z.y>z.x) - (z.x>z.y),
  )->doubleTable
  rep<- doubleTable %>% 
    group_by(moon.x) %>% 
    summarise(dx=sum(dx),dy=sum(dy),dz=sum(dz)) %>% 
    rename(moon=moon.x)
  rep
}

#########
# Part 1
########
n<-1000
for(k in 1:n){
  vel<-vel %>% right_join(gravity(pos),by="moon") %>% 
    mutate(x=x+dx,
           y=y+dy,
           z=z+dz) %>% 
    select(c(moon,x,y,z))
  pos<-pos%>% right_join(vel,by="moon") %>% 
    mutate(x=x.x+x.y,
           y=y.x+y.y,
           z=z.x+z.y) %>% 
    select(c(moon,x,y,z))
}

energy<-function(pos,vel){
  colnames(vel)<-c("moon","xx","yy","zz")
  tab<-full_join(pos,vel,by="moon")
  tab %>% select(-moon) %>% mutate_all(abs) %>% 
    mutate(energy=(x+y+z)*(xx+yy+zz)) %>% 
    summarise(sum(energy))
}

energy(pos,vel)


#########
# Part 2
########

# Enough to check if we are back to starting point 
# (because steps are reversible)
# Enough to check on three moons.
# (because of constant sums)
# Enough to check only vel=0 ???

# Méthode 1:
# Force (quasi-) brute; suffisant pour le premier jeu test, pas au-delà.
#----
checkPosTest<-function(pos,vel){
  vel<-vel %>% select(-moon)
  if(any(vel)!=0) return (FALSE)
  if(pos$x != c(-1,2,4,3)) return (FALSE)
  if(pos$y != c(0,-10,-8,5)) return (FALSE)
  if(pos$z != c(2,-7,8,-1)) return (FALSE)
  return(TRUE)
}
checkPosTest(pos,vel)

checkPos<-function(pos,vel){
  vel<-vel %>% select(-moon)
  if(any((vel)!=0)) return (FALSE)
  if(any(pos$x != c(-8,-11,8,-2))) return (FALSE)
  if(any(pos$y != c(-18,-14,-3,-16))) return (FALSE)
  if(any(pos$z != c(6,4,-10,1))) return (FALSE)
  return(TRUE)
}
checkPos(pos,vel)

checkVel<-function(vel){
  vel<-vel %>% select(-moon)
  if(any((vel)!=0)) return (FALSE)
  return(TRUE)
}
checkVel(vel)

n<-100000
for(k in 1:n){
  vel<-vel %>% right_join(gravity(pos),by="moon") %>% 
    mutate(x=x+dx,
           y=y+dy,
           z=z+dz) %>% 
    select(c(moon,x,y,z))
  pos<-pos%>% right_join(vel,by="moon") %>% 
    mutate(x=x.x+x.y,
           y=y.x+y.y,
           z=z.x+z.y) %>% 
    select(c(moon,x,y,z))
  # if(checkPos(pos,vel)){break}
  if(checkVel(vel)) {break}
}
k
# Test : 2772 parfait mais long...
# Exo : A 200000, toujours pas de répétition...

# pos100000<-pos
# vel100000<-vel

pos200000<-pos
vel200000<-vel

#####################################################
# Méthode 2
# Simpler model : coordinate by coordinate, then lcm
# Modèle simplifié coord par coord, puis ppcm

##########################
# Fonction rankRepeat
# Paramètres :
# vect = positions initiales
# n = limite sup du nombre d'itérations
# Renvoie le premier rang de retour en position initiale avec velocité nulle
##########################
rankRepeat<-function(vect,n=1000000){
  dt<-data.frame(pos=vect,vel=0,plus=0,minus=0)
  for( k in 1 : n){
    dt<-dt %>% rowwise %>% mutate(
      plus=sum(dt$pos>pos),
      minus=sum(dt$pos<pos)
    ) %>% ungroup %>% 
      mutate(vel=vel+plus-minus) %>% 
      mutate(pos=pos+vel)
    if(!any(dt$vel!=0)) if(!any(dt$pos!=vect)) break
  }
  k
}

kx<-rankRepeat(c(-8,-11,8,-2))
ky<-rankRepeat(c(-18,-14,-3,-16))
kz<-rankRepeat(c(6,4,-10,1))

# x: k= 286332
# y: k= 167624
# z: k= 96236

#  install.packages("pracma")
require(pracma)
Lcm(kx,ky) %>% Lcm(kz)
# 288684633706728



# TODO: un beau graphique animé avec les positions des lunes de Jupiter...

