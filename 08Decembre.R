#########################
# Advent Of Code 2019  #
# 8 Decembre          #
######################

############
# Part 1
############

# Import du jeu de données
X08DecembreInput <- read_csv("08DecembreInput.txt", 
                             col_names = FALSE, col_types = cols(X1 = col_character()))
input8dec<-X08DecembreInput$X1
nchar(input8dec)

strsplit(input8dec,split="") %>% unlist->input
layers<-matrix(input,byrow=T,ncol = 25*6)

# Décompte des 0,1,2 dans chaque couche
countChar<-function(char,layer){
  sum(layers[layer,]==char)
}
countChar(0,1)

result<-data.frame(layer=1:nrow(layers))
result %>% rowwise %>% 
  mutate(nbZero=countChar(0,layer))%>% 
  mutate(nbUn=countChar(1,layer))%>% 
  mutate(nbDeux=countChar(2,layer))%>% 
  mutate(prod=nbUn*nbDeux)%>%
  ungroup %>% 
  filter(nbZero==min(nbZero)) %>% 
  select(prod)

#  or: arrange(nbZero) %>% head(1)

# 1320

############
# Part 2
############

pixels<-layers[1,]
for(j in 1:150){
  i<-2
  while (pixels[j]=="2" & i<151){
    pixels[j]<-layers[i,j]
    i<-i+1
  }
}
table(pixels)
image<-matrix(pixels,nrow=6,byrow = T)
image

dt<-data.frame(
  x=rep(1:25,times=6),
  y=rep(1:6,each=25),
  col=pixels
)

######
# Plot
######

# Avec ggformula
install.packages("ggformula")
require(ggformula)
dt %>% gf_tile(gformula = (-y)~x,fill=~col) %>% 
  gf_refine(scale_fill_manual(values = c("white", "black"),guide=FALSE))  %>%
  gf_theme(theme_void())

#Avec esquisse
install.packages("esquisse")
require(esquisse)
ggplot(dt) +
  aes(x = x, y = -y, fill = col) +
  geom_tile(size = 1L) +
  scale_fill_hue() +
  theme_void()

#RCYKR

#########################
# Fonctions paramétrables
#########################
signalToLayers<-function(width,height,signal){
  matrix(signal,byrow=T,ncol = width*height)
}
layersToPixels<-function(layers){
  pixels<-layers[1,]
  for(j in 1:ncol(layers)){
    i<-2
    while (pixels[j]=="2" & i<ncol(layers)+1){
      pixels[j]<-layers[i,j]
      i<-i+1
    }
  }
  pixels
}
plotImage<-function(width,height,pixels){
  dt<-data.frame(
    x=rep(1:width,times=height),
    y=rep(1:height,each=width),
    col=pixels
  )
  dt %>% 
    gf_tile(gformula = (-y)~x,fill=~col) %>% 
    gf_refine(scale_fill_manual(values = c("white", "black"),guide=FALSE)) %>%
    gf_theme(theme_void())
}
readImage<-function(width,height,signal){
  layers<-signalToLayers(width,height,signal)
  pixels<-layersToPixels(layers)
  plotImage(width,height,pixels)
}
readImage(width = 25,height = 6,signal = input)
