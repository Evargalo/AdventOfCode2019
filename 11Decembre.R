#########################
# Advent Of Code 2019  #
# 11 Decembre         #
######################

# options(digits=18)
source("intCode.R")

############
# Part 1
############
# Import du jeu de données

X11DecInput <- read_csv("11DecInput.txt", 
                        col_names = FALSE)
View(X11DecInput)
X11DecInput %>% gather ->input
input11dec<-input$value

# currentPanelColor<-0 # Part 1
currentPanelColor<-1 # Part 2
currentPanel<-c(0,0)
currentDirection<-0
knownPanels<-data.frame(x=0,y=0,c=0)
program<-input11dec

over<-0
currentIndex<-0
relativeBase<-0
while(over!=99){
  res<-intCode(program,currentPanelColor,currentIndex,relativeBase)
  res$vecteur->program
  res$currentIndex->currentIndex
  res$relativeBase->relativeBase
  res$output->output 
  output[1]->newColor
  knownPanels$c[knownPanels$x==currentPanel[1]&
                  knownPanels$y==currentPanel[2]] <- newColor
  output[2]->turn
  if (turn==0) currentDirection<-(currentDirection-1)%%4 #turn left
  if (turn==1) currentDirection<-(currentDirection+1)%%4 #turn right
  currentPanel <- case_when(
    currentDirection==0 ~ currentPanel+c(0,1), #move north
    currentDirection==1 ~ currentPanel+c(1,0), #move east
    currentDirection==2 ~ currentPanel+c(0,-1), #move south
    currentDirection==3 ~ currentPanel+c(-1,0) #move west
  )
  newPanel<-data.frame(x=currentPanel[1],y=currentPanel[2])
  ij<-inner_join(knownPanels,newPanel,by=c("x","y"))
  if(nrow(ij)==0) {
    knownPanels<-bind_rows(knownPanels,newPanel) 
    currentPanelColor<-0
  } else{
    currentPanelColor<-ij$c
  }
  over<-res$over
  print(res$log)
} 

nrow(knownPanels)
# 1876
table(knownPanels$c)
# Tous ont été peints

knownPanels %>% mutate_all(as.integer)-> knownPanels

knownPanels %>%  
  gf_tile(gformula = y~x,fill=~c) %>% 
#  gf_refine(scale_fill_manual(values = c("white", "black"),guide=FALSE)) %>%
  gf_theme(theme_void())

# CGPJCGCL
