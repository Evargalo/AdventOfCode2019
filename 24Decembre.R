###############
# 24 Decembre
###############
require(readr)
require(dplyr)
input<-read_delim("##.##
.#.##
##..#
#.#..
.###.
",";",col_names=FALSE)
                
                
strsplit(input$X1,split="") %>% unlist ->inputVect

##
# Part 2
##

layer0<-data.frame(x=rep(1:5,each=5),
                   y=rep(1:5,5),
                   bug=FALSE,
                   level=0) %>% filter(x!=3 | y!=3)
layer<-function(k){
  layer0 %>% mutate(level=k)
}
layer(7)


cells<-data.frame(x=rep(1:5,each=5),
                  y=rep(1:5,5),
                  bug=inputVect=="#",
                  level=0) %>% filter(x!=3 | y!=3)

neighbours<-function(xx,yy,ll){
  cells %>% filter((abs(xx-x)+abs(yy-y))==1 & level==ll) -> samelevel
  otherlevel<-data.frame(x=c(),y=c(),bug=c(),level=c())  
  if(xx==1) cells %>% filter(x==2 & y==3 & level==ll+1) %>% bind_rows((otherlevel))-> otherlevel
  if(xx==5) cells %>% filter(x==4 & y==3 & level==ll+1) %>% bind_rows((otherlevel)) -> otherlevel
  if(yy==1) cells %>% filter(x==3 & y==2 & level==ll+1) %>% bind_rows((otherlevel)) -> otherlevel
  if(yy==5) cells %>% filter(x==3 & y==4 & level==ll+1) %>% bind_rows((otherlevel)) -> otherlevel
  if(xx==2 & yy==3) cells %>% filter(x==1 & level==ll-1) -> otherlevel
  if(xx==4 & yy==3) cells %>% filter(x==5 & level==ll-1) -> otherlevel
  if(xx==3 & yy==2) cells %>% filter(y==1 & level==ll-1) -> otherlevel
  if(xx==3 & yy==4) cells %>% filter(y==5 & level==ll-1) -> otherlevel
  
  bind_rows(samelevel,otherlevel)
}
neighbours(2,3,1)

calcNewState<-function(xx,yy,ll){
  nei<-neighbours(xx,yy,ll) 
  sumbugs<-sum(nei$bug)
  sumbugs==1 | (!cells$bug[cells$x==xx & cells$y==yy & cells$level==ll] & sumbugs==2) 
}
calcNewState(2,3,0)

# for(k in 1:200){
#   print(k)
#   print(sum(cells$bug))
#   cells <- cells %>% bind_rows(layer(k)) %>% bind_rows(layer(-k))  
#   cells %>% 
#     rowwise %>% 
#     mutate(step1=calcNewState(x,y,level))%>% 
#     select(x,y,level,step1) %>% 
#     rename(bug=step1)->cells
# }
# sum(cells$bug)
# step51 %>% filter(abs(level)<27) -> cells
# cells %>% 
#   rowwise %>% 
#   mutate(step1=calcNewState(x,y,level))%>% 
#   select(x,y,level,step1) %>% 
#   rename(bug=step1)->cells
# cells->step52
for(k in 1:100){
  print(k)
  cells <- cells %>% bind_rows(layer(k)) %>% bind_rows(layer(-k))  
  cells %>% 
    rowwise %>% 
    mutate(step1=calcNewState(x,y,level))%>% 
    select(x,y,level,step1) %>% 
    rename(bug=step1)->cells
  print(sum(cells$bug))
  cells %>% 
    rowwise %>% 
    mutate(step1=calcNewState(x,y,level))%>% 
    select(x,y,level,step1) %>% 
    rename(bug=step1)->cells
  print(sum(cells$bug))
  if(k%%10==0) assign(value = cells,x=paste0("step",2*k))
}

print(sum(step200$bug))
# 1934
