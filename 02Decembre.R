library(readr)
X02DecInput <- read_csv("02DecInput.txt", 
                        col_names = FALSE)
View(X02DecInput)
install.packages("dplyr")
install.packages("tidyr")
require(dplyr)
require(tidyr)
X02DecInput %>% gather ->input
inputvect<-input$value

process<-function(value){
  currentIndex<-1
  action<-0
  while (action!=99){
    action<-value[currentIndex]
    val1<-value[value[currentIndex+1]+1]
    val2<-value[value[currentIndex+2]+1]
    pos<-value[currentIndex+3]+1
    if (action==1) value[pos]<-val1+val2
    if (action==2) value[pos]<-val1*val2
    currentIndex<-currentIndex+4
  }
  value
}

test<-c(1,0,0,0,99)
process(test)
process(inputvect)

# Part 2

noun<-rep(0:99,100)
verb<-rep(0:99,each=100)
test<-data.frame(noun,verb)
noun<-12
verb<-2
attempt<-function(noun,verb){
  output<-inputvect
  output[2]<-noun
  output[3]<-verb
  process(output)[1]->result
  result
}

attempt(12,2)
attempt(1,1)
test %>%  mutate(result=attempt(noun,verb))
test$result<-attempt(test$noun,test$verb)
test %>% head


X02DecInput2 <- read_csv("02DecInput.txt", 
                         col_names = FALSE)
X02DecInput2 %>% gather ->input
inputvect<-input$value

for(i in 1:10000){
  test$result[i]<-attempt(test$noun[i],test$verb[i])
}
test %>% filter(result==19690720)
test$result
