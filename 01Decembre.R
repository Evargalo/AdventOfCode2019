
fuel<-function(mass){mass %/% 3 -2}
fuel(17)
fuel(1:10)

X01DecInput <- read_csv("01DecInput.txt", 
                        col_names = FALSE)
X01DecInput$X1 ->input
sum(fuel(input))

# Part 2

X01DecInput <- read_csv("01DecInput_2.txt", 
                        col_names = FALSE)
X01DecInput$X1 ->input

fuel<-function(mass){max(0,mass %/% 3 -2)}

module<-function(mass){
  loadFuel<-fuel(mass)
  totFuel<-loadFuel
  while(loadFuel>0){
    loadFuel<-fuel(loadFuel)
    totFuel<-totFuel+loadFuel
  }
  totFuel
}

module(100756)

test<-data.frame(input)
test$module<-0
for (i in 1:100){
  test$module[i]<-module(test$input[i])
}
sum(test$module)


