# 04dec.R
require(tidyr)
require(dplyr)
x<-359282:820401
dt<-data.frame(x)
dt %>% mutate(a=(x %/% 100000)%%10,
              b=(x %/% 10000)%%10,
              c=(x %/% 1000)%%10,
              d=(x %/% 100)%%10,
              e=(x %/% 10)%%10,
              f=x%%10
) -> dt

dt %>% filter(
  a<=b & b<=c & c<=d & d<=e & e<=f &
    (a==b | b==c | c==d | d==e | e==f)
)             %>% count

# Part 2

dt %>% filter(
  a<=b & b<=c & c<=d & d<=e & e<=f &
    ((a==b & a!=c) | (b==c & b!=d & a!=c) | (c==d & c!=e & b!=d) | (d==e & d!=f & c!=e) | (e==f& d!=f)) 
)             %>% count
