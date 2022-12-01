a<- "Abby"
b1<- 45.6
b2<- "45.6"
c1<- 0:3
b1+b2
b1+c
v1<- c(-2:2)
v2<- c(v1*3)
sum(v2)
vec_4<- c(1:12)
mat_1<- matrix(vec_4, ncol = 4, nrow = 3, byrow = TRUE)
mat_2<- matrix(vec_4, ncol = 4, nrow = 3, byrow = FALSE)
my_list_1<-list(5.2, "five point two", c(0:5))
names(my_list_1) <-c("two", "one", "three")
my_list_1[["three"]]
my_list_1[["one"]]
my_vec = rep(1:3, 5)
my_vec

my_vec= rep(1:3,5)
my_vec
my_bool_vec<-ifelse(my_vec==3,TRUE, FALSE)
data.frame(my_vec, my_bool_vec)

my_bool_vec[c(my_vec)]
my_vec[my_bool_vec]

