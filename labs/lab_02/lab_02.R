my_matrix = matrix(data = 1:9, nrow = 3, ncol = 3)
sum(my_matrix)
for (i in 1:10)
{
  print(i)
}
print_number = function(n)
{
  print(paste0("The value of the number is ", n))
}
rnorm(10)
rnorm(n = 10, sd = 1)
rnorm(sd = 1, mean = 0, n = 10)
?rnorm
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
n = 12345
vec_2= sample(3, n, replace= FALSE)
head(vec_2)
vec_1[vec_2]
?sample

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
if(vec_1 = 3)
  print(vec_2, TRUE)
if(vec_1 != 3)
  print(vec_2, FALSE)

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
vec_2 = sample(1, n, replace = TRUE)
head(vec_2)
vec_1[vec_2]

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 ==3)
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

for (i in 1:10)
{
  print(i)
}

for (i in 1:10 )
{
  print( i, 
    paste0( "This is loop iteration:1", 
          sample(1:10)))

for (i in 1:10)
print_number= function(n=1:10)
{
print(
  paste0( "This is loop iteration:1", n)) 
}

for (i in 1:10)
  print( 
      paste0( "This is loop iteration: 1" ))
1:10

for (i in 1:n)
{
  print(i)
}

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2<-ifelse(vec_1==3,TRUE, FALSE)
vec_1[vec_2]

vec_2<-ifelse(vec_1==3,TRUE, FALSE)
vec_1[vec_2]

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)
sum(vec_1==3)


n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

for (i in 1:10)
{
  print(i)
}

for (i in 1:10)
  print( 
    paste0( "This is loop iteration: 1" ))

for (i in 1:10)
{
  print(i)
}



runif(n)

for (i in 1:n)
{
  print(i)
}


n=17
vec_1<-sample( 1:10, n, replace=TRUE)

for (i in vec_1)
  print( 
    paste0( "The element of vec_1 at index 1:10 is n" ))



  