for (i in 1:10)
{
  print(i)
}

n= sample(x= 1:10, size=1, replace= TRUE)
for (i in 1:n)
{
  print(i)
}

n= 17
vec_1 = sample(1:10, n, replace = TRUE)
vec_1

for(i in 1:n)
{  print(paste("The element of vec_1 at index", 1:n, "is", vec_1))
}
  
create_and_print_vec= function(n, min=1, max= 10)
{
  vec_1 = sample(1:n, n, replace = TRUE)
  vec_1
  
  for(i in vec_1)
  {  print(paste("The element of vec_1 at index", 1:n, "is", vec_1))
 break } 
  
}

create_and_print_vec(20)

n= 17
vec_1 = sample(1:10, n, replace = TRUE)
vec_1

for(i in 1:n)
{  print(paste("The element of vec_1 at index", 1:n, "is", vec_1))
break
  }

