

for_cpp=function(i=0,n=9,op="++",...){
  if(op=="++"){
    print("yes honey")
  }
  
  for (j in i:n) {
    make
  }
}

library(foreach)

?`foreach-package`

for_cris(i=0,2,"++") 

foreach(i=array(0:9)) %do% {
  print(i)

}



for (i in 0:9) {
  print(i)
}



print_for <- function(i,n) {
  print(i)
  i=i+1
  if(i<n){
    print_for(i,n)
  }
}

print_for(0,10)

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()


f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))


i=0
while (i<6) {
  i=1/(i+1)+i
}


#x^x limites

x=1

y=0
d=-1
i=1

while (abs(d)>10**(-10)) {
cat(x)
y[i]=x^x
i=i+1
d=d/(1+i/10)
x=d
}
y
d=1
i
while (abs(d)>10**(-10)) {
  y[i]=x^x
  i=i+1
  d=d/(1+i/10)
  x=d
}
y
plot(y)

#forma corta 

x=seq(-1,1,length.out = 1000)
y=x^x
y
plot(y)



