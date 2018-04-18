
library(igraph)
library(data.table)
mydata <- read.table(file.choose())
a=c()
b=c()
c=c()
wt=c()
var=c()
len_row=nrow(mydata)
for(i in 1:len_row)
  a=append(a,mydata[i,1])
for(i in 1:len_row)
  c=append(c,mydata[i,2])
for(i in 1:len_row)
  b=append(b,mydata[i,3])
for(i in 1:len_row){
  cal=(a[i]+4*c[i]+b[i])/6
  wt=append(wt,cal)
}
for(i in 1:len_row){
  cal=((b[i]-a[i])/6)^2
  var=append(var,cal)
}
dum=as.numeric(readline("Enter the number of dummy activities:"))
for(i in 1:dum)
  wt=append(wt,0)

'''relations <- data.frame(from=c(1,2,2,3,4,3,5,6,4,7,3),
                        to=c(2,3,4,5,5,6,6,7,7,8,4),activities=c("A","B","C","D","E","F","G","H","I","J","DUM"),
                        weight=c(16.5,17.5,18.5,16,20.33,23.33,8.5,23,25,13,0))'''
relations <- data.frame(from=c(1,2,3,3,3,3,3,3,3,10,11,12,13,13,13,15,17,4,5,6,7,8,9,14,16),to=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,12,12,12,12,12,12,15,15),activities=c("A","B","C","D","E","F","G","H","I","J","k","L","M","N","O","P","Q","DUM","DUM","DUM","DUM","DUM","DUM","DUM","DUM"),weight=wt)

g <- graph.data.frame(relations, directed=TRUE)

plot(g)
es=c()
for(i in 1:18)
{
  es[i]=0
}
es[1]=0
k=1
for(i in (1:18))
{
  
  neigh=c()
  neigh=neighbors(g,i)
  
  for (j in neigh)
  {
    a=es[i]+g[i,j]
    es[j]=max(es[j],a)
  }
  
}
lc=c()
for(i in 1:18)
{
  lc[i]=Inf
}
lc[18]=es[18]
i=17
while(i>=1)
{
  neigh=c()
  neigh=neighbors(g,i)
  for( j in neigh)
  {
    a=lc[j]-g[i,j]
    lc[i]=min(lc[i],a)
  }
  i=i-1
}
len=nrow(relations)-dum
total_float=c()
for(i in 1:len){
  total_float[i]=0
}
k=1

for(i1 in 1:len){
  i=relations[i1,1]
  j=relations[i1,2]
  total_float[k]=lc[j]-es[i]-g[i,j]
  k=k+1
}
cp=c()
print("Critical Path:")
for(i1 in 1:len){
  if(total_float[i1]==0){
    a1=relations[i1,3]
    a1=as.character(a1)
    cp=c(cp,a1)
  }
}
var1=sum(var)
avg=var1/len
sum1=0
for(i1 in 1:len){
  if(total_float[i1]==0){
    sum1=sum1+var[i1]
  }
}
sigma=sqrt(sum1)
E=es[18]
x=as.integer(readline("time of project completion:"))
pnorm(x,mean=E,sd=sigma)


