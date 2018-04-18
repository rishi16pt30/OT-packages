library(igraph)
v=c(1,2,2,3,3,4,3,5,3,6,3,7,3,8,3,9,3,10,10,11,11,12,12,13,13,14,13,15,13,16,15,17,17,18,4,12,5,12,6,12,7,12,8,12,9,12,14,15,16,15)
g1<-graph(edges=v,n=max(v),directed=T)
n=max(v)
E(g1)$label<-c('A','B','C','D','E','F','G','H','I','J','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y')
E(g1)$weight<-c(11,7,18,3,9,9,9,6,5,15,3,10,28,11,8,19,22)
plot(g1)
ES<-c()
for(i in 1:n){
  ES[i]=0
}
g1[]
for(i in 1:n){
  neigh<-c()
  neigh<-neighbors(g1,i)
  for (j in neigh){
    a=ES[i]+g1[i,j]
    ES[j]=max(a,ES[j])
  }
}
ES
LC<-c()
for(i in 1:n){
  LC[i]=Inf
}
LC[n]=ES[n]
i=18
while(i>=1)
{
  neigh<-neighbors(g1,i)
  for(j in neigh){
    a=LC[j]-g1[i,j]
    a
    LC[i]=min(LC[i],a)
  }
  i=i-1

}
LC
totalfloat=c()
k=1
l=2
while(l<length(v)){
  i=v[k]
  j=v[l]
  tf=round(LC[j]-ES[i]-g1[i,j],2)
  totalfloat=c(totalfloat,tf)
  k=k+2
  l=l+2
}
cp=c()
for (i in 1:length(totalfloat)){
  if(totalfloat[i]==0)
  {
    cp=c(cp,E(g1)$label[i])
  }
}
cp
var=c(12.25,12.25,20.25,25,11.11,5.44,6.25,36,16,4)
critvar=0
for(i in 1:length(totalfloat))
{
  if(totalfloat[i]==0)
  {
    critvar=critvar+var[i]
  }
}
critvar
sigma=sqrt(critvar)
ct=ES[n]
x=104
pnorm(x,mean=ct,sd=sigma)

