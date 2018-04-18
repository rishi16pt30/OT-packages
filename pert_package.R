library(igraph)
f=read.csv(file.choose(),header=FALSE)
ed=as.matrix(f,nrow=25,ncol=3)
v=c(1,2,2,4,4,8,8,9,9,10,10,11,11,12,12,13,13,16,16,17,17,21,21,22,2,3,3,5,5,6,6,7,7,14,14,15,15,18,18,19,19,20,20,23,23,24,24,25,22,25,7,10,15,13,17,19,21,23)
g1<-graph(edges=v,n=max(v),directed=T)
n=max(v)
E(g1)$label<-c('a1','a2','a3','a4','a5','a6','a7','a8','a9','a10','a11','a12','a13','a14','a15','a16','a17','a18','a19','a20','a21','a22','a23','a24','a25','d','d','d','d')
we<-c()
for(i in 1:nrow(ed))
{
  w=as.integer(ed[i,2])+4*(as.integer(ed[i,4]))+as.integer(ed[1,3])/6
  we<-c(we,w)
}
E(g1)$weight=we
plot(g1,edge.arrow.size=.3,vertex.color="gold",vertex.size=18,vertex.frame.color="grey",vertex.label.color="black",vertex.label.dist=2)
es<-c()
for(i in 1:n)
{
  es[i]=0
}
for(i in 1:n)
{
  neigh<-c()
  neigh<-neighbors(g1,i)
  for(j in neigh)
  {
    a=es[i]+g1[i,j]
    es[j]=max(a,es[j])
  }
}
lc<-c()
for(i in 1:n)
{
  lc[i]=Inf
}
lc[n]=es[n]
i=24
while(i>=1)
{
  neigh<-neighbors(g1,i)
  for(j in neigh)
  {
    a=lc[j]-g1[i,j]
    lc[i]=min(a,lc[i])
  }
  i=i-1
}
totalfloat=c()
k=1
l=2
while(l<length(v)-7)
{
  i=v[k]
  j=v[l]
  tf=round(lc[j]-es[i]-g1[i,j],2)
  totalfloat=c(totalfloat,tf)
  k=k+2
  l=l+2
}
cp=c()
for(i in 1:length(totalfloat))
{
  if(totalfloat[i]==0)
  {
    cp=c(cp,E(g1)$label[i])
  }
}
variance=c()
for(i in 1:n)
{
  v=as.integer(ed[i,3])-as.integer(ed[i,2])
  var=sqrt(v/6)
  variance=c(variance,var)
}
critvar=0
for(i in 1:length(totalfloat))
{
  if(totalfloat[i]==0)
  {
    critvar=critvar+variance[i]
  }
}
sigma=sqrt(critvar)
ct=es[n]
x=104
pnorm(x,mean=ct,sd=sigma)
plot(pnorm)

  