
simplex<-read.csv(file.choose())
rows=nrow(simplex)
cols=ncol(simplex)-1

elements=c()
for(i in 2:(cols+1))
{
  thatcol=c(simplex[,i])
  elements=c(elements,thatcol)
}

mat=matrix(data=elements,nrow=rows,ncol=cols,byrow=FALSE)
mat1 = mat
#print(mat)

decvars=c()
for(i in 1:(cols-rows))
{
  decvars=c(decvars,c(0))
}

while(TRUE) 
{
  z=mat[,cols]
  min_row=which.min(z)
  ratio=matrix(c(0),1,(cols-1))
  for(i in 1:cols)
  {
    if(mat[min_row,i]!=0)
      ratio[i] = mat[rows,i]/mat[min_row,i]
    else
      ratio[i] = 0
    
    if(ratio[i]>=0)
      ratio[i] = Inf
    else
      ratio[i] = abs(ratio[i])
      
  }
  
  ratio=ratio[1:(cols-1)]
  min_col=which.min(ratio)
  if(min_col <= (cols-rows))
  {
    decvars[min_col]=min_row  
  }
  
  if(mat[min_row,min_col]!=0)
    mat[min_row,]=mat[min_row,]/mat[min_row,min_col]
  else
    mat[min_row,] = 0  
  
  for(i in 1:rows)
  {
    if(i!=min_row)
    {
      mat[i,]=mat[i,]-mat[i,min_col]*mat[min_row,]
    }
  }
  #print(mat)
  
  is_neg=sum(mat[1:(rows-1),cols]<0)
  if(is_neg>0)
    break

}

x = c()
for(i in 1:(cols-rows))
{
  if(decvars[i]==0)
    x[i] = 0
  
  else
    x[i] = mat[decvars[i],cols]
}

x1 = c()
for(i in 1:(cols-rows))
{  
  x[i] = ceiling(-1*mat1[i,cols])
  x1[i] = -1*mat1[i,cols]
}

z = mat1[rows,(1:(rows-2))]
z1 = z
q = 10*x1
#print(sum(q))

for(i in 1:length(q))
  q[i] = q[i] - (4*x[i])

xm = x
for(s in 1:4)
{
  x = xm
  count = mat1[rows-1,cols] - sum(x)
  while(count>0)
  {
    m = which.max(z)
    if(q[m] <= count)
    {
      x[m] = x[m]+q[m]
      count = count-q[m]
      q[m] = 0
      z[m] = -Inf
    }
      
    else
    {
      x[m] = x[m]+count
      q[m] = q[m]-count
      count = 0
    }
  }
    
  c = 0
  cat("\n\n\n For Seller ",s," :- \n")
  for(j in 1:length(x))
  {
    cat("\n\tx",j," = ",x[j])
    c = c+x[j]*z1[j]
  }
    
  cat("\n\nHis commission is ",c*0.2)
}
