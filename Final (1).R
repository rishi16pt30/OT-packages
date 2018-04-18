Table <- read.csv( file.choose() )
rows = nrow( Table )
cols = ncol( Table ) - 1

Elements=c()

for( i in 2 : ( cols + 1 ) )
{
  thatcol = c( Table[ , i ] )
  Elements = c( Elements, thatcol )
}

mat = matrix( Elements, rows, cols )
#mat2 = mat[ , cols ]
 
Data_file <- read.csv( file.choose() ) 
Data_rows = nrow( Data_file )
Data_cols = ncol( Data_file ) - 1

Data_elements = c()
for( i in 2 : ( Data_cols + 1 ) )
{
  thatcol = c( Data_file[ , i ] )
  Data_elements = c( Data_elements, thatcol )
}

mat1 = matrix( Data_elements, Data_rows, Data_cols )

Decision_variables=c()
for( i in 1 : ( cols - rows ) )
{
  Decision_variables = c( Decision_variables, c(0) )
}

while( TRUE ) 
{
  objective_function = mat[ , cols ]
  min_row = which.min( objective_function )
  #print( min_row )
  
  ratio = matrix( c(0), 1, ( cols - 1 ) )
  for( i in 1 : cols )
  {
    if( mat[ min_row, i ] >= 0)
      ratio[i] = Inf
    else
      ratio[i] = abs( mat[ rows, i ] / mat[ min_row, i ] )
  }
  ratio = ratio[ 1 : ( cols - 1 ) ]
  #print( ratio )
  
  min_col = which.min( ratio )
  print( min_col )
  
  if( min_col >= 1 && min_col <= ( cols - rows ) )
  {
    Decision_variables[ min_col ] = min_row  
  }
  #print( Decision_variables )
  
  mat[ min_row, ] = mat[ min_row, ] / mat[ min_row, min_col ]
  
  for( i in 1 : rows )
  {
    if( i != min_row )
    {
      mat[ i, ] = mat[ i, ] - mat[ i, min_col ] * mat[ min_row, ]
    }
  }
  #print( mat )
  
  is_negative = sum( mat[ 1 : ( rows - 1 ), cols ] < 0 )
  if( is_negative == 0 )
    break
}

solution = matrix( c(0), 1, ( cols - rows ) )

for( i in 1 : ( cols - rows ) )
{
  if( Decision_variables[ i ] == 0 )
  {
    solution[ 1, i ] = 0
    #cat( "\nx", i, "= 0" )
  }
  else
  {
    solution[ 1, i ] = mat[ Decision_variables[ i ], cols ]
    #cat( "\nx", i, "=", mat[ Decision_variables[ i ], cols ] )
  }
}

#cat( "\nz =", mat[ rows, cols ] )
print( solution )

x1 = matrix( solution[ 1, 1 ], 19, 1 )
x2 = matrix( solution[ 1, 2 ], 19, 1 )
x3 = matrix( solution[ 1, 3 ], 19, 1 )
Deviation = abs( mat1[ , 2 ] - x1 ) + abs( mat1[ , 3 ] - x2 ) + abs( mat1[ ,  4 ] - x3 ) 

x = which.min( Deviation )

flag = 0

if(-Data_file[ x, 3 ] <= mat2[ 1 ] && Data_file[ x, 3 ] <= mat2[ 2 ] )
{
  if( -Data_file[ x, 4 ] <= mat2[ 3 ] && Data_file[ x, 4 ] <= mat2[ 4 ] )
  {
    if( -Data_file[ x, 5 ] <= mat2[ 5 ] && Data_file[ x, 5 ] <= mat2[ 6 ] )
    {
      flag = 1
      print( Data_file[ x,1 ], Data_file[ x,2 ], Data_file[ x,3 ], Data_file[ x,4 ] )
    }
  }
}

if( flag == 0 )
{
  print("No solution")
}