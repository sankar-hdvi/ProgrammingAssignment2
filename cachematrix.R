## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  ## Initialize the output
  output <<- NULL
  
  ## Function to get the input data
  get <- function() 
  {
    x
  }
  
  ## Function to get the cached matrix
  getdata <- function()
  {
    output <- matrixData 
    return (output)
  }
  
  ## Function to set the data to matrix
  setdata <- function(Inputdata) 
  {
    matrixData <<- Inputdata
  }
  
  ## Function to get the cached inverse
  getInverse<- function()
  {
    output <- cachedInverse 
    return (output)
  }
  
  ## Function to set the cached inverse
  setInverse<- function(locdata)
  {
    cachedInverse <<- locdata
  }
  
  ## Output the list of functions
  list( get = get, setdata = setdata, getdata= getdata,getInverse = getInverse,setInverse = setInverse)
      
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  ## First to make sure that there is an inversed matrix available in the cache
  cachedInverse <- x$getInverse()
  
  ## If the inverse has already been calculated (and the matrix has not changed
  ## , then the it retrieves the inverse from the cache.
  if((!is.null(cachedInverse)) & (identical(x$get(), x$getdata())))
  {
    print("getting cached data")
    return(cachedInverse)
  }
  
  ## The data seems to be new, get it
  newInputdata <- x$get()
  
  ## Calculate the inverse of a matrix
  cachedInverse <- solve(newInputdata)
  
  ## Cache the new input data
  x$setdata(newInputdata)
  
  ## Cache the new computed inverse object
  x$setInverse(cachedInverse)
  
  ## returned the inverse object
  cachedInverse
}

