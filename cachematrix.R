## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix sets the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  ## inverse is set to NULL every time a new makeCachematrix 
  ## is created  
	  inverse <- NULL 
  
  ## the set-function is declared 
  ## the new matrix is saved over the original x
  ## the cached inverse is reset
	  set <- function(y){
	    x <<- y
	    inverse <<- NULL 
	  }
  
  ## the get-function is declared
  ## it returns the matrix used by the intitialisation 
  ## or by the set function
	  get <- function() x
  
  ## declares the setinverse-function that 
  ## saves the passed inversed matrix into the cache
	  setinverse <- function(myinv) inverse <<- myinv
  
  ## declares the getinverse-function that
  ## returns the inveresed matrix from cache
  ## if setinverse has not been called before
  ## it returns null
	  getinverse <- function() inverse
  
  ## returns a list with four elements containing
  ## the functions declared above 
	  list(set = set, get = get,
	       setinverse = setinverse, getinverse = getinverse
	       )
}


## cacheSolve accepts as input a object created
## with makeCacheMatrix above. If there is a cached 
## inversed matrix it returns it directly, if not
## it calculates it and saves it into the cache and
## returns it. 

cacheSolve <- function(x, ...) {
  ## retrieves inverse from cache
	  inverse <- x$getinverse()
  
  ## if it is not null it returns the saved
  ## inversed matrix and exits
	  if(!is.null(inverse)){

	## prints out message to tell its using a 
	## cached inversed matrix
	    message("getting cached matrix")
	    return(inverse)
	  }
  
  ## if cache is empty it calculates the inversed matrix
  ## message tells that a calculation is being done
	  message("calculating inversed matrix")
 
	 ## retrieves the original matrix used in 
	 ## makeCacheMatrix inizialisation
	  data <- x$get()
  
  ## calculates the inverse
  inverse <- solve(data, ...)
  
  ## saves the inverse in the cache
  x$setinverse(inverse)
  
  ## returns the inversed matrix
  inverse
}

## Example of usage. 
## Initalising hubbe with a random three by three matrix
## 
## hubbe <- makeCacheMatrix(matrix(c(7, 5, 3, 1, -1, -2, 4, 6, 9), 3, 3))
## 
## using the get-method to show the matrix
## hubbe$get()
##     [,1] [,2] [,3]
## [1,]    7    1    4
## [2,]    5   -1    6
## [3,]    3   -2    9
## 
## solve the matrix the first time
## cache is empty so it is calculated and set
## > cacheSolve(hubbe)
## calculating inversed matrix
##             [,1] [,2]       [,3]
## [1,] -0.08823529  0.5 -0.2941176
## [2,]  0.79411765 -1.5  0.6470588
## [3,]  0.20588235 -0.5  0.3529412
##
## second time it uses the cached reslut
## > cacheSolve(hubbe)
## getting cached matrix
##            [,1] [,2]       [,3]
## [1,] -0.08823529  0.5 -0.2941176
## [2,]  0.79411765 -1.5  0.6470588
## [3,]  0.20588235 -0.5  0.3529412
## 
## cached result is returned again
## > cacheSolve(hubbe)
## getting cached matrix
##            [,1] [,2]       [,3]
## [1,] -0.08823529  0.5 -0.2941176
## [2,]  0.79411765 -1.5  0.6470588
## [3,]  0.20588235 -0.5  0.3529412