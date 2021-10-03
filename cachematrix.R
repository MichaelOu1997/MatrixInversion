## We want to inverse any matrix and cache the result.

## In the frist function, I am going to create a list of 4 elements: set, get,  
## setm and getm for storing the value an caching the result of matrix inversion.

makeCacheMatrix <- function(x = matrix()){
              m <- NULL
              set <- function(y){
                x <<- y
                m <<- NULL
              }
              get <- function()x
              setm <- function(inverse) m <<- inverse
              getm <- function() m
              list(set = set, get = get,
                   setm = setm,
                   getm = getm)
}


## Inverse the matrix and Cache the results!

cacheSolve <- function(x, ...) {
         m <- x$getm()
         if(!is.null(m)){
                  message("getting cached data")
                  return(m)
         }
         data <- x$get()
         m <- solve(data,...)
         x$setm(m)
         m
}
## check the program to ensure we are good

m <- matrix(sample(1:1000,9),3,3)
First <- makeCacheMatrix(m)
Two <- cacheSolve(First)
