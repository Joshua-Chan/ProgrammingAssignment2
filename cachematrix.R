## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}






## Testing

a1 <- c(3, 2, 5) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 

# bind the three vectors into a matrix  
# using rbind() which is basically 
# row-wise binding. 

A <- rbind(a1, a2, a3) 
solve(A)

test <- makeCacheMatrix(A)
cacheSolve(test)
cacheSolve(test) # should say "getting cached data"


