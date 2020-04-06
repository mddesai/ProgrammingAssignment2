## These functions help solve the issue of certain costly calculations like the inverse of a matrix by caching

## Here we assume that supplied matrix is always invertible

## makeCacheMatrix creates a special "matrix" object that caches the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of any (inversible) matrix returned by "makeCacheMatrix()"
## If the inverse for a certain matrix has been calculated in prior computations, then this function is able to
## retrieve it.

cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()
  if (!is.null(i)) 
  {
    message("Retrieving Data from Cache!")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Testing above function using 2x2 matrix (1,2,3,4)

# 1. Create Matrix
B = matrix(c(1,2,3,4),2,2)

# 2. Create special Matrix
B1=makeCacheMatrix(B)

# 3. Solve for inverse of Matrix
cacheSolve(B1)

# 4. Retrieve Matrix from Cache
cacheSolve(B1)
