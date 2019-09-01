## This program is used to create a special matrix and then find its inverse
## It also involves caching the inverse for future use.

## makeCacheMatrix function is written to create a special matrix and cache its inverse
## inverted_matrix is a variable that stores the inverse of matrix
## Value of inverted_matrix is set when we use solve() in cachSolve function

makeCacheMatrix <- function(x = matrix()) 
   {
        inverted_matrix <- NULL
        set <- function(y) 
        {
                x <<- y
                inverted_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverted_matrix <<- inverse
        getinverse <- function() inverted_matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

    }


## cacheSolve function is used to find the inverse of a matrix if it is not calculated earlier
## It is also used to fetch the cached value of inverse of matrix if it has already been calculated in the past

cacheSolve <- function(x, ...) 
    {
        inverted_matrix <- x$getinverse()
        if (!is.null(inverted_matrix)) 
        {
          message("Getting Cached Data")
          return(inverted_matrix)
        }
        data <- x$get()
        inverted_matrix <- solve(data, ...)
        x$setinverse(inverted_matrix)
        inverted_matrix
    }
