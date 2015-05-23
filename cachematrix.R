## Together, these two functions will allow me to define a matrix and
## quickly call up its inverse, calling on a cache if one exists to
## save some time. Apparently my matrices are big and my time is
## very, very valuable.

## makeCacheMatrix will hold a defined matrix and a given cache
## that is passed to it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## initializes m, which will serve as the cache
  set <- function(y) { ## function will set the matrix
    x <<- y ## defines x of a parent function
    m <<- NULL ## whenever we set the matrix, we need to reset the cache
  }
  get <- function() { ## function will return the matrix
    x
  }
  savecache <- function(cache) { ## function will save the cache to m
    m <<- cache
  }
  getcache <- function() { ## function will show the cache
    m
  }
  list(set = set, get = get,
       savecache = savecache,
       getcache = getcache) ## names a list of functions
}


## cacheSolve will check to see if a given matrix's inverse has been
## cached already. If it has, it will return the cache. Otherwise,
## it will invert the matrix, save the inversion to a cache, and 
## then print the inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getcache() ## assigns m from makeCacheMatrix's cache for 'x'
  if(!is.null(m)) { ## reacts if the cache is defined
    message("getting cached data") ## signals that data is cached
    return(m) ## prints cache
  }
  data <- x$get() ## pulls the matrix 'x', assigns to 'data'
  m <- solve(data, ...) ## inverts matrix and assigns it to amount 'm'
  x$savecache(m) ## saves 'm' to the cache
  m ## prints this amount 'm'
}

## Use these functions on the command line by introducing a matrix 
## 'p' with 'makeCacheMatrix(p)' and solving it with 
## 'cacheSolve(makeCacheMatrix(p))'. The easiest way may be to take 
## it in steps, as follows:
##
## 1. Define the matrix.
### > p <- matrix(1:4, nrow=2)
##
## 2. Introduce the matrix.
### > platypus <- makeCacheMatrix(p)
##
## 2.5 (optional) Check the matrix has been introduced correctly.
### > platypus$get()
##
## 3. Solve the introduced matrix.
### > cacheSolve(platypus)