## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# usage: 
# note: mat1 must have an inverse (see https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html)
# mat1 <- matrix(c(5,1,0,3,-1,2,4,0,-1), nrow=3, ncol=3, byrow=TRUE)
# cm <- makeCacheMatrix(mat1)
# cacheSolve(cm)
# cacheSolve(cm)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
