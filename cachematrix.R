## makeCacheMatrix function and the cacheSolve functions are to be used in tandem;
## These functions are written as a part of the R Programming Assignment 2 showing the caching feature of R
## makeCacheMatrix creates a list object that can perform certain functions on a matrix and return the value to us 
## cacheSolve function will check if inverse of a given matrix exists and will return that value and if not, return the inverse of the matrix and cache the value

## makeCacheMatrix function creates a list object that can perform certain manipulations on a given matrix
## get function will get the matrix value
## set function will initiate the matrix
## getinv will get the inverse of the matrix - if not stored before, will give NULL
## setinv will initiate the inverse of a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
 ##Initiating the inv_matrix variable as NULL - this will hold the inverse of our matrix x  
 
    inv_matrix <- NULL

    get <- function() x
    set <- function(y)
    {
        x <<- y
        ## when we initiate a matrix, we are assigning its inv_matrix as NULL to ensure that whenever the matrix changes, we reset the inv_matrix value in the cache
         inv_matrix <<- NULL
       
    }
    getinv <- function() inv_matrix
    setinv <- function(y)
    {
        ## this is where the magic happens - inverse value is cached
         inv_matrix <<- y
        
    }
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve function will check if inverse value of a matrix exists, if yes, will return the cached value if not will calculate the value

cacheSolve <- function(x, ...) {
     
    inv_matrix <- x$getinv()

    ##Checking if the inverse value exists in the cache and return that value
    if (!is.null(inv_matrix))
    {
        message("Cached Matrix")
        return (inv_matrix)
    }
    inv_matrix <- solve(x$get())
    ##Setting the inverse value of the matrix
    x$setinv(inv_matrix)
    return(inv_matrix)

}

##  You can test the functions using the following code after loading the R file on your environment - 
## Here we are initiating 2 matrices and storing their inverses and are checking that the function indeed works as designed
##  x <- matrix(1:4, nrow = 2, ncol = 2)  
##  y <- matrix(3:7, nrow = 2, ncol = 2)
##  m1 <- makeCacheMatrix(x)
##  m2 <- makeCacheMatrix(y)
##  cacheSolve(m1)
##  cacheSolve(m2)