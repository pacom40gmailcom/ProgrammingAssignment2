## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Francisco García Mateo
    ## pacom40@gmail.com
    ## December, 21th of 2017
    
    ## Initialice inverse matrix to null
    inverseMatrix <- NULL
    
    # state the value of matrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # get value of matrix
    get <- function() x
    
    # set inverse matrix calculatin with solve function
    setinverse <- function(solve) inverseMatrix <<- solve
    
    # get inverse matrix
    getinverse <- function() inverseMatrix
    
    # set list values
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x = matrix()){
    ## Francisco García Mateo
    ## pacom40@gmail.com
    ## December, 21th of 2017
    
    # Number of rows and columns
    nrow <- dim(x$get())[1] # Number of rows
    ncol <- dim(x$get())[2] # Number of columns
    
    # If it isn't a square matrix isn't possible to calculate
    if (nrow != ncol) {
        message("It's not possible calculate inverse matrix. It's not a square matrix")
        return(x)
    }
    
    # It's a square matrix
    # Get the inverse matrix
    inversematrix <- x$getinverse()
    
    #It's not null. Recover from cache 
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    
    # Inverse matrix isn't in cache
    previousmatrix <- x$get()
    
    # Calculate the inverse matrix
    inversematrix <- solve(previousmatrix)
    
    #Set Invermatrix param
    x$setinverse(inversematrix)
    
    # return inverse matrix
    inversematrix
    
} 