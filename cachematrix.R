##Contains two Fucntions: first makeCacheMatrix,where the input is a matrix and
## the function process it to a special "matrix" where the inverse matrix of the
##matrix can be cached
##second function cacheSolve gives you the inverse matrix as an output.

## makeCacheMatrix creats a list with 4 function:
## 1. function you can define a new matrix to your 
##variable via variable$set(new_matrix)
##2. function returns the matrix, which was assigned in the first function.
##3. functions assign the calculated inverse Matrix to im(=cached inverse Matrix)
##4. functions retrun the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        im<- NULL;
        set<- function(y){
                x<<-y
                im<<-NULL
        }
        get<- function() x
        setinverse<- function(inverse) im<<-inverse
        getinverse<- function() im
        list(set = set, get =get, setinverse = setinverse, 
             getinverse = getinverse)
        
}
##first the programm checks if there is already a cached inverse Matrix,
##if it exists it uses the cached matrix and return it.
##Otherwise the function will calculate it and and save the inverse 
##matrix in the cache.

cacheSolve <- function(x, ...) {
       im <- x$getinverse()
       if (is.matrix(im)){
               message("getting cached data")
               return(im)
       }
        matrix<- x$get() 
        im<- solve(matrix);
        x$setinverse(im)
        im
         
}
