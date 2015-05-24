## To eliminate repetitive calculations for matrix inverse, this code allows us to cache the inverse of a matrix
## without repeated computations if the value already exists 

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){                               ##set the value of the matrix
              x<<-y  
              m<<-NULL
        }
        get <- function() {                             ##get the matrix
                x
        }
        setinverse <- function(inverse){                ##set the inverse of the matrix
                m <<- inverse
        }
        getinverse <- function(){                       ##get its inverse 
                m      
        } 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()        
        if(!is.null(m)) {                               ##if inverse exists, return it           
                message("getting cached inverse")
                return(m)                               ##we get the value and exit
        }
        data <- x$get()                                 ##otherwise compute it 
        if(det(data)==0){                               ##test if determinent is zero
                m<-NULL                                 ##if true then no inverse exists
                message("this matrix doesn't have an inverse")
        }else{                                          #else we can compute its inverse
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}
