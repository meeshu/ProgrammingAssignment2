##There are 2 functions- makeCacheMatrix which creates a cache matrix that holds the the most recently 
##calculated inverse of the matrix if the input matrix is different. The second function cacheSolve checks 
##If the input matrix is same or not. If it is, then it retrieves from cache else it
##calculates the inverse for new matrix, prints it and assigns it to cache overwriting the previous one

## The function creates a special matrix that acts like a cache

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setinverse<- function(inv) m<<-inv
      getinverse<- function() m
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This will return th inverse if present on cache else calculates it/ outputs it and places it on cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached inversed data ")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
