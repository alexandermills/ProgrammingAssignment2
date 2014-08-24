## The special matrix structure is defined by makeCacheMatrix function. 
## This special matrix structure can hold inverse of matrix x (in cache) and is considered 
## as user-defined class. The function cacheSolve allows to get the inverse of x either 
## by calculation or by extracting from cache (if the defined special matrix structure does not change)

## The main problem with these functions is that if we re-define x (putting it in makeCacheMatrix) 
## and x does not differ from previously defined x the function cacheSolve 
## will be recalculate inverse matrix instead of getting it from cache


## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        setMx<-function(z) {
		message("creating new special matrix")
		x<<-z
		inv_x<<-matrix()	
			
	}
	getMx<-function() x
 	setInvMx<-function(inv_y) inv_x<<-inv_y
	getInvMx<-function() inv_x
		
	list(setMx = setMx(x), getMx = getMx,
	                setInvMx = setInvMx,
      	                getInvMx = getInvMx)


}


## The function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {


         
	y<-x$getMx()
	
	inv_y <- x$getInvMx()
        if(is.null(inv_y) || is.na(inv_y) ) {
      	        ## in case of absence of inverse matrix in cache
		inv_y<-solve(y)		
		x$setInvMx(inv_y)	
		          	
        } else {
		message("getting cached data")
    	}
	
	inv_y

}
