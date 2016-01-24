## This function inverse a matrix by using cache.

## The first function, makeVector creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse_matrix
##get the value of the inverse_matrix

makeCacheMatrix <- function(x = matrix()) {
        InvMat<-NULL
        set<-function(y){
                x<<-y
                InvMat<<-NULL           

        }
        get<-function() x
        setInvMat<-function(inverse_matrix) {
                InvMat<<-inverse_matrix
        }
        getInvMat<-function() InvMat
        list(set=set, get=get, getInvMat=getInvMat,
                setInvMat=setInvMat)
}


## The following function calculates the mean of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse_matrix has already been calculated. If so, it gets 
## the inverse_matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse_matrix of the data and sets 
## the value of the inverse_matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        InvMat<-x$getInvMat()
                if(!is.null(InvMat)){
                        message("getting cached data")
                        return(InvMat)
                }
                data<-x$get()
                InvMat<-solve(data)
                x$setInvMat(InvMat)
                InvMat
}
