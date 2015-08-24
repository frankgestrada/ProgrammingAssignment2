makeCacheMatrix <- function(x = matrix()) {
M <- NULL
set <- function(y) {
x <<- y
M <<- NULL
}
get <- function() x
invM <- function(inverse) M<<- inverse
retM <- function() M
list(set=set, get=get, invM=invM, retM=retM)
}

cacheSolve <- function(x, ...) {
M<- x$retM()
if(!is.null(M)) {
message("getting cached data.")
return(M)
}
Matrix<- x$get()
M<- solve(Matrix)
x$invM(M)
M
}
