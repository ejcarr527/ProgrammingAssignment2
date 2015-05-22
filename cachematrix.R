## 
##      The functions here will demonstrat the lexical scoping capabilities that
##      the R programming language has.  Primarily this means that R has a 
##      hierarchical set of environments where we are able to create functions
##      within functions and complete assignment states to variables in other
##      environments (ancestors).
##
##      Specifically, this assignment is to cache an inverse to a matrix.
##      Each time that we try to inverse a matrix it can be very expensive
##      in computing costs.  If we can cache the matrix and we determine that
##      nothing needs to be done then we can save some CPU cycles and this
##      could save on performance.
##

##
##      Function:       makeCacheMatrix()
##
##      Description:    
##
##      Input:          - x     :       Existing matrix -- if none given new matrix defined

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
                
                ##
                ##       Just print out the environment for fun
                ##
                
                print(environnment())
        }
        
        get<-function() {
                x
        }
        
        setmatrix<-function(solve) {
                m<<- solve
        }
        
        getmatrix<-function() {
                m
        }
        
        list(set=set, get=get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        d<-x$get()
        m<-solve(d, ...)
        x$setmatrix(m)
        m
}
