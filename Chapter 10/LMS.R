
# Least Mean Square Algorithm

# Requires init weight matrix
# Requires init bias matrix
# Requires learning rate
# Requires input vectors
# Requires expected output vectors
# Requires maximum iterations
# Requires tolerance for exiting condition
# Requires mask boolean for whether or not to print out values inbetween iterations
# Requires useBias boolean to designate whether or not to include bias in calcualtions

LMS = function(init, bias, learning_rate, input, expect, maxIter, tol, mask, useBias) {
  
  n = ncol(input)
  w = init
  b = bias
  
  for(i in 1:maxIter) { # loop over iterations
    
    if(!mask) { # if the user does want to print out statements
      print(sprintf("     Iteration %d", i))
    }
    
    for(j in 1:n) { # loop over input vectors
      
      if(useBias) { # does the user want to include a bias
        a = w%*%input[,j]+b
      }
      else {
        a = w%*%input[,j]
      }
      
      # Error
      e = expect[j] - a 
      
      # New weight
      w = w + 2*learning_rate*e%*%t(input[,j])
      
      if(useBias) { # update bias
        b = b + 2*learning_rate*e
      }
      
      if(!mask) { # if the user does want to print out statements
        print(sprintf("INPUT VECTOR: %d", j))
        print("a value: ")
        print(a)
        print("error value: ")
        print(e)
        print("new weight value: ")
        print(w)
        print("new bias value: ")
        print(b)
      }
    }
    
    
    error = 0 # calculate total error for all vectors
    
    for(j in 1:n) {
      
      if(useBias) { # does the user want to include a bias
        a = w%*%input[,j]+b
      }
      else {
        a = w%*%input[,j]
      }
      
      e = expect[j] - a
      
      error = error + e^2
      
    }
    
    # if the square root of the sum of the square
    # errors is greater than the tol, then algo
    # has not converged
    converged = 1
    if(norm(error, "f")>tol) { 
      converged = 0
    }
    
    if(converged) {
      print("    Algorithm CONVERGED:")
      print("Current weight is: ")
      print(w)
      if(useBias) { # does the user want to include a bias
        print("Current bias is: ")
        print(b)
      }
      
      print(sprintf("Iterations taken: %d", i))
      if(useBias) { # does the user want to include a bias
        return(list(w=w, b=b))
      }
      return(list(w=w))
    }
    
    
  }
  
  # Reached end of iterations, so calculate current error
  
  error = 0
  
  for(j in 1:n) {
    
    a = w%*%input[,j]+b
    
    e = expect[j] - a
    
    error = error + abs(e)
    
  }
  
  print("MAXIMUM ITERATIONS REACHED")
  print("Current weight is: ")
  print(w)
  if(useBias) { # does the user want to include a bias
    print("Current bias is: ")
    print(b)
  }
  print(sprintf("Iterations taken: %d", i))
  print("ERROR: ")
  print(error)
  if(useBias) { # does the user want to include a bias
    return(list(w=w, b=b))
  }
  return(list(w=w))
  
}

# example input

## class 1
p1 = matrix(c(1, 1, 1, 0, 0, 1, 0,0,0,1,0,0,0,1,0,0), ncol=1, byrow = TRUE)
p2 = matrix(c(0, 1, 1, 1, 0, 0, 1, 0,0,0,1,0,0,0,1,0), ncol=1, byrow = TRUE)

## class 2
p3 = matrix(c(1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0), ncol=1, byrow = TRUE)
p4 = matrix(c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1), ncol=1, byrow = TRUE)

# combined input vectors 
p = matrix(c(p1, p2, p3, p4), ncol=4, byrow=FALSE)

# expected output for each input vector
expected = c(1, 1, -1, -1)

# initial starting points
init = matrix(0, ncol=16)

val = LMS(init, 0, 0.01, p, expected, 1000, 1e-10, 1, 1)

val$w%*%p1+val$b
val$w%*%p2+val$b
val$w%*%p3+val$b
val$w%*%p4+val$b