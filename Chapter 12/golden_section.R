
# Golden Section Search for finding learning rate
# 
# Requires line about to minimize
# Requires function
# Requires step_size
# Requires Maximum Iteration 
# Requires tolerance for breaking condition
# Requires mask boolean for output
golden_search = function(line, fun, step_size, maxIter, tol, mask) {
  
  fa = fun(line(0))
  
  bprev = 0
  fbprev = fa
  
  second_bprev = -step_size
  second_fbprev = fun(line(-step_size))
  
  if(!mask) {
    print("   INTERVAL LOCATION SEARCH   ")
    print("Initial step: 0")
    print(sprintf("Fb0: %f", fbprev))
  }
  index = 1
  
  while(TRUE) {
    
    b = step_size*index
    x = line(b)
    fb = fun(x)
    
    if(!mask) {
      print(sprintf("Step increase: %f", b))
      print(sprintf("Fb: %f", fb))
    }
    
    
    
    if(fbprev < fb) { # increase in function within two consecutive iterations
      
      if(!mask) {
        print("INCREASE IN FUNCTION FOUND:")
        print(sprintf("Interval: [%f, %f]", second_bprev, b))
        print("")
      }
      
      break
    }
    second_bprev = bprev
    second_fbprev = fbprev
    
    bprev = b
    fbprev = fb
    index = index * 2
  }
  
  
  
  if(!mask) {
    print("   INTERVAL LINEAR MINIMIZATION   ")
  }
  
  a = second_bprev
  fa = second_fbprev
  tau = 0.618
  for(i in 1:maxIter) {
    
    if(abs(b-a)<tol) {
      print(" ALGORITHM CONVERGED ")
      print(sprintf("Iterations taken: %d", i))
      print(sprintf("Learning Rate: %f", a))
      return(a)
    }
    
    c = a + (1-tau)*(b-a)
    d = b-(1-tau)*(b-a)
    
    fc = fun(line(c))
    fd = fun(line(d))
    
    if(!mask) {
      print(sprintf("  ITERATION %d", i))
      print(sprintf("c point: %f, fc: %f", c, fc))
      print(sprintf("d point: %f, fd: %f", d, fd))
    }
    
    if(fc > fd) {
      
      a=c
      c=d
      d=b-(1-tau)*(b-a)
      fa=fc
      fc=fd
      fd = fun(line(d))
      if(!mask) {
        print("F(c) > F(d)")
        print(sprintf("a=c,c=d, d=%f, fd=%f", d, fd))
      }
      
    }
    else {
      
      b=d
      d=c
      c=a+(1-tau)*(b-a)
      fb=fd
      fd=fc
      fc = fun(line(c))
      if(!mask) {
        print("F(c) < F(d)")
        print(sprintf("b=d,d=c, c=%f, fc=%f", c, fc))
      }
    }
    
  }
  
  print(" MAXIMUM ITERATIONS TAKEN")
  print(sprintf("Current Learning Rate: %f", (b+a)/2))
  print(sprintf("Error: %f", abs(b-a)))
  return((b+a)/2)
}

# function
fun = function(X) {
  A = matrix(c(10, -6, -6, 10), ncol=2, nrow=2)
  d = matrix(c(4,4),nrow=1)
  0.5*t(X)%*%A%*%X+d%*%X
}

# line about to minimize
line = function(alpha) {
  x0 = matrix(c(-1,-2.5), nrow=2)
  g0 = matrix(c(9,-15), nrow=2)
  x0+alpha*g0
}


# Call the function for first three iterations
rate=golden_search(line, fun, 0.075, 3, 1e-10, 0)



# Now I call the function to run until convergence:
rate=golden_search(line, fun, 0.075, 100, 1e-10, 1)