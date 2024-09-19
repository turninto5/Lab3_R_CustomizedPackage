euclidean <- function(num1, num2){
  # ensure num1 is smaller to decrease the times of iteration
  if (num1 > num2){
    n <- num1
    num1 <- num2
    num2 <- n
  }
  
  # a potential shortcut
  if (num2 %% num1 == 0)
    return (num1)
  
  
  # if the shortcut doesn't hit, define it
  trial_division_fractorization <- function(x){
    # use a hash table to record all of prime factor of x
    
    # how to acquire keys in brief, using all()
    prime_list <- function(x){
      if(x <2)
        return(c())
      primes <- c(2)
      for (i in 3:x){
        if (all(i %% 2:(i-1) != 0))
          primes <- c(primes, i)
      }
      return(primes)
    }
    
    keys <- prime_list(x = as.integer(sqrt(x)))
    # use a new environment to implement hash table
    hash_tab <- new.env(hash = TRUE)
    for (key in as.character(keys)){
      assign(x = key, value = 0, envir = hash_tab)
      # hash_tab[[key]] <- 0
      # !!!!! keys are character !!!!!
      # double square brackets represent operation on a single variable
    }
    
    # find the factors of x
    
    i <- 1
    n <- length(keys)
    while (i <= n){
      if (x %% keys[i] == 0){
        # value ++
        hash_tab[[as.character(keys[i])]] <- hash_tab[[as.character(keys[i])]] + 1
        x <- x / keys[i]
        
        if (x %in% keys) {
          # value ++
          hash_tab[[as.character(x)]] <- hash_tab[[as.character(x)]] + 1
          break
        }
        next # like 4 can be divided by 2 for twice
      }
      i <- i+1
    }
    
    return(hash_tab)
  }
  
  # the prime factors and its frequency
  # type: env
  prime_factors1 <- trial_division_fractorization(num1)
  prime_factors2 <- trial_division_fractorization(num2)
  
  # list of prime factor or keys
  keys1 <- ls(envir = prime_factors1)
  keys2 <- ls(envir = prime_factors2)
  
  # the greatest common divisor
  gcd <- 1
  
  # iterate the shorter one
  for(key in keys1){
    if(get(x = key, envir = prime_factors1) != 0 & key %in% keys2){
      value1 <- get(x = key, envir = prime_factors1)
      value2 <- get(x = key, envir = prime_factors2)
      
      gcd <- gcd * as.numeric(key) ** min(value1, value2)
    }
  }
  
  return(gcd)
}
