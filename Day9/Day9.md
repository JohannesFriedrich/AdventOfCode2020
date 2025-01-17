Day9
================
Johannes Friedrich
12/9/2020

## Puzzle 1

``` r
## read in the file
input <- scan(file = "input.txt", what = double(), sep="\n")
```

The idea:

-   write function to sum up all combinations in the vector of length
    (25, in general: variable).
    -   use the `combn` function to find all combinations and sums
-   write function to check the sums found before
-   main function:
    -   Start from index 1-25: get all sums -&gt; check. If failed: Go
        to 2-26 -&gt; get all sums -&gt; check, etc.

``` r
sumUp <- function(data, loopNr, length){
  
  temp <- data[(0+loopNr):(length+loopNr-1)]
  
  pairs <- combn(temp, 2, sum)

  return(pairs)
  
} 

check_nr <- function(sums, data, loopNr, length){
  
  list(nr = data[length+loopNr], 
       available = data[length+loopNr] %in% sums)
  
}

loop <- function(data, length){
  
  res <- list()
  i <- 1
  res$available <- TRUE
  while(res$available == TRUE){
  
    res <- data %>% 
      sumUp(i, length) %>% 
      check_nr(data, i, length)
  
    if(res$available == FALSE) {
      print("HOORAY")
      print(res$nr)
      return(res$nr)
    }
    i <- i + 1 
  }
}
```

``` r
invalid <- loop(input, 25)
```

    ## [1] "HOORAY"
    ## [1] 1721308972

## Puzzle 2

The idea is to start from the first index of the input vector and sum up
until the sum of the invalid nr. is reached or above. Then go to index
2, etc.

``` r
loop2 <- function(data, invalidNr){
  
  for(i in 1:length(data)){
    j <- 1
    sum_temp <- 0
  
    while(sum_temp < invalidNr){
    
      sum_temp <- sum(data[i:(i+j)])
      ifelse(sum_temp == invalidNr, break, j <- j +1)
    }
  
    if (sum_temp == invalidNr){
      print("HOORAY!")
      print(min(data[i:(i+j)]) + max(data[i:(i+j)]))
      break
    }
  }
}
```

``` r
loop2(input, invalid)
```

    ## [1] "HOORAY!"
    ## [1] 209694133
