Day3
================
Johannes Friedrich
12/3/2020

## Puzzle 1

``` r
## read in the file
input <- readLines("input.txt")
```

``` r
tree <- 0

## helper function to get right position in the string vector: if the input nr can be divided by 31 (the length of each line) with a rest, then just return the mod value. If it is exactly a multiple of 31, then return

which_element <- function(nr){
    
  if(nr %%31 != 0){
    return(nr %% 31)
  } else {
    ## return(nr - ((nr %/%31)-1)*31)
    return(31)
  }
}

for (line in 1:(length(input))){
  
  ## which element to choose?
  n <- which_element((line-1)*3+1)
  # print(c(n, (line-1)*3+1))
  
  character <- stringr::str_split(input[line], "") %>% 
    unlist() %>%
    nth(n) 
  
  if (character == "#") tree <- tree+1
  ##if (character == "#")
  
  
    
}
tree
```

    ## [1] 237

## Puzzle 2

A modification of the used for-loop above. Now we can adjust the
stepwidth in each line and also the line step (needed for the last slop
1 - 2)

``` r
walk <- function(offset, offset_line){
  
  trees <- 0
  step <- 0

  for (line in seq(1,length(input), offset_line)){
  
    ## which element to choose?
    n <- which_element(step*offset+1)
    step <- step +1

    ## Check if current character is a tree #
    isTree <- stringr::str_split(input[line], "") %>% 
      unlist() %>%
      nth(n) %>% 
      str_detect("#")
    
    ## Sum up trees
    trees <- trees + isTree
    
  }
  return(trees)
}
```

``` r
walk(1,1) *walk(3,1) * walk(5,1) * walk(7,1) * walk(1,2)
```

    ## [1] 2106818610