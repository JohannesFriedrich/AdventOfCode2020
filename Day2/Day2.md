Day2
================
Johannes Friedrich
12/2/2020

## Puzzle 1

``` r
## read in the file
input <- read_delim("input.txt", ":", 
    escape_double = FALSE, col_names = FALSE, 
    trim_ws = TRUE, col_types = cols(
  X1 = col_character(),
  X2 = col_character()
))
```

``` r
all_data <- lapply(1:nrow(input), function(x){
  
  data_temp <- input$X1[x]
  
  ## Split strings
  temp <- stringr::str_split(data_temp, " ") 
  nr <- unlist(stringr::str_split(temp[[1]][1],"-"))
  
  ## return a data-frame for each row
  return(data.frame(
    min = as.numeric(nr[1]),
    max = as.numeric(nr[2]),
    letter = temp[[1]][2],
    code = input$X2[x]
  ))
  
}) %>% 
  bind_rows()
```

``` r
## Count rowwise all letters of type ' letter' in column `code`
## add new column *``correct` if the value of `count` is in between min and max
## Count all correct and non-correct codes
all_data %>% 
  rowwise() %>% 
  mutate(count = stringr::str_split(code, "") %>% 
           unlist() %>% 
           str_count(letter) %>% 
           sum(),
         correct = count <= max & count >= min) %>% 
  group_by(correct) %>% 
  summarise(n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   correct `n()`
    ##   <lgl>   <int>
    ## 1 FALSE     533
    ## 2 TRUE      467

## Puzzle 2

``` r
all_data2 <- lapply(1:nrow(all_data), function(x){
  
  data_temp <- all_data[x,] 
  
    temp <- stringr::str_split(data_temp$code, "") %>% 
           unlist() %>% 
           str_count(data_temp$letter)%>% 
      {which(. %in% 1)}
    
    result <- c(which(data_temp$min %in% temp), which(data_temp$max %in% temp ))
    
    if (length(result) == 1)
      return(TRUE)
    else
      return(FALSE)
    
}) %>% 
  unlist() %>% 
  sum()
```
