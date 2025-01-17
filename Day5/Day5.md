Day5
================
Johannes Friedrich
12/5/2020

## Puzzle 1

``` r
## read in the file line by line
input <- scan("input.txt", what=character(), sep="\n",blank.lines.skip=FALSE)
```

``` r
row <- str_sub(input, 1, 7)
col <- str_sub(input, -3)
```

``` r
upper_half <- function(vec){
  
  return(vec[((length(vec)/2)+1):length(vec)])
  
}

lower_half <- function(vec){
  
  return(vec[1:(length(vec)/2)])
  
}

get_result <- function(input, check_character_lower, check_character_upper, max_nr){
  
  characters <- unlist(str_split(input, ""))
  final <- 0:max_nr
  
  for(c in 1:length(characters)){
    
    if(characters[c] == check_character_lower){
      final <- lower_half(final)
    }
    if(characters[c] == check_character_upper){
      final <- upper_half(final)
    }
  
  }
  
  return(final)
  
}
```

``` r
row_final <- mapply(get_result, row, "F", "B", 127)
col_final <- mapply(get_result, col, "L", "R", 7)
```

``` r
df <- data.frame(
  input = input, 
  row = row,
  col = col,
  row_final = row_final,
  col_final = col_final,
  seat_id = row_final*8+col_final
)
```

``` r
df %>% 
  summarise(max(seat_id))
```

    ##   max(seat_id)
    ## 1          813

\#\# Puzzle 2

``` r
df %>% 
  filter(row_final != 0,
         row_final != 127) %>% 
arrange(seat_id)  %>% 
  mutate(diff = seat_id -lag(seat_id)) %>% 
  filter(diff == 2) %>% 
  select(row_final, col_final,seat_id) %>% 
  mutate(seat_id) -1
```

    ##   row_final col_final seat_id
    ## 1        75         4     612
