Day8
================
Johannes Friedrich
12/8/2020

## Puzzle 1

``` r
## read in the file

input <- scan(file = "input.txt", what = character(), sep="\n", blank.lines.skip=FALSE)

df <- input %>% 
  as.data.frame() %>% 
  rename("data" =".") %>% 
    mutate(id = row_number()) %>% 
  rowwise() %>% 
  mutate(instr = word(data, 1),
         comm = str_split(data, " ") %>% 
           unlist() %>% 
           {.[[2]]} %>% 
           as.numeric(),
         visited = c(FALSE)) %>% ## create ID %>% 
  ungroup()
```

``` r
find_acc <- function(data){
  
  current_line <- 1
  acc_value <- 0
  data$visited <- FALSE

  
  
while (data$visited[current_line] != TRUE) {
  
  current_comm <- data$instr[current_line]
  
  if (current_comm == "acc"){ 
    acc_value <- acc_value + data$comm[current_line]
    data$visited[current_line] <- TRUE
    current_line <- current_line + 1
  }
  
  if (current_comm == "jmp"){ 
    data$visited[current_line] <- TRUE
    current_line <- current_line + data$comm[current_line]
  }
  
  if (current_comm == "nop"){ 
    data$visited[current_line] <- TRUE
    current_line <- current_line + 1
  }
  
  if (current_line >= nrow(data)) {
    print("Run complete!")
    print(acc_value)
    break
  }
  
  
}
  
  return(acc_value)
}

print(find_acc(df))
```

    ## [1] 2058

## Puzzle 2

``` r
fix <- function (data, i) {
  op <- data$instr[i]
  fixed <- data
  if (op == "jmp"){
    fixed$instr[i] = 'nop'
  }
  if (op == "nop"){
    fixed$instr[i] = 'jmp'
  }
  fixed
}
```

``` r
for(i in 1:nrow(df)) {
df %>% 
  fix(i) %>% 
  find_acc()
}
```

    ## [1] "Run complete!"
    ## [1] 1000
