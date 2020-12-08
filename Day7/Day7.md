Day7
================
Johannes Friedrich
12/7/2020

## Puzzle 1

``` r
## read in the file

input <- scan(file = "input.txt", what = "", sep="\n") %>% 
  as.data.frame() %>% 
  rename("data" =".") %>% 
  mutate(id = cumsum(. != ' '))
  

test <- scan(file = "test.txt", what = "", sep="\n") %>% 
  as.data.frame() %>% 
  rename("data" =".") %>% 
  mutate(id = cumsum(. != ' '))
```

``` r
## exctract numbers
nr_childs_in_line <- function(line){ 
  stringr::str_extract_all(line, "\\d") %>% 
  unlist() %>% 
  as.integer()
}


get_children <- function(line){
  
  length <- nr_childs_in_line(line) %>% 
    length()
  
  lapply(1:length, function(x){
    
     stringr::word(line,x*3+1, x*3+2)
    
  }) %>% 
    unlist()
  
}

remove_unness_data <- function(line){
  line %>% 
    str_remove_all(c("bags" = "", "bag" = "","contain " = "", ","= "", " \\." = "")) %>% 
    str_replace_all("  ", " ")
  
}

df <- input %>% 
rowwise() %>% 
  mutate(line_cleaned_one_element = remove_unness_data(data), 
         father  = stringr::word(line_cleaned_one_element,1, 2),
         children = map(line_cleaned_one_element, get_children),
         nr_bags_in_line =  map(data, nr_childs_in_line))
```

``` r
children = c()

children_diff <- "shiny gold"

repeat{
  
  children_new <- lapply(children_diff, function(child){
    
      df %>% 
      unnest(children) %>% 
      rowwise() %>% 
      filter(children == child) %>% 
      select(father) %>% 
      ungroup()
  }) %>% 
    unlist() %>% 
    unname()
  
  if (all(children_new %in% children)) break
  
  children <- union(children, children_new)
  children_diff <- intersect(children, children_new)

}

length(children)
```

    ## [1] 272

## Puzzle 2

``` r
children = c()

children_diff <- "shiny gold"

repeat{
  
  children_new <- lapply(children_diff, function(child){
    
      df %>% 
      unnest(children) %>% 
      rowwise() %>% 
      filter(children == child) %>% 
      select(father) %>% 
      ungroup()
  }) %>% 
    unlist() %>% 
    unname()
  
  if (all(children_new %in% children)) break
  
  children <- union(children, children_new)
  children_diff <- intersect(children, children_new)

}

length(children)
```

    ## [1] 272
