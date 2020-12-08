Day6
================
Johannes Friedrich
12/6/2020

## Puzzle 1

``` r
## read in the file

input <- scan(file = "input.txt", what = character(), sep="\n", blank.lines.skip=FALSE) %>% 
  as.data.frame() %>% 
  rename("data" =".") %>% 
  mutate(id = cumsum(. == '')) %>% ## create IDs
  filter(data != "") ## remove empty lines now
```

``` r
df <- input %>% 
  mutate(char = str_split(data, "")) %>% 
  group_by(id) %>% 
  summarise(uni_char = length(Reduce(union, char))) %>% 
  summarise(res = sum(uni_char))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
df
```

    ## # A tibble: 1 x 1
    ##     res
    ##   <int>
    ## 1  6587

## Puzzle 2

``` r
df <- input %>% 
  mutate(char = str_split(data, "")) %>% 
  group_by(id) %>% 
  summarise(uni_char = length(Reduce(intersect, char))) %>% 
  summarise(res = sum(uni_char))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
df
```

    ## # A tibble: 1 x 1
    ##     res
    ##   <int>
    ## 1  3235
