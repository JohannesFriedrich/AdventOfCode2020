Day4
================
Johannes Friedrich
12/4/2020

## Puzzle 1

``` r
## read in the file line by line
input <- scan("input.txt", what="", sep="\n",blank.lines.skip=FALSE) %>% 
  as.data.frame() %>% 
  rename("data" =".") %>% 
  mutate(id = cumsum(. == '')) %>% ## create IDs
  filter(data != "") ## remove empty lines now
```

``` r
# create data.frame 
# idea: read line by line, split by whitespace and then split by ":" to get key:value pairs

df <- NULL
for (line in 1:nrow(input)){
  
  temp <- unlist(str_split(input$data[line], " "))
  temp <- str_split(temp, ":") %>% 
    purrr::map_dfr(~as.data.frame(t(.x))) %>% 
    rename("key" = "V1",
           "value" = "V2") %>% 
    mutate(id = input$id[line])
  
  df <- df %>% 
    bind_rows(temp)

}
```

``` r
valid <- c("byr","iyr", "eyr", "hgt", "hcl", "ecl", "pid")

 
df %>% 
  group_by(id) %>% 
  mutate(all_keys = all(valid %in% key)) %>%  ## check if the needed keys are available 
  filter(all_keys == TRUE) %>% 
  group_by(id) %>% 
  tally() %>% 
  nrow()
```

    ## [1] 210

## Puzzle 2

``` r
valid_eyecolor <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
valid_hcl <- c("0","1","2","3","4", "5","6", "7", "8", "9", "a", "b", "c", "d", "e", "f")

df <- df %>% 
  group_by(id) %>% 
  mutate(all_keys = all(valid %in% key)) %>%  ## check if the needed keys are available 
  filter(all_keys == TRUE) %>% 
  spread(key, value) %>% 
  group_by(id) %>% 
  mutate(byr = as.numeric(byr),
         cid = as.numeric(cid),
         eyr = as.numeric(eyr),
         iyr = as.numeric(iyr),
         pid = pid,
         unit_hgt = if_else(str_sub(hgt,-2) == "cm","cm", "in")) %>% 
  filter(byr <= 2002,
         byr >= 1920,
         iyr >= 2010, 
         iyr <= 2020,
         eyr >= 2020,
         eyr <= 2030,
         ecl %in% valid_eyecolor,
         nchar(pid) == 9,
         str_sub(hcl,1,1) =="#",
         nchar(str_sub(hcl,2,-1)) == 6
         #all(valid_hcl %in% str_split(str_sub(hcl,2,-1), ""))  
         )
df
```

    ## # A tibble: 134 x 11
    ## # Groups:   id [134]
    ##       id all_keys   byr   cid ecl     eyr hcl     hgt     iyr pid       unit_hgt
    ##    <int> <lgl>    <dbl> <dbl> <chr> <dbl> <chr>   <chr> <dbl> <chr>     <chr>   
    ##  1     3 TRUE      1970   222 gry    2026 #866857 185cm  2016 269105457 cm      
    ##  2     7 TRUE      1939    NA oth    2026 #888785 178cm  2020 595705064 cm      
    ##  3     8 TRUE      1980    NA brn    2020 #efcc98 159cm  2016 139063139 cm      
    ##  4     9 TRUE      1997    NA brn    2022 #602927 179cm  2011 646870519 cm      
    ##  5    10 TRUE      1962    NA oth    2023 #ceb3a1 170cm  2014 243067344 cm      
    ##  6    11 TRUE      1941    94 oth    2026 #866857 180cm  2010 704529614 cm      
    ##  7    13 TRUE      2002   242 blu    2020 #866857 151cm  2012 770262094 cm      
    ##  8    15 TRUE      1925   323 grn    2030 #6b5442 178cm  2020 285882039 cm      
    ##  9    16 TRUE      1990    NA hzl    2024 #7d3b0c 192cm  2019 986123633 cm      
    ## 10    18 TRUE      1934   303 amb    2026 #eec6fb 163cm  2013 721792159 cm      
    ## # … with 124 more rows
