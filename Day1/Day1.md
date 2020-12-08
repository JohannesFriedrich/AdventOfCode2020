Day1
================
Johannes Friedrich
12/1/2020

## Puzzle 1

``` r
## read in the file

input <- scan(file = "input.txt", what = integer())

pairs <- combn(input, 2)
sums <- combn(input, 2, sum)

Reduce(`*`, pairs[,which(sums == 2020)])
```

    ## [1] 319531

## Puzzle 2

``` r
pairs <- combn(input, 3)
sums <- combn(input, 3, sum)

Reduce(`*`, pairs[,which(sums == 2020)])
```

    ## [1] 244300320
