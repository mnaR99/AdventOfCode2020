### Day 9: Encoding Error

With your neighbor happily enjoying their video game, you turn your
attention to an open data port on the little screen in the seat in front
of you.

Though the port is non-standard, you manage to connect it to your
computer through the clever use of several paperclips. Upon connection,
the port outputs a series of numbers (your puzzle input).

The data appears to be encrypted with the eXchange-Masking Addition
System (XMAS) which, conveniently for you, is an old cypher with an
important weakness.

XMAS starts by transmitting a preamble of 25 numbers. After that, each
number you receive should be the sum of any two of the 25 immediately
previous numbers. The two numbers will have different values, and there
might be more than one such pair.

Here is an example which only considers the previous 5 numbers (and has
a preamble of length 5):

    35
    20
    15
    25
    47
    40
    62
    55
    65
    95
    102
    117
    150
    182
    127
    219
    299
    277
    309
    576

In this example, after the 5-number preamble, almost every number is the
sum of two of the previous 5 numbers; the only number that does not
follow this rule is `127`.

#### Part 1

The first step of attacking the weakness in the XMAS data is to find the
first number in the list (after the preamble) which is not the sum of
two of the 25 numbers before it. What is the first number that does not
have this property?

``` r
library(purrr)

numbers <- 
  here::here("Inputs/input_day09.txt") %>% 
  readr::read_lines() %>% 
  as.numeric()

n <- 
  imap(
    numbers[-(1:25)],
    ~ intersect(.x - numbers[.y:(.y+24)], numbers[.y:(.y+24)])
  ) %>% 
  detect_index(~ length(.x) < 2) + 25

numbers[n]
```

    ## [1] 70639851

#### Part 2

The final step in breaking the XMAS encryption relies on the invalid
number you just found: you must find a contiguous set of at least two
numbers in your list which sum to the invalid number from step 1.

Again consider the above example:

    35
    20
    15
    25
    47
    40
    62
    55
    65
    95
    102
    117
    150
    182
    127
    219
    299
    277
    309
    576

In this list, adding up all of the numbers from `15` through `40`
produces the invalid number from step 1, `127`. (Of course, the
contiguous set of numbers in your actual list might be much longer.)

To find the encryption weakness, add together the smallest and largest
number in this contiguous range; in this example, these are `15` and
`47`, producing `62`.

What is the encryption weakness in your XMAS-encrypted list of numbers?

``` r
n2 <- 
  map(
    1:(n-1), 
    ~ cumsum(numbers[.x:(n-1)])
  ) %>% 
  detect_index(~ numbers[n] %in% .x)

contiguous <- 
  numbers[n2:(n-1)] %>% 
  cumsum() %>% 
  keep(~.x < numbers[n]) %>% 
  length()

numbers[n2:(n2+contiguous)] %>% 
  range() %>% 
  sum()
```

    ## [1] 8249240

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
