### Day 1: Report Repair

After saving Christmas five years in a row, you’ve decided to take a
vacation at a nice resort on a tropical island. Surely, Christmas will
go on without you.

The tropical island has its own currency and is entirely cash-only. The
gold coins used there have a little picture of a starfish; the locals
just call them stars. None of the currency exchanges seem to have heard
of them, but somehow, you’ll need to find fifty of these coins by the
time you arrive so you can pay the deposit on your room.

Before you leave, the Elves in accounting just need you to fix your
expense report (your puzzle input); apparently, something isn’t quite
adding up.

Specifically, they need you to find the two entries that sum to 2020 and
then multiply those two numbers together.

``` r
library(purrr)
report <- readr::read_lines(here::here("Inputs/input_day01.txt")) %>% as.numeric()
```

#### Part 1

Find the two entries that sum to 2020; what do you get if you multiply
them together?

``` r
diffs <- 2020 - report
prod(intersect(diffs, report))
```

    ## [1] 1007331

#### Part 2

In your expense report, what is the product of the three entries that
sum to 2020?

``` r
map(diffs, ~ intersect(.x - report, report)) %>% 
  flatten_dbl() %>%
  unique() %>% 
  prod()
```

    ## [1] 48914340

[`Home`](../README.md)
