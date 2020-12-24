### Day 10: Adapter Array

Patched into the aircraft’s data port, you discover weather forecasts of
a massive tropical storm. Before you can figure out whether it will
impact your vacation plans, however, your device suddenly turns off!

Its battery is dead.

You’ll need to plug it in. There’s only one problem: the charging outlet
near your seat produces the wrong number of jolts. Always prepared, you
make a list of all of the joltage adapters in your bag.

Each of your joltage adapters is rated for a specific output joltage
(your puzzle input). Any given adapter can take an input `1`, `2`, or
`3` jolts lower than its rating and still produce its rated output
joltage.

In addition, your device has a built-in joltage adapter rated for `3`
jolts higher than the highest-rated adapter in your bag. (If your
adapter list were `3`, `9`, and `6`, your device’s built-in adapter
would be rated for `12` jolts.)

Treat the charging outlet near your seat as having an effective joltage
rating of `0`.

Since you have some time to kill, you might as well test all of your
adapters. Wouldn’t want to get to your resort and realize you can’t even
charge your device!

If you use every adapter in your bag at once, what is the distribution
of joltage differences between the charging outlet, the adapters, and
your device?

For example, suppose that in your bag, you have adapters with the
following joltage ratings:

    16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4

With these adapters, your device’s built-in joltage adapter would be
rated for `19 + 3 = 22` jolts, 3 higher than the highest-rated adapter.

In this example, when using every adapter, there are `7` differences of
1 jolt and `5` differences of 3 jolts.

#### Part 1

Find a chain that uses all of your adapters to connect the charging
outlet to your device’s built-in adapter and count the joltage
differences between the charging outlet, the adapters, and your device.
What is the number of 1-jolt differences multiplied by the number of
3-jolt differences?

``` r
library(purrr)

jolts <- here::here("Inputs/input_day10.txt") %>% 
  readr::read_lines() %>% 
  as.numeric()

chain <- c(0, sort(jolts), max(jolts) + 3) 

prod(table(diff(chain))[c("1","3")])
```

    ## [1] 2380

#### Part 2

To completely determine whether you have enough adapters, you’ll need to
figure out how many different ways they can be arranged. Every
arrangement needs to connect the charging outlet to your device. The
previous rules about when adapters can successfully connect still apply.

The example above supports the following arrangements:

    (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
    (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
    (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
    (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
    (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
    (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
    (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
    (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)

(The charging outlet and your device’s built-in adapter are shown in
parentheses.) Given the adapters from the example, the total number of
arrangements that connect the charging outlet to your device is `8`.

You glance back down at your bag and try to remember why you brought so
many adapters; there must be more than a trillion valid ways to arrange
them! Surely, there must be an efficient way to count the arrangements.

What is the total number of distinct ways you can arrange the adapters
to connect the charging outlet to your device?

``` r
library(memoise)

n_combinations <- memoise(function(x){
  
  if (length(x) == 1) {
    return(1)
  }

  possible_indeces <- which(x[2:4] - x[1] <= 3)

  sum(map_dbl(possible_indeces, ~ n_combinations(tail(x, - .))))
  
})

as.character(n_combinations(chain))
```

    ## [1] "48358655787008"

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
