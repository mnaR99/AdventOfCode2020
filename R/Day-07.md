### Day 7: Handy Haversacks

You land at the regional airport in time for your next flight. In fact,
it looks like you’ll even have time to grab some food: all flights are
currently delayed due to issues in luggage processing.

Due to recent aviation regulations, many rules (your puzzle input) are
being enforced about bags and their contents; bags must be color-coded
and must contain specific quantities of other color-coded bags.
Apparently, nobody responsible for these regulations considered how long
they would take to enforce!

For example, consider the following rules:

    light red bags contain 1 bright white bag, 2 muted yellow bags.
    dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    bright white bags contain 1 shiny gold bag.
    muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    faded blue bags contain no other bags.
    dotted black bags contain no other bags.

You have a `shiny gold` bag. If you wanted to carry it in at least one
other bag, how many different bag colors would be valid for the
outermost bag? (In other words: how many colors can, eventually, contain
at least one `shiny gold` bag?)

So, in this example, the number of bag colors that can eventually
contain at least one `shiny gold` bag is `4`.

#### Part 1

How many bag colors can eventually contain at least one `shiny gold`
bag? (The list of rules is quite long; make sure you get all of it.)

``` r
library(tidyverse)

rules <- tibble(rule = read_lines(here::here("Inputs/input_day07.txt")))

tidy_rules <- rules %>% 
  separate(rule, into = c("from", "contain"), sep = " bags contain ") %>% 
  separate_rows(contain, sep = ", ") %>% 
  extract(contain, into = c("number", "to"), regex = "(\\d+) (.+) bag.*", convert = T) %>% 
  filter(!is.na(number))

which_bags_contain <- function(search_for){
  
  from <- 
    tidy_rules %>% 
    filter(to %in% search_for) %>% 
    pull(from)
  
  if (length(from) == 0) {
    return(search_for)
  }
  
  map(from, ~ which_bags_contain(.x)) %>% 
    flatten_chr() %>% 
    c(from) %>% 
    unique()
}

length(which_bags_contain("shiny gold"))
```

    ## [1] 265

#### Part 2

It’s getting pretty expensive to fly these days - not because of ticket
prices, but because of the ridiculous number of bags you need to buy!

Consider again your shiny gold bag and the rules from the above example:

-   `faded blue` bags contain `0` other bags.
-   `dotted black` bags contain `0` other bags.
-   `vibrant plum` bags contain `11` other bags: 5 `faded blue` bags and
    6 `dotted black` bags.
-   `dark olive` bags contain `7` other bags: 3 `faded blue` bags and 4
    `dotted black` bags.

So, a single `shiny gold` bag must contain 1 `dark olive` bag (and the 7
bags within it) plus 2 `vibrant plum` bags (and the 11 bags within each
of those): `1 + 1*7 + 2 + 2*11` = `32` bags!

Of course, the actual rules have a small chance of going several levels
deeper than this example; be sure to count all of the bags, even if the
nesting becomes topologically impractical!

How many individual bags are required inside your single shiny gold bag?

``` r
total_bags <- function(search_for){

  contained <- tidy_rules %>% 
    filter(from %in% search_for)
  
  if (nrow(contained) == 0) {
    return(0)
  }
  
  map2_dbl(
    contained$number, contained$to, 
    ~ .x + .x * total_bags(.y)
  ) %>% 
    sum()
  
}

total_bags("shiny gold")
```

    ## [1] 14177

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
