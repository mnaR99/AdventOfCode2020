### Day 6: Custom Customs

As your flight approaches the regional airport where you’ll switch to a
much larger plane, customs declaration forms are distributed to the
passengers.

The form asks a series of 26 yes-or-no questions marked a through z. All
you need to do is identify the questions for which anyone in your group
answers “yes”. Since your group is just you, this doesn’t take very
long.

However, the person sitting next to you seems to be experiencing a
language barrier and asks if you can help. For each of the people in
their group, you write down the questions for which they answer “yes”,
one per line. For example:

    abcx
    abcy
    abcz

In this group, there are 6 questions to which anyone answered “yes”: a,
b, c, x, y, and z. (Duplicate answers to the same question don’t count
extra; each question counts at most once.)

Another group asks for your help, then another, and eventually you’ve
collected answers from every group on the plane (your puzzle input).
Each group’s answers are separated by a blank line, and within each
group, each person’s answers are on a single line. For example:

    abc

    a
    b
    c

    ab
    ac

    a
    a
    a
    a

    b

In this example, the sum of these counts is `3 + 3 + 3 + 1 + 1 = 11`.

#### Part 1

For each group, count the number of questions to which anyone answered
“yes”. What is the sum of those counts?

``` r
library(stringr)
library(purrr)

groups <- readr::read_file(here::here("Inputs/input_day06.txt")) %>% 
  str_split("\n\n") %>% unlist() 

groups %>%
  str_remove_all("\\n") %>% 
  map_dbl(
    ~ str_split(.x, "") %>% unlist %>% unique %>% length
  ) %>% 
  sum()
```

    ## [1] 6273

#### Part 2

As you finish the last group’s customs declaration, you notice that you
misread one word in the instructions:

You don’t need to identify the questions to which anyone answered “yes”;
you need to identify the questions to which everyone answered “yes”!

For each group, count the number of questions to which everyone answered
“yes”. What is the sum of those counts?

``` r
groups %>% 
  str_split("\\n") %>%
  map_dbl(
    ~ str_split(.x, "") %>% reduce(intersect) %>% length
  ) %>% 
  sum()
```

    ## [1] 3254

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
