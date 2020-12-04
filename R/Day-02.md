### Day 2: Report Repair

Your flight departs in a few days from the coastal airport; the easiest
way down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad
day. “Something’s wrong with our computers; we can’t log in!” You ask if
you can take a look.

Their password database seems to be a little corrupted: some of the
passwords wouldn’t have been allowed by the Official Toboggan Corporate
Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle
input) of passwords (according to the corrupted database) and the
corporate policy when that password was set.

``` r
library(tidyverse)

tidypwds <- readr::read_csv(here::here("Inputs/input_day02.txt"), col_names = "pwd") %>% 
  extract(pwd, into = c("l","u","letter","password"), regex = "(\\d+)-(\\d+) (.): (.+)$", convert = TRUE)
```

#### Part 1

Each line gives the password policy and then the password. The password
policy indicates the lowest and highest number of times a given letter
must appear for the password to be valid. For example, 1-3 a means that
the password must contain a at least 1 time and at most 3 times.

How many passwords are valid according to their policies?

``` r
tidypwds %>% 
  rowwise() %>% 
  filter(
    between(str_count(password, letter), l, u)
  ) %>% 
  nrow()
```

    ## [1] 398

#### Part 2

While it appears you validated the passwords correctly, they don’t seem
to be what the Official Toboggan Corporate Authentication System is
expecting.

The shopkeeper suddenly realizes that he just accidentally explained the
password policy rules from his old job at the sled rental place down the
street! The Official Toboggan Corporate Policy actually works a little
differently.

Each policy actually describes two positions in the password, where 1
means the first character, 2 means the second character, and so on. (Be
careful; Toboggan Corporate Policies have no concept of “index zero”!)
Exactly one of these positions must contain the given letter. Other
occurrences of the letter are irrelevant for the purposes of policy
enforcement.

How many passwords are valid according to the new interpretation of the
policies?

``` r
tidypwds %>% 
  rowwise() %>% 
  filter(
    sum(str_sub(password, c(l, u), c(l, u)) == letter) == 1
  ) %>% 
  nrow()
```

    ## [1] 562

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
