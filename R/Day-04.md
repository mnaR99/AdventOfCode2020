### Day 4: Passport Processing

You arrive at the airport only to realize that you grabbed your North
Pole Credentials instead of your passport. While these documents are
extremely similar, North Pole Credentials aren’t issued by a country and
therefore aren’t actually valid documentation for travel in most of the
world.

It seems like you’re not the only one having problems, though; a very
long line has formed for the automatic passport scanners, and the delay
could upset your travel itinerary.

Due to some questionable network security, you realize you might be able
to solve both of these problems at the same time.

The automatic passport scanners are slow because they’re having trouble
detecting which passports have all required fields. The expected fields
are as follows:

-   `byr` (Birth Year)
-   `iyr` (Issue Year)
-   `eyr` (Expiration Year)
-   `hgt` (Height)
-   `hcl` (Hair Color)
-   `ecl` (Eye Color)
-   `pid` (Passport ID)
-   `cid` (Country ID)

Passport data is validated in batch files (your puzzle input). Each
passport is represented as a sequence of key:value pairs separated by
spaces or newlines. Passports are separated by blank lines.

Here is an example batch file containing four passports:

    ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
    byr:1937 iyr:2017 cid:147 hgt:183cm

    iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
    hcl:#cfa07d byr:1929

    hcl:#ae17e1 iyr:2013
    eyr:2024
    ecl:brn pid:760753108 byr:1931
    hgt:179cm

    hcl:#cfa07d eyr:2025 pid:166559648
    iyr:2011 ecl:brn hgt:59in

#### Part 1

Count the number of valid passports - those that have all required
fields. Treat `cid` as optional. In your batch file, how many passports
are valid?

``` r
library(tidyverse)

passports <- readr::read_file(here::here("Inputs/input_day04.txt")) %>%
  str_split("\n\n") %>% unlist()

vpass <-
  passports %>% 
  str_extract_all(".{3}:[^\\s]+") %>% 
  map(~ str_subset(.x, pattern = "cid:", negate = TRUE)) %>% 
  keep(~length(.x) == 7)

length(vpass)
```

    ## [1] 202

#### Part 2

The line is moving more quickly now, but you overhear airport security
talking about how passports with invalid data are getting through.
Better add some data validation, quick!

You can continue to ignore the cid field, but each other field has
strict rules about what values are valid for automatic validation:

-   `byr` (Birth Year) - four digits; at least 1920 and at most 2002.
-   `iyr` (Issue Year) - four digits; at least 2010 and at most 2020.
-   `eyr` (Expiration Year) - four digits; at least 2020 and at
    most 2030.
-   `hgt` (Height) - a number followed by either cm or in:
    -   If cm, the number must be at least 150 and at most 193.
    -   If in, the number must be at least 59 and at most 76.
-   `hcl` (Hair Color) - a \# followed by exactly six characters 0-9 or
    a-f.
-   `ecl` (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-   `pid` (Passport ID) - a nine-digit number, including leading zeroes.
-   `cid` (Country ID) - ignored, missing or not.

Your job is to count the passports where all required fields are both
present and valid according to the above rules.

Count the number of valid passports - those that have all required
fields and valid values. Continue to treat `cid` as optional. In your
batch file, how many passports are valid?

``` r
vpass %>% 
  map(
    ~ sort(.x) %>% str_remove(".{3}:") %>% 
      set_names(c("byr","ecl","eyr","hcl","hgt","iyr","pid"))
  ) %>% 
  bind_rows() %>% 
  extract(hgt, into = c("hgt","hgt_m"), regex = "(\\d+)(cm|in)", convert = TRUE) %>%
  filter(
    as.numeric(byr) %>% between(1920,2002),
    as.numeric(iyr) %>% between(2010,2020),
    as.numeric(eyr) %>% between(2020,2030),
    case_when(
      hgt_m == "cm" ~ hgt %>% between(150,193),
      hgt_m == "in" ~ hgt %>% between(59,76),
      TRUE ~ FALSE
    ),
    hcl %>% str_detect("^#[0-9a-f]{6}$"),
    ecl %in% c("amb","blu","brn","gry","grn","hzl","oth"),
    pid %>% str_detect("^[0-9]{9}$")
  ) %>% 
  nrow()
```

    ## [1] 137

[`Home`](../README.md)
