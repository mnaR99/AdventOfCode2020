### Day 5: Binary Boarding

You board your plane only to discover a new problem: you dropped your
boarding pass! You aren’t sure which seat is yours, and all of the
flight attendants are busy with the flood of people that suddenly made
it through passport control.

You write a quick program to use your phone’s camera to scan all of the
nearby boarding passes (your puzzle input); perhaps you can find your
seat through process of elimination.

Instead of zones or groups, this airline uses binary space partitioning
to seat people. A seat might be specified like `FBFBBFFRLR`, where `F`
means “front”, `B` means “back”, `L` means “left”, and `R` means
“right”.

The first 7 characters will either be `F` or `B`; these specify exactly
one of the 128 rows on the plane (numbered 0 through 127). Each letter
tells you which half of a region the given seat is in. Start with the
whole list of rows; the first letter indicates whether the seat is in
the front (0 through 63) or the back (64 through 127). The next letter
indicates which half of that region the seat is in, and so on until
you’re left with exactly one row.

The last three characters will be either `L` or `R`; these specify
exactly one of the 8 columns of seats on the plane (numbered 0 through
7). The same process as above proceeds again, this time with only three
steps. `L` means to keep the lower half, while `R` means to keep the
upper half.

So, decoding `FBFBBFFRLR` reveals that it is the seat at row 44, column
5.

Every seat also has a unique seat ID: multiply the row by 8, then add
the column. In this example, the seat has ID `44 * 8 + 5 = 357`.

As a sanity check, look through your list of boarding passes. What is
the highest seat ID on a boarding pass?

#### Part 1

``` r
library(purrr)

passes <- readr::read_lines(here::here("Inputs/input_day05.txt"))

seats <- 
  passes %>% 
  stringr::str_split("") %>% 
  map_dbl(
    function(letter) {
      rnge <- c(0, 127, 0, 7)
    
      for (i in seq_along(letter)) {
        
        if ( letter[i] == "F" ) {
          rnge[2] <- rnge[2] - 2**(7-i)
        } 
        else if( letter[i] == "B" ) {
          rnge[1] <- rnge[1] + 2**(7-i)
        } 
        else if( letter[i] == "L" ) {
          rnge[4] <- rnge[4] - 2**(3-(i-7))
        } 
        else {
          rnge[3] <- rnge[3] + 2**(3-(i-7))
        }
      }
      rnge[1] * 8 + rnge[3]
    }
  )

max(seats)
```

    ## [1] 888

#### Part 2

Ding! The “fasten seat belt” signs have turned on. Time to find your
seat.

It’s a completely full flight, so your seat should be the only missing
boarding pass in your list. However, there’s a catch: some of the seats
at the very front and back of the plane don’t exist on this aircraft, so
they’ll be missing from your list as well.

Your seat wasn’t at the very front or back, though; the seats with IDs
+1 and -1 from yours will be in your list.

What is the ID of your seat?

``` r
seq(min(seats), max(seats)) %>% 
  setdiff(seats)
```

    ## [1] 522

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
