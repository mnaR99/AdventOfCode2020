### Day 12: Rain Risk

Your ferry made decent progress toward the island, but the storm came in
faster than anyone expected. The ferry needs to take evasive actions!

Unfortunately, the ship’s navigation computer seems to be
malfunctioning; rather than giving a route directly to safety, it
produced extremely circuitous instructions. When the captain uses the PA
system to ask if anyone can help, you quickly volunteer.

The navigation instructions (your puzzle input) consists of a sequence
of single-character actions paired with integer input values. After
staring at them for a few minutes, you work out what they probably mean:

-   Action `N` means to move north by the given value.
-   Action `S` means to move south by the given value.
-   Action `E` means to move east by the given value.
-   Action `W` means to move west by the given value.
-   Action `L` means to turn left the given number of degrees.
-   Action `R` means to turn right the given number of degrees.
-   Action `F` means to move forward by the given value in the direction
    the ship is currently facing.

The ship starts by facing east. Only the `L` and `R` actions change the
direction the ship is facing. (That is, if the ship is facing east and
the next instruction is `N10`, the ship would move north 10 units, but
would still move east if the following action were `F`.)

#### Part 1

Figure out where the navigation instructions lead. What is the Manhattan
distance (sum of the absolute values of its east/west position and its
north/south position) between that location and the ship’s starting
position?

``` r
library(stringr)

instructions <- readr::read_lines(here::here("Inputs/input_day12.txt"))

pos <- c(0, 0); dir <- c(1, 0)

for (i in seq_along(instructions)) {
  
  action <- str_extract(instructions[i], "^.")
  n <- as.numeric(str_extract(instructions[i], "\\d+"))
  
  if (action == "F") {pos <- pos + n * dir} 
  else if (action == "N") {pos <- pos + n * c( 0, 1)} 
  else if (action == "E") {pos <- pos + n * c( 1, 0)} 
  else if (action == "W") {pos <- pos + n * c(-1, 0)} 
  else if (action == "S") {pos <- pos + n * c( 0,-1)} 
  else {
    if (action == "R") {n <- -n}
    dir <- c(cospi(n/180)*dir[1] - sinpi(n/180)*dir[2], sinpi(n/180)*dir[1] + cospi(n/180)*dir[2])
  } 
  
}

sum(abs(pos))
```

    ## [1] 582

#### Part 2

Before you can give the destination to the captain, you realize that the
actual action meanings were printed on the back of the instructions the
whole time.

Almost all of the actions indicate how to move a waypoint which is
relative to the ship’s position:

-   Action `N` means to move the waypoint north by the given value.
-   Action `S` means to move the waypoint south by the given value.
-   Action `E` means to move the waypoint east by the given value.
-   Action `W` means to move the waypoint west by the given value.
-   Action `L` means to rotate the waypoint around the ship left
    (counter-clockwise) the given number of degrees.
-   Action `R` means to rotate the waypoint around the ship right
    (clockwise) the given number of degrees.
-   Action `F` means to move forward to the waypoint a number of times
    equal to the given value.

The waypoint starts 10 units east and 1 unit north relative to the ship.
The waypoint is relative to the ship; that is, if the ship moves, the
waypoint moves with it.

Figure out where the navigation instructions actually lead. What is the
Manhattan distance between that location and the ship’s starting
position?

``` r
pos <- c(0, 0); dir <- c(10, 1)

for (i in seq_along(instructions)) {
  
  action <- str_extract(instructions[i], "^.")
  n <- as.numeric(str_extract(instructions[i], "\\d+"))
  
  if (action == "F") {pos <- pos + n * dir} 
  else if (action == "N") {dir <- dir + n * c( 0, 1)} 
  else if (action == "E") {dir <- dir + n * c( 1, 0)} 
  else if (action == "W") {dir <- dir + n * c(-1, 0)} 
  else if (action == "S") {dir <- dir + n * c( 0,-1)} 
  else {
    if (action == "R") {n <- -n}
    dir <- c(cospi(n/180)*dir[1] - sinpi(n/180)*dir[2], sinpi(n/180)*dir[1] + cospi(n/180)*dir[2])
  } 

}

sum(abs(pos))
```

    ## [1] 52069

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
