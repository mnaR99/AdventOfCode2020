### Day 3: Toboggan Trajectory

With the toboggan login problems resolved, you set off toward the
airport. While travel by toboggan might be easy, it’s certainly not
safe: there’s very minimal steering and the area is covered in trees.
You’ll need to see which angles will take you near the fewest trees.

Due to the local geology, trees in this area only grow on exact integer
coordinates in a grid. You make a map (your puzzle input) of the open
squares (.) and trees (\#) you can see.

These aren’t the only trees, though; due to something you read about
once involving arboreal genetics and biome stability, the same pattern
repeats to the right many times:

    ..##.......|..##.........##.........##.........##.........##.......  --->
    #...#...#..|#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
    .#....#..#.|.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
    ..#.#...#.#|..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
    .#...##..#.|.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
    ..#.##.....|..#.##.......#.##.......#.##.......#.##.......#.##.....  --->
    .#.#.#....#|.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
    .#........#|.#........#.#........#.#........#.#........#.#........#
    #.##...#...|#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
    #...##....#|#...##....##...##....##...##....##...##....##...##....#
    .#..#...#.#|.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

You start on the open square (.) in the top-left corner and need to
reach the bottom (below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a
cheaper model that prefers rational numbers); start by counting all the
trees you would encounter for the slope right 3, down 1:

From your starting position at the top-left, check the position that is
right 3 and down 1. Then, check the position that is right 3 and down 1
from there, and so on until you go past the bottom of the map.

#### Part 1

Starting at the top-left corner of your map and following a slope of
right 3 and down 1, how many trees would you encounter?

``` r
library(purrr)

grid <- readr::read_lines(here::here("Inputs/input_day03.txt")) %>% 
  stringr::str_split("",simplify = TRUE)

right <- 3; down <- 1

y <- seq(1, nrow(grid), down)
x <- accumulate(seq_along(y), ~.x + right)

grid_reps <- ceiling(max(x) / ncol(grid))
new_grid <- reduce(rep(list(grid), grid_reps), cbind)

sum(map2_chr(x, y, ~new_grid[.y, .x]) == "#")
```

    ## [1] 211

#### Part 2

Time to check the rest of the slopes - you need to minimize the
probability of a sudden arboreal stop, after all.

Determine the number of trees you would encounter if, for each of the
following slopes, you start at the top-left corner and traverse the map
all the way to the bottom:

-   Right 1, down 1.
-   Right 3, down 1. *(This is the slope you already checked.)*
-   Right 5, down 1.
-   Right 7, down 1.
-   Right 1, down 2.

What do you get if you multiply together the number of trees encountered
on each of the listed slopes?

``` r
count_trees <- function(right, down){
  y <- seq(1, nrow(grid), down)
  x <- accumulate(seq_along(y), ~.x + right)
  
  grid_reps <- ceiling(max(x) / ncol(grid))
  new_grid <- reduce(rep(list(grid), grid_reps), cbind)
  
  sum(map2_chr(x, y, ~new_grid[.y, .x]) == "#")
}

map2_dbl(c(1,3,5,7,1), c(1,1,1,1,2), count_trees) %>% 
  prod()
```

    ## [1] 3584591857

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
