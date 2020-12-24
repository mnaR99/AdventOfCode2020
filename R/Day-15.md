### Day 15: Rambunctious Recitation

You catch the airport shuttle and try to book a new flight to your
vacation island. Due to the storm, all direct flights have been
cancelled, but a route is available to get around the storm. You take
it.

While you wait for your flight, you decide to check in with the Elves
back at the North Pole. They’re playing a memory game and are ever so
excited to explain the rules!

In this game, the players take turns saying numbers. They begin by
taking turns reading from a list of starting numbers (your puzzle
input). Then, each turn consists of considering the most recently spoken
number:

-   If that was the first time the number has been spoken, the current
    player says `0`.
-   Otherwise, the number had been spoken before; the current player
    announces how many turns apart the number is from when it was
    previously spoken.

So, after the starting numbers, each turn results in that player
speaking aloud either `0` (if the last number is new) or an age (if the
last number is a repeat).

#### Part 1

Given your starting numbers, what will be the `2020`th number spoken?

``` r
numbers <- c(0,3,1,6,7,5)

find_number <- function(numbers, pos){
  
  last_indeces <- c()
  
  last_indeces[head(numbers, -1) + 1] <- seq_along(head(numbers, -1))
  
  for (i in (length(numbers) + 1):pos) {
    
    most_recent <- tail(numbers, 1)
  
    if (!is.na(last_indeces[most_recent + 1])) {
      numbers[i] <- i - 1 - last_indeces[most_recent + 1]
    } else {
      numbers[i] <- 0
    }
    
    last_indeces[most_recent + 1] <- i - 1
    
  }
  
  numbers[pos]
}

find_number(numbers, 2020)
```

    ## [1] 852

#### Part 2

Impressed, the Elves issue you a challenge: determine the `30000000`th
number spoken.

Given your starting numbers, what will be the `30000000`th number
spoken?

``` r
find_number(numbers, 30000000)
```

    ## [1] 6007666

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
