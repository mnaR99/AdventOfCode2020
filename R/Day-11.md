### Day 11: Seating System

Your plane lands with plenty of time to spare. The final leg of your
journey is a ferry that goes directly to the tropical island where you
can finally start your vacation. As you reach the waiting area to board
the ferry, you realize you’re so early, nobody else has even arrived
yet!

By modeling the process people use to choose (or abandon) their seat in
the waiting area, you’re pretty sure you can predict the best place to
sit. You make a quick map of the seat layout (your puzzle input).

The seat layout fits neatly on a grid. Each position is either floor
(`.`), an empty seat (`L`), or an occupied seat (`#`). For example, the
initial seat layout might look like this:

    L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL

Now, you just need to model the people who will be arriving shortly.
Fortunately, people are entirely predictable and always follow a simple
set of rules. All decisions are based on the number of occupied seats
adjacent to a given seat (one of the eight positions immediately up,
down, left, right, or diagonal from the seat). The following rules are
applied to every seat simultaneously:

-   If a seat is empty (`L`) and there are no occupied seats adjacent to
    it, the seat becomes occupied.
-   If a seat is occupied (`#`) and four or more seats adjacent to it
    are also occupied, the seat becomes empty.
-   Otherwise, the seat’s state does not change.

Floor (`.`) never changes; seats don’t move, and nobody sits on the
floor.

#### Part 1

Simulate your seating area by applying the seating rules repeatedly
until no seats change state. How many seats end up occupied?

``` r
library(stringr)

grid <- readr::read_lines("Inputs/input_day11.txt") %>% 
  str_split("", simplify = T)


adjacent_seats_oc <- function(matrix, rw, cl){
  
  nw <- cbind(".", matrix, "."); 
  nw <- rbind(".", nw , ".")
  nw <- nw[rw:(rw+2), cl:(cl+2)]
  nw[2,2] <- "."
  sum(nw == "#")
  
}

rows <- 1:nrow(grid); cols <- 1:ncol(grid); seats <- grid

while (TRUE) {
  
  new_cycle <- seats

  for (rw in rows) {
    for (cl in cols) {
      
      if (seats[rw, cl] == "L" && adjacent_seats_oc(seats, rw, cl) == 0) {
        new_cycle[rw, cl] <- "#"
        
      } else if (seats[rw, cl] == "#" && adjacent_seats_oc(seats, rw, cl) >= 4) {
        new_cycle[rw, cl] <- "L"
      }
      
    }
  }
  
  if (!identical(seats, new_cycle)) {
    seats <- new_cycle
    
  } else {
    break
    
  }
  
}

sum(seats == "#")
```

    ## [1] 2441

#### Part 2

As soon as people start to arrive, you realize your mistake. People
don’t just care about adjacent seats - they care about the first seat
they can see in each of those eight directions!

Now, instead of considering just the eight immediately adjacent seats,
consider the first seat in each of those eight directions.

Also, people seem to be more tolerant than you expected: it now takes
five or more visible occupied seats for an occupied seat to become empty
(rather than four or more from the previous rules). The other rules
still apply: empty seats that see no occupied seats become occupied,
seats matching no rule don’t change, and floor never changes.

Given the new visibility method and the rule change for occupied seats
becoming empty, once equilibrium is reached, how many seats end up
occupied?

``` r
adjacent_seats_oc_2 <- function(matrix, rw, cl){
  
  nw <- cbind(".", matrix, ".") 
  nw <- rbind(".", nw , ".")
  
  first_seat <- function(x){
    
    if (is.matrix(x)) {
      x <- diag(x)
    }
    
    ss <- str_subset(x, "L|#")
    
    if (length(ss) == 0) {
      return(0)
    } 
    
    as.numeric(ss[1] == "#")
  }
  
  count <- first_seat(nw[rw+1, (cl+2):ncol(nw)])
  count <- count + first_seat(nw[rw+1, cl:1])
  count <- count + first_seat(nw[(rw+2):nrow(nw), cl+1])
  count <- count + first_seat(nw[rw:1, cl+1])
  count <- count + first_seat(nw[(rw+2):nrow(nw), (cl+2):(ncol(nw))])
  count <- count + first_seat(nw[rw:1, (cl+2):(ncol(nw))])
  count <- count + first_seat(nw[rw:1, cl:1])
  count <- count + first_seat(nw[(rw+2):nrow(nw), cl:1])

  count
}

seats <- grid

while (TRUE) {
  
  new_cycle <- seats

  for (rw in rows) {
    for (cl in cols) {
      if (seats[rw, cl] == "L" && adjacent_seats_oc_2(seats, rw, cl) == 0) {
        new_cycle[rw, cl] <- "#"
        
      } else if (seats[rw, cl] == "#" && adjacent_seats_oc_2(seats, rw, cl) >= 5) {
        new_cycle[rw, cl] <- "L"
        
      }
    }
  }
  
  if (!identical(seats, new_cycle)) {
    seats <- new_cycle
  } else {
    break
  }
}

sum(seats == "#")
```

    ## [1] 2190

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
