### Day 8: Handheld Halting

Your flight to the major airline hub reaches cruising altitude without
incident. While you consider checking the in-flight menu for one of
those drinks that come with a little umbrella, you are interrupted by
the kid sitting next to you.

Their handheld game console won’t turn on! They ask if you can take a
look.

You narrow the problem down to a strange infinite loop in the boot code
(your puzzle input) of the device. You should be able to fix it, but
first you need to be able to run the code in isolation.

The boot code is represented as a text file with one instruction per
line of text. Each instruction consists of an operation (`acc`, `jmp`,
or `nop`) and an argument (a signed number like `+4` or `-20`).

-   `acc` increases or decreases a single global value called the
    accumulator by the value given in the argument. For example,
    `acc +7` would increase the accumulator by 7. The accumulator starts
    at `0`. After an `acc` instruction, the instruction immediately
    below it is executed next.

-   `jmp` jumps to a new instruction relative to itself. The next
    instruction to execute is found using the argument as an offset from
    the `jmp` instruction; for example, `jmp +2` would skip the next
    instruction, `jmp +1` would continue to the instruction immediately
    below it, and `jmp -20` would cause the instruction 20 lines above
    to be executed next.

-   `nop` stands for No OPeration - it does nothing. The instruction
    immediately below it is executed next.

The moment the program tries to run any instruction a second time, you
know it will never terminate.

#### Part 1

Run your copy of the boot code. Immediately before any instruction is
executed a second time, what value is in the accumulator?

``` r
library(stringr)

instructions <- readr::read_lines(here::here("Inputs/input_day08.txt"))

acc <- 0; index <- 1; index_cache <- 1

while(sum(index_cache == index) == 1){
  
  if (str_detect(instructions[index], "acc")) {
    acc <- acc + as.numeric(str_extract(instructions[index], "(\\+|-)\\d+"))
    index <- index + 1
    
  } else if (str_detect(instructions[index], "jmp")) {
    index <- index + as.numeric(str_extract(instructions[index], "(\\+|-)\\d+"))
    
  } else {
    index <- index + 1
    
  }
  
  index_cache <- c(index_cache, index)
  
}

acc
```

    ## [1] 1859

#### Part 2

After some careful analysis, you believe that exactly one instruction is
corrupted.

Somewhere in the program, either a `jmp` is supposed to be a `nop`, or a
`nop` is supposed to be a `jmp`. (No `acc` instructions were harmed in
the corruption of this boot code.)

The program is supposed to terminate by attempting to execute an
instruction immediately after the last instruction in the file. By
changing exactly one `jmp` or `nop`, you can repair the boot code and
make it terminate correctly.

Fix the program so that it terminates normally by changing exactly one
`jmp` (to `nop`) or `nop` (to `jmp`). What is the value of the
accumulator after the program terminates?

``` r
possible_programs <- list(); j <- 1

for (i in seq_along(instructions)) {
  
  if (str_detect(instructions[i], "jmp")) {
    possible_programs[[j]] <- instructions
    possible_programs[[j]][i] <- str_replace(instructions[i], "jmp", "nop")
    j <- j + 1
    
  } else if (str_detect(instructions[i], "nop")) {
    possible_programs[[j]] <- instructions
    possible_programs[[j]][i] <- str_replace(instructions[i], "nop", "jmp")
    j <- j + 1
    
  }
  
}

h <- 1; right_program <- FALSE

while (!right_program) {
  
  instructions <- possible_programs[[h]]
  
  acc <- 0; index <- 1; index_cache <- 1

  while(sum(index_cache == index) == 1 & index <= 683){
    
    if (str_detect(instructions[index], "acc")) {
      acc <- acc + as.numeric(str_extract(instructions[index], "(\\+|-)\\d+"))
      index <- index + 1
      
    } else if (str_detect(instructions[index], "jmp")) {
      index <- index + as.numeric(str_extract(instructions[index], "(\\+|-)\\d+"))
      
    } else {
      index <- index + 1
      
    }
    index_cache <- c(index_cache, index)
  
  }
  
  h <- h + 1; right_program <- index > 683
  
}

acc
```

    ## [1] 1235

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
