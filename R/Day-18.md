### Day 18: Operation Order

As you look out the window and notice a heavily-forested continent
slowly appear over the horizon, you are interrupted by the child sitting
next to you. They’re curious if you could help them with their math
homework.

Unfortunately, it seems like this “math” follows different rules than
you remember.

The homework (your puzzle input) consists of a series of expressions
that consist of addition (`+`), multiplication (`*`), and parentheses
(`(...)`). Just like normal math, parentheses indicate that the
expression inside must be evaluated before it can be used by the
surrounding expression. Addition still finds the sum of the numbers on
both sides of the operator, and multiplication still finds the product.

However, the rules of operator precedence have changed. Rather than
evaluating multiplication before addition, the operators have the same
precedence, and are evaluated left-to-right regardless of the order in
which they appear.

For example, the steps to evaluate the expression
`1 + 2 * 3 + 4 * 5 + 6` are as follows:

    1 + 2 * 3 + 4 * 5 + 6
      3   * 3 + 4 * 5 + 6
          9   + 4 * 5 + 6
             13   * 5 + 6
                 65   + 6
                     71

Parentheses can override this order; for example, here is what happens
if parentheses are added to form `1 + (2 * 3) + (4 * (5 + 6))`:

    1 + (2 * 3) + (4 * (5 + 6))
    1 +    6    + (4 * (5 + 6))
         7      + (4 * (5 + 6))
         7      + (4 *   11   )
         7      +     44
                51

#### Part 1

Before you can help with the homework, you need to understand it
yourself. Evaluate the expression on each line of the homework; what is
the sum of the resulting values?

``` r
library(purrr)
library(stringr)

hw <- readr::read_lines(here::here("Inputs/input_day18.txt"))

simple_math <- function(x){
  
  same_precedence <- function(a, b){
    if (str_detect(b, "\\+")) {
      return(as.character(as.numeric(a) + as.numeric(str_extract(b, "\\d+"))))
    } else {
      return(as.character(as.numeric(a) * as.numeric(str_extract(b, "\\d+"))))
    } 
  }
  
  x %>% 
    str_extract_all("((\\+|\\*)\\s)?\\d+") %>% 
    unlist() %>% 
    reduce(same_precedence)
}

solve_problem <- function(problem, math){
  
  while (str_detect(problem, "\\(.+\\)")) {
    
    parth <- problem %>% 
      str_extract_all("\\([\\d\\s+*-]+\\)") %>% 
      unlist()
    
    res <- parth %>% 
      map_chr(
        ~ str_remove_all(.x, "[\\(\\)]") %>% 
          math()
      )
    
    walk2(
      parth, res,
      function(x, y) str_replace(problem, coll(x), y) ->> problem
    )
  }
  
  problem %>% 
    math()
}

hw %>% 
  map_chr(solve_problem, simple_math) %>% 
  as.numeric() %>% 
  sum() %>% 
  as.character()
```

    ## [1] "4491283311856"

#### Part 2

You manage to answer the child’s questions and they finish part 1 of
their homework, but get stuck when they reach the next section: advanced
math.

Now, addition and multiplication have different precedence levels, but
they’re not the ones you’re familiar with. Instead, addition is
evaluated before multiplication.

For example, the steps to evaluate the expression
`1 + 2 * 3 + 4 * 5 + 6` are now as follows:

    1 + 2 * 3 + 4 * 5 + 6
      3   * 3 + 4 * 5 + 6
      3   *   7   * 5 + 6
      3   *   7   *  11
         21       *  11
             231

What do you get if you add up the results of evaluating the homework
problems using these new rules?

``` r
advance_math <-  function(x){
  x %>% 
    str_split("\\s\\*\\s") %>% 
    unlist() %>% 
    map_dbl(~ eval(parse(text = .x))) %>%
    prod() %>% 
    as.character()
}

hw %>% 
  map_chr(solve_problem, advance_math) %>% 
  as.numeric() %>% 
  sum() %>% 
  as.character()
```

    ## [1] "68852578641904"

[`Home`](https://github.com/mnaR99/AdventOfCode2020)
