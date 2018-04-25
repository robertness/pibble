context("nesting")
library(pibble)

test_that('Nesting tibble operates as expected', {
  .data <- as_tibble(expand.grid(LETTERS[1:2], c('1', '2'), c('x', 'y', 'z')))
  expected <- .data %>%
    group_by(Var1, Var2) %>%
    nest(.key = 'children') %>%
    mutate(n = map_int(children, ~ nrow(.x))) %>%
    select(Var1, Var2, n, children) %>%
    rename(name = Var2) %>%
    ungroup %>%
    group_by(Var1) %>%
    nest(.key = 'children') %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(Var1, n , children) %>%
    rename(name = Var1) %>%
    ungroup %>%
    {tibble(name = 'test', n = sum(.$n), children = list(.))}
  actual <- nest_into_tree(.data, "test")
  expect_equal(select(expected, -children), select(actual, -children))
  expect_equal(
    select(expected$children[[1]], -children),
    select(actual$children[[1]], -children)
  )
  for(i in 1:2){
    expect_equal(
      select(expected$children[[1]]$children[[i]], -children),
      select(actual$children[[1]]$children[[i]], -children)
    )
  }
  for(i in 1:2){
    for(j in 1:2){
      expect_equal(
        expected$children[[1]]$children[[i]]$children[[j]],
        actual$children[[1]]$children[[i]]$children[[j]]
      )
    }
  }
})
