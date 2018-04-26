context("nesting")
library(pibble)

test_that('Nesting tibble operates as expected', {
  .data <- as_tibble(expand.grid(LETTERS[1:2], c('1', '2'), c('x', 'y', 'z')))

  expected <- .data %>%
    group_by(Var1, Var2, Var3) %>%
    summarize(n = n()) %>%
    rename(item = Var3) %>%
    select(Var1, Var2, item, n) %>%
    ungroup %>%

    group_by(Var1, Var2) %>%
    nest(.key = 'children') %>%
    rename(item = Var2) %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(Var1, item, children, n) %>%
    ungroup %>%

    group_by(Var1) %>%
    nest(.key = 'children') %>%
    rename(item = Var1) %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(item, children, n) %>%
    ungroup %>%

    {tibble(item = 'test', children = list(.), n = sum(.$n))}

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

test_that('Nesting tibble works with repetition as expected', {
  .data <- as_tibble(expand.grid(LETTERS[1:2], c('1', '2'), c('x', 'x', 'y', 'y', 'z', 'z')))

  expected <- .data %>%
    group_by(Var1, Var2, Var3) %>%
    summarize(n = n()) %>%
    ungroup %>%
    rename(item = Var3) %>%
    select(Var1, Var2, item, n) %>%

    group_by(Var1, Var2) %>%
    nest(.key = 'children') %>%
    ungroup %>%
    rename(item = Var2) %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(Var1, item, children, n) %>%

    group_by(Var1) %>%
    nest(.key = 'children') %>%
    ungroup %>%
    rename(item = Var1) %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(item, children, n) %>%

    {tibble(item = 'test', children = list(.), n = sum(.$n))}

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

test_that('Nesting tibble works with lists', {
  l1 = c(
    list(wl1 = c('foo', 'bar'), wl2 = c('bag', 'of', 'words')),
    list(wl3 = c('hello', 'world'), wl4 = c('greetings', 'planet'))
    )
  l2 = list(
    list(LETTERS[1:2]),
    list(LETTERS[1:2]),
    list(LETTERS[3:4]),
    list(LETTERS[3:4]),
    list(LETTERS[5:6]),
    list(LETTERS[5:6])
  )
  .data <- as_tibble(expand.grid(LETTERS[1:2], l1, l2))

  encode_list_item <- function(.x) paste(as.character(.x), collapse = "_")

  list1_idx = structure(.data$Var2, names = map_chr(.data$Var2, encode_list_item))
  list2_idx <- structure(.data$Var3, names = map_chr(.data$Var3, encode_list_item))
  expected <- .data %>%
    mutate(
      Var2 = map_chr(Var2, encode_list_item),
      Var3 = map_chr(Var3, encode_list_item)
    ) %>%
    group_by(Var1, Var2, Var3) %>%
    summarize(n = n()) %>%
    ungroup %>%
    mutate(
      Var2 = list1_idx[Var2],
      Var3 = list2_idx[Var3]
    ) %>%
    rename(item = Var3) %>%
    select(Var1, Var2, item, n) %>%

    mutate(
      Var2 = map_chr(Var2, encode_list_item)
    ) %>%
    group_by(Var1, Var2) %>%
    nest(.key = 'children') %>%
    ungroup %>%
    mutate(
      Var2 = list1_idx[Var2]
    ) %>%
    rename(item = Var2) %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(Var1, item, children, n) %>%

    group_by(Var1) %>%
    nest(.key = 'children') %>%
    ungroup %>%
    rename(item = Var1) %>%
    mutate(n = map_int(children, ~ sum(.x$n))) %>%
    select(item, children, n) %>%


    {tibble(item = 'test', children = list(.), n = sum(.$n))}

  actual <- nest_into_tree(.data, "test")

  expect_equal(select(expected, -children), select(actual, -children))
  expect_equal(
    select(expected$children[[1]], -children),
    select(actual$children[[1]], -children)
  )
  for(i in 1:2){
    expect_equal(
      select(expected$children[[1]]$children[[i]], -children)$n,
      select(actual$children[[1]]$children[[i]], -children)$n
    )
    expect_identical(
      actual$children[[1]]$children[[i]]$item,
      actual$children[[1]]$children[[i]]$item
    )
  }
  for(i in 1:2){
    for(j in 1:2){
      expect_equal(
        expected$children[[1]]$children[[i]]$children[[j]]$n,
        actual$children[[1]]$children[[i]]$children[[j]]$n
      )
      expect_identical(
        actual$children[[1]]$children[[i]]$children[[j]]$item,
        actual$children[[1]]$children[[i]]$children[[j]]$item
      )
    }
  }
})
