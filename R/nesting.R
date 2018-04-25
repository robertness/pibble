nest_tibbles_inner <- function(df){
  if(ncol(df) == 1){
    return(df)
  } else {
    df %>%
      rename_(name = names(df)[1]) %>%
      group_by(name) %>%
      nest(.key = 'children') %>%
      mutate(
        n = map_int(children, ~ nrow(.x)),
        children = map(children, nest_tibbles_inner)
      ) %>%
      select(name, n, children)
  }
}

#' Convert a tibble to a tree of nested tibbles
#'
#' nest_into_tree iterative nests the tibble, forming a tree structure.
#' The algorithm moves from the left most column, calling `group_by` and `nest`.
#' Such that the unique values/levels of the left-most column is at the level of
#' the tree right below the root node, and the levels of the right-most column
#' form root nodes.
#'
#' @param df a dataframe to be converted
#' @param name the name to be assigned to the root node
#'
#' @return A tibble containing a sequence of nested tibbles.
#'  At each level of nesting is a tibble with three columns;
#' column `name`` contains the levels of the column of the input dataframe at that
#' level, column `n` contains the number of rows at each subsequent level of nesting,
#' and column `children` contains the next layer of nested tibbles.
#'
#' @export
nest_into_tree <- function(df, name){
  nested <- nest_tibbles_inner(df)
  tibble(
    name = name,
    n = sum(nested$n),
    children = list(nested)
  )
}
