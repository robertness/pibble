#' Encodes list items into characters so lists can be used for grouping and nesting.
encode_list_item <- function(.x) paste(as.character(.x), collapse = "_")

#' Recursive function used by 'nest_into_tree'
nest_tibbles_inner <- function(df){
  df <- rename_(df, item = names(df)[1])
  if(ncol(df) == 1){
    if(is.list(df$item)){
      list_item <- unique(df$item)
      names(list_item) <- map_chr(list_item, encode_list_item)
      df <- df %>%
        mutate(item = map_chr(df$item, encode_list_item)) %>%
        group_by(item) %>%
        summarize(n = n()) %>%
        ungroup %>%
        mutate(item = list_item[item])

    } else{
      df <- df %>%
        group_by(item) %>%
        summarize(n = n()) %>%
        ungroup()
    }
  } else {
    if(is.list(df$item)){
      list_item <- unique(df$item)
      names(list_item) <- map_chr(list_item, encode_list_item)
      df <- df %>%
        mutate(item = map_chr(df$item, encode_list_item)) %>%
        group_by(item) %>%
        nest(.key = 'children') %>%
        ungroup %>%
        mutate(item = list_item[item])
    }
    else {
      df <- df %>%
        group_by(item) %>%
        nest(.key = 'children') %>%
        ungroup
    }
    mutate(df,
           n = map_int(children, ~ nrow(.x)),
           children = map(children, nest_tibbles_inner)
           ) %>%
      select(item, children, n)
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
    item = name,
    children = list(nested),
    n = sum(nested$n)
  )
}
