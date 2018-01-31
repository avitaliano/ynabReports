sumExpenses <- function(ynab.df ,
                        initial.month,
                        final.month,
                        expenses.groups,
                        total = TRUE) {

  if (total){
    expenses.df <- ynab.df %>% filter(Category.Group %in% expenses.groups) %>%
      filter(month >= initial.month & month <= final.month) %>%
      group_by(Category.Group) %>%
      summarize(amount = sum(amount) * -1)
  } else {
    expenses.df <- ynab.df %>% filter(Category.Group %in% expenses.groups) %>%
      filter(month >= initial.month & month <= final.month) %>%
      group_by( month, Category.Group) %>%
      summarize(amount = sum(amount) * -1)
  }


  return(expenses.df)
}

# TODO: add here expenses related analysis
