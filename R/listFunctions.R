# auxiliary functions

listCategories <- function(ynab.register){
  # ignores empty category
  return ( setdiff(sort(with(ynab.register, unique(Category))), ""))
}

listCategoryGroups <- function(ynab.register, list.category = FALSE){

  # ignores empty group
  groups.category <- setdiff(sort(with(ynab.register,
                                       unique(Category.Group.Category))), "")
  groups <- setdiff(sort(with(ynab.register, unique(Category.Group))), "" )

  ifelse(list.category, return(groups.category), return(groups))
}

listAccounts <- function(ynab.register){
  return (sort(with(ynab.register, unique(Account))))
}

firstDate <- function(ynab.register){
  return(with(ynab.register, min(month)))
}

lastDate <- function(ynab.register){
  return(with(ynab.register, max(month)))
}


