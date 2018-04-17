

#sample functions
removeOutlier <- function(x, qt_l = .25,
                          qt_h = .75,
                          na.rm = TRUE){
  qnt <- quantile(x, probs=c(qt_l, qt_h), na.rm = na.rm)
  #caps <- quantile(x, probs=c(.05, .95), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA

  return(x)
}

# sample lazy evaluation
groupYNABRegisters <- function(register.df = r.df,
                             year_interval,
                             ignored_categories,
                             spread = FALSE,
                             grp_columns = c("database"),
                             summarise = "sum(Outflow)"){


  group_by = lapply(grp_columns, as.symbol)

  sum.df <- register.df %>%
    filter(year %in% year_interval &
             !is_transfer &
             has_category &
             !Category %in% ignored_categories ) %>%
    group_by_(.dots = group_by) %>%
    summarise_(.dots = setNames(summarise, "sum"))

  if(spread){
    # denormalizes database into colums and summarize
    sum.df <- sum.df %>% spread_(.dots = group_by, sum)
    sum.df$Total <- rowSums(sum.df[,-(1:2)], na.rm = TRUE)
  }
  return(sum.df)
}
