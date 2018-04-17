source("ynab_reports.R")


# test set
register.df <- r.df
r.df <- loadMostRecentRegister("Data")
unique(r.df$Category.Group)

s <- summaryYNAB(r.df, "2016-05-01")
s$totals

# TO-DO (Arnaldo):
# function that calculates real monthly activity, considering credit card expenses as
# future expense.

# r.df <- loadMostRecentRegister("Data")
# database <- as.Date("2016-07-01")
# s <- summaryYNAB(r.df, database)