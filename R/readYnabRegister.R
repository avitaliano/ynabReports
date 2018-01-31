
readYnabRegister <- function(filename, sep = "\t", encoding = "UTF-8"){
        # Loads YNAB register TSV file into a data frame and creates extra columns
        #
        # Args:
        #   file: the name of the file which the data are to be read from.
        #   sep: the field separator character. Default is '\t'
        #   encoding: encoding to be assumed for input strings.
        #
        # Returns:
        #   The data frame created

        # reads YNAB register TSV file
        register.df = read.csv(filename, sep = sep,
                               encoding = encoding,
                               header = TRUE,
                               stringsAsFactors = FALSE)

        # modifies some columns
        # Rename column. Don't know why this column name is changed!
        names(register.df)[1] <- "Account"

        # Date column to datetime
        register.df$Date <- as.Date(register.df$Date, format = "%d/%m/%Y" )

        # Inflow/Outfow to numeric.
        # removes non digits and replaces 'comma' to 'dot'.
        register.df$Inflow.currency <- stringr::str_extract(register.df$Inflow, "[a-zA-Z\\$]+")
        register.df$Inflow <-  as.numeric(gsub("\\,",
                                               gsub("[^0-9^,^.]",
                                                    register.df$Inflow,
                                                    replacement = ""),
                                               replacement = "." ))
        register.df$Outflow.currency <- stringr::str_extract(register.df$Outflow, "[a-zA-Z\\$]+")
        register.df$Outflow <-  as.numeric(gsub("\\,",
                                                gsub("[^0-9^,^.]",
                                                     register.df$Outflow,
                                                     replacement = ""),
                                                replacement = "." ))

        #add extra columns: day, month, year
        register.df$day <- as.integer(format(register.df$Date, "%d"))
        register.df$month <- as.Date(format(register.df$Date, "%Y-%m-01"))
        register.df$year <- as.integer(format(register.df$Date, "%Y"))

        # add weekday as a factor, sunday as first day of the week
        weekday_vector <- weekdays(register.df$Date, abbreviate = TRUE)
        weekday <- unique(weekday_vector)
        n_weekday <- unique(as.integer(format(register.df$Date, "%w")))
        week_levels = weekday[order(n_weekday)]
        register.df$weekday <- factor(weekday_vector, levels = week_levels)

        # boolean as (it's a transfer)
        register.df$is_transfer <- substr(register.df$Payee, 1, 10) == "Transfer :"

        # boolean as (it has category defined)
        register.df$has_category <- register.df$Category.Group.Category != ""

        # amount as (inflow - outflow)
        register.df$amount <- register.df$Inflow - register.df$Outflow

        # returns data frame
        return(register.df)
}
