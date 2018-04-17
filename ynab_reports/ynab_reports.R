#libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(plotly)
library(formattable)

loadMostRecentRegister <- function(path){
        # reads most recent Register file
        r.filename <- sort(dir(path, pattern = "*Register.tsv"),
                           decreasing = TRUE)[1]
        r.filename <- paste0(path, "/", r.filename)
        register.df = loadYNABRegister(r.filename)
        return(register.df)
}

loadYNABRegister <- function(file, sep = "\t", encoding = "UTF-8"){
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
        register.df = read.csv(file, sep = sep,
                               encoding = encoding, header = TRUE, stringsAsFactors = FALSE)
        
        # modifies some columns
        # Rename column. Don't know why this column name is changed!
        names(register.df)[1] <- "Account"
        
        # Date column to datetime
        register.df$Date <- as.Date(register.df$Date, format = "%d/%m/%Y" )
        
        # Inflow/Outfow to numeric.
        # removes non digits and replaces 'comma' to 'dot'.
        register.df$Inflow.currency <- str_extract(register.df$Inflow, "[a-zA-Z\\$]+")
        register.df$Inflow <-  as.numeric(gsub("\\,",
                                               gsub("[^0-9^,^.]",
                                                    register.df$Inflow,
                                                    replacement = ""),
                                               replacement = "." ))
        register.df$Outflow.currency <- str_extract(register.df$Outflow, "[a-zA-Z\\$]+")
        register.df$Outflow <-  as.numeric(gsub("\\,",
                                                gsub("[^0-9^,^.]",
                                                     register.df$Outflow,
                                                     replacement = ""),
                                                replacement = "." ))
        
        #add extra columns: day, month, year
        register.df$day <- as.integer(format(register.df$Date, "%d"))
        #register.df$database <- as.integer(format(register.df$Date, "%Y%m"))
        register.df$database <- as.Date(format(register.df$Date, "%Y-%m-01"))
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

sumYNABIncome <- function(register.df, month.begin, month.end, spread = FALSE) {
        
        if(missing(register.df)) stop("Missing register data frame.")
        
        if(missing(month.begin)) stop("Missing initial database.")
        
        if(missing(month.end)) stop("Missing final database.")
        
        income.df <- register.df %>%
                filter(Category.Group == "Inflow" &
                               database >= month.begin & database <= month.end) %>%
                group_by(database, Account) %>%
                summarize(sum = sum(Inflow)) %>%
                filter(sum != 0)
        
        if(spread){
                # denormalizes database into colums and calculates totals
                income.df <- income.df %>% spread(database, sum)
                income.df$Total <- rowSums(income.df[,-1], na.rm = TRUE)
                income.df <- rbind(income.df, c( "Totals", colSums(income.df[, -1], na.rm = TRUE)))
        }
        
        return(income.df)
}

sumYNABExpenses <- function(register.df, month.begin, month.end, spread = FALSE){
        
        if(missing(register.df)) stop("Missing register data frame.")
        
        if(missing(month.begin)) stop("Missing initial database.")
        
        if(missing(month.end)) stop("Missing final database.")
        
        #TO-DO: set as argument
        ignored_groups <- c("Investimentos", "Inflow")
        
        expenses.df <- register.df %>%
                filter(!Category.Group %in% ignored_groups &
                               database >= month.begin & database <= month.end &
                               !is_transfer &
                               has_category) %>%
                group_by(database, Category) %>%
                summarise(sum = sum(Outflow) - sum(Inflow))
        
        if(spread){
                # denormalizes database into colums and calculates totals
                expenses.df <- expenses.df %>% spread(database, sum)
                expenses.df$Total <- rowSums(expenses.df[,-1], na.rm = TRUE)
                expenses.df <- rbind(expenses.df, c( "Totals", colSums(expenses.df[, -1], na.rm = TRUE)))
        }
        
        return(expenses.df)
}

sumYNABExpensesByGroup <- function(register.df, month.begin, month.end, 
                                   spread = FALSE, ggplotly = FALSE){
        
        if(missing(register.df)) stop("Missing register data frame.")
        
        if(missing(month.begin)) stop("Missing initial database.")
        
        if(missing(month.end)) stop("Missing final database.")
        
        #TO-DO: set as argument
        ignored_groups <- c("Investimentos", "Inflow")
        
        expenses.df <- register.df %>%
                filter(!Category.Group %in% ignored_groups &
                               database >= month.begin & database <= month.end &
                               !is_transfer &
                               has_category) %>%
                group_by(database, Category.Group) %>%
                summarise(sum = sum(Outflow) - sum(Inflow))
        
        #plot
        if(month.begin == month.end){
                g <- ggplot(data = expenses.df, aes(x = database, y = sum, fill = Category.Group )) +
                geom_bar( stat = "identity", width = 1) +
                        coord_polar(theta="y") +
                        theme_minimal() +
                        labs(title = "Monthly Expenses by Group",
                             x = "", y = "Amount") +
                        scale_fill_discrete(name = "Expense Groups")
        }else{
                g <- ggplot(data = expenses.df, aes(x = database, y = sum, fill = Category.Group )) +
                        geom_bar( stat = "identity", width = 20 ) +
                        #                          position=position_dodge()) +
                        theme_minimal() +
                        labs(title = "Monthly Expenses by Group",
                             x = "Months", y = "Amount") +
                        scale_fill_discrete(name = "Expense Groups")
        }

        
        if(ggplotly) g <- ggplotly()
        
        if(spread){
                # denormalizes database into colums and calculates totals
                expenses.df <- expenses.df %>% spread(database, sum)
                expenses.df$Total <- rowSums(expenses.df[,-1], na.rm = TRUE)
                expenses.df <- rbind(expenses.df, c( "Totals", colSums(expenses.df[, -1], na.rm = TRUE)))
                expenses.df <- expenses.df %>% arrange(desc(Total))
        }
        
        return(list(plot = g, df = expenses.df))
}

sumYNABInvestments <- function(register.df, month.begin, month.end, spread = FALSE,
                               investment.group){
        
        if(missing(register.df)) stop("Missing register data frame.")
        
        if(missing(month.begin)) stop("Missing initial database.")
        
        if(missing(month.end)) stop("Missing final database.")
        
        if(missing(investment.group)){
                investment.group <- "Investimentos"
        }
        
        investments.df <- register.df %>%
                filter(Category.Group == investment.group &
                               database >= month.begin & database <= month.end) %>%
                group_by(database, Category) %>%
                summarise(sum = sum(Outflow) - sum(Inflow))
        
        if(spread){
                # denormalizes database into colums and calculates totals
                investments.df <- investments.df %>% spread(database, sum)
                investments.df$Total <- rowSums(investments.df[,-1], na.rm = TRUE)
                investments.df <- rbind(investments.df, c( "Totals", colSums(investments.df[, -1], na.rm = TRUE)))
        }
        
        return(investments.df)
}

sumByMonth <- function(register.df, month.begin, month.end,
                       ggplotly = FALSE, spread = FALSE){
        
        if(missing(register.df)) stop("Missing register data frame.")
        
        if(missing(month.begin)) stop("Missing initial database.")
        
        if(missing(month.end)) stop("Missing final database.")
        
        #summarizes Income
        in.df <- sumYNABIncome(r.df, month.begin, month.end, spread = FALSE) %>%
                group_by(database) %>%
                summarise(sum = sum(sum)) %>%
                mutate(type = "Income")
        
        #summarizes Expenses
        out.df <- sumYNABExpenses(r.df, month.begin, month.end, spread = FALSE) %>%
                group_by(database) %>%
                summarise(sum = sum(sum)) %>%
                mutate(type = "Expenses")
        
        #summarizes Investments
        invest.df <- sumYNABInvestments(r.df, month.begin, month.end, spread = FALSE) %>%
                group_by(database) %>%
                summarise(sum = sum(sum)) %>%
                mutate(type = "Investments")
        
        
        monthly.df <- rbind(in.df, out.df, invest.df)
        monthly.df$type <- factor(monthly.df$type, levels = c("Income", "Expenses", "Investments"))
        
        #plot
        g <- ggplot(data = monthly.df, aes(x = database, y = sum, fill = type )) +
                geom_bar( stat = "identity", width = 20,
                          position=position_dodge()) +
                scale_fill_manual(values=c("dodgerblue", "tomato", "aquamarine1")) +
                labs(title = "Monthly Expenses/Incomes/Investments",
                     x = "Months", y = "Amount") +
                scale_fill_discrete(name = "Type")
        if(ggplotly) g <- ggplotly()
        
        # # spread values by database
        if(spread){
                monthly.df <- monthly.df %>% spread(key = database, value = sum)
        }
        
        return(list(plot = g, df = monthly.df))
}

# reclassify categories for analysis
loadClasses <- function(){
        l = list(
                list(class = "Financiamento",
                     categories = c("Hipoteca")),
                
                list(class = "Alimentação",
                     categories = c("Mercado",
                                    "Almoço/Lanches Flavia",
                                    "Almoço/Lanches Arnaldo")),
                
                list(class = "Casa",
                     categories = c("Lavanderia/Faxina",
                                    "Manutenção Casa",
                                    "Utilidades",
                                    "Roupas",
                                    "Condomínio")),
                
                list(class = "Impostos", c("Impostos/Taxas")),
                
                list(class = "Transporte",
                     categories = c("Combustível",
                                    "Manutenção Carro",
                                    "Taxi/Onibus/Estacionamento")),
                
                list(class = "Saúde",
                     categories = c("Consultas/Medicamentos",
                                    "Plano Saúde",
                                    "Yoga/Academia")),
                
                list(class = "Diversão",
                     categories = c("Clube",
                                    "Fun Money Flavia",
                                    "Fun Money Arnaldo",
                                    "Viagens",
                                    "Restaurante",
                                    "Presentes")),
                
                list(class = "Outros",
                     categories = c("Doações",
                                    "Sindicato",
                                    "Eventualidades"))
        )
        return(l)
}

reclassifyYNABCategories <- function(register.df, class.list){
        
        setClass <- function(register.df, list){
                class <- list[[1]]
                categories <- list[[2]]
                return( register.df %>%
                                filter(Category %in% categories) %>%
                                mutate( class = class))
        }
        
        register.df$class <- NULL
        return(do.call("rbind", lapply(class.list, setClass, register.df = register.df)))
        
}

sumByClass <- function(class.df, month.begin, month.end,
                       ggplotly = FALSE){
        
        if(missing(class.df)) stop("Missing classes data frame.")
        if(missing(month.begin)) stop("Missing initial database.")
        if(missing(month.end)) stop("Missing final database.")
        
        if(month.begin == month.end){
                
                # sums by class
                temp.df <- class.df %>%
                        filter(database >= month.begin & database <= month.end) %>%
                        group_by(class, database) %>%
                        summarise(sum = sum(Outflow) - sum(Inflow)) %>%
                        arrange(desc(sum))
                return.df <- temp.df
                
                #plot
                ## if more then one month, use barplot. otherwise pie.
                g <- ggplot(data = temp.df, aes(x = database, y = sum, fill = class )) +
                        geom_bar( stat = "identity", width = 1) +
                        coord_polar(theta="y") +
                        theme_minimal() +
                        labs(title = paste0("Expenses by Class - ", format(month.end, "%b/%Y")),
                             x = "Months", y = "Amount") +
                        scale_fill_discrete(name = "Expenses Classes")
                
        }else{ 
                # sums by class and spreads
                temp.df <- class.df %>%
                        filter(database >= month.begin & database <= month.end) %>%
                        group_by(class, database) %>%
                        summarise(sum = sum(Outflow) - sum(Inflow))

                g <- ggplot(data = temp.df, aes(x = database, y = sum, fill = class )) +
                        geom_bar( stat = "identity", width = 20) +
                        theme_minimal() +
                        labs(title = "Monthly Expenses by Class",
                             x = "Months", y = "Amount") + 
                        scale_fill_discrete(name = "Expenses Classes")
                
                return.df <- temp.df %>% spread(database, sum)
                return.df$total <- rowSums(return.df[,-1], na.rm = TRUE)
                return.df <- return.df %>% arrange(desc(total))
        }
        
        if(ggplotly) g <- ggplotly()
        return(list(df = return.df, plot = g))
}

summaryYNAB <- function(register.df, database) {
        
        database <- as.Date(database)
        
        currency <- unique(register.df$Outflow.currency)[1]
        
        # sum totals
        totals <- sumByMonth(register.df, month.begin = database, month.end = database)
        
        income <- as.numeric(totals$df %>% filter(type == "Income") %>% select(sum))
        expenses <- as.numeric(totals$df %>% filter(type == "Expenses") %>% select(sum))
        investments <- as.numeric(totals$df %>% filter(type == "Investments") %>% select(sum))
        
        # TO-DO: can't find out how to use locale to format currency
        income.formatted <- paste(currency, format(income, big.mark = ".", decimal.mark = ","))
        expenses.formatted <- paste(currency, format(expenses, big.mark = ".", decimal.mark = ","))
        investments.formatted <- paste(currency, format(investments, big.mark = ".", decimal.mark = ","))
        
        # sum by classes
        
        classes = loadClasses()
        register.df <- reclassifyYNABCategories(register.df, classes)
        
        sumClass.month <- sumByClass(register.df, database, database)
        sumClass.year <- sumByClass(register.df, format(database, "%Y-01-01"), database)
        
        return.list <- list( totals = totals, income = income, income.formatted = income.formatted,
                             expenses = expenses, expenses.formatted = expenses.formatted,
                             investments = investments,
                             investments.formatted = investments.formatted,
                             sumClass.month = sumClass.month,
                             sumClass.year = sumClass.year
        )
        
        return(return.list)
}
