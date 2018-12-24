#' configure year Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords config_year
#' @export
#' @examples
#' config_year()
config_year <- function(input_year){
        if (input_year %in% seq(1997, 2010)) {
                year <- as.character(input_year)
                ilocSido <- 0 + 1
                ilocSgg <- 1 + 1
                rangeDigit5 <- c("V3", "V4", "V5", "V6", "V7")
                ilocEmployee <- 29 + 1
                col_sgg <- 2
                col_sido <- 1
        } else if (input_year %in% seq(1994, 1995)) {
                year <- as.character(input_year)
                ilocSido <- 0 + 1
                ilocSgg <- 1 + 1
                rangeDigit5 <- c("V3", "V4", "V5", "V6", "V7")
                ilocEmployee <- 15 + 1
                col_sgg <- 2
                col_sido <- 1
        } else if (input_year == 1996) {
                year <- as.character(input_year)
                ilocSido <- 0 + 1
                ilocSgg <- 1 + 1
                rangeDigit5 <- c("V3", "V4", "V5", "V6", "V7")
                ilocEmployee <- 25 + 1
                col_sgg <- 2
                col_sido <- 1
        } else if (input_year %in% seq(2011, 2012)) {
                year <- as.character(input_year)
                ilocSido <- 0 + 1
                ilocSgg <- 1 + 1
                rangeDigit5 <- c("V3", "V4", "V5", "V6", "V7")
                ilocEmployee <- 27 + 1
                col_sgg <- 2
                col_sido <- 1
        } else if (input_year %in% seq(2013, 2014)) {
                year <- as.character(input_year)
                ilocSido <- 0 + 1
                ilocSgg <- 1 + 1
                rangeDigit5 <- c("V4", "V5", "V6", "V7", "V8")
                ilocEmployee <- 28 + 1
                col_sgg <- 3
                col_sido <- 2
        } else if (input_year == 2015) {
                year <- as.character(input_year)
                ilocSido <- 0 + 1
                ilocSgg <- 1 + 1
                rangeDigit5 <- c("V9", "V10", "V11", "V12", "V13")
                ilocEmployee <- 13 + 1
                col_sgg <- 3
                col_sido <- 2
        }
        return(year)
        return(ilocSido)
        return(ilocSgg)
        return(rangeDigit5)
        return(ilocEmployee)
        return(col_sgg)
        return(col_sido)
}

sdcode <- c("11", "21", "22", "23", "24", "25", "26", "29")

sgg_code.df <-
UV <- function(data){
        config_fun <- config_year()
        '%ni%' <- Negate('%in%')
        sgg_name <- paste0("sgg", year)

        sgg_code.df <- eval(parse(text=paste0("sgg_code_",year)))


        data <- left_join(x = data,
                          y = sgg_code.df,
                          by = sgg_name) %>%
                mutate(digit_5 = do.call(paste0, data[rangeDigit5])) %>%
                mutate(employee = as.numeric(data[, ilocEmployee])) %>%
                select(c(sgg2015, digit_5, employee)) %>%
                group_by(sgg2015, digit_5) %>%
                summarise(employee = sum(employee))

        UV <- data %>%
                mutate(digit_2 = substr(digit_5, 1, 3))
        sum_emp <- UV %>%        # Create total employee by sigungu
                group_by(sgg2015) %>%
                summarise(sum_employee = sum(employee))
        UV <- left_join(x = UV,
                        y = sum_emp,
                        by = 'sgg2015') %>%    # Calculate pi (shares of 5 digit)
                mutate(pi = employee/sum_employee)

        uv_col_name <- paste0("UV", year)

        pg <- UV %>%
                group_by(sgg2015, digit_2) %>%
                summarise(pg_sum = sum(pi)) %>%                                 # Calculate Pg
                mutate(log_pg = log2(1/pg_sum)) %>%                             # Calculate log2(1/Pg)
                mutate(UV = pg_sum * log_pg)

        UV <- pg %>%                                                            # Sum of UV by sigungu
                group_by(sgg2015) %>%
                summarise(UV = sum(UV)) %>%
                rename(!!quo_name(uv_col_name) := UV)

        return(UV)



}


RV <- function(data) {
        config_fun <- config_year()
        '%ni%' <- Negate('%in%')
        sgg_name <- paste0("sgg", year)

        sgg_code.df <- eval(parse(text=paste0("sgg_code_",year)))


        data <- left_join(x = data,
                          y = sgg_code.df,
                          by = sgg_name) %>%
                mutate(digit_5 = do.call(paste0, data[rangeDigit5])) %>%
                mutate(employee = as.numeric(data[, ilocEmployee])) %>%
                select(c(sgg2015, digit_5, employee)) %>%
                group_by(sgg2015, digit_5) %>%
                summarise(employee = sum(employee))


        RV <- data %>%                            # Create digit 2 code
                mutate(digit_2 = substr(digit_5, 1, 3))

        sum_emp <- RV %>%                              # Calculate pi (shares of digit 5)
                group_by(sgg2015) %>%
                summarise(sum_employee = sum(employee))
        RV <- left_join(x = RV, y = sum_emp, by = 'sgg2015') %>%
                mutate(pi = employee/sum_employee)

        pg <- RV %>%                                                            # Create Pg
                group_by(sgg2015, digit_2) %>%
                summarise(pg_sum = sum(pi))

        RV <- left_join(x = RV, y = pg, by = c('sgg2015', 'digit_2'))



        rv_col_name <- paste0("RV", year)
        ####_Hg ####
        RV <- RV %>%
                mutate(hg = (pi/pg_sum) * (log2(1/(pi/pg_sum)))) %>%            # Calculate Hg
                group_by(sgg2015, digit_2) %>%
                summarise(hg_sum = sum(hg),
                          pg = sum(pi)) %>%
                mutate(RV = pg * hg_sum) %>%                                # Calculate RV
                group_by(sgg2015) %>%                                           # Sum of RV by sigungu
                summarise(RV = sum(RV)) %>%
                rename(!!quo_name(rv_col_name) := RV)

        return(RV)
}



