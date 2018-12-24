#' Calculation of Related Variety based on Korean Business Survey data.
#'
#' This function allows you to calculate Related Variety index based on 2015 administration.
#' @param data Put your Korean Business Survey data dataframe.
#' @param year Put `year` you want to calculate.
#' @export
#' @examples
#' data <- read.table("KoreanBusinessSurvey.txt", sep = "\t", colClasses = "character")
#' RV(data = data, year = 2013)


RV <- function(data, year) {

        "%ni%" <- Negate("%in%")
        sgg_name <- paste0("sgg", year)
        sdcode <- c("11", "21", "22", "23", "24", "25", "26", "29")

        configYear <- config_year(input_year = year)
        # sgg_code.df <- eval(parse(text=paste0("sgg_code_", year)))



        if (year %ni% "2015") {
                sgg_code.df <- eval(parse(text=paste0("sgg_code_", year)))

                data <- data %>%
                        mutate(sggCode = paste0(substr(data[, configYear$col_sgg],
                                                       configYear$ilocSido,
                                                       configYear$ilocSgg), "0")) %>%
                        mutate(!!quo_name(sgg_name) :=
                                       case_when(data[, configYear$col_sido] %in% sdcode ~ paste0(data[, configYear$col_sido], "000"),
                                                 TRUE ~ paste0(data[, configYear$col_sido], sggCode)))

                data <- left_join(x = data,
                                  y = sgg_code.df,
                                  by = sgg_name) %>%
                        mutate(digit_5 = do.call(paste0, data[configYear$rangeDigit5])) %>%
                        mutate(employee = as.numeric(data[, configYear$ilocEmployee])) %>%
                        select(c(sgg2015, digit_5, employee)) %>%
                        group_by(sgg2015, digit_5) %>%
                        summarise(employee = sum(employee))

                rv.val <- data %>%                            # Create digit 2 code
                        mutate(digit_2 = substr(digit_5, 1, 3))

                sum_emp <- rv.val %>%                              # Calculate pi (shares of digit 5)
                        group_by(sgg2015) %>%
                        summarise(sum_employee = sum(employee))
                rv.val <- left_join(x = rv.val, y = sum_emp, by = 'sgg2015') %>%
                        mutate(pi = employee/sum_employee)

                pg <- rv.val %>%                                                            # Create Pg
                        group_by(sgg2015, digit_2) %>%
                        summarise(pg_sum = sum(pi))

                rv.val <- left_join(x = rv.val, y = pg, by = c('sgg2015', 'digit_2'))



                rv_col_name <- paste0("RV", year)
                ####_Hg ####
                rv.val <- rv.val %>%
                        mutate(hg = (pi/pg_sum) * (log2(1/(pi/pg_sum)))) %>%            # Calculate Hg
                        group_by(sgg2015, digit_2) %>%
                        summarise(hg_sum = sum(hg),
                                  pg = sum(pi)) %>%
                        mutate(RV = pg * hg_sum) %>%                                # Calculate RV
                        group_by(sgg2015) %>%                                           # Sum of RV by sigungu
                        summarise(RV = sum(RV)) %>%
                        rename(!!quo_name(rv_col_name) := RV)




        } else if (year == "2015"){

                data <- data %>%
                        mutate(sggCode = paste0(substr(data[, configYear$col_sgg],
                                                       configYear$ilocSido,
                                                       configYear$ilocSgg), "0")) %>%
                        mutate(!!quo_name(sgg_name) :=
                                       case_when(data[, configYear$col_sido] %in% sdcode ~ paste0(data[, configYear$col_sido], "000"),
                                                 TRUE ~ paste0(data[, configYear$col_sido], sggCode)))
                data <- data %>%
                        mutate(digit_5 = do.call(paste0, data[configYear$rangeDigit5])) %>%
                        mutate(employee = as.numeric(data[, configYear$ilocEmployee])) %>%
                        select(c(sgg2015, digit_5, employee)) %>%
                        group_by(sgg2015, digit_5) %>%
                        summarise(employee = sum(employee))

                # UV <- data
                rv.val <- data %>%                            # Create digit 2 code
                        mutate(digit_2 = substr(digit_5, 1, 3))

                sum_emp <- rv.val %>%                              # Calculate pi (shares of digit 5)
                        group_by(sgg2015) %>%
                        summarise(sum_employee = sum(employee))
                rv.val <- left_join(x = rv.val, y = sum_emp, by = 'sgg2015') %>%
                        mutate(pi = employee/sum_employee)

                pg <- rv.val %>%                                                            # Create Pg
                        group_by(sgg2015, digit_2) %>%
                        summarise(pg_sum = sum(pi))

                rv.val <- left_join(x = rv.val, y = pg, by = c('sgg2015', 'digit_2'))



                rv_col_name <- paste0("RV", year)
                ####_Hg ####
                rv.val <- rv.val %>%
                        mutate(hg = (pi/pg_sum) * (log2(1/(pi/pg_sum)))) %>%            # Calculate Hg
                        group_by(sgg2015, digit_2) %>%
                        summarise(hg_sum = sum(hg),
                                  pg = sum(pi)) %>%
                        mutate(RV = pg * hg_sum) %>%                                # Calculate RV
                        group_by(sgg2015) %>%                                           # Sum of RV by sigungu
                        summarise(RV = sum(RV)) %>%
                        rename(!!quo_name(rv_col_name) := RV)
        }

        return(rv.val)


}
