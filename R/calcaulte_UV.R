#' Calculation of Unrelated Variety based on Korean Business Survey data.
#'
#' This function allows you to calculate Unrelated Variety index based on 2015 administration.
#' @param data Put your Korean Business Survey data dataframe.
#' @param year Put `year` you want to calculate.
#' @export
#' @examples
#' data <- read.table("KoreanBusinessSurvey.txt", sep = "\t", colClasses = "character")
#' UV(data = data, year = 2013)


UV <- function(data, year) {

        "%ni%" <- Negate("%in%")
        sgg_name <- paste0("sgg", year)
        sdcode <- c("11", "21", "22", "23", "24", "25", "26", "29")

        configYear <- config_year(input_year = year)




        if (year %ni% "2015") {

                sgg_code.df <- eval(parse(text=paste0("sgg_code_", year)))

                data <- data %>%
                        dplyr::mutate(sggCode = paste0(substr(data[, configYear$col_sgg],
                                                       configYear$ilocSido,
                                                       configYear$ilocSgg), "0")) %>%
                        dplyr::mutate(!!quo_name(sgg_name) :=
                                       case_when(data[, configYear$col_sido] %in% sdcode ~ paste0(data[, configYear$col_sido], "000"),
                                                 TRUE ~ paste0(data[, configYear$col_sido], sggCode)))

                data <- dplyr::left_join(x = data,
                                  y = sgg_code.df,
                                  by = sgg_name) %>%
                        dplyr::mutate(digit_5 = do.call(paste0, data[configYear$rangeDigit5])) %>%
                        dplyr::mutate(employee = as.numeric(data[, configYear$ilocEmployee])) %>%
                        dplyr:: select(c(sgg2015, digit_5, employee)) %>%
                        dplyr::group_by(sgg2015, digit_5) %>%
                        dplyr::summarise(employee = sum(employee))

                uv.val <- data %>%
                        dplyr::mutate(digit_2 = substr(digit_5, 1, 3))
                sum_emp <- uv.val %>%        # Create total employee by sigungu
                        dplyr::group_by(sgg2015) %>%
                        dplyr::summarise(sum_employee = sum(employee))
                uv.val <- dplyr::left_join(x = uv.val,
                                y = sum_emp,
                                by = 'sgg2015') %>%    # Calculate pi (shares of 5 digit)
                        dplyr::mutate(pi = employee/sum_employee)

                uv_col_name <- paste0("UV", year)

                pg <- uv.val %>%
                        dplyr::group_by(sgg2015, digit_2) %>%
                        dplyr::summarise(pg_sum = sum(pi)) %>%                                 # Calculate Pg
                        dplyr::mutate(log_pg = log2(1/pg_sum)) %>%                             # Calculate log2(1/Pg)
                        dplyr::mutate(UV = pg_sum * log_pg)

                uv.val <- pg %>%                                                            # Sum of UV by sigungu
                        dplyr::group_by(sgg2015) %>%
                        dplyr::summarise(UV = sum(UV)) %>%
                        dplyr:: rename(!!quo_name(uv_col_name) := UV)




        } else if (year == "2015"){

                data <- data %>%
                        dplyr::mutate(sggCode = paste0(substr(data[, configYear$col_sgg],
                                                       configYear$ilocSido,
                                                       configYear$ilocSgg), "0")) %>%
                        dplyr::mutate(!!quo_name(sgg_name) :=
                                       case_when(data[, configYear$col_sido] %in% sdcode ~ paste0(data[, configYear$col_sido], "000"),
                                                 TRUE ~ paste0(data[, configYear$col_sido], sggCode)))
                data <- data %>%
                        dplyr::mutate(digit_5 = do.call(paste0, data[configYear$rangeDigit5])) %>%
                        dplyr::mutate(employee = as.numeric(data[, configYear$ilocEmployee])) %>%
                        dplyr:: select(c(sgg2015, digit_5, employee)) %>%
                        dplyr::group_by(sgg2015, digit_5) %>%
                        dplyr::summarise(employee = sum(employee))

                # UV <- data
                uv.val <- data %>%
                        dplyr::mutate(digit_2 = substr(digit_5, 1, 3))
                sum_emp <- uv.val %>%        # Create total employee by sigungu
                        dplyr::group_by(sgg2015) %>%
                        dplyr::summarise(sum_employee = sum(employee))
                uv.val <- dplyr::left_join(x = uv.val,
                                    y = sum_emp,
                                    by = 'sgg2015') %>%    # Calculate pi (shares of 5 digit)
                        dplyr::mutate(pi = employee/sum_employee)

                uv_col_name <- paste0("UV", year)

                pg <- uv.val %>%
                        dplyr::group_by(sgg2015, digit_2) %>%
                        dplyr::summarise(pg_sum = sum(pi)) %>%                                 # Calculate Pg
                        dplyr::mutate(log_pg = log2(1/pg_sum)) %>%                             # Calculate log2(1/Pg)
                        dplyr::mutate(UV = pg_sum * log_pg)

                uv.val <- pg %>%                                                            # Sum of UV by sigungu
                        dplyr::group_by(sgg2015) %>%
                        dplyr::summarise(UV = sum(UV)) %>%
                        dplyr:: rename(!!quo_name(uv_col_name) := UV)
        }

        return(uv.val)


}


