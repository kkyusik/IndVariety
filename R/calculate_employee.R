#' configure year Function
#'
#' This function allows you to express your love of cats.
#' @param data Load your Korea Business Survey data.
#' @param year Put `year` you want to analyze.
#' @keywords employee_number
#' @export
#' @examples
#' employee_number()


employee_number <- function(data, year) {
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

                emp_col_name <- paste0("employee", year)
                emp.df <- data %>% group_by(sgg2015) %>%
                        summarise(employee = sum(employee)) %>%
                        rename(!!quo_name(emp_col_name) := employee)





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


                emp_col_name <- paste0("employee", year)
                emp.df <- data %>% group_by(sgg2015) %>%
                        summarise(employee = sum(employee)) %>%
                        rename(!!quo_name(emp_col_name) := employee)
        }

        return(emp.df)
}
