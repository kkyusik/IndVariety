#' configure year Function
#'
#' This function allows you to express your love of cats.
#' @param input_year Put `year` you want to analyze.
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
                year = as.character(input_year)
                ilocSido = 0 + 1
                ilocSgg = 1 + 1
                rangeDigit5 = c("V4", "V5", "V6", "V7", "V8")
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
# return(year)
# return(ilocSido)
# return(ilocSgg)
# return(rangeDigit5)
# return(ilocEmployee)
# return(col_sgg)
# return(col_sido)
        # return(c(year, ilocSido))
        summary = list(year = year,
                       ilocSido = ilocSido,
                       ilocSgg= ilocSgg,
                       rangeDigit5 = rangeDigit5,
                       ilocEmployee = ilocEmployee,
                       col_sgg = col_sgg,
                       col_sido = col_sido)
        return(summary)
}




