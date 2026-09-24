#' Detect Five Consecutive Increases in Incidence
#'
#' Flags an alarm when the incidence (case count) has increased strictly in each
#' of the five preceding transitions. Thus, an alarm requires six consecutive
#' observations, with every observation larger than the one before it. Ties and
#' decreases reset the run of increases.
#'
#' Only the final `number_of_time_units` observations are monitored. Earlier
#' observations are used to establish whether a run was already in progress at
#' the start of the monitoring period.
#'
#' @param data_aggregated A data frame of aggregated surveillance data containing
#'   a numeric `cases` column.
#' @param number_of_time_units Integer specifying the number of final time units
#'   for which alarms should be generated.
#' @param time_unit A character specifying the aggregation unit. One of
#'   `"weekly"`, `"biweekly"`, or `"monthly"`.
#'
#' @return `data_aggregated` with logical `alarms` and numeric `upperbound` and
#'   `expected` columns added. The latter two columns are `NA` because this
#'   rule-based method does not estimate a baseline.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   year = rep(2024, 7),
#'   week = 1:7,
#'   cases = c(1, 2, 3, 4, 5, 6, 4)
#' )
#' get_signals_consecutive_increases(data, number_of_time_units = 7)
get_signals_consecutive_increases <- function(data_aggregated,
                                              number_of_time_units = 52,
                                              time_unit = "weekly") {
  checkmate::assert_data_frame(data_aggregated, min.rows = 1)
  checkmate::assert_names(names(data_aggregated), must.include = "cases")
  checkmate::assert_numeric(data_aggregated$cases, any.missing = FALSE)
  checkmate::assert_integerish(
    number_of_time_units,
    lower = 1,
    len = 1
  )
  checkmate::assert_choice(
    time_unit,
    choices = c("weekly", "biweekly", "monthly"),
    null.ok = FALSE
  )

  if (number_of_time_units > nrow(data_aggregated)) {
    warning(paste0(
      "The number of time units you want to generate alarms for (n = ",
      number_of_time_units, ") is higher than the number of time units ",
      "you have in your data (n = ", nrow(data_aggregated), ")."
    ))
    return(NULL)
  }

  increases <- c(FALSE, diff(data_aggregated$cases) > 0)
  run_length <- sequence(rle(increases)$lengths)
  run_length[!increases] <- 0L
  alarms <- run_length >= 5L

  monitoring_start <- nrow(data_aggregated) - number_of_time_units + 1L
  alarms[seq_len(monitoring_start - 1L)] <- NA

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- NA_real_
  data_aggregated$expected <- NA_real_
  data_aggregated
}
