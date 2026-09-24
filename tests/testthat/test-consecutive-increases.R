test_that("five consecutive increases trigger an alarm", {
  data <- data.frame(
    year = rep(2024, 10),
    week = seq_len(10),
    cases = c(1, 2, 3, 4, 5, 6, 6, 7, 8, 9)
  )

  result <- get_signals_consecutive_increases(
    data,
    number_of_time_units = 10
  )

  expect_identical(
    result$alarms,
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_true(all(is.na(result$upperbound)))
  expect_true(all(is.na(result$expected)))
})

test_that("alarms are limited to the monitoring period", {
  data <- data.frame(
    year = rep(2024, 8),
    week = seq_len(8),
    cases = seq_len(8)
  )

  result <- get_signals_consecutive_increases(
    data,
    number_of_time_units = 2
  )

  expect_true(all(is.na(result$alarms[1:6])))
  expect_identical(result$alarms[7:8], c(TRUE, TRUE))
})

test_that("five increases method works through the public API", {
  data <- data.frame(
    case_id = as.character(seq_len(21)),
    date_report = as.Date("2024-01-01") + c(
      rep(0, 1), rep(7, 2), rep(14, 3), rep(21, 4), rep(28, 5), rep(35, 6)
    ),
    age = rep(20L, 21)
  )

  result <- get_signals(
    preprocess_data(data),
    method = "five increases",
    number_of_time_units = 6
  )

  expect_identical(result$alarms, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_true(all(result$method == "five increases"))
})

test_that("ties and decreases reset consecutive increases", {
  data <- data.frame(cases = c(1, 2, 3, 3, 4, 5, 6, 7, 6, 7, 8, 9, 10, 11))
  result <- get_signals_consecutive_increases(data, number_of_time_units = 14)

  expect_equal(which(result$alarms), c(8, 14))
})
