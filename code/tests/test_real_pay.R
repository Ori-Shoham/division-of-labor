# =============================================================================
# File: code/tests/test_real_pay.R
#
# Purpose:
#   Lightweight checks for code/lib/real_pay.R without touching restricted data.
# =============================================================================

source("code/lib/real_pay.R")

tmp_index <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(
    ym = as.Date(c("2019-12-01", "2020-01-01")),
    index = c(100, 125)
  ),
  tmp_index,
  row.names = FALSE
)

df <- data.frame(
  ym = as.Date(c("2019-12-01", "2020-01-01", "2020-01-01")),
  paygu_dv = c(100, 100, -9),
  basrate = c(10, 20, -1)
)

out <- add_real_pay_vars(
  df,
  ym_col = "ym",
  vars = c("paygu_dv", "basrate"),
  price_index_path = tmp_index,
  base_ym = as.Date("2019-12-01")
)

stopifnot(out$paygu_dv_real[1] == 100)
stopifnot(out$paygu_dv_real[2] == 80)
stopifnot(is.na(out$paygu_dv_real[3]))
stopifnot(out$basrate_real[1] == 10)
stopifnot(out$basrate_real[2] == 16)
stopifnot(is.na(out$basrate_real[3]))

missing_month_failed <- FALSE
tryCatch(
  add_real_pay_vars(
    data.frame(
      ym = as.Date("2020-02-01"),
      paygu_dv = 100
    ),
    vars = "paygu_dv",
    price_index_path = tmp_index,
    base_ym = as.Date("2019-12-01")
  ),
  error = function(e) {
    missing_month_failed <<- grepl("missing required month", conditionMessage(e))
  }
)

stopifnot(missing_month_failed)

cat("real_pay helper tests passed.\n")
