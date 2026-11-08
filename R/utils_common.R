# =============================================================================
# Shared helpers used by the Forecasting and Aggregate Planning modules
# =============================================================================

# ---- Reading uploaded files (CSV or Excel) -----------------------------------
read_uploaded_table <- function(path, name, sheet = NULL) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path, sheet = if (is.null(sheet) || sheet == "") 1 else sheet)
  } else if (ext %in% c("csv", "txt")) {
    first <- readLines(path, n = 1, warn = FALSE)
    n_semi  <- lengths(regmatches(first, gregexpr(";", first, fixed = TRUE)))
    n_comma <- lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))
    if (n_semi > n_comma) {
      # Semicolon separated (e.g. Excel with Turkish locale): decimal comma
      df <- utils::read.csv(path, sep = ";", dec = ",", check.names = FALSE,
                            stringsAsFactors = FALSE)
    } else {
      df <- utils::read.csv(path, sep = ",", check.names = FALSE,
                            stringsAsFactors = FALSE)
    }
  } else {
    stop("Unsupported file type. Please upload a .csv, .xlsx or .xls file.")
  }
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  df
}

# Converts a column to numbers ("1.234,5" / "1,234.5" / " 12 " are handled)
to_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- gsub(" ", "", x, fixed = TRUE)
  both <- grepl(",", x, fixed = TRUE) & grepl(".", x, fixed = TRUE)
  x[both] <- gsub(".", "", x[both], fixed = TRUE)   # 1.234,5 -> 1234,5
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Names of the columns that are (mostly) numeric
numeric_columns <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(character(0))
  ok <- vapply(df, function(col) {
    v <- to_number(col)
    sum(!is.na(v)) >= max(2, 0.8 * sum(!is.na(col) & as.character(col) != ""))
  }, logical(1))
  names(df)[ok]
}

# Picks the first column whose name matches one of the patterns
guess_column <- function(cols, patterns, fallback = 1) {
  for (p in patterns) {
    hit <- grep(p, cols, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  if (length(cols) >= fallback) cols[fallback] else if (length(cols)) cols[1] else NULL
}

# ---- Error measures ------------------------------------------------------------
# Error convention used in class: e_t = F_t - D_t
error_measures <- function(actual, forecast) {
  ok <- !is.na(actual) & !is.na(forecast)
  e  <- forecast[ok] - actual[ok]
  d  <- actual[ok]
  idx <- which(ok)
  mape_ok <- d != 0
  list(
    MAD  = if (length(e)) mean(abs(e)) else NA_real_,
    MSE  = if (length(e)) mean(e^2) else NA_real_,
    MAPE = if (any(mape_ok)) 100 * mean(abs(e[mape_ok] / d[mape_ok])) else NA_real_,
    n    = length(e),
    from = if (length(idx)) min(idx) else NA,
    to   = if (length(idx)) max(idx) else NA
  )
}

error_table <- function(em) {
  span <- if (is.na(em$from)) "-" else paste0(em$from, "–", em$to)
  data.frame(
    Measure = c("MAD", "MSE", "MAPE (%)"),
    Value = c(em$MAD, em$MSE, em$MAPE),
    `Periods evaluated` = span,
    check.names = FALSE
  )
}

# Value of the selected criterion (used when searching for the best parameter)
criterion_value <- function(actual, forecast, criterion = "MSE") {
  em <- error_measures(actual, forecast)
  v <- em[[criterion]]
  if (is.null(v) || is.na(v)) Inf else v
}

# ---- Regression for initial values ---------------------------------------------
# Fits D_t = a + b t on the first m periods. a = level at t = 0, b = slope.
regression_init <- function(y, m = length(y)) {
  m <- max(2, min(m, length(y)))
  t <- seq_len(m)
  fit <- stats::lm(y[1:m] ~ t)
  list(a = unname(coef(fit)[1]), b = unname(coef(fit)[2]), m = m)
}

# ---- Small formatting helpers ----------------------------------------------------
fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}
fmt_money <- function(x, digits = 2) paste0("$", fmt_num(x, digits))

# Parses "0.5, 0.3, 0.2" into a numeric vector
parse_numbers <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(numeric(0))
  parts <- strsplit(txt, "[;,[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  suppressWarnings(as.numeric(parts))
}

# ---- Plot colours (same style as the class examples) -------------------------------
col_actual   <- "#555555"
col_forecast <- "#2C6FB7"
col_next     <- "#C8551B"
col_alt      <- "#2E8B57"
