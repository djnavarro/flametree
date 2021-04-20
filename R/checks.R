# collection of reusable minimal tests

check_not_null <- function(x, name) {
  if(is.null(x)) {
    stop("`", name, "` must not be null", call. = FALSE)
  }
}

# NOTE: don't forget that is.na(NaN) returns TRUE
check_not_na <- function(x, name) {
  if(any(is.na(x))) {
    stop("`", name, "` must not contain missing values", call. = FALSE)
  }
}

check_numeric <- function(x, name) {
  if(!is.numeric(x)) {
    stop("`", name, "` must be numeric", call. = FALSE)
  }
}

# NOTE: is.integer(1) returns FALSE, check_soft_integer lets this through
check_soft_integer <- function(x, name) {
  check_numeric(x, name)
  weird <- is.na(x) | is.infinite(x)
  if(any(weird) & !is.integer(x[weird])) {
    stop("`", name, "` must be integer valued", call. = FALSE)
  }
  if(any(x[!weird] != as.integer(x[!weird]))) {
    stop("`", name, "` must be integer valued", call. = FALSE)
  }
}

check_length_exact <- function(x, name, len) {
  if(length(x) != len) {
    stop("`", name, "` must have length ", len, call. = FALSE)
  }
}

check_length_minimum <- function(x, name, len) {
  if(!(length(x) >= len)) {
    stop("`", name, "` must be at least length ", len, call. = FALSE)
  }
}

check_value_minimum <- function(x, name, val) {
  check_numeric(x, name)
  if(any(x < val)) {
    stop("`", name, "` cannot be less than ", val, call. = FALSE)
  }
}

check_value_maximum <- function(x, name, val) {
  check_numeric(x, name)
  if(any(x > val)) {
    stop("`", name, "` cannot be greater than ", val, call. = FALSE)
  }
}
