# collection of reusable minimal error-throwing functions



# checks that have direct tests -------------------------------------------

ft__check_not_null <- function(x, name) {
  if(is.null(x)) {
    stop("`", name, "` must not be null", call. = FALSE)
  }
}

# NOTE: don't forget that is.na(NaN) returns TRUE
ft__check_not_na <- function(x, name) {
  if(any(is.na(x))) {
    stop("`", name, "` must not contain missing values", call. = FALSE)
  }
}

ft__check_not_closure <- function(x, name) {
  if(typeof(x) == "closure" | class(x) == "function") {
    stop("`", name, "` must not be a function or closure", call. = FALSE)
  }
}

ft__check_numeric <- function(x, name) {
  if(!is.numeric(x)) {
    stop("`", name, "` must be numeric", call. = FALSE)
  }
}

ft__check_character <- function(x, name) {
  if(!is.character(x)) {
    stop("`", name, "` must be character", call. = FALSE)
  }
}

ft__check_logical <- function(x, name) {
  if(!is.logical(x)) {
    stop("`", name, "` must be logical", call. = FALSE)
  }
}

ft__check_atomic <- function(x, name) {
  if(!is.atomic(x)) {
    stop("`", name, "` must be atomic", call. = FALSE)
  }
}

# NOTE: is.integer(1) returns FALSE, ft__check_soft_integer lets this through
ft__check_soft_integer <- function(x, name) {
  ft__check_numeric(x, name)
  weird <- is.na(x) | is.infinite(x)
  if(any(weird) & !is.integer(x[weird])) {
    stop("`", name, "` must be integer valued", call. = FALSE)
  }
  if(any(x[!weird] != as.integer(x[!weird]))) {
    stop("`", name, "` must be integer valued", call. = FALSE)
  }
}



# checks that are tested only indirectly ----------------------------------

ft__check_length_exact <- function(x, name, len) {
  if(length(x) != len) {
    stop("`", name, "` must have length ", len, call. = FALSE)
  }
}

ft__check_length_minimum <- function(x, name, len) {
  if(!(length(x) >= len)) {
    stop("`", name, "` must be at least length ", len, call. = FALSE)
  }
}

ft__check_dataframe <- function(x, name) {
  if(!inherits(x, "data.frame")) {
    stop("`", name, "` must be a data frame", call. = FALSE)
  }
}

ft__check_value_minimum <- function(x, name, val) {
  ft__check_numeric(x, name)
  if(any(x < val)) {
    stop("`", name, "` cannot be less than ", val, call. = FALSE)
  }
}

# no longer used now that prune is gone
#
# ft__check_value_maximum <- function(x, name, val) {
#   ft__check_numeric(x, name)
#   if(any(x > val)) {
#     stop("`", name, "` cannot be greater than ", val, call. = FALSE)
#   }
# }
