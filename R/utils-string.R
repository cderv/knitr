# replace parts of a string with new values; `pos` is a matrix of positions and
# each row is a pair of [start, end]
str_replace = function(x, pos, value) {
  if (length(x) != 1) stop("Only a character scalar is supported.")
  # extract parts of the string that are outside [start, end]
  m = rbind(pos[, 1] - 1, pos[, 2] + 1)
  m = matrix(c(1, m, nchar(x)), nrow = 2)
  y = substring(x, m[1, ], m[2, ])
  paste(rbind(y, c(value, '')), collapse = '')
}

# insert a value after i-th character in the string x
str_insert = function(x, i, value) {
  if (i <= 0) return(paste0(value, x))
  n = nchar(x)
  if (n == 0 || i >= n) return(paste0(x, value))
  paste0(substr(x, 1, i), value, substr(x, i + 1, n))
}

# a simplified replacement for stringr::str_locate_all() that returns a list
# having an element for every element of 'string'; every list element is an
# integer matrix having a row per match, and two columns: 'start' and 'end'.
str_locate = function(x, pattern, all = TRUE) {
  out = (if (all) gregexpr else regexpr)(pattern, x, perl = TRUE)
  if (all) lapply(out, location) else location(out)
}

location = function(x) {
  len = attr(x, 'match.length')
  if (length(x) == 1 && x == -1) x = integer()
  cbind(start = x, end = x + len - 1L)
}

# a replacement for stringr::str_extract_all()
str_extract = function(x, pattern) {
  m = gregexpr(pattern, x, perl = TRUE)
  regmatches(x, m)
}

str_match = function(x, pattern) {
  # gregexec() was added in R 4.1.0; for lower versions of R, use fallback
  if (is.function(gregexec <- baseenv()[['gregexec']])) {
    m = gregexec(pattern, x, perl = TRUE)
  } else {
    x = unlist(str_extract(x, pattern))
    m = regexec(pattern, x, perl = TRUE)
  }
  do.call(cbind, regmatches(x, m))
}
