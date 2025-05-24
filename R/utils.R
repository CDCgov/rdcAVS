#' Resolve non-ASCII character issues
#'
#' @param file_path `str` Path to file where non-ASCII characters are located.
#' @param output_path `str` Output path
#'
#' @returns NULL
#' @keywords internal
process_non_ascii_file <- function(file_path, output_path) {
  lines <- readLines(file_path, encoding = "UTF-8")
  escaped_lines <- vapply(lines, escape_non_ascii_only, character(1))
  writeLines(escaped_lines, output_path, useBytes = TRUE)
}


#' Function to escape only non-ASCII characters that are non-valid R
#'
#' @param line `str` A string.
#'
#' @returns `str` A string to escape non-ASCII characters
#' @keywords internal
#'
escape_non_ascii_only <- function(line) {
  chars <- strsplit(line, "")[[1]]
  escaped <- vapply(chars, function(char) {
    if (!is_ascii_char(char) && !(char %in% c('"', "'"))) {
      stringi::stri_escape_unicode(char)
    } else {
      char
    }
  }, character(1))
  paste0(escaped, collapse = "")
}

#' Function to safely check if a character is ASCII
#'
#' @param char `chr`
#'
#' @returns `bool` Whether it is an ASCII character or not
#' @keywords internal
#'
is_ascii_char <- function(char) {
  all(charToRaw(char) <= as.raw(127))
}
