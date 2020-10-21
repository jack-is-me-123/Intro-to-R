#' Flag possible reading disability
#'
#' @param math_score a score out of 100
#' @param english_score a score out of 100
#' @param history_score a score out of 100
#'
#' @return a flag for those who may have reading disability
#'
#' @examples reading_flag(math_score = 80, english_score = 60, history_score = 40)
reading_flag <- function(math_score, english_score, history_score) {
  case_when(math_score >= 1.5*english_score & 
              math_score >= 1.5* history_score ~ "FLAG",
            TRUE ~ "no flag")
}