

regex_escape <- function (x) {
    gsub("([\\.\\|\\{\\^\\$\\*\\+\\?\\[])", "\\\\\\1", x)
}
