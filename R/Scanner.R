#' String Scanner
#' 
#' A simple string scanner that is used by the template parser to find tokens in template strings.
#' 
#' @param string character to initialize the Scanner with
#' @importFrom stringr str_locate str_length
Scanner <- function (string) {
    this <- new.env()
    this$string <- string
    this$tail <- string
    this$pos <- 1
    
    # Returns `true` if the tail is empty (end of string).
    this$eos <- function() {
        return(this$tail == "")
    }
    
    
    # Tries to match the given regular expression at the current position.
    # Returns the matched text if it can match, the empty string otherwise.
    this$scan <- function (pattern) {
        match = str_locate(this$tail, pattern)
        if (is.na(match[1]) || match[1] != 1) {
            return("")
        }
        string = substring(this$tail, 1, match[2])
        this$tail = substring(this$tail, str_length(string) + 1)
        this$pos <- this$pos + str_length(string)
        return(string)
    }
    
    # Skips all text until the given regular expression can be matched. Returns
    # the skipped string, which is the entire tail if no match can be made.
    this$scanUntil = function (pattern) {
        index = str_locate(this$tail, pattern)[1]
        if (is.na(index)) {
            match <- this$tail
            this$tail <- ""
        } else if (index == 1) {
            match <- ""
        } else {
            match <- substring(this$tail, 1, index-1)
            this$tail <- substring(this$tail, index)
        }
        this$pos <- this$pos + str_length(match)
        return(match)
    }
    
    class(this) <- "Scanner"
    return(this)
}











