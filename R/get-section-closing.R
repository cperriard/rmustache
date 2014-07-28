get_section_closing <- function (tree) {
    value <- 0
    for (i in 1:length(tree)) {
        if (tree[[i]]$type == "/") {
            value <- i
            break
        }
    }
    return(value)
}