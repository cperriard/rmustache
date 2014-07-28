#' @export
nest_parts <- function (tree) {
    while ((i <- get_section_closing(tree)) > 0) {
        #browser()
        # get opening tag
        for (j in i:1) {
            if (!tree[[j]]$closed && tree[[j]]$type %in% c("#", "^")) {
                break
            }
        }
        
        if (i - j > 0) {
            tree[[j]]$value <- tree[(j+1):(i-1)]
            tree[[j]]$closed <- TRUE
        }
        tree[(j+1):i] <- NULL
    }
    return(tree)
}