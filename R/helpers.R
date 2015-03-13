# Forms the given array of `tokens` into a nested tree structure where
# tokens that represent a section have two additional items: 1) an array of
# all tokens that appear in that section and 2) the index in the original
# template that represents the end of that section.

nestTokens <- function (tokens) {
    nestedTokens <- list()
    collector <- nestedTokens
    sections <- list()
    
    
    for (i in 1:length(tokens)) {
        token <- tokens[[i]]
        type <- token[[1]]
        if (type %in% c("#", "^")) {
            collector[[length(collector) + 1]] <- token
            sections[[length(sections) + 1]] <- token
            token[[5]] <- list()
            collector <- token[[5]]
            
        } else if (type == "/") {
            section <- sections[[length(sections)]]
            sections[[length(sections)]] <- NULL
            section[[6]] <- token[[3]]
            if (length(sections) > 0) {
                collector <- sections[[length(sections)]][[4]]
            } else {
                collector <- nestedTokens
            }
        } else {
            collector[[length(collector) + 1]] <- token
        }
    }
    
    return(nestedTokens)
}


