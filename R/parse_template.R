
#' @import stringr
#' @export
parse_template <- function (template) {
    
    string <- template
    tail <- template
    tree <- list()
    open.tags <- c()
    
    delimiter.open <- regex_escape("{{")
    delimiter.close <- regex_escape("}}")
    
    while (tail != "") {
        #browser()
        pos <- regexpr(delimiter.open, tail)
        match.length <- attr(pos, "match.length")
        if (pos > 1 | pos == -1) {
            if (pos == -1) {
                pos <- nchar(tail)+1
                match.length <- 0
            }
            value <- substr(tail, 1, pos-1)
            
        } else {
            value <- ""
        }
        if (value != ""){
            #browser()
            # split string into newline and remove lines containing only whitespace
            splitted.value <- unlist(str_split(value, "\n"))
            if (length(splitted.value) > 1){
                for (i in 1:length(splitted.value)) {
                    if ((str_trim(splitted.value[i], side = "left") != "" && 
                            i < length(splitted.value)) | 
                            (str_trim(splitted.value[i], side = "left") == "") && 
                            i == 1 && 
                            tree[[length(tree)]]$type %in% c("name")) {
                        splitted.value[i] <- paste0(splitted.value[i], "\n") #paste0(str_trim(splitted.value[i], side = "left"), "\n")
                    }
                }
            }
            
            value <- paste0(splitted.value, collapse = "")
            
#             if (length(tree) > 0 && 
#                     length(splitted.value) > 1 && 
#                     splitted.value[1] == "" &&
#                     tree[[length(tree)]]$type == "name") {
#                 browser()
#                 value <- paste0("\n", value)
#             }
            
            if (value != ""){
                tree <- c(tree, 
                          list(rmustachePart(type = "string", 
                                             value = value)))
            }
        }
        
        tail <- substr(tail, pos+match.length, nchar(tail))
        
        type.pos <- regexpr("^[\\&\\{#^/>=]", tail)
        if (type.pos == 1) {
            type <- substr(tail, 1, 1)
        } else {
            type = "name"
        }
        
        if (type == "{") {
            pos <- regexpr("}", tail)
            name <- substr(tail, 2, pos-1)
            tail <- substr(tail, pos+attr(pos, "match.length"), nchar(tail))
            pos <- regexpr(delimiter.close, tail)
            tail <- substr(tail, pos+attr(pos, "match.length"), nchar(tail))
        } else {
            pos <- regexpr(delimiter.close, tail)
            start <- ifelse(type == "name", 1, 2)
            name <- substr(tail, start, pos-1)
            tail <- substr(tail, pos+attr(pos, "match.length"), nchar(tail))
        }
        if (type == "=") {
            #change delimiter
            name <- substr(name, 1, nchar(name)-1)
            delimiter <- unlist(strsplit(name, "[[:space:]]+"))
            delimiter.open <- regex_escape(delimiter[1])
            delimiter.close <- regex_escape(delimiter[2])
            
            
        } else if (name != ""){
            if (type %in% c("#", "^")) {
                open.tags <- c(open.tags, name)
            }
            if (type == "/") {
                index <- open.tags == name
                if (sum(index) == 0) {
                    stop("Template error: There is a closing tag (", name, ") without corresponding opening tag.")
                }
                open.tags <- open.tags[-which.max(index)]
            }
            
            tree <- c(tree, list(rmustachePart(type = type, name = name)))
        }
    }
    
    if (length(open.tags) > 0) {
        stop("Template error: There are one or more opened tags that are not closed. ('", paste0(open.tags, collapse = "', '"), "')")
    }
    return(tree)
}
