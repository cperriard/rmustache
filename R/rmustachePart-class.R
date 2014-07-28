#' @import methods
#' @export rmustachePart
#' @exportClass rmustachePart 
rmustachePart <- setRefClass("rmustachePart", 
                             fields = list(
                                 type = "character", 
                                 name = "ANY", 
                                 value = "ANY", 
                                 closed = "logical"
                             ), 
                             
                             methods = list(
                                 initialize = function (type = c("string", 
                                                                 "name", 
                                                                 "{", 
                                                                 "&", 
                                                                 "#", 
                                                                 "^", 
                                                                 "/", 
                                                                 ">", 
                                                                 "root"), 
                                                        closed = FALSE, 
                                                        name = NULL, 
                                                        value = NULL
                                                        ) {
                                     .self$type <- match.arg(type)
                                     .self$closed <- closed
                                     .self$name <- name
                                     .self$value <- value
                                     
                                 }
                                 , 
                                 show = function () {
                                     cat("--- rmustachePart: start ---") 
                                     if (.self$type != "string"){
                                         cat("\n    name:", .self$name)
                                     }
                                     cat("\n    type:", .self$type)
                                     if (.self$type %in% c("#", "^")) {
                                         cat("\n    section closed:", .self$closed)
                                     }
                                     cat("\n    value: ")
                                     if (class(.self$value) == "list") {
                                         cat("list of rustache parts:", length(.self$value), "part(s)\n")
                                     }
                                     methods::show(.self$value)
                                     cat("\n--- rmustachePart: end   ---\n"
                                         )
                                 }, 
                                 
                                 render = function (data) {
                                     is.in.data <- !is.null(.self$name) && .self$name %in% names(data)
                                     
                                     if (.self$type == "string") {
                                         return(ifelse(!is.null(.self$value), .self$value, ""))
                                     } else {
                                         if (.self$type %in% c("name", "{", "&")) {
                                             if (is.in.data) {
                                                 return(data[[.self$name]])
                                             } else {
                                                 return("")
                                             }
                                             
                                         } else if (.self$type == "#") {
                                             if (is.in.data && !identical(FALSE, data[[.self$name]]) && length(data[[.self$name]]) > 0) {
                                                 for (i in 1:length(.self$value)) {
                                                     .self$value[[i]] <- .self$value[[i]]$render(data)
                                                 }
                                                 return(paste0(unlist(.self$value), collapse = ""))
                                             } else {
                                                 return("")
                                             }
                                         } else if (.self$type == "^") {
                                             if (!is.in.data) {
                                                 if (is.list(.self$value)) {
                                                     for (i in 1:length(.self$value)) {
                                                         .self$value[[i]] <- .self$value[[i]]$render(data)
                                                     }
                                                     return(paste0(unlist(.self$value), collapse = ""))
                                                 } else {
                                                     return(ifelse(!is.null(.self$value), .self$value, ""))
                                                 }
                                             }
                                         } else if (.self$type %in% c("root", ">")) {
                                             for (i in 1:length(.self$value)) {
                                                 .self$value[[i]] <- .self$value[[i]]$render(data)
                                             }
                                             return(paste0(unlist(.self$value), collapse = ""))
                                         }
                                     }
                                         
                                 }
                                 
                             )
)
