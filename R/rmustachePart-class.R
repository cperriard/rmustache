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
                                         cat("list of rmustache parts:", length(.self$value), "part(s)\n")
                                     }
                                     methods::show(.self$value)
                                     cat("\n--- rmustachePart: end   ---\n"
                                         )
                                 }, 
                                 
                                 render = function (data, context = NULL) {
                                     if (is.null(context)) {
                                         context <- rmustacheContext(data)
                                     }
                                     
                                     if (.self$type == "string") {
                                         return(ifelse(!is.null(.self$value), .self$value, ""))
                                     } else {
                                         if (.self$type != "root") {
                                             .value <- context$lookup(.self$name)
                                         }
                                         
                                         if (.self$type %in% c("name", "{", "&")) {
                                             #browser()
                                             return(ifelse(!is.null(.value), .value, ""))
                                                 
                                         } else if (.self$type == "#") {
                                             #browser()
                                             if (!is.null(.value) && !identical(FALSE, .value) && length(.value) > 0) {
                                                 .stash <- character(0)
                                                 .tmp <- character(0)
                                                 for (j in 1:length(.value)) {
                                                     if (length(.value) > 1) {
                                                         .value.tmp <- .value[[j]]
                                                     } else {
                                                         .value.tmp <- .value
                                                     }
                                                     for (i in 1:length(.self$value)) {
                                                         .tmp[[i]] <- .self$value[[i]]$render(data, context = rmustacheContext(.value.tmp, context))
                                                     }
                                                     .stash <- c(.stash, unlist(.tmp))
                                                     .tmp <- character(0)
                                                 }
                                                 
                                                 return(paste0(.stash, collapse = ""))
                                             } else {
                                                 return("")
                                             }
                                         } else if (.self$type == "^") {
#                                              if (!is.in.data) {
#                                                  if (is.list(.self$value)) {
#                                                      for (i in 1:length(.self$value)) {
#                                                          .self$value[[i]] <- .self$value[[i]]$render(data)
#                                                      }
#                                                      return(paste0(unlist(.self$value), collapse = ""))
#                                                  } else {
#                                                      return(ifelse(!is.null(.self$value), .self$value, ""))
#                                                  }
#                                              }
                                         } else if (.self$type %in% c("root", ">")) {
                                             for (i in 1:length(.self$value)) {
                                                 .self$value[[i]] <- .self$value[[i]]$render(data, context)
                                             }
                                             return(paste0(unlist(.self$value), collapse = ""))
                                         }
                                     }
                                         
                                 }
                                 
                             )
)
