#' @import methods
#' @export rmustacheContext
#' @exportClass rmustacheContext
rmustacheContext <- setRefClass("rmustacheContext", 
                                fields = list(
                                    view = "ANY", 
                                    cache = "ANY", 
                                    parent = "ANY"
                                    ), 
                                methods = list(
                                    
                                    initialize = function (view = NULL, parent = NULL) {
                                        if (!is.null(view)) {
                                            .self$view <- view
                                        } else {
                                            .self$view <- list()
                                        }
                                        .self$parent <- parent
                                        .self$cache <- list("." = .self$view)
                                    }, 
                                    
                                    push = function (view) {
                                        return(rmustacheContext(view, parent = .self))
                                    }, 
                                    
                                    lookup = function (name) {
                                        value <- NULL
                                        #browser()

                                        if (name %in% names(.self$cache)) {
                                            value <- .self$cache[[name]]
                                        } else {
                                            context <- .self
                                            
                                            while (!is.null(context)) {
#                                                 if (name.indexOf('.') > 0) {
#                                                     value = context.view;
#                                                     names = name.split('.');
#                                                     index = 0;
#                                                     
#                                                     while (value != null && index < names.length)
#                                                         value = value[names[index++]];
#                                                 } else {
                                                    value <- context$view[[name]]
#                                                 }
                                                
                                                if (!is.null(value)) {
                                                    break
                                                }
                                                    
                                                
                                                context <- context$parent;
                                            }
                                            
                                            .self$cache[[name]] <- value
                                        }
                                        
#                                         if (isFunction(value))
#                                             value = value.call(this.view);
                                        
                                        return(value)
                                    }
                                )
)