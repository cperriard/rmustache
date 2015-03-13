#' Represents a rendering context by wrapping a view object and maintaining a reference to the parent context.

Context <- function Context(view, parentContext = NULL) {
    
    this <- new.env()
    this$view <- view
    this$cache <- list('.' = this$view) ## why not an empty list?
    this$parent <- parentContext
    
    
    # Creates a new context using the given view with this context as the parent.
    this$push <- function (view) {
        return(Context(view, this))
    }
    
    # Returns the value of the given name in this context, traversing
    # up the context hierarchy if the value is absent in this context's view.
    this$lookup <- function (name) {
        if (name %in% names(this$cache)) {
            value <- this$cache[[name]]
        } else {
            context <- this
            while (!is.null(context)) {
                if (str_detect(name, "\\.")) {
                    value <- context$view;
                    names <- unlist(str_split(name, "\\."))
                    index <- 0
                    while (!is.null(value) && index < length(names)) {
                        value <- value[[names[index]]]
                        index <- index + 1
                    }
                        
                } else if (class(context$view) == "list") {
                    value = context$view[[name]]
                }
                if (!is.null(value))
                    break
                context <- context$parent
            }
            this$cache[name] <- value
        }
        if (class(value) == "function") {
            value <- do.call(value, list(this = this$view)) 
        }
            
        return(value)
    }
    
    
    return(this)
}


