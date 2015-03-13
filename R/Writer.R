
# A Writer knows how to take a stream of tokens and render them to a
# string, given a context. It also maintains a cache of templates to
# avoid the need to parse the same template twice.
Writer <- function () {
    this <- new.env()
    this$cache <- list()
    
    # Clears all cached templates in this writer.
    this$clearCache <- function () {
        this$cache <- list()
    }
    
    # Parses and caches the given `template` and returns the array of tokens
    # that is generated from the parse.
    this$parse <- function (template, tags) {
        tokens <- 
        
        if (is.null(tokens)) {
            tockens <- parseTemplate(template, tags)
            this$cache[[template]] <- tockens
        }
        
        return tokens;
    }
    
    # High-level method that is used to render the given `template` with
    # the given `view`.
    #
    # The optional `partials` argument may be an object that contains the
    # names and templates of partials that are used in the template. It may
    # also be a function that is used to load partial templates on the fly
    # that takes a single argument: the name of the partial.
    this$render <- function (template, view, partials) {
        tokens <- this$parse(template)
        context = ifelse(class(view) == "Context", view, Context(view))
        return(this$renderTokens(tokens, context, partials, template))
    }
    
    # Low-level method that renders the given array of `tokens` using
    # the given `context` and `partials`.
    #
    # Note: The `originalTemplate` is only ever used to extract the portion
    # of the original template that was contained in a higher-order section.
    # If the template doesn't use higher-order sections, this argument may
    # be omitted.

    this$renderTokens <- function (tokens, context, partials, originalTemplate) {
        buffer <- ""
        
        for (i in 1:length(tokens)) {
            value <- NULL
            token <- tokens[[i]]
            symbol <- token[[1]]
            
            if (symbol == '#') {
                value = this$ll_renderSection(token, context, partials, originalTemplate)
            } else if (symbol == '^') {
                value = this$ll_renderInverted(token, context, partials, originalTemplate)
            } else if (symbol == '>') { 
                value = this$ll_renderPartial(token, context, partials, originalTemplate)
            } else if (symbol == '&') { 
                value = this$ll_unescapedValue(token, context) 
            } else if (symbol == 'name') { 
                value = this$ll_escapedValue(token, context) 
            } else if (symbol == 'text') { 
                value = this$ll_rawValue(token)
            }
            
            if (!is.null(value)) {
                buffer = paste0(buffer, value)
            }
                
        }
        
        return(buffer)
    }
    
    this$ll_renderSection <- function (token, context, partials, originalTemplate) {
        self <- this
        buffer <- ""
        value <- context$lookup(token[[2]]);
        
        # This function is used to render an arbitrary template
        # in the current context by higher-order sections.
        subRender <- function (template) {
            return(self$render(template, context, partials))
        }
        
        if (is.null(value)) {
            return()
        }
        
        if (is.atomic(value) && length(value) > 1) {
            for (j in 1:length(value)) {
                buffer <- paste0(buffer, 
                                 this$renderTokens(token[[5]], context$push(value[j]), partials, originalTemplate)
                )
            }
        } else if (class(value) == "list") { # || (is.atomic(value) && length(value) == 1)) {
            buffer <- paste0(buffer, 
                             this$renderTokens(token[[5]], context$push(value), partials, originalTemplate)
            )
        } else if (class(value) == "function") {
            if (class(originalTemplate) != "character") {
                error("Cannot use higher-order sections without the original template")
            }
            
            # Extract the portion of the original template that the section contains.
            value <- do.call(value, list(substring(originalTemplate, token[[4]], token[[6]]), 
                                         subRender, 
                                         this = context$view)
                             )
            
            if (!is.null(value)) {
                buffer <- paste0(buffer, value)
            }

        } else {
            buffer <- paste0(buffer, 
                             this$renderTokens(token[[5]], context, partials, originalTemplate)
            )
        }
        return(buffer)
    }
    
    this$ll_renderInverted <- function (token, context, partials, originalTemplate) {
        value <- context$lookup(token[[2]])
        
        # Use JavaScript's definition of falsy. Include empty arrays.
        # See https://github.com/janl/mustache.js/issues/186
        if (identical(as.logical(value), FALSE) || length(value) == 0) {
            return(this$renderTokens(token[[5]], context, partials, originalTemplate))
        }
    }

    this$ll._renderPartial <- function(token, context, partials = NULL) {
        if (is.null(partials)) {
            return()
        }
        
        value <- NULL
        if (class(partials) == "function") {
            value <- partials(token[[2]])
        } else {
            value <- partials[[token[[1]]]]
        }
        
        if (is.null(value)) {
            return(this$renderTokens(this$parse(value), context, partials, value))
        }
            
    }

    this$ll_unescapedValue <- function(token, context) {
        value <- context$lookup(token[[2]])
        if (!is.null(value)){
            return(value)
        }
    }

    this$ll_escapedValue <- function(token, context) {
        value <- context$lookup(token[[2]])
        if (!is.null(value)) {
            return(escape(value))
        }
    }

    this$ll_rawValue <- function(token) {
        return(token[[2]])
    }
    
    class(this) <- "Writer"
    return(this)
    
}

