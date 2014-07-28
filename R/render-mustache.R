#' Render mustache template with data
#' 
#' @param template character, containing the template
#' @param data named list, containing the data
#' 
#' @export
render_mustache <- function (template, data) {    
    rmustache.template <- rmustachePart(type = "root", 
                                   value = nest_parts(parse_template(template)))
    
    return(rmustache.template$render(data = data))
}