#' Represents a rendering context by wrapping a view object and maintaining a reference to the parent context.

Context <- function Context(view, parentContext = NULL) {
    
    this <- new.env()
    this$view <- view
    this$cache <- list('.' = this$view)
    this$parent <- parentContext
    

    # Creates a new context using the given view with this context as the parent.
    this$push <- function (view) {
            return(Context(view, this))
        }

    return(this)
}


/**
    * Returns the value of the given name in this context, traversing
* up the context hierarchy if the value is absent in this context's view.
*/
Context.prototype.lookup = function (name) {
var cache = this.cache;
var value;
if (name in cache) {
value = cache[name];
} else {
var context = this, names, index;
while (context) {
if (name.indexOf('.') > 0) {
value = context.view;
names = name.split('.');
index = 0;
while (value != null && index < names.length)
value = value[names[index++]];
} else if (typeof context.view == 'object') {
value = context.view[name];
}
if (value != null)
break;
context = context.parent;
}
cache[name] = value;
}
if (isFunction(value))
value = value.call(this.view);
return value;
};