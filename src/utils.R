`Map.List` <- function(f, l) Map(f, names(l), l)

`build.constructor` <- function(name, defn) {
    function(...) {
        args <- list(...)
        instance <- mapply(function(f, x) f(x), defn[names(args)], args, SIMPLIFY=FALSE)
        class(instance) <- name
        instance 
    }
}

`build.pretty.as.character` <- function(name) {
    function(instance) {
        fields.pretty <- mapply(function(k, v)paste(k, '=', v), names(instance), instance)
        paste(name, "{", paste(fields.pretty, collapse=", "), "}")
    }
}
