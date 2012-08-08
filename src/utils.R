##  ___                  _                                                   ##
## | _ \  ______ _ _ ___| |___ __ _____ r-ØTWS - Comms Between R and an IB   ##
## |   /_|_ / -_) '_/ _ \  _\ V  V (_-<          TWS API <-> ØMQ Proxy       ##
## |_|_(_)__\___|_| \___/\__|\_/\_//__/ Copyright © 2012, James Brotchie     ##
##                                      http://zerotick.org/                 ##

`Map.List` <- function(f, l) Map(f, names(l), l)

`build.constructor` <- function(name, defn) {
    function(...) {
        args <- list(...)

        # Catch invalid constructor arguments.
        invalid <- names(args)[sapply(defn[names(args)], is.null)]
        if (length(invalid)) stop(paste('Unknown field[s]', invalid, 'passed to', name, 'constructor.'))

        instance <- mapply(function(f, x) f(x), defn[names(args)], args, SIMPLIFY=FALSE)
        class(instance) <- name
        instance 
    }
}

`build.pretty.as.character` <- function(name) {
    function(instance) {
        fields.pretty <- mapply(
            function(k, v) {
                if (is.list(v)) v <- paste(lapply(v, as.character), collapse=", ")
                paste(k, '=', v)
            }, names(instance), instance)
        paste(name, "{", paste(fields.pretty, collapse=", "), "}")
    }
}
