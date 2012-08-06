
`ListMap` <- function(f, l) Map(f, names(l), l)

f <- function(...) {
    types <- list(...)
    ListMap(function(key, value){
        print(paste(key, value))
    }, types)
}

f(hello=5, world=6)
