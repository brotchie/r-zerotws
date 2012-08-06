with.na.default <- function(parser, default) {
    function(x) {
        value <- parser(x)
        ifelse(is.na(value), default, value)
    }
}

.int <- with.na.default(as.integer, 0)
.string <- as.character
.double <- with.na.default(as.numeric, 0.0)
.boolean <- function(x) {
    if (x == '') {
        FALSE
    } else {
        as.integer(x) > 0
    }
}
.ticktype <- function(x) ticktypes.by.id[[x]]

