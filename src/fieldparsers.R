with.na.default <- function(parser, default) {
    function(x) {
        value <- parser(x)
        ifelse(is.na(value), default, value)
    }
}

.int <- with.na.default(as.integer, 0)
.string <- as.character

.double.max = "1.7976931348623157E308"
.double <- function(x) {
    if (x == .double.max) {
        NA
    } else {
        value <- as.numeric(x)
        if (is.na(value)) {
            0.0
        } else {
            value
        }
    }
}
.boolean <- function(x) {
    if (x == '') {
        FALSE
    } else {
        as.integer(x) > 0
    }
}
.ticktype <- function(x) ticktypes.by.id[[x]]

