##  ___                  _                                                   ##
## | _ \  ______ _ _ ___| |___ __ _____ r-ØTWS - Comms Between R and an IB   ##
## |   /_|_ / -_) '_/ _ \  _\ V  V (_-<          TWS API <-> ØMQ Proxy       ##
## |_|_(_)__\___|_| \___/\__|\_/\_//__/ Copyright © 2012, James Brotchie     ##
##                                      http://zerotick.org/                 ##

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

