source('ribtws.R')

##### ZeroMQ connections #####
ibtws.broadcast.endpoint <- "ipc:///var/tmp/ibtws/broadcast"
ibtws.command.endpoint <- "ipc:///var/tmp/ibtws/command"

ctx <- init.context()

ibtws.broadcast <- init.socket(ctx, "ZMQ_SUB")
connect.socket(ibtws.broadcast, ibtws.broadcast.endpoint)
subscribe(ibtws.broadcast, "")

ibtws.command <- init.socket(ctx, "ZMQ_REQ")
connect.socket(ibtws.command, ibtws.command.endpoint)

`send.ibtws` <- function(socket, fields) {
    strfields <- sapply(fields, function(x) if (is.na(x) || is.null(x)) '' else as.character(x))
    rawfields <- writeBin(strfields, raw(), size=1L, useBytes=TRUE)
    send.socket(socket, rawfields, serialize=FALSE)
}

send.ibtws(ibtws.command, list(2, 1, 1));
print(receive.socket(ibtws.command, unserialize=FALSE))

#send.ibtws(ibtws.command, list(1, 9, 1, NA, "BHP", "STK", NA, NA, NA, NA, "SMART", NA, "AUD", NA, 0, NA, 0))
#print(receive.socket(ibtws.command, unserialize=FALSE))

receive.ibtws <- function(socket) {
   rawmsg <- receive.socket(socket, unserialize=FALSE)
   fields <- parseFields(rawmsg)
   IB.Message.parse(fields)
}

send.ibtws(ibtws.command, list(16, 1));
receive.socket(ibtws.command, unserialize=FALSE)

repeat {
   msg <- receive.ibtws(ibtws.broadcast)
   print(msg)
}
