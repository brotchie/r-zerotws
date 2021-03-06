##  ___                  _                                                   ##
## | _ \  ______ _ _ ___| |___ __ _____ r-ØTWS - Comms Between R and an IB   ##
## |   /_|_ / -_) '_/ _ \  _\ V  V (_-<          TWS API <-> ØMQ Proxy       ##
## |_|_(_)__\___|_| \___/\__|\_/\_//__/ Copyright © 2012, James Brotchie     ##
##                                      http://zerotick.org/                 ##

library(RSQLite)

source('ribtws.R')

DATABASE.PATH = '/home/ganon/projects/ib-zmq/message.db'

engine <- SQLite()
db <- dbConnect(engine, DATABASE.PATH)
messages <- dbGetQuery(db, 'SELECT * FROM messages')

observed <- list()

for (rawmsg in messages$content) {
    fields <- parseFields(rawmsg)
    msg <- IB.Message.parse(fields)
    observed <- c(observed, .int(fields[1]))
    print(msg)
}


unobserved <- unlist(setdiff(messages.native, observed))


print('Observed Message Types')
print(names(messages.native)[messages.native %in% observed])
print('Unobserved Message Types')
print(names(messages.native)[messages.native %in% unobserved])
