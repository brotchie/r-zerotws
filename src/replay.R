library(RSQLite)

source('ribtws.R')

DATABASE.PATH = '/home/ganon/projects/ib-zmq/message.db'

engine <- SQLite()
db <- dbConnect(engine, DATABASE.PATH)
messages <- dbGetQuery(db, 'SELECT * FROM messages')

for (rawmsg in messages$content) {
    fields <- parseFields(rawmsg)
    msg <- IB.Message.parse(fields)
    print(msg)
}
