##  ___                  _                                                   ##
## | _ \  ______ _ _ ___| |___ __ _____ r-ØTWS - Comms Between R and an IB   ##
## |   /_|_ / -_) '_/ _ \  _\ V  V (_-<          TWS API <-> ØMQ Proxy       ##
## |_|_(_)__\___|_| \___/\__|\_/\_//__/ (c) 2012, James Brotchie             ##
##                                      http://zerotick.org/                 ##

`IB.Order.definition` <- list(
        order.id = .int
      , client.id = .int
      , perm.id = .int
      , action = .string
      , quantity.total = .int
      , order.type = .string
      , price.limit = .double
      , price.aux = .double
      , timeinforce = .string
      , oca.group = .string
      , oca.type = .int
      , order.ref = .string
      , transmit = .boolean
      , parent.id = .int
      , order.block = .boolean
      , sweeptofill = .boolean
      , displaysize = .int
      , trigger.method = .int
      , outsidehours = .boolean
      , hidden = .boolean
      , goodaftertime = .string
      , goodtilldate = .string
      , overridepercentageconstraints = .boolean
      , rule80A = .string
      , allornone = .boolean
      , minquantity = .int
      , percentoffset = .double
      , trailstopprice = .double
      , trailingpercent = .double
      , fa.group = .string
      , fa.profile = .string
      , fa.method = .string
      , fa.percentage = .string
      , openclose = .string
      , origin = .int
      , shortsaleslot = .int
      , designatedlocation = .string
      , exemptcode = .int
      , discretionaryamount = .double
      , etradeonly = .boolean
      , firmquoteonly = .boolean
      , nbbopricecap = .double
      , opt.out.smartrouting = .boolean
      , auctionstrategy = .int
      , startingprice = .double
      , stockrefprice = .double
      , delta = .double
      , stockrangelower = .double
      , stockrangeupper = .double
      , volatility = .double
      , volatility.type = .int
      , continuousupdate = .int
      , referenceprice.type = .int
      , deltaneutral.ordertype = .string
      , deltaneutral.auxprice = .double
      , deltaneutral.conid = .int
      , deltaneutral.settlingfirm = .string
      , deltaneutral.clearingaccount = .string
      , deltaneutral.clearingintent = .string
      , basispoints = .double
      , basispoints.type = .int
      , scale.initiallevel.size = .int
      , scale.sublevel.size = .int
      , scale.price.increment = .double
      , scale.price.adjust.value = .double
      , scale.price.adjust.interval = .int
      , scale.profit.offset = .double
      , scale.autoreset = .boolean
      , scale.initial.position = .int
      , scale.initial.fill.quantity = .int
      , scale.random.percent = .boolean
      , hedge.type = .string
      , hedge.param = .string
      , account = .string
      , settlingfirm = .string
      , clearing.account = .string
      , clearing.intent = .string
      , algo.strategy = .string
      , whatif = .boolean
      , not.held = .boolean
)

`IB.Order` <- build.constructor('IB.Order', IB.Order.definition)
`as.character.IB.Order` <- build.pretty.as.character('Order')

`IB.OrderState.definition` <- list(
      status = .string
    , margin.initial = .string
    , margin.maint = .string
    , equitywithloan = .string
    , commission = .double
    , commission.min = .double
    , commission.max = .double
    , commission.currency = .string
    , warning = .string
)
`IB.OrderState` <- build.constructor('IB.OrderState', IB.OrderState.definition)
`as.character.IB.OrderState` <- build.pretty.as.character('OrderState')
