# r-ØTWS
## R package for communicating with an Interactive Brokers ZeroMQ proxy

This is an API for communicating with the Interactive Brokers TWS API via
a ZeroMQ proxy (for example, https://github.com/brotchie/ib-zmq)

Example usage:

    # Listen for API messages.
    ibtws.broadcast.endpoint <- "ipc:///var/tmp/ibtws/broadcast"
    ibtws.broadcast <- init.socket(ctx, "ZMQ_SUB")
    subscribe(ibtws.broadcast, "")
    
    # Establish connection to command socket.
    ibtws.command.endpoint <- "ipc:///var/tmp/ibtws/command"
    ibtws.command <- init.socket(ctx, "ZMQ_REQ")
    connect.socket(ibtws.command, ibtws.command.endpoint)
    
    # Request open orders.
    send.ibtws(ibtws.command, list(16, 1));
    receive.socket(ibtws.command, unserialize=FALSE)
    
    # Receive and print messages.
    repeat {
      msg <- receive.ibtws(ibtws.broadcast)
      print(msg)
    }
    
Deserialized OPEN_ORDER message for an EFP portfolio on Facebook:

    OpenOrder {
        order = Order {
            order.id = 0, action = BUY, quantity.total = 1, order.type = LMT,
            price.limit = 1, price.aux = 0, timeinforce = DAY, oca.group = ,
            account = DU99751, openclose = C, origin = 0, order.ref = ,
            client.id = 0, perm.id = 1834963389, outsidehours = FALSE,
            hidden = FALSE, discretionaryamount = 0, goodaftertime = ,
            fa.group = , fa.method = , fa.percentage = , fa.profile = ,
            goodtilldate = , rule80A = , percentoffset = 0, settlingfirm = ,
            shortsaleslot = 0, designatedlocation = , exemptcode = -1,
            auctionstrategy = 0, startingprice = 0, stockrefprice = 0,
            delta = 0, stockrangelower = 0, stockrangeupper = 0,
            displaysize = 0, order.block = FALSE, sweeptofill = FALSE,
            allornone = FALSE, minquantity = 0, oca.type = 3,
            etradeonly = FALSE, firmquoteonly = FALSE, nbbopricecap = 0,
            parent.id = 0, trigger.method = 0, volatility = 0,
            volatility.type = 0, deltaneutral.ordertype = None,
            deltaneutral.auxprice = 0, continuousupdate = 0,
            stockrangelower = 0, stockrangeupper = 0, referenceprice.type = 0,
            trailstopprice = 0, trailingpercent = 0, basispoints = 0,
            basispoints.type = 2, scale.initiallevel.size = 0,
            scale.sublevel.size = 0, scale.price.increment = 0, hedge.type = ,
            opt.out.smartrouting = FALSE, clearing.account = ,
            clearing.intent = IB, not.held = FALSE, whatif = FALSE,
            order.combo.legs = , smart.routing.params =  },
        contract = Contract {
            contract.id = 28812380, symbol = FB, security.type = BAG, expiry = ,
            strike = 0, right = ?, exchange = SMART, currency = USD,
            symbol.local = FB,
            combo.legs.description = 107113386|100,108126831|1,
            combo.legs = {
                ComboLeg { 
                    contract.id = 107113386, ratio = 100,
                    action = BUY, exchange = SMART, openclose = 0,
                    shortsaleslot = 0, designatedlocation = , exemptcode = -1 },
                ComboLeg {
                    contract.id = 108126831, ratio = 1, action = BUY,
                    exchange = SMART, openclose = 0, shortsaleslot = 0,
                    designatedlocation = , exemptcode = -1 }
                }
            }
      }


  
    