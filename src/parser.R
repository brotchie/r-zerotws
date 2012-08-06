`parseFields` <- function(msg) {
    fields <- readBin(msg, 'character', n=sum(msg == 0x00), size=1L)
    fields
}

MP <- IB.Message.Parser <- function(name, ...) {
    msg.types  <- list(...)
    msg.parser <- function(fields) {
        mapply(function(type, value) type(value), msg.types, fields, SIMPLIFY=FALSE)
    }
    function(fields) {
        datafields <- fields[-2:-1]
        IB.Message(name, msg.parser(datafields))
    }
}

build <- function(constructor, x, indexes) {
    values <- as.list(x[unlist(indexes)])
    names(values) <- names(indexes)
    do.call(constructor, values)
}

`parse.count` <- function(n, parser) lapply(sequence(n), parser)

`IB.Message.parsers` <- list(

      TICK_PRICE        = MP( 'TickPrice'
                            , ticker.id       = .int
                            , tick.type       = .ticktype
                            , price           = .double
                            , size            = .int
                            , can.autoexecute = .int )

    , TICK_SIZE         = MP( 'TickSize'
                            , ticker.id = .int
                            , tick.type = .ticktype
                            , size      = .int )

    , TICK_GENERIC      = MP( 'TickGeneric'
                            , ticker.id = .int
                            , tick.type = .ticktype
                            , value     = .double )

    , TICK_STRING       = MP( 'TickString'
                            , ticker.id = .int
                            , tick.type = .ticktype
                            , value     = .string )

    , TICK_EFP          = MP( 'TickEFP'
                            , ticker.id             = .int
                            , tick.type             = .ticktype
                            , basispoints           = .double
                            , basispoints.formatted = .string
                            , impliedfuturesprice   = .double
                            , holddays              = .int
                            , futureexpiry          = .string
                            , dividendimpact        = .double
                            , dividendstoexpiry     = .double )

    , ERR_MSG           = MP( 'ErrorMessage'
                            , error.id = .int
                            , code     = .int
                            , message  = .string )

    , ACCT_VALUE        = MP( 'AccountValue'
                            , key      = .string
                            , value    = .double
                            , currency = .string )

    , PORTFOLIO_VALUE   = function(x) IB.Message( 'PortfolioValue', list(
                                    contract = IB.Contract(
                                      contract.id      = as.integer(x[3])
                                    , symbol           = x[4]
                                    , security.type    = x[5]
                                    , expiry           = x[6]
                                    , strike           = as.numeric(x[7])
                                    , right            = x[8]
                                    , multiplier       = x[9]
                                    , exchange.primary = x[10]
                                    , currency         = x[11]
                                    , symbol.local     = x[12])
                                  , position          = as.integer(x[13])
                                  , price.market      = as.numeric(x[14])
                                  , value.market      = as.numeric(x[15])
                                  , cost.average      = as.numeric(x[16])
                                  , profit.unrealized = as.numeric(x[17])
                                  , profit.realized   = as.numeric(x[18])
                                  , account.name      = x[19]))
    , OPEN_ORDER        = function(x) {
                              contract <- build(IB.Contract, x[4:12], list(
                                    contract.id   = 1
                                  , symbol        = 2
                                  , security.type = 3
                                  , expiry        = 4
                                  , strike        = 5
                                  , right         = 6
                                  , exchange      = 7 
                                  , currency      = 8
                                  , symbol.local  = 9 ))
                            
                              ### Combo legs ###
                              delta.neutral.type.index = 61
                              delta.neutral.type = x[delta.neutral.type.index]

                              continuous.update.index = delta.neutral.type.index + 2 + ifelse(delta.neutral.type != '', 4, 0)

                              combo.legs.index <- delta.neutral.type.index + 8 + ifelse(delta.neutral.type != '', 4, 0)
                              combo.legs.description <- x[combo.legs.index]
                              combo.legs.count <- .int(x[combo.legs.index+1])
                              combo.legs <- parse.count(combo.legs.count, function(n) {
                                                        start = combo.legs.index + 2 + 8*(n-1)
                                                        build(IB.ComboLeg, x[start:(start+8)], list(
                                                              contract.id        = 1
                                                            , ratio              = 2
                                                            , action             = 3
                                                            , exchange           = 4
                                                            , openclose          = 5
                                                            , shortsaleslot      = 6
                                                            , designatedlocation = 7
                                                            , exemptcode         = 8))
                                                        })
                              ### Order combo legs ###
                              order.combo.legs.index <- combo.legs.index + 2 + 8*combo.legs.count
                              order.combo.legs.count <- .int(x[order.combo.legs.index])
                              order.combo.legs <- parse.count(order.combo.legs.count, function(n) {
                                                                  index = order.combo.legs.index + 1 + n
                                                                  .double(x[index])
                                                              })
                              ### Smart routing parameters ###
                              smart.routing.params.index <- order.combo.legs.index + 1 + order.combo.legs.count
                              smart.routing.params.count <- .int(x[smart.routing.params.index])
                              smart.routing.params <- as.list(x[smart.routing.params.index + 2 * sequence(smart.routing.params.count)])
                              names(smart.routing.params) <- x[smart.routing.params.index + 2 * sequence(smart.routing.params.count) - 1]

                              ### Scale trader ###
                              scale.price.index <- smart.routing.params.index + 1 + 2*smart.routing.params.count
                              scale.price.increment <- .double(x[scale.price.index+2])

                              ### Hedging ###
                              hedge.type.index <- scale.price.index + 3 + ifelse(scale.price.increment > 0, 7, 0)
                              hedge.type <- x[hedge.type.index]

                              opt.out.smartrouting.index <- hedge.type.index + 1 + ifelse(hedge.type != '', 1, 0)

                              ### Undercomp ###
                              undercomp.active.index <- hedge.type.index + 5 + ifelse(hedge.type != '', 1, 0)
                              undercomp.active = .boolean(x[undercomp.active.index])

                              ### Algo strategy ###
                              algo.strategy.index <- undercomp.active.index + 1 + ifelse(undercomp.active, 3, 0)
                              algo.strategy <- x[algo.strategy.index]
                              algo.params.count <- .int(x[algo.strategy.index + 1])
                              algo.params <- as.list(x[algo.strategy.index + 1 + 2 * sequence(algo.params.count)])
                              names(algo.params) <- x[algo.strategy.index + 2 * sequence(algo.params.count)]

                              ### Order state ###
                              order.state.index <- algo.strategy.index + 2 + 2 * algo.params.count
                              order.state <- build(IB.OrderState, x[0:8 + order.state.index], list(
                                                         status             = 1
                                                       , margin.initial     = 2
                                                       , margin.maint       = 3
                                                       , equitywithloan     = 4
                                                       , commission          = 5
                                                       , commission.min      = 6
                                                       , commission.max      = 7
                                                       , commission.currency = 8
                                                       , warning            = 9))

                              order.idx <- list(
                                         order.id                = 3
                                       , action                  = 13
                                       , quantity.total          = 14
                                       , order.type              = 15
                                       , price.limit             = 16
                                       , price.aux               = 17
                                       , timeinforce             = 18
                                       , oca.group               = 19
                                       , account                 = 20
                                       , openclose               = 21
                                       , origin                  = 22
                                       , order.ref               = 23
                                       , client.id               = 24
                                       , perm.id                 = 25
                                       , outsidehours            = 26
                                       , hidden                  = 27
                                       , discretionaryamount     = 28
                                       , goodaftertime           = 29
                                       , fa.group                = 31
                                       , fa.method               = 32
                                       , fa.percentage           = 33
                                       , fa.profile              = 34
                                       , goodtilldate            = 35
                                       , rule80A                 = 36
                                       , percentoffset           = 37
                                       , settlingfirm            = 38
                                       , shortsaleslot           = 39
                                       , designatedlocation      = 40
                                       , exemptcode              = 41
                                       , auctionstrategy         = 42
                                       , startingprice           = 43
                                       , stockrefprice           = 44
                                       , delta                   = 45
                                       , stockrangelower         = 46
                                       , stockrangeupper         = 47
                                       , displaysize             = 48
                                       , order.block             = 49
                                       , sweeptofill             = 50
                                       , allornone               = 51
                                       , minquantity             = 52
                                       , oca.type                = 53
                                       , etradeonly              = 54
                                       , firmquoteonly           = 55
                                       , nbbopricecap            = 56
                                       , parent.id               = 57
                                       , trigger.method          = 58
                                       , volatility              = 59
                                       , volatility.type         = 60
                                       , deltaneutral.ordertype  = 61
                                       , deltaneutral.auxprice   = 62
                                       , continuousupdate        = continuous.update.index
                                       , stockrangelower         = continuous.update.index + 1
                                       , stockrangeupper         = continuous.update.index + 2
                                       , referenceprice.type     = continuous.update.index + 3
                                       , trailstopprice          = continuous.update.index + 4
                                       , trailingpercent         = continuous.update.index + 5
                                       , basispoints             = continuous.update.index + 6
                                       , basispoints.type        = continuous.update.index + 7
                                       , scale.initiallevel.size = scale.price.index
                                       , scale.sublevel.size     = scale.price.index + 1
                                       , scale.price.increment   = scale.price.index + 2
                                       , hedge.type              = hedge.type.index
                                       , opt.out.smartrouting    = opt.out.smartrouting.index
                                       , clearing.account        = opt.out.smartrouting.index + 1
                                       , clearing.intent         = opt.out.smartrouting.index + 2
                                       , not.held                = opt.out.smartrouting.index + 3
                                       , whatif                  = order.state.index - 1
                                       )

                              if (scale.price.increment > 0) {
                                  order.idx <- c(order.idx, list(
                                        scale.price.adjust.value    = scale.price.index + 3
                                      , scale.price.adjust.interval = scale.price.index + 4
                                      , scale.profit.offset         = scale.price.index + 5
                                      , scale.autoreset             = scale.price.index + 6
                                      , scale.initial.position      = scale.price.index + 7
                                      , scale.initial.fill.quantity = scale.price.index + 8
                                      , scale.random.percent        = scale.price.index + 9))
                              }

                              if (hedge.type != '') order.idx <- c(order.idx, list(hedge.param = hedge.type.index + 1))
                              order <- build(IB.Order, x, order.idx)
                              order$order.combo.legs <- order.combo.legs
                              order$smart.routing.params <- smart.routing.params
            
                              contract$combo.legs.description <- combo.legs.description
                              contract$combo.legs <- combo.legs

                              contract$undercomp <- if (undercomp.active) {
                                                          build( IB.UnderComp
                                                               , x[1:4] + undercomp.active.index
                                                               , list( contract.id = 1
                                                                     , delta       = 2
                                                                     , price       = 3))
                                                    } else {
                                                        NULL
                                                    }
                              IB.Message('OpenOrder', list( order = order
                                                          , contract = contract))
                          }
    , OPEN_ORDER_END    = MP( 'OpenOrderEnd' )
    , ACCT_UPDATE_TIME  = MP( 'AccountUpdateTime'
                            , timestamp=.string )

    , ACCT_DOWNLOAD_END = MP( 'AccountDownloadEnd'
                            , account.name=.string )
)

`IB.Message.parsers.by.id` <- IB.Message.parsers
names(IB.Message.parsers.by.id) <- messages.native[names(IB.Message.parsers)]

`IB.Message.parse` <- function(fields) {
    id <- fields[1]
    if (id %in% names(IB.Message.parsers.by.id)) {
        print(messages.native[messages.native == id])
        IB.Message.parsers.by.id[[id]](fields)
    } else {
        IB.Message('Unknown', list(values=fields[-2:-1]))
    }
}


