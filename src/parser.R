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

      TICK_PRICE         = MP( 'TickPrice'
                             , ticker.id       = .int
                             , tick.type       = .ticktype
                             , price           = .double
                             , size            = .int
                             , can.autoexecute = .int )

    , TICK_SIZE          = MP( 'TickSize'
                             , ticker.id = .int
                             , tick.type = .ticktype
                             , size      = .int )

    , ORDER_STATUS       = MP( 'OrderStatus'
                             , order.id           = .int
                             , status             = .string
                             , filled             = .int
                             , remaining          = .int
                             , average.fill.price = .double
                             , perm.id            = .int
                             , parent.id          = .int
                             , last.fill.price    = .double
                             , client.id          = .int
                             , why.held           = .string )

    , ERR_MSG            = MP( 'ErrorMessage'
                             , error.id = .int
                             , code     = .int
                             , message  = .string )

    , OPEN_ORDER         = function(x) {
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

    , ACCT_VALUE         = MP( 'AccountValue'
                             , key      = .string
                             , value    = .double
                             , currency = .string )

    , PORTFOLIO_VALUE    = function(x) {
                               contract <- build(IB.Contract, x, list(
                                   contract.id      = 3
                                 , symbol           = 4
                                 , security.type    = 5
                                 , expiry           = 6
                                 , strike           = 7
                                 , right            = 8
                                 , multiplier       = 9
                                 , exchange.primary = 10
                                 , currency         = 11
                                 , symbol.local     = 12 ))

                               IB.Message( 'PortfolioValue', list(
                                           contract       = contract
                                         , position       = .int(x[13])
                                         , market.price   = .double(x[14])
                                         , market.value   = .double(x[15])
                                         , average.cost   = .double(x[16])
                                         , unrealized.pnl = .double(x[17])
                                         , realized.pnl   = .double(x[18])
                                         , account.name   = .string(x[19]) ))
                           }

    , ACCT_UPDATE_TIME   = MP( 'AccountUpdateTime'
                             , timestamp = .string )

    , NEXT_VALID_ID      = MP( 'NextValidID'
                             , order.id = .int )

    , CONTRACT_DATA      = function(x) {
                               request.id <- .int(x[3])
                               contract <- build(IB.Contract, x, list(
                                     symbol           = 4
                                   , security.type    = 5
                                   , expiry           = 6
                                   , strike           = 7
                                   , right            = 8
                                   , exchange         = 9
                                   , currency         = 10
                                   , symbol.local     = 11
                                   , contract.id      = 14
                                   , multiplier       = 16
                                   , exchange.primary = 22 ))

                               details <- build(IB.ContractDetails, x, list(
                                     market.name       = 12
                                   , trading.class     = 13
                                   , tick.min          = 15
                                   , order.types       = 17
                                   , valid.exchanges   = 18
                                   , price.magnifier   = 19
                                   , under.contract.id = 20
                                   , name.long         = 21
                                   , contract.month    = 23
                                   , industry          = 24
                                   , category          = 25
                                   , subcategory       = 26
                                   , timezone.id       = 27
                                   , trading.hours     = 28
                                   , liquid.hours      = 29
                                   , ev.rule           = 30
                                   , ev.multiplier     = 31 ))

                               security.id.list.count <- .int(x[31])
                               details$security.id.list <- as.list(x[31 + 2 * sequence(security.id.list.count)])
                               names(details$security.id.list) <- x[31 + 2 * sequence(security.id.list.count) - 1]

                               IB.Message('ContractData', list( contract = contract
                                                              , details = details))
                           }
    , EXECUTION_DATA     = function(x) {
                               contract <- build(IB.Contract, x, list(
                                     contract.id   = 5
                                   , symbol        = 6
                                   , security.type = 7
                                   , expiry        = 8
                                   , strike        = 9
                                   , right         = 10
                                   , multiplier    = 11
                                   , exchange      = 12
                                   , currency      = 13
                                   , symbol.local  = 14 ))

                               execution <- build(IB.Execution, x, list(
                                     order.id       = 4
                                   , execution.id   = 15
                                   , time           = 16
                                   , account.number = 17
                                   , exchange       = 18
                                   , side           = 19
                                   , shares         = 20
                                   , price          = 21
                                   , perm.id        = 22
                                   , client.id      = 23
                                   , liquidation    = 24
                                   , cum.quantity   = 25
                                   , average.price  = 26
                                   , order.ref      = 27
                                   , ev.rule        = 28
                                   , ev.multiplier  = 29 ))
                               IB.Message( 'ExecutionData', list(
                                           request.id = .int(x[3])
                                         , contract   = contract
                                         , execution  = execution))
                           }

    , MARKET_DEPTH       = MP( 'MarketDepth'
                             , depth.id  = .int
                             , position  = .int
                             , operation = .int
                             , side      = .int
                             , price     = .double
                             , size      = .int )

    , MARKET_DEPTH_L2     = MP( 'MarketDepthL2'
                             , depth.id     = .int
                             , position     = .int
                             , market.maker = .string
                             , operation    = .int
                             , side         = .int
                             , price        = .double
                             , size         = .int )

    , NEWS_BULLETINS     = MP( 'NewsBulletins'
                             , news.msg.id          = .int
                             , news.msg.type        = .int
                             , news.msg             = .string
                             , originating.exchange = .string )

    , MANAGED_ACCTS      = MP( 'ManagedAccounts'
                             , accounts.list = .string )

    , RECEIVE_FA         = MP( 'ReceiveFA'
                             , fa.data.type = .int
                             , xml          = .string )

    , HISTORICAL_DATA    = function(x) {
                               item.count <- .int(x[6]) 
                               bars <- parse.count(item.count, function(n) {
                                   i <- 7 + 9*(n-1)
                                   list( date      = .string(x[i])
                                       , open      = .double(x[i+1])
                                       , high      = .double(x[i+2])
                                       , low       = .double(x[i+3])
                                       , close     = .double(x[i+4])
                                       , volume    = .int(x[i+5])
                                       , wap       = .double(x[i+6])
                                       , has.gaps  = .string(x[i+7])
                                       , bar.count = .int(x[i+8]))
                               })
                               IB.Message( 'HistoricalData', list(
                                           request.id = .int(x[3])
                                         , start.date = .string(x[4])
                                         , end.date   = .string(x[5])
                                         , bars       = bars ))
                           }

    # TODO: Implement these message types.

    #, BOND_CONTRACT_DATA

    #, SCANNER_PARAMETERS
    
    #, SCANNER_DATA

    #, TICK_OPTION_COMPUTATION

    , TICK_GENERIC       = MP( 'TickGeneric'
                             , ticker.id = .int
                             , tick.type = .ticktype
                             , value     = .double )

    , TICK_STRING        = MP( 'TickString'
                             , ticker.id = .int
                             , tick.type = .ticktype
                             , value     = .string )

    , TICK_EFP           = MP( 'TickEFP'
                             , ticker.id             = .int
                             , tick.type             = .ticktype
                             , basispoints           = .double
                             , basispoints.formatted = .string
                             , impliedfuturesprice   = .double
                             , holddays              = .int
                             , futureexpiry          = .string
                             , dividendimpact        = .double
                             , dividendstoexpiry     = .double )

    , CURRENT_TIME       = MP( 'CurrentTime'
                             , time = .int )

    , REAL_TIME_BARS     = MP( 'RealTimeBars'
                             , request.id = .int
                             , time       = .int
                             , open       = .double
                             , high       = .double
                             , low        = .double
                             , close      = .double
                             , volume     = .int
                             , wap        = .double
                             , count      = .int )

    , FUNDAMENTAL_DATA   = MP( 'FundamentalData'
                             , request.id = .int
                             , data = .string )

    , CONTRACT_DATA_END  = MP( 'ContractDataEnd', request.id = .int )

    , OPEN_ORDER_END     = MP( 'OpenOrderEnd' )

    , ACCT_DOWNLOAD_END  = MP( 'AccountDownloadEnd'
                             , account.name = .string )

    , EXECUTION_DATA_END = MP( 'ExecutionDataEnd', request.id = .int )

    #, DELTA_NEUTRAL_VALIDATION = 

    , TICK_SNAPSHOT_END  = MP( 'TickSnapshotEnd', request.id = .int )

    , MARKET_DATA_TYPE   = MP( 'MarketDataType'
                           , request.id       = .int
                           , market.data.type = .int )
    , COMMISSION_REPORT  = MP( 'CommissionReport'
                           , execution.id = .string
                           , commission   = .double
                           , currency     = .string
                           , realized.pnl = .double
                           , yield        = .double
                           , yield.redemption.date = .int )
    
)

`IB.Message.parsers.by.id` <- IB.Message.parsers
names(IB.Message.parsers.by.id) <- messages.native[names(IB.Message.parsers)]

`IB.Message.parse` <- function(fields) {
    id <- fields[1]
    if (id %in% names(IB.Message.parsers.by.id)) {
        print(messages.native[messages.native == id])
        IB.Message.parsers.by.id[[id]](fields)
    } else {
        IB.Message('Unknown', list(values=fields))
    }
}


