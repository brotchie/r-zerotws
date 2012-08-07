##  ___                  _                                                   ##
## | _ \  ______ _ _ ___| |___ __ _____ r-ØTWS - Comms Between R and an IB   ##
## |   /_|_ / -_) '_/ _ \  _\ V  V (_-<          TWS API <-> ØMQ Proxy       ##
## |_|_(_)__\___|_| \___/\__|\_/\_//__/ (c) 2012, James Brotchie             ##
##                                      http://zerotick.org/                 ##

`IB.Contract.definition` <- list(
      contract.id            = .int
    , symbol                 = .string
    , security.type          = .string
    , expiry                 = .string
    , strike                 = .double
    , right                  = .string
    , multiplier             = .string
    , exchange               = .string
    , currency               = .string
    , symbol.local           = .string
    , exchange.primary       = .string
    , include.expired        = .boolean
    , security.id.type       = .string
    , security.id            = .string
    , combo.legs.description = .string
)

`IB.Contract` <- build.constructor('IB.Contract', IB.Contract.definition)
`as.character.IB.Contract` <- build.pretty.as.character('Contract')

`IB.ContractDetails.definition` <- list(
      market.name         = .string
    , trading.class       = .string
    , tick.min            = .double
    , price.magnifier     = .int
    , order.types         = .string
    , valid.exchanges     = .string
    , under.contract.id   = .int
    , name.long           = .string
    , contract.month      = .string
    , industry            = .string
    , category            = .string
    , subcategory         = .string
    , timezone.id         = .string
    , trading.hours       = .string
    , liquid.hours        = .string
    , ev.rule             = .string
    , ev.multiplier       = .double
    , cusip               = .string
    , ratings             = .string
    , desc.append         = .string
    , bond.type           = .string
    , coupon.type         = .string
    , callable            = .boolean
    , putable             = .boolean
    , coupon              = .double
    , convertible         = .boolean
    , maturity            = .string
    , issue.date          = .string
    , next.option.date    = .string
    , next.option.type    = .string
    , next.option.partial = .boolean
    , notes               = .string
)
`IB.ContractDetails` <- build.constructor('IB.ContractDetails', IB.ContractDetails.definition)
`as.character.IB.ContractDetails` <- build.pretty.as.character('ContractDetails')

`IB.ComboLeg.definition` <- list(
      contract.id        = .int
    , ratio              = .int
    , action             = .string
    , exchange           = .string
    , openclose          = .int
    , shortsaleslot      = .int
    , designatedlocation = .string
    , exemptcode         = .int
)

`IB.ComboLeg` <- build.constructor('IB.ComboLeg', IB.ComboLeg.definition)
`as.character.IB.ComboLeg` <- build.pretty.as.character('ComboLeg')

`IB.UnderComp.definition` <- list(
      contract.id = .int
    , delta       = .double
    , price       = .double
)

`IB.UnderComp` <- build.constructor('IB.UnderComp', IB.UnderComp.definition)
`as.character.IB.UnderComp` <- build.pretty.as.character('UnderComp')
