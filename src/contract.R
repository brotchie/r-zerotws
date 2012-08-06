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
