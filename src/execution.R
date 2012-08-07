`IB.Execution.definition` <- list(
     order.id       = .int
   , client.id      = .int
   , execution.id   = .string
   , time           = .string
   , account.number = .string
   , exchange       = .string
   , side           = .string
   , shares         = .int
   , price          = .double
   , perm.id        = .int
   , liquidation    = .int
   , cum.quantity   = .int
   , average.price  = .double
   , order.ref      = .string
   , ev.rule        = .string
   , ev.multiplier  = .double
)

`IB.Execution` <- build.constructor('IB.Execution', IB.Execution.definition)
`as.character.IB.Execution` <- build.pretty.as.character('Execution')
