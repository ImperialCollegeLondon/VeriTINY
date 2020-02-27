module SimulationTypes
open SharedTypes

type Block = Megablock * Map<NetIdentifier,Net> * Map<NetIdentifier,Net>

// list of Synchronous Megablocks
let syncMegaLst: Megablock list = [Name "DFF"]
