module SimulationTypes
open SharedTypes

type Block = Megablock * Map<NetIdentifier,Net> * Map<NetIdentifier,Net>
