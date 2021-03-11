__precompile__()

module HierarchicalGraphPlot

    using DataStructures
    using LightGraphs
    using MetaGraphs

    version = v"0.2.0"

    export versionHGP
    versionHGP() = @info string( "Running version ", version,
        " of HierarchicalGraphPlot module in Julia v", VERSION )
    
    include( "graphpreparation.jl" )
    include( "noderanks.jl" )

end  # module HierarchicalGraphPlot
