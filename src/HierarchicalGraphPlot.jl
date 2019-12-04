__precompile__()

module HierarchicalGraphPlot

    using DataStructures
    using LightGraphs
    using MetaGraphs
    using Statistics
    using StatsBase

    version = v"0.1.0"

    export versionHGP
    versionHGP() = @info string( "Running version ", version,
        " of HierarchicalGraphPlot module in Julia v", VERSION )
    
    include( "graphpreparation.jl" )
    include( "noderanks.jl" )
    include( "rankorder.jl" )

end  # module HierarchicalGraphPlot
