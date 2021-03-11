using Dates
using HierarchicalGraphPlot
using LightGraphs
using MetaGraphs
using Test

HGP = HierarchicalGraphPlot

versionHGP()
println()

tStart = now()

@testset "Full test" begin

include( "graphpreparation.jl" )
include( "noderanks.jl" )

end  # @testset "Full test"

tElapsed = ( now() - tStart ).value / 1000
@info string( "Unit tests completed in ", tElapsed, " seconds." )