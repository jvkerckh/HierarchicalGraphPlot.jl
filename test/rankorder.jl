@testset "Node ordering tests" begin

testGraph = MetaDiGraph( 8 )
add_edge!.( Ref( testGraph ),
    [ 1, 1, 1, 2, 3, 4, 5, 6, 7 ],
    [ 2, 5, 6, 3, 4, 8, 7, 7, 8 ] )
generateGraphNodeRanks!( testGraph )

testGraph2 = MetaDiGraph( 9 )
add_edge!.( Ref( testGraph2 ),
    [ 1, 1, 5, 5, 2, 2, 6, 3, 3, 4, 4, 8 ],
    [ 3, 2, 6, 3, 3, 4, 7, 7, 8, 8, 9, 9 ] )
generateGraphNodeRanks!( testGraph2 )

@testset "function addDummyNodes!" begin
    HGP.addDummyNodes!( testGraph )
    @test ( nv( testGraph ) == 9 ) &&
        ( get_prop( testGraph, 9, :rank ) == 3 ) &&
        !has_edge( testGraph, 7, 8 ) &&
        all( has_edge.( Ref( testGraph ), [ 7, 9 ], [ 9, 8 ] ) ) &&
        all( HGP.getEdgeWeight.( Ref( testGraph ), [ 7, 9 ], [ 9, 8 ] ) .== 2 )

    HGP.addDummyNodes!( testGraph2 )
    @test ( nv( testGraph2 ) == 11 ) &&
        all( get_prop.( Ref( testGraph2 ), [ 10, 11 ], :rank ) .== [ 1, 3 ] ) &&
        !any( has_edge.( Ref( testGraph2 ), [ 1, 4 ], [ 3, 9 ] ) ) &&
        all( has_edge.( Ref( testGraph2 ), [ 1, 10, 4, 11 ],
        [ 10, 3, 11, 9 ] ) ) && all( HGP.getEdgeWeight.( Ref( testGraph2 ),
        [ 1, 10, 4, 11 ], [ 10, 3, 11, 9 ] ) .== 2 )
end  # @testset "function addDummyNodes!"

@testset "function generateInitialOrder!" begin
    HGP.generateInitialOrder!( testGraph )
    @test all( has_prop.( Ref( testGraph ), vertices( testGraph ), :rankPos ) )

    HGP.generateInitialOrder!( testGraph2 )
    @test all( has_prop.( Ref( testGraph2 ), vertices( testGraph2 ),
        :rankPos ) )
end  # @testset "function generateInitialOrder!"

@testset "function countEdgeCrossings" begin
    set_prop!.( Ref( testGraph ), [ 2, 5, 6 ], :rankPos, [ 3, 1, 2 ] )
    @test HGP.countEdgeCrossings( testGraph ) == 2
    HGP.generateInitialOrder!( testGraph )
    @test HGP.countEdgeCrossings( testGraph ) == 0

    HGP.generateInitialOrder!( testGraph2 )
    @test HGP.countEdgeCrossings( testGraph2 ) == 4
end  # @testset "function countEdgeCrossings"

@testset "function weightedMedianNodeOrdering" begin
    ranks = get_prop.( Ref( testGraph ), vertices( testGraph ), :rank )
    HGP.generateInitialOrder!( testGraph )
    set_prop!.( Ref( testGraph ), [ 2, 5, 6 ], :rankPos, [ 3, 1, 2 ] )
    newOrder = HGP.weightedMedianNodeOrdering( testGraph,
        get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ),
        get_prop.( Ref( testGraph ), vertices( testGraph ), :rankPos ), true )
    @test HGP.countEdgeCrossings( testGraph, ranks, newOrder ) == 0
    
    HGP.generateInitialOrder!( testGraph )
    set_prop!.( Ref( testGraph ), [ 2, 5, 6 ], :rankPos, [ 3, 1, 2 ] )
    newOrder = HGP.weightedMedianNodeOrdering( testGraph,
        get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ),
        get_prop.( Ref( testGraph ), vertices( testGraph ), :rankPos ), false )
    @test HGP.countEdgeCrossings( testGraph, ranks, newOrder ) == 0

    ranks = get_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rank )
    HGP.generateInitialOrder!( testGraph2 )
    newOrder = HGP.weightedMedianNodeOrdering( testGraph2,
        get_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rank ),
        get_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rankPos ), true )
    @test HGP.countEdgeCrossings( testGraph2, ranks, newOrder ) == 3

    HGP.generateInitialOrder!( testGraph2 )
    newOrder = HGP.weightedMedianNodeOrdering( testGraph2,
        get_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rank ),
        get_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rankPos ),
        false )
    @test HGP.countEdgeCrossings( testGraph2, ranks, newOrder ) == 0
end  # @testset "function weightedMedianNodeOrdering"

@testset "function orderGraphNodesPerRank!" begin
    HGP.orderGraphNodesPerRank!( testGraph )
    @test HGP.countEdgeCrossings( testGraph ) == 0

    HGP.orderGraphNodesPerRank!( testGraph2 )
    @test HGP.countEdgeCrossings( testGraph2 ) == 0
end  # @testset "function orderGraphNodesPerRank!"

end  # @testset "Node ordering tests"