@testset "Node ranking tests" begin

testGraph = MetaDiGraph( 8 )
add_edge!.( Ref( testGraph ),
    [ 1, 1, 1, 2, 3, 4, 5, 6, 7 ],
    [ 2, 5, 6, 3, 4, 8, 7, 7, 8 ] )

testGraph2 = MetaDiGraph( 9 )
add_edge!.( Ref( testGraph2 ),
    [ 1, 1, 5, 5, 2, 2, 6, 3, 3, 4, 4, 8 ],
    [ 3, 2, 6, 3, 3, 4, 7, 7, 8, 8, 9, 9 ] )

@testset "function generateInitialNodeRanks!" begin
    HGP.generateInitialNodeRanks!( testGraph )
    @test all( has_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) )

    HGP.generateInitialNodeRanks!( testGraph2 )
    @test all( has_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rank ) )
end  # @testset "function generateInitialNodeRanks!"

@testset "function generateFeasibleTree" begin
    fTree = HGP.generateFeasibleTree!( testGraph )
    @test ( nv( fTree ) == 8 ) && ( ne( fTree ) == 7 ) &&
        is_connected( fTree ) && ( get_prop( fTree, 1, :rank ) == 0 ) &&
        ( get_prop( fTree, 8, :rank ) == 4 ) &&
        all( has_prop.( Ref( fTree ), collect( edges( fTree ) ), :cut ) )
        
    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 0, 1, 2, 3, 2, 2, 3, 4 ] )
    fTree = HGP.generateFeasibleTree!( testGraph )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .==
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] ) && ( get_prop( fTree, 1, :rank ) == 0 ) &&
        ( get_prop( fTree, 8, :rank ) == 4 ) &&
        all( 0 .<= get_prop.( Ref( fTree ), vertices( fTree ), :rank ) .<= 4 )

    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 1, 4, 5, 7, 9, 3, 5, 8 ] )
    fTree = HGP.generateFeasibleTree!( testGraph )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .==
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] ) && ( get_prop( fTree, 1, :rank ) == 0 ) &&
        ( get_prop( fTree, 8, :rank ) == 4 ) &&
        all( 0 .<= get_prop.( Ref( fTree ), vertices( fTree ), :rank ) .<= 4 )

    fTree2 = HGP.generateFeasibleTree!( testGraph2 )
    @test ( nv( fTree2 ) == 9 ) && ( ne( fTree2 ) == 8 ) &&
        all( get_prop.( Ref( testGraph2 ), vertices( testGraph2 ), :rank ) ==
            [ 0, 1, 2, 2, 0, 1, 3, 3, 4 ] )
end  # @testset "function generateFeasibleTree"

@testset "function generateGraphNodeRanks!" begin
    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] )
    generateGraphNodeRanks!( testGraph )
    graphEdges = collect( edges( testGraph ) )
    @test all( HGP.getEdgeMinSlack.( Ref( testGraph ), graphEdges ) .<=
        HGP.getEdgeSlack.( Ref( testGraph ), graphEdges ) )
    
    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 0, 1, 2, 3, 2, 2, 3, 4 ] )
    generateGraphNodeRanks!( testGraph )
    graphEdges = collect( edges( testGraph ) )
    @test all( HGP.getEdgeMinSlack.( Ref( testGraph ), graphEdges ) .<=
        HGP.getEdgeSlack.( Ref( testGraph ), graphEdges ) )

    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 1, 4, 5, 7, 9, 3, 5, 8 ] )
    generateGraphNodeRanks!( testGraph )
    graphEdges = collect( edges( testGraph ) )
    @test all( HGP.getEdgeMinSlack.( Ref( testGraph ), graphEdges ) .<=
        HGP.getEdgeSlack.( Ref( testGraph ), graphEdges ) )
    
    generateGraphNodeRanks!( testGraph2 )
    graphEdges = collect( edges( testGraph ) )
    @test all( HGP.getEdgeMinSlack.( Ref( testGraph ), graphEdges ) .<=
        HGP.getEdgeSlack.( Ref( testGraph ), graphEdges ) )
end  # @testset "function generateGraphNodeRanks!"

end  # @testset "Node ranking tests"