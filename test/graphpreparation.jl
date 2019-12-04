@testset "Graph preparation tests" begin

testGraph = SimpleDiGraph( 8 )
add_edge!.( Ref( testGraph ),
    [ 1, 1, 1, 2, 3, 4, 5, 6, 7 ],
    [ 2, 5, 6, 3, 4, 8, 7, 7, 8 ] )

testGraph2 = MetaDiGraph( 8 )
add_edge!.( Ref( testGraph2 ),
    [ 1, 1, 1, 2, 3, 4, 5, 6, 7 ],
    [ 2, 5, 6, 3, 4, 8, 7, 7, 8 ] )

@testset "function removeGraphCycles!" begin
    doubleEdges, flippedEdges, mgrf = HGP.removeGraphCycles!( testGraph )
    @test isempty( doubleEdges ) && isempty( flippedEdges )

    doubleEdges, flippedEdges, mgrf = HGP.removeGraphCycles!( testGraph2 )
    @test isempty( doubleEdges ) && isempty( flippedEdges )

    testGraph3 = SimpleDiGraph( 2 )
    add_edge!.( Ref( testGraph3 ), [ 1, 2 ], [ 2, 1 ] )
    doubleEdges, flippedEdges, mgrf = HGP.removeGraphCycles!( testGraph3 )
    @test !isempty( doubleEdges ) && isempty( flippedEdges )

    testGraph4 = MetaDiGraph( 2 )
    add_edge!.( Ref( testGraph4 ), [ 1, 2 ], [ 2, 1 ] )
    doubleEdges, flippedEdges, mgrf = HGP.removeGraphCycles!( testGraph4 )
    @test !isempty( doubleEdges ) && isempty( flippedEdges )

    testGraph5 = SimpleDiGraph( 4 )
    add_edge!.( Ref( testGraph5 ), [ 1, 2, 3, 4 ], [ 2, 3, 4, 1 ] )
    doubleEdges, flippedEdges, mgrf = HGP.removeGraphCycles!( testGraph5 )
    @test isempty( doubleEdges ) && !isempty( flippedEdges )

    testGraph6 = SimpleDiGraph( 4 )
    add_edge!.( Ref( testGraph6 ), [ 1, 2, 3, 4 ], [ 2, 3, 4, 1 ] )
    doubleEdges, flippedEdges, mgrf = HGP.removeGraphCycles!( testGraph6 )
    @test isempty( doubleEdges ) && !isempty( flippedEdges )
end  # @testset "function removeGraphCycles!"

end  # @testset "Graph hierarchy tests"