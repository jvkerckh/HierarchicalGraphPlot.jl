@testset "Node ranking tests" begin

testGraph = MetaDiGraph( 8 )
add_edge!.( Ref( testGraph ),
    [ 1, 1, 1, 2, 3, 4, 5, 6, 7 ],
    [ 2, 5, 6, 3, 4, 8, 7, 7, 8 ] )

@testset "function generateInitialNodeRanks!" begin
    HGP.generateInitialNodeRanks!( testGraph )
    @test all( has_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) )
end  # @testset "function generateInitialNodeRanks!"

@testset "function generateFeasibleTree" begin
    fTree = HGP.generateFeasibleTree( testGraph, true )
    @test ( nv( fTree ) == 8 ) && ( ne( fTree ) == 7 ) &&
        is_connected( fTree ) && ( get_prop( fTree, 1, :rank ) == 0 ) &&
        ( get_prop( fTree, 8, :rank ) == 4 ) &&
        all( has_prop.( Ref( fTree ), collect( edges( fTree ) ), :cut ) )
        
    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 0, 1, 2, 3, 2, 2, 3, 4 ] )
    fTree = HGP.generateFeasibleTree( testGraph, false )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .==
        [ 0, 1, 2, 3, 2, 2, 3, 4 ] ) && ( get_prop( fTree, 1, :rank ) == 0 ) &&
        ( get_prop( fTree, 8, :rank ) == 4 ) &&
        all( 0 .<= get_prop.( Ref( fTree ), vertices( fTree ), :rank ) .<= 4 )

    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 1, 4, 5, 7, 9, 3, 5, 8 ] )
    fTree = HGP.generateFeasibleTree( testGraph, false )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .==
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] ) && ( get_prop( fTree, 1, :rank ) == 0 ) &&
        ( get_prop( fTree, 8, :rank ) == 4 ) &&
        all( 0 .<= get_prop.( Ref( fTree ), vertices( fTree ), :rank ) .<= 4 )
end  # @testset "function generateFeasibleTree"

@testset "function generateGraphNodeRanks!" begin
    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] )
    generateGraphNodeRanks!( testGraph, true )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .== 
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] )
        
    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 0, 1, 2, 3, 2, 2, 3, 4 ] )
    generateGraphNodeRanks!( testGraph, false )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .== 
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] )

    set_prop!.( Ref( testGraph ), vertices( testGraph ), :rank,
        [ 1, 4, 5, 7, 9, 3, 5, 8 ] )
    generateGraphNodeRanks!( testGraph, false )
    @test all( get_prop.( Ref( testGraph ), vertices( testGraph ), :rank ) .== 
        [ 0, 1, 2, 3, 1, 1, 2, 4 ] )
end  # @testset "function generateGraphNodeRanks!"

end  # @testset "Node ranking tests"