function orderGraphNodesPerRank!( grf::MetaDiGraph; maxIter::Int = 24 )

    if maxIter <= 0
        @warn "Negative max number of iterations entered, setting to 24."
        maxIter = 24
    end  # if maxIter <= 0

    addDummyNodes!( grf )
    ranks, currentRankOrder = generateInitialOrder!( grf )
    bestCrossings = countEdgeCrossings( grf )
    ii = 1

    while ( bestCrossings > 0 ) && ( ii <= maxIter )
        currentRankOrder = weightedMedianNodeOrdering( grf, ranks,
            currentRankOrder, isodd( ii ) )
        currentCrossings = countEdgeCrossings( grf, ranks, currentRankOrder )
        
        if currentCrossings < bestCrossings
            bestCrossings = currentCrossings
            set_prop!.( Ref( grf ), vertices( grf ), :rankPos,
                currentRankOrder )
        end  # if currentCrossings < bestCrossings

        ii += 1
    end  # while ( bestCrossings > 0 ) && ...

end  # orderGraphNodesPerRank!( grf, maxIter )


function addDummyNodes!( grf::MetaDiGraph )

    rankLevels = get_prop( grf, :rankLevels )

    for edge in edges( grf )
        intermediateRanks = filter( rank -> get_prop( grf, edge.src, :rank ) <
            rank < get_prop( grf, edge.dst, :rank ), rankLevels )
        sort!( intermediateRanks )

        # Only take action if the edge spans multiple rank levels.
        if !isempty( intermediateRanks )
            nNodes = nv( grf )
            add_vertices!( grf, length( intermediateRanks ) )
            set_prop!.( Ref( grf ), nNodes .+ (1:length( intermediateRanks ) ),
                :rank, intermediateRanks )
            set_prop!.( Ref( grf ), nNodes .+ (1:length( intermediateRanks ) ),
                :dummy, true )

            # Between two dummy nodes.
            for ii in 2:length( intermediateRanks )
                newEdge = Edge( nNodes + ii - 1, nNodes + ii )
                add_edge!( grf, newEdge )
                setEdgeWeight!( grf, newEdge, 8.0 * getEdgeWeight( grf, edge ) )
            end  # for ii in 2:length( intermediateRanks )

            # From source node to first dummy node.
            newEdge = Edge( edge.src, nNodes + 1 )
            add_edge!( grf, newEdge )
            setEdgeWeight!( grf, newEdge, 2.0 * getEdgeWeight( grf, edge ) )

            # From last dummy node to destination node.
            newEdge = Edge( nv( grf ), edge.dst )
            add_edge!( grf, newEdge )

            for prop in keys( props( grf, edge ) )
                set_prop!( grf, newEdge, prop, get_prop( grf, edge, prop ) )
            end  # for prop in keys( props( grf, edge ) )

            setEdgeWeight!( grf, newEdge, 2.0 * getEdgeWeight( grf, edge ) )

            # Remove original edge.
            clear_props!( grf, edge )
            rem_edge!( grf, edge )
        end  # if !isempty( intermediateRanks )
    end  # for edge in edges( grf )

end  # addDummyNodes!( grf )


function generateInitialOrder!( grf::MetaDiGraph )

    rankLevels = get_prop( grf, :rankLevels )
    orderedNodes = filter( node -> isempty( inneighbors( grf, node ) ),
        vertices( grf ) )
    ii = 1
    
    while length( orderedNodes ) < nv( grf )
        append!( orderedNodes, filter( node -> node ∉ orderedNodes,
            outneighbors( grf, orderedNodes[ ii ] ) ) )
        ii += 1
    end  # while length( orderedNodes ) < nv( grf )

    for rank in rankLevels
        nodesOfRank = filter( node -> get_prop( grf, node, :rank ) == rank,
            orderedNodes )
        set_prop!.( Ref( grf ), nodesOfRank, :rankPos,
            eachindex( nodesOfRank ) )
    end  # for rank in rankLevels

    return get_prop.( Ref( grf ), vertices( grf ), :rank ),
        get_prop.( Ref( grf ), vertices( grf ), :rankPos )

end  # generateInitialOrder!( grf )


function countEdgeCrossings( grf::MetaDiGraph, ranks::Vector{Float64},
    rankOrder::Vector{Int} )

    crossings = 0
    graphEdges = collect( edges( grf ) )
    rankLevels = get_prop( grf, :rankLevels )
    nLevels = length( rankLevels )

    for ii in 1:(nLevels - 1)
        rank, nextRank = rankLevels[ ii ], rankLevels[ ii + 1 ]
        rankEdges = filter( graphEdges ) do edge
            return ( ranks[ edge.src ] == rank ) &&
                ( ranks[ edge.dst ] == nextRank )
        end  # filter( graphEdges ) do edge
        nEdges = length( rankEdges )
        
        for jj in 1:( nEdges - 1 ), kk in ( jj + 1 ):nEdges
            edge1, edge2 = rankEdges[ jj ], rankEdges[ kk ]

            if ( edge1.src != edge2.src ) && ( edge1.dst != edge2.dst ) &&
                xor( rankOrder[ edge1.src ] - rankOrder[ edge2.src ] > 0,
                rankOrder[ edge1.dst ] - rankOrder[ edge2.dst ] > 0 )
                crossings += 1
            end  # if ( edge1.src != edge2.src ) && ...
        end  # for edge1 in rankEdges[ 1:(end - 1) ], ...
    end  # for ii in 1:(nLevels - 1)

    return crossings

end  # countEdgeCrossings( grf, ranks, rankOrder )

countEdgeCrossings( grf::MetaDiGraph ) = countEdgeCrossings( grf,
    get_prop.( Ref( grf ),  vertices( grf ), :rank ),
    get_prop.( Ref( grf ), vertices( grf ), :rankPos ) )


function weightedMedianNodeOrdering( grf::MetaDiGraph, ranks::Vector{Float64},
    rankOrder::Vector{Int}, isDown::Bool )

    rankLevels = get_prop( grf, :rankLevels )
    nLevels = length( rankLevels )
    medians = zeros( Float64, nv( grf ) )

    for ii in ( isDown ? ( 2:nLevels ) : ( (nLevels - 1):-1:1 ) )
        nodesInRank = findall( ranks .== rankLevels[ ii ] )
        nodesInRank = nodesInRank[ sortperm( rankOrder[ nodesInRank ] ) ]

        for node in nodesInRank
            nodeNeighbors = ( isDown ? inneighbors : outneighbors )( grf, node )
            
            if !isempty( nodeNeighbors )
                medians[ node ] = median( rankOrder[ nodeNeighbors ],
                    StatsBase.weights( isDown ? getEdgeWeight.( Ref( grf ),
                    nodeNeighbors, node ) : getEdgeWeight.( Ref( grf ),
                    node, nodeNeighbors ) ) )
            end  # if !isempty( nodeNeighbors )
        end  # for node in nodesInRank

        freeNodes = filter( node -> medians[ node ] != 0, nodesInRank )
        currentOrder = rankOrder[ freeNodes ]
        sort!( currentOrder )
        newOrderedNodes = freeNodes[ sortperm( medians[ freeNodes ] ) ]
        rankOrder[ newOrderedNodes ] = currentOrder
    end  # for ii in ( isDown ? ( 1:(nLevels - 1) ) : ( nLevels:-1:2 ) )

    return rankOrder

end  # weightedMedianNodeOrdering( grf, ranks, rankOrder, isDown )

# ========

function orderNodes( dgrf::MetaDiGraph; maxIter::Int = 24 )::Vector{Vector{Int}}

    if maxIter <= 0
        warn( "Negative max number of iterations entered, setting to 24." )
        maxIter = 24
    end  # if maxIter <= 0

    order = generateInitOrder( dgrf )
    bestOrder = deepcopy( order )
    bestCrossings = countCrossings( dgrf, bestOrder )

    nIter = 1

    while ( nIter <= maxIter ) && ( bestCrossings > 0 )
        weightedMedian( dgrf, order, nIter )
        transposeNodes( dgrf, order )
        orderCrossings = countCrossings( dgrf, order )

        if orderCrossings < bestCrossings
            bestOrder = deepcopy( order )
            bestCrossings = orderCrossings
        end  # if orderCrossings < bestCrossings

        nIter += 1
    end  # while nIter <= 100

    return bestOrder

end  # orderNodes( dgrf, order )


function generateInitOrder( dgrf::MetaDiGraph )::Vector{Vector{Int}}

    ranks = get_prop.( dgrf, vertices( dgrf ), :rank )
    order = find( isempty.( dgrf.graph.badjlist ) )
    ii = 1

    # First, get all nodes with rank 0 in the order vector. Then, for each node
    #   in the order queue, add all its direct successor nodes.
    while length( order ) < length( ranks )
        node = order[ ii ]
        append!( order, filter( tmpNode -> ( tmpNode ∉ order ) &&
            ( ranks[ node ] + 1 == ranks[ tmpNode ] ),
            dgrf.graph.fadjlist[ node ] ) )
        ii += 1
    end  # while length( order ) < length( ranks )

    ranks = ranks[ order ]
    order = map( r -> order[ find( ranks .== r ) ], 1:maximum( ranks ) )
    return map( rankOrder -> rankOrder[ randperm( length( rankOrder ) ) ],
        order )

end  # generateInitOrder( dgrf::MetaDiGraph )


function transposeNodes( dgrf::MetaDiGraph, order::Vector{Vector{Int}} )::Void

    isImproved = true
    currentCrossings = countCrossings( dgrf, order )

    while isImproved
        isImproved = false

        for rank in eachindex( order )
            orderRank = order[ rank ]
            dstNode = orderRank[ 1 ]

            for ii in 2:length( orderRank )
                srcNode = dstNode
                dstNode = orderRank[ ii ]

                # Get number of crossings when order is changed.
                order[ rank ][ [ ii-1, ii ] ] = order[ rank ][ [ ii, ii-1 ] ]
                newCrossings = countCrossings( dgrf, order )

                if newCrossings < currentCrossings
                    currentCrossings = newCrossings
                    isImproved = true
                else
                    order[ rank ][ [ ii-1, ii ] ] =
                        order[ rank ][ [ ii, ii-1 ] ]
                end  # if newCrossings < currentCrossings
            end  # for ii in 2:length( orderRank )
        end  # for rank in eachindex( order )
    end  # while isImproved

    return

end  # transposeNodes( dgrf, order )


function countCrossings( dgrf::MetaDiGraph, order::Vector{Vector{Int}} )::Int

    crossings = 0
    targetNodes = order[ 1 ]

    for ll = 2:length( order )
        sourceNodes = targetNodes
        targetNodes = order[ ll ]

        if ( length( sourceNodes ) > 1 ) && ( length( targetNodes ) > 1 )
            for ii1 in 1:(length( sourceNodes ) - 1),
                jj2 in 1:(length( targetNodes ) - 1)
                src1 = sourceNodes[ ii1 ]
                dst2 = targetNodes[ jj2 ]

                for ii2 in (ii1 + 1):length( sourceNodes ),
                    jj1 in (jj2 + 1):length( targetNodes )
                    src2 = sourceNodes[ ii2 ]
                    dst1 = targetNodes[ jj1 ]

                    if has_edge( dgrf, src1, dst1 ) &&
                        has_edge( dgrf, src2, dst2 )
                        # crossings += 1
                        crossings += getEdgeWeight( dgrf, src1, dst1 ) +
                            getEdgeWeight( dgrf, src2, dst2 )
                    end  # has_edge( dgrf, ii1, jj1 ) &&
                end  # for ii2 in (ii1 + 1):length( sourceNodes ), ...
            end  # for ii1 in 1:(length( sourceNodes ) - 1), ...
        end  # if ( length( sourceNodes ) > 1 ) && ...
    end  # for ll = 2:length( order )

    return crossings

end  # countCrossings( dgrf, order )


function weightedMedian( dgrf::MetaDiGraph, order::Vector{Vector{Int}},
    nIter::Int )::Void

    maxRank = length( order )

    for rr in ( isodd( nIter ) ? ( 2:maxRank ) : ( (maxRank - 1):-1:1 ) )
        md = map( node -> medianNodeValue( node, dgrf, order, isodd( nIter ) ),
            order[ rr ] )
        order[ rr ] = order[ rr ][ sortperm( md ) ]
    end  # for rr in ( isodd( nIter ) ? ...

    return

end  # weightedMedian( dgrf, order, nIter )


function medianNodeValue( node::Int, dgrf::MetaDiGraph,
    order::Vector{Vector{Int}}, preRank::Bool )::Float64

    # Find the sorted indices of the node predecessors/successors in the
    #   appropriate rank.
    nodeRank = get_prop( dgrf, node, :rank )
    nodesInAdjRank = sort( order[ nodeRank - ( preRank ? 1 : -1 ) ] )
    adjOrderInRank = sort( find( map( tmpNode -> tmpNode ∈
        ( preRank ? dgrf.graph.badjlist : dgrf.graph.fadjlist )[ node ],
        nodesInAdjRank ) ) )

    nAdj = length( adjOrderInRank )
    m = ( nAdj + 1 ) / 2

    if nAdj == 0
        return 1.0
    end

    if nAdj == 2
        return mean( adjOrderInRank )
    end

    if isodd( nAdj )
        return adjOrderInRank[ Int( m ) ]
    end

    left = adjOrderInRank[ floor( Int, m ) ] - adjOrderInRank[ 1 ]
    right = adjOrderInRank[ end ] - adjOrderInRank[ ceil( Int, m ) ]
    return ( adjOrderInRank[ floor( Int, m ) ] * right +
        adjOrderInRank[ ceil( Int, m ) ] * left ) / ( left + right )

end  # medianNodeValue( node, dgrf, order, preRank )
