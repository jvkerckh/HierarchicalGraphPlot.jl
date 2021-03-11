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
