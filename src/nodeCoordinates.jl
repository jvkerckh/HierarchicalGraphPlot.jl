function generateNodeCoordinates!( dgrf::MetaDiGraph,
    order::Vector{Vector{Int}}, nodeWidths::Vector{Int}, nodeGap::Int;
    maxIter::Int = 8 )::Void

    if maxIter <= 0
        warn( "Negative max number of iterations entered, setting to 8." )
        maxIter = 8
    end  # if maxIter <= 0

    # Get initial set of coordinates.
    agrf = generateInitCoords!( dgrf, order, nodeWidths, nodeGap )
    bestCoords = get_prop.( agrf, vertices( dgrf ), :rank )
    bestDist = getWeightedDistance( dgrf, bestCoords )
    # println( "Initial: ", bestCoords )
    # println( "Initial weighted distance: ", bestDist )

    for ii in 1:( length( vertices( dgrf ) ) == 1 ? 0 : maxIter )
        # info( "Iteration ", ii )
        medianNodePosition!( agrf, dgrf, order, isodd( ii ), true )
        newCoords = get_prop.( agrf, vertices( dgrf ), :rank )
        newCoords -= minimum( newCoords ) - 1
        # println( "After median node: ", newCoords, " (weight ",
        #     getWeightedDistance( dgrf, newCoords ), ")", '\n' )

        medianNodePosition!( agrf, dgrf, order, isodd( ii ), false )
        newCoords = get_prop.( agrf, vertices( dgrf ), :rank )
        newCoords -= minimum( newCoords ) - 1
        # println( "After min edge: ", newCoords, " (weight ",
        #     getWeightedDistance( dgrf, newCoords ), ")", '\n' )

        minNodePosition!( agrf, dgrf, order )
        newCoords = get_prop.( agrf, vertices( dgrf ), :rank )
        newCoords -= minimum( newCoords ) - 1
        # println( "After min node: ", newCoords, " (weight ",
        #     getWeightedDistance( dgrf, newCoords ), ")", '\n' )

        straightenLongEdges!( agrf, dgrf, order )
        newCoords = get_prop.( agrf, vertices( dgrf ), :rank )
        newCoords -= minimum( newCoords ) - 1
        # println( "After long edge straighten: ", newCoords, " (weight ",
        #     getWeightedDistance( dgrf, newCoords ), ")", '\n' )

        # compactNodes!( agrf, dgrf, order )
        # newCoords = get_prop.( agrf, vertices( dgrf ), :rank )
        # newCoords -= minimum( newCoords ) - 1
        # println( "After compacting: ", newCoords, " (weight ",
        #     getWeightedDistance( dgrf, newCoords ), ")", '\n' )

        adjustEdgeNodeRanks!( agrf, length( vertices( dgrf ) ) )
        # verifySlacks( agrf )
        # generateRanks( agrf, false )
        # newCoords = get_prop.( agrf, vertices( dgrf ), :rank )
        # newCoords -= minimum( newCoords ) - 1
        # println( "After compacting: ", newCoords )
        # println( "Ranks of current iteration: ", get_prop.( agrf, vertices( dgrf ), :rank ) )
        wDist = getWeightedDistance( dgrf, newCoords )
        # println( "Weighted distance of current solution: ", wDist )

        if wDist < bestDist
            # println( "Solution improved by ", bestDist - wDist )
            bestDist = wDist
            bestCoords = newCoords
        end  # if wDist < bestDist
    end  # for ii in 0:(maxIter - 1)

    set_prop!.( dgrf, vertices( dgrf ), :coord, bestCoords .- 1 )
    # println( "Best solution: ", bestCoords, " (weight ",
        # getWeightedDistance( dgrf, bestCoords ), ")" )
    return

end  # generateNodeCoordinates!( dgrf, order, nodeWidths, nodeGap )


function generateInitCoords!( dgrf::MetaDiGraph, order::Vector{Vector{Int}},
    nodeWidths::Vector{Int}, nodeGap::Int )::MetaDiGraph

    agrf = generateAuxiliaryGraph( dgrf, order, nodeWidths, nodeGap )

    for rr in eachindex( order )
        rOrder = order[ rr ]
        set_prop!( agrf, rOrder[ 1 ], :rank, 1 )

        for ii in 2:length( rOrder )
            set_prop!( agrf, rOrder[ ii ], :rank,
                get_prop( agrf, rOrder[ ii - 1 ], :rank ) +
                get_prop( agrf, rOrder[ ii - 1 ], rOrder[ ii ], :minSlack ) )
        end  # for ii in 2:length( rOrder )
    end  # for rr in eachindex( order )

    return agrf

end  # generateInitCoords!( dgrf, order, nodeWidths, nodeGap )


function generateAuxiliaryGraph( dgrf::MetaDiGraph,
    order::Vector{Vector{Int}}, nodeWidths::Vector{Int},
    nodeGap::Int )::MetaDiGraph

    # Initialise auxiliary graph.
    agrf = MetaDiGraph( length( vertices( dgrf ) ) )

    # Add edges ordering nodes in each rank.
    for rank in eachindex( order )
        rankOrder = order[ rank ]

        for ii in 2:length( rankOrder )
            minSlack = nodeGap
            srcNode = rankOrder[ ii - 1 ]
            dstNode = rankOrder[ ii ]
            minSlack += has_prop( dgrf, srcNode, :dummy ) ? 0.0 :
                nodeWidths[ srcNode ] / 2
            minSlack += has_prop( dgrf, dstNode, :dummy ) ? 0.0 :
                nodeWidths[ dstNode ] / 2
            edge = Edge( srcNode, dstNode )
            add_edge!( agrf, edge )
            set_prop!( agrf, edge, :minSlack, ceil( Int, minSlack ) )
            setEdgeWeight!( agrf, edge, 0.0 )
        end  # for ii in 2:length( rankOrder )
    end  # for rank in eachindex( order )

    # Add edges for edges in original graph.
    for edge in edges( dgrf )
        add_vertex!( agrf, :edge, ( edge.src, edge.dst ) )
        tmpEdge = Edge( length( vertices( agrf ) ), edge.src )
        add_edge!( agrf, tmpEdge )
        set_prop!( agrf, tmpEdge, :minSlack, 0 )
        setEdgeWeight!( agrf, tmpEdge, getEdgeWeight( agrf, edge ) )
        tmpEdge = Edge( length( vertices( agrf ) ), edge.dst )
        add_edge!( agrf, tmpEdge )
        set_prop!( agrf, tmpEdge, :minSlack, 0 )
        setEdgeWeight!( agrf, tmpEdge, getEdgeWeight( agrf, edge ) )
    end  # for edge in edges( dgrf )

    return agrf

end  # generateAuxiliaryGraph( dgrf, order )


function getWeightedDistance( dgrf::MetaDiGraph, coords::Vector{Int} )::Float64

    wDist = map( collect( edges( dgrf ) ) ) do edge
        return getEdgeWeight( dgrf, edge ) *
            abs( coords[ edge.src ] - coords[ edge.dst ] )
    end  # map( collect( edges( dgrf ) ) ) do edge

    return sum( wDist )

end  # getWeightedDistance( dgrf, coords )


function medianNodePosition!( agrf::MetaDiGraph, dgrf::MetaDiGraph,
    order::Vector{Vector{Int}}, isDown::Bool, allEdges::Bool )::Void

    nodeOrder = getNodeOrder( dgrf, isDown )
    # println( "Node order: ", nodeOrder )
    limits = repeat( [ -Inf +Inf ], length( vertices( dgrf ) ) )

    if !allEdges
        filter!( node -> !has_prop( dgrf, node, :dummy ), nodeOrder )

        # Lock dummy nodes in place.
        for node in vertices( dgrf )
            if has_prop( dgrf, node, :dummy )
                nodeCoord = get_prop( agrf, node, :rank )
                sameRank = order[ get_prop( dgrf, node, :rank ) ]
                nodePos = findfirst( node .== sameRank )
                adjustLimits!( limits, nodePos, nodeCoord, agrf, sameRank )
            end  # if has_prop( dgrf, node, :dummy )
        end  # for node in vertices( dgrf )
    end  # if !allEdges

    for node in nodeOrder
        # info( "Placing node: ", node )
        sameRank = order[ get_prop( dgrf, node, :rank ) ]
        # print( "All nodes in same rank: ", sameRank )
        nodePos = findfirst( node .== sameRank )
        # println( "; Position of node in rank: ", nodePos )

        # Suggest new coordinate.
        adjNodes = isDown ? dgrf.graph.fadjlist[ node ] :
            dgrf.graph.badjlist[ node ]
        # println( "Adjacent nodes: ", adjNodes )

        if !allEdges
            adjNodes = filter( node -> !has_prop( dgrf, node, :dummy ),
                adjNodes )
            # XXX filter!( node -> !has_prop( dgrf, node, :dummy ), adjNodes )
            #   generates odd artefacts.
        end  # if !allEdges

        edgeWeights = isDown ? getEdgeWeight.( dgrf, node, adjNodes ) :
            getEdgeWeight.( dgrf, adjNodes, node )
        # println( "Weight of edges: ", edgeWeights )
        newCoord = isempty( adjNodes ) ?
            get_prop( agrf, node, :rank ) : ( isDown ? ceil : floor )( Int,
            median( get_prop.( agrf, adjNodes, :rank ) ) )

        if limits[ node, 1 ] > newCoord
            newCoord = Int( limits[ node, 1 ] )
        end  # if limits[ node, 1 ] > newCoord

        if limits[ node, 2 ] < newCoord
            newCoord = Int( limits[ node, 2 ] )
        end  # if limits[ node, 1 ] > newCoord

        # println( "Suggested new coordinate: ", newCoord )
        set_prop!( agrf, node, :rank, newCoord )
        adjustLimits!( limits, nodePos, newCoord, agrf, sameRank )
    end  # for node in nodeOrder

    verifySlacks( agrf, order )

    # newCoords = Int.( limits[ :, 1 ] )
    # set_prop!.( agrf, vertices( dgrf ), :rank, newCoords )
    return

end  # medianNodePosition!( agrf, dgrf, order, isDown )


function verifySlacks( agrf::MetaDiGraph )::Void

    for edge in edges( agrf )
        if all( has_prop.( agrf, [ edge.src, edge.dst ], :rank ) )
            slack = get_prop( agrf, edge.dst, :rank ) -
                get_prop( agrf, edge.src, :rank )
            minSlack = get_prop( agrf, edge, :minSlack )

            if slack < minSlack
                warn( "Edge slack problem on ", edge, ": slack = ", slack, " (min ",
                    minSlack, ")" )
            end
        else
            if !has_prop( agrf, edge.src, :rank )
                warn( "Node ", edge.src, " doesn't have rank yet." )
            end

            if !has_prop( agrf, edge.dst, :rank )
                warn( "Node ", edge.dst, " doesn't have rank yet." )
            end
        end
    end  # for edge in edges( agrf )

    return

end  # verifySlacks( agrf )

function verifySlacks( agrf::MetaDiGraph, order::Vector{Vector{Int}} )::Void

    for rank in eachindex( order )
        rankOrder = order[ rank ]
        endRank = get_prop( agrf, rankOrder[ 1 ], :rank )

        for ii in 2:length( rankOrder )
            startRank = endRank
            endRank = get_prop( agrf, rankOrder[ ii ], :rank )
            minSlack = get_prop( agrf, rankOrder[ ii - 1 ], rankOrder[ ii ],
                :minSlack )

            if endRank - startRank < minSlack
                warn( "Node separation problem between ", rankOrder[ ii - 1 ],
                    " and ", rankOrder[ ii ], ": separation = ",
                    endRank - startRank, " (min ", minSlack, ")" )
            end
        end
    end

    return

end  # verifySlacks( agrf, order )


function getNodeOrder( dgrf::MetaDiGraph, isDown::Bool )::Vector{Int}

    weightSum = map( node -> ( sum( isDown ?
            getEdgeWeight.( dgrf, node, dgrf.graph.fadjlist[ node ] ) :
            - getEdgeWeight.( dgrf, dgrf.graph.badjlist[ node ], node ) ),
            get_prop( dgrf, node, :rank ) ),
            vertices( dgrf ) )
    return sortperm( weightSum, rev = isDown )

end  # getNodeOrder( dgrf::MetaDiGraph, isDown::Bool )


function adjustLimits!( limits::Array{Float64,2}, nodePos::Int,
    newCoord::Int, agrf::MetaDiGraph, sameRank::Vector{Int} )::Void

    node = sameRank[ nodePos ]
    limits[ node, : ] = newCoord

    for ii in (nodePos - 1):-1:1
        prevNode = sameRank[ ii + 1 ]
        tmpNode = sameRank[ ii ]

        limits[ tmpNode, 2 ] = min( limits[ tmpNode, 2 ],
            limits[ prevNode, 2 ] -
            get_prop( agrf, tmpNode, prevNode, :minSlack ) )
    end  # for ii in (nodePos - 1):-1:1

    for ii in (nodePos + 1):length( sameRank )
        prevNode = sameRank[ ii - 1 ]
        tmpNode = sameRank[ ii ]

        limits[ tmpNode, 1 ] = max( limits[ tmpNode, 1 ],
            limits[ prevNode, 1 ] +
            get_prop( agrf, prevNode, tmpNode, :minSlack ) )
    end  # for ii in (nodePos + 1):length( sameRank )

    return

end  # adjustLimits!( limits, node, newCoord, agrf, sameRank )


function minNodePosition!( agrf::MetaDiGraph, dgrf::MetaDiGraph,
    order::Vector{Vector{Int}} )::Void

    nodeQueue = Queue( Int )
    enqueue!.( nodeQueue, vertices( dgrf ) )

    while !isempty( nodeQueue )
        node = dequeue!( nodeQueue )
        adjNodes = vcat( dgrf.graph.fadjlist[ node ],
            dgrf.graph.badjlist[ node ] )
        potentialPos = determinePotentialPositions( agrf, dgrf, node, adjNodes,
            order )
        potentialWeights = computePotentialWeights( agrf, dgrf, node, adjNodes,
            potentialPos )
        posInd = findmin( potentialWeights )[ 2 ]

        # If a position that results in a lower weight is found, shift the node
        #   to that position, and enqueue all its adjacent nodes which aren't in
        #   queue already.
        if posInd > 1
            set_prop!( agrf, node, :rank, potentialPos[ posInd ] )
            enqueue!.( nodeQueue, filter( tmpNode -> tmpNode ∉ nodeQueue,
                adjNodes ) )
        end  # if posInd > 1
    end  # while !sempty( nodeQueue )

    verifySlacks( agrf, order )
    return

end  # minNodePosition!( agrf, dgrf, order )


function determinePotentialPositions( agrf::MetaDiGraph, dgrf::MetaDiGraph,
    node::Int, adjNodes::Vector{Int}, order::Vector{Vector{Int}} )::Vector{Int}

    # Find position of node in its rank.
    sameRank = order[ get_prop( dgrf, node, :rank ) ]
    posInRank = findfirst( sameRank .== node )

    # Get list of potential node positions.
    potentialPos = get_prop.( agrf, vcat( node, adjNodes ), :rank )

    # Filter list of node positions if needed.
    if posInRank > 1
        minCoord = get_prop( agrf, sameRank[ posInRank - 1 ], :rank ) +
            get_prop( agrf, sameRank[ posInRank - 1 ], node, :minSlack )
        potentialPos = vcat( filter( pos -> pos >= minCoord, potentialPos ),
            minCoord )
    end  # if posInRank > 1

    if posInRank < length( sameRank )
        maxCoord = get_prop( agrf, sameRank[ posInRank + 1 ], :rank ) -
            get_prop( agrf, node, sameRank[ posInRank + 1 ], :minSlack )
        potentialPos = vcat( filter( pos -> pos <= maxCoord, potentialPos ),
            maxCoord )
    end  # if posInRank < length( sameRank )

    return potentialPos

end  # determinePotentialPositions( agrf, dgrf, node, adjNodes, order )


function computePotentialWeights( agrf::MetaDiGraph, dgrf::MetaDiGraph,
    node::Int, adjNodes::Vector{Int},
    potentialPos::Vector{Int} )::Vector{Float64}

    edgeWeights = getEdgeWeight.( dgrf, adjNodes, node ) +
        getEdgeWeight.( dgrf, node, adjNodes )
    adjNodePos = get_prop.( agrf, adjNodes, :rank )

    return map( pos -> sum( abs.( adjNodePos - pos ) .* edgeWeights ),
        potentialPos )

end  # computePotentialWeights( agrf, dgrf, node, adjNodes, order )


function straightenLongEdges!( agrf::MetaDiGraph, dgrf::MetaDiGraph,
    order::Vector{Vector{Int}} )::Void

    dummyNodes = filter( node -> has_prop( dgrf, node, :dummy ),
        vertices( dgrf ) )
    tmpLongEdges = get_prop.( dgrf, dummyNodes, :edge )
    longEdges = unique( tmpLongEdges )
    dummyNodeChains = map( edge -> dummyNodes[ Ref( edge ) .== tmpLongEdges ],
        longEdges )
    sortInd = sortperm( dummyNodeChains, rev = true,
        lt = ( a, b ) -> length( a ) < length( b ) )
    longEdges = longEdges[ sortInd ]
    dummyNodeChains = dummyNodeChains[ sortInd ]

    isImproved = true

    while isImproved
        isImproved = false

        for ii in eachindex( longEdges )
            edge = longEdges[ ii ]
            nodeChain = dummyNodeChains[ ii ]
            currentPos = get_prop.( agrf, nodeChain, :rank )
            posLims = hcat( fill( -Inf, length( nodeChain ) ),
                fill( Inf, length( nodeChain ) ) )

            for jj in eachindex( nodeChain )
                node = nodeChain[ jj ]
                sameRank = order[ get_prop( dgrf, node, :rank ) ]
                rankPos = findfirst( node .== sameRank )

                if rankPos > 1
                    posLims[ jj, 1 ] = get_prop( agrf, sameRank[ rankPos - 1 ],
                        :rank ) + get_prop( agrf, sameRank[ rankPos - 1 ], node,
                        :minSlack )
                end  # if rankPos > 1

                if rankPos < length( sameRank )
                    posLims[ jj, 2 ] = get_prop( agrf, sameRank[ rankPos + 1 ],
                        :rank ) - get_prop( agrf, node, sameRank[ rankPos + 1 ],
                        :minSlack )
                end  # if rankPos > 1
            end  # for jj in eachindex( nodeChain )

            minPos = maximum( posLims[ :, 1 ] )
            maxPos = minimum( posLims[ :, 2 ] )
            currentPos = get_prop.( agrf, nodeChain, :rank )
            srcPos, dstPos = get_prop.( agrf, edge, :rank )


            if length( nodeChain ) == 1
                potentialPos = [ minPos, maxPos, srcPos, dstPos,
                    floor( Int, ( srcPos + dstPos ) / 2 ) ]
                potentialWeights = ( srcPos - potentialPos ) .^ 2 +
                    ( dstPos - potentialPos ) .^ 2
                potentialWeights[ .!( minPos .<= potentialPos .<= maxPos ) ] =
                    Inf
                bestInd = findmin( potentialWeights )[ 2 ]
                bestPos = Int( potentialPos[ bestInd ] )

                if any( bestPos .!= currentPos )
                    isImproved = true
                    set_prop!.( agrf, nodeChain, :rank, bestPos )
                end  # if any( bestPos .!= currentPos )
            # If the highest lower limit is lower than the lowest upper
            #   limit, we can straighten the entire edge and move it closer
            #   to its anchor points.
            elseif minPos <= maxPos
                potentialPos = [ minPos, maxPos, srcPos, dstPos ]
                potentialWeights = sqrt.( abs.( srcPos - potentialPos ) ) +
                    sqrt.( abs.( dstPos - potentialPos ) )
                potentialWeights[ .!( minPos .<= potentialPos .<= maxPos ) ] =
                    Inf
                bestInd = findmin( potentialWeights )[ 2 ]
                bestPos = Int( potentialPos[ bestInd ] )

                if any( bestPos .!= currentPos )
                    isImproved = true
                    set_prop!.( agrf, nodeChain, :rank, bestPos )
                end  # if any( bestPos .!= currentPos )
            # Otherwise, make the best of it.
            else
            end  # if minPos <= maxPos
        end  # for ii in eachindex( longEdges )
    end  # while isImproved

    return

end  # straightenLongEdges!( agrf, dgrf )


function compactNodes!( agrf::MetaDiGraph, dgrf::MetaDiGraph,
    order::Vector{Vector{Int}} )::Void

    coords = get_prop.( agrf, vertices( dgrf ), :rank )
    minCoord = minimum( coords )
    maxCoord = maximum( coords )

    for rank in eachindex( order )
        orderRank = order[ rank ]

        for ii in eachindex( orderRank )
            node = orderRank[ ii ]
            cCoord = coords[ node ]
            loLim, hiLim = minCoord, maxCoord

            # Find the allowed region to shift node.
            if ii > 1
                loLim = coords[ orderRank[ ii - 1 ] ] + getMinSlack( agrf,
                    orderRank[ ii - 1 ], node )
            end  # if ii > 1

            if ii < length( orderRank )
                hiLim = coords[ orderRank[ ii + 1 ] ] - getMinSlack( agrf, node,
                    orderRank[ ii + 1 ] )
            end  # if ii < length( orderRank )

            if loLim < hiLim
                # println( "Node ", node, ": feasible range [", loLim, ", ",
                #     hiLim, "], ", cCoord, " now" )
                loWeight = hiWeight = cWeight = getWeightedDistance( dgrf,
                    coords )

                # See what happens if node is moved all the way to one end...
                if loLim < cCoord
                    coords[ node ] = Int( loLim )
                    # println( coords )
                    loWeight = getWeightedDistance( dgrf, coords )
                    # println( "Moving node to minimum coord gives weight: ",
                    #     loWeight )

                    if loWeight < cWeight
                        cWeight = loWeight
                    else
                        coords[ node ] = cCoord
                    end  # if loWeight < cWeight
                end  # if loLim < cCoord

                # ... and to the other end, and retains best solution.
                if hiLim > cCoord
                    coords[ node ] = Int( hiLim )
                    # println( coords )
                    hiWeight = getWeightedDistance( dgrf, coords )
                    # println( "Moving node to maximum coord gives weight: ",
                    #     hiWeight )

                    if hiWeight < cWeight
                        cWeight = hiWeight
                    else
                        coords[ node ] = cCoord
                    end  # if hiWeight < cWeight
                end  # if hiLim > cCoord
            end  # if loLim < hiLim
        end  # for ii in eachindex( orderRank )
    end  # for rank in eachindex( order )

    set_prop!.( agrf, vertices( dgrf ), :rank, coords )
    return

end  # compactNodes!( agrf, dgrf, order )


function adjustEdgeNodeRanks!( agrf::MetaDiGraph, nNodes::Int )::Void

    for node in (nNodes + 1):length( vertices( agrf ) )
        newRank = minimum( get_prop.( agrf, agrf.graph.fadjlist[ node ],
            :rank ) )
        set_prop!( agrf, node, :rank, newRank )
    end  # for node in (nNodes + 1):length( vertices( agrf ) )

    return

end  # adjustEdgeNodeRanks!( agrf, nNodes )


function centerEnds!( agrf::MetaDiGraph, dgrf::MetaDiGraph )::Void

    for node in vertices( dgrf )
        # If node has no successors, place it at median position of
        #   predecessors.
        if isempty( dgrf.graph.fadjlist[ node ] )
            preds = dgrf.graph.badjlist[ node ]
            newPos = ceil( Int, median( get_prop.( agrf, preds, :rank ),
                Weights( getEdgeWeight.( dgrf, preds, node ) ) ) )
            set_prop!( agrf, node, :rank, newPos )
        # If node has no predecessors, place it at median position of
        #   successors.
        elseif isempty( dgrf.graph.badjlist[ node ] )
            succs = dgrf.graph.fadjlist[ node ]
            newPos = floor( Int, median( get_prop.( agrf, succs, :rank ),
                Weights( getEdgeWeight.( dgrf, node, succs ) ) ) )
            set_prop!( agrf, node, :rank, newPos )
        end  # if isempty( dgrf.graph.fadjlist[ node ] )
    end  # for node in vertices( dgrf )

    return

end  # centerEnds!( agrf, dgrf )
