export  generateGraphNodeRanks!

function generateGraphNodeRanks!( grf::MetaDiGraph,
    isInit::Bool = true )::Nothing

    fTree = generateFeasibleTree( grf, isInit )
    treeRanks = get_prop.( Ref( fTree ), vertices( fTree ), :rank )

    while !( (leaveEdge = findLeaveEdge( fTree )) isa Nothing )
        # Get a new edge to replace the bad edge with.
        headNodes = breakFeasibleTree( SimpleGraph( fTree.graph ), leaveEdge )
        validEdges = filter( edge -> ( edge.src ∈ headNodes ) &&
            ( edge.dst ∉ headNodes ), collect( edges( grf ) ) )
        relativeEdgeSlacks = map( validEdges ) do edge
            return ( treeRanks[ edge.dst ] - treeRanks[ edge.src ] ) -
                getEdgeMinSlack( grf, edge )
        end  # map( validEdges ) do edge
        enterEdge = rand( validEdges[ relativeEdgeSlacks .==
            minimum( relativeEdgeSlacks ) ] )
        
        # Exchange the edges.
        clear_props!( fTree, leaveEdge )
        rem_edge!( fTree, leaveEdge )
        add_edge!( fTree, enterEdge )

        for prop in keys( props( grf, enterEdge ) )
            add_prop!( fTree, enterEdge, prop,
                get_prop( grf, enterEdge, prop ) )
        end  # for prop in keys( props( grf, enterEdge ) )

        generateCutValues!( fTree, grf )

        # Adjust the ranks of the nodes in the head of the tree.
        treeRanks[ headNodes ] .-= treeRanks[ enterEdge.src ] +
            getEdgeMinSlack( grf, enterEdge ) - treeRanks[ enterEdge.dst ]
    end  # while !isa( edge = findLeaveEdge( fTree ), Nothing )

    treeRanks .-= minimum( treeRanks )
    set_prop!.( Ref( grf ), vertices( grf ), :rank, treeRanks )
    return

end  # generateGraphNodeRanks!( grf, isInit )


function generateFeasibleTree( grf::MetaDiGraph, isInit::Bool )

    # Generate initial node ranks if needed.
    if isInit || !all( has_prop.( Ref( grf ), vertices( grf ), :rank ) )
        generateInitialNodeRanks!( grf )
    end  # if isInit || ...

    graphEdges = collect( edges( grf ) )
    treeRanks = zeros( Float64, nv( grf ) )
    fTree = MetaDiGraph( nv( grf ) )

    # Add a random start node to the feasible tree.
    inTree = [ rand( vertices( grf ) ) ]

    # In each step, add an edge from/to a non-tree node which has minimal rank
    #   difference (slack) in the original graph.
    while length( inTree ) < nv( grf )
        # Find all edges incident on the current feasible tree.
        validEdges = filter( edge -> xor( edge.src ∈ inTree,
            edge.dst ∈ inTree ), graphEdges )
        edgeSlacks = map( edge -> get_prop( grf, edge.dst, :rank ) -
            get_prop( grf, edge.src, :rank ), validEdges )

        # Get all tight edges among incident edges.
        isTight = edgeSlacks .== getEdgeMinSlack.( Ref( grf ), validEdges )
        !any(isTight) && (isTight = edgeSlacks .== minimum(edgeSlacks))  # Insurance!
        tightEdges = validEdges[ isTight ]
        tightSlacks = edgeSlacks[ isTight ]
        
        # If there are no candidates (bad initial ranks), re-initialise the 
        #   node ranks and restart.
        # if isempty( tightEdges )
        #     inTree = [ inTree[ 1 ] ]
        #     generateInitialNodeRanks!( grf )
        #     continue
        # end  # if isempty( tightEdges )

        # Select tight edge with minimal slack.
        minSlack = minimum( tightSlacks )
        newEdge = rand( tightEdges[ tightSlacks .== minSlack ] )

        # Find the new node and its rank in the tree.
        isNewDst = newEdge.src ∈ inTree
        treeNode = getfield( newEdge, isNewDst ? :src : :dst )
        newNode = getfield( newEdge, isNewDst ? :dst : :src )
        treeRanks[ newNode ] = treeRanks[ treeNode ] +
            getEdgeMinSlack( grf, newEdge ) * ( isNewDst ? 1 : -1 )

        # Add the new edge.
        add_edge!( fTree, newEdge )
        setEdgeWeight!( fTree, newEdge, getEdgeWeight( grf, newEdge ) )
        setEdgeMinSlack!( fTree, newEdge, getEdgeMinSlack( grf, newEdge ) )
        push!( inTree, newNode )
    end  # for ii in 1:( nv( grf ) - 1 )

    # Set all node ranks in the feasible tree.
    treeRanks .-= minimum( treeRanks )
    set_prop!.( Ref( fTree ), vertices( fTree ), :rank, treeRanks )

    # Generate cut values for each edge in the feasible tree.
    generateCutValues!( fTree, grf )
    return fTree

end  # generateFeasibleTree( grf, isInit )


function generateInitialNodeRanks!( grf::MetaDiGraph )

    ranks = - ones( Float64, nv( grf ) )
    unprocessedClearNodes = Queue{Int}()

    # In each step, get the list of nodes that no unprocessed in neighbours,
    #   and assign them the lowest possible rank which respects the minimum
    #   slacks for each incoming edge.
    while any( ranks .< 0 )
        getClearNodes( grf, ranks, unprocessedClearNodes )
        node = dequeue!( unprocessedClearNodes )

        minRanks = map( inneighbors( grf, node ) ) do srcNode
            return ranks[ srcNode ] + getEdgeMinSlack( grf, srcNode, node )
        end  # map( inneighbors( grf, node ) ) do srcNode

        ranks[ node ] = isempty( minRanks ) ? 0 : maximum( minRanks )
    end  # while any( ranks .< 0 )

    set_prop!.( Ref( grf ), vertices( grf ), :rank, ranks )

end  # generateInitialNodeRanks!( grf )


function getClearNodes( grf::MetaDiGraph, ranks::Vector{Float64},
    unprocessed::Queue{Int} )

    for node in filter( node -> ranks[ node ] < 0, vertices( grf ) )
        if ( node ∉ unprocessed ) &&
            all( ranks[ inneighbors( grf, node ) ] .>= 0 )
            enqueue!( unprocessed, node )
        end  # if ( node ∉ unprocessed ) &&
    end  # for node in filter( node -> ranks[ node ] < 0, vertices( grf ) )

end  # getClearNodes( grf, ranks, unprocessed )


function generateCutValues!( fTree::MetaDiGraph, grf::MetaDiGraph )

    uTree = SimpleGraph( fTree.graph )
    grfEdges = collect( edges( grf ) )

    for edge in edges( fTree )
        headNodes = breakFeasibleTree( uTree, edge )
        cutEdges = filter( tmpEdge -> xor( tmpEdge.src ∈ headNodes,
            tmpEdge.dst ∈ headNodes ), grfEdges )
        isPosEdge = map( tmpEdge -> tmpEdge.dst ∈ headNodes, cutEdges )
        cutVal = sum( getEdgeWeight.( Ref( grf ), cutEdges[ isPosEdge ] ) )

        if !all( isPosEdge )
            cutVal -= sum( getEdgeWeight.( Ref( grf ),
                cutEdges[ .!isPosEdge ] ) )
        end  # if !all( isPosEdge )

        set_prop!( fTree, edge, :cut, cutVal )
    end  # for edge in edges( fTree )

end  # generateCutValues!( fTree::MetaDiGraph, grf::MetaDiGraph )


function breakFeasibleTree( uTree::SimpleGraph, edge::Edge )

    ii = 1
    # We have to remember that the edge is defined as tail-to-head (src-to-dst).
    headNodes = [ edge.dst ]
    
    while ii <= length( headNodes )
        newNodes = neighbors( uTree, headNodes[ ii ] )

        if ii == 1
            append!( headNodes, newNodes[ newNodes .!= edge.src ] )
        else
            append!( headNodes, filter( node -> node ∉ headNodes, newNodes ) )
        end  # if ii == 1

        ii += 1
    end  # while ii <= length( headNodes )

    return headNodes

end  # breakFeasibleTree( uTree, edge )


function findLeaveEdge( fTree::MetaDiGraph )

    negCutEdges = filter( edge -> get_prop( fTree, edge, :cut ) < 0,
        collect( edges( fTree ) ) )
    return isempty( negCutEdges ) ? nothing : rand( negCutEdges )

end  # findLeaveEdge( fTree )

# =============

function generateRanks( mgrf::MetaDiGraph, addDummy::Bool = true,
    isInit::Bool = false )::MetaDiGraph

    # Initialise ranked graph.
    rgrf = nothing

    # Generate initial feasible tree.
    fTree = generateFeasibleTree( mgrf, isInit )
    ranks = get_prop.( fTree, vertices( fTree ), :rank )
    treeEdges = collect( edges( fTree ) )
    grfEdges = collect( edges( mgrf ) )
    isOkay = true

    while any( get_prop.( fTree, treeEdges, :cut ) .< 0 )
        # Select a tree edge which has a negative cut value.
        negCutInds = find( get_prop.( fTree, treeEdges, :cut ) .< 0 )
        leaveEdge = treeEdges[ rand( negCutInds ) ]

        # Split the tree up in the head part and the tail part.
        headInds = findHeadNodes( fTree, leaveEdge )
        headNodes = vertices( fTree )[ headInds ]
        tailNodes = vertices( fTree )[ .!headInds ]

        # For all edges going from the head part to the tail part, compute the
        #   slack and choose an edge with minimal slack to replace the negative
        #   cut edge with.
        edgeSlacks = map( grfEdges ) do edge
            if has_edge( fTree, edge )
                return typemax( Int )
            end  # if has_edge( fTree, edge )

            if ( edge.src ∈ headNodes ) && ( edge.dst ∈ tailNodes )
                return abs( ranks[ edge.src ] - ranks[ edge.dst ] )
            end  # if ( edge.src ∈ inTree ) && ...

            return return typemax( Int )
        end  # map( grfEdges ) do edge

        enterEdge = grfEdges[ rand( find( edgeSlacks .==
            minimum( edgeSlacks ) ) ) ]
        add_edge!( fTree, enterEdge )
        rem_edge!( fTree, leaveEdge )
        treeEdges = collect( edges( fTree ) )

        # Recalculate the ranks of the nodes and the cut values of the edges.
        ranks[ tailNodes ] += ranks[ enterEdge.src ] - ranks[ enterEdge.dst ] +
            getEdgeMinSlack( mgrf, enterEdge )
        set_prop!.( fTree, tailNodes, :rank, ranks[ tailNodes ] )
        generateCutValues!( fTree, mgrf )

        if any( edge -> ranks[ edge.dst ] - ranks[ edge.src ] <
            getEdgeMinSlack( mgrf, edge ), edges( fTree ) )
            edgeInds = find( map( edge ->
                ranks[ edge.dst ] - ranks[ edge.src ] <
                getEdgeMinSlack( mgrf, edge ), treeEdges ) )
            fEdges = treeEdges[ edgeInds ]
            warn( "Problem!" )
            foreach( edge -> println( edge, ": Slack = ",
                ranks[ edge.dst ] - ranks[ edge.src ], " (min ",
                getEdgeMinSlack( mgrf, edge ), ")" ), fEdges )
        end  # if( any( edge -> ...
    end  # while any( get_prop.( fTree, treeEdges, :cut ) .< 0 )

    # Make sure ALL slack constraints are respected! For some reason, this is a
    #   necessary step.
    validateSlacks( mgrf, ranks, grfEdges )

    ranks -= minimum( ranks ) - 1

    if addDummy
        rgrf = addDummyNodes( mgrf, ranks )
    else
        rgrf = deepcopy( mgrf )
        set_prop!.( rgrf, vertices( rgrf ), :rank, ranks )
    end  # if addDummy

    return rgrf

end  # generateRanks( mgrf, addDummy, isInit )

generateRanks( grf::SimpleDiGraph, addDummy::Bool = true ) =
    generateRanks( MetaDiGraph( grf ), addDummy )


"""
```
generateFeasibleTree( grf::SimpleDiGraph )
```
This function generates a feasible tree based on the directed graph 'grf',
assuming edge weights of 1.0 for each edge.

This function returns a `MetaDiGraph`, the feasible tree with the cut value of
each edge stored in the property `:cut`, and the rank of each node stored in the
property `:rank`.
"""
generateFeasibleTree( grf::SimpleDiGraph )::MetaDiGraph =
    generateFeasibleTree( MetaDiGraph( grf ), false )


"""
```
generateInitRanks( mgrf::SimpleDiGraph )
```
This function generates initial ranks for the directed graph `mgrf`, assuming
that all edges have minimum slack 1.

This function returns a `Vector{Int}` containing the ranks for each node of the
graph.
"""
generateInitRanks( grf::SimpleDiGraph )::Vector{Int} =
    generateInitRanks( MetaDiGraph( grf ) )


"""
```
generateCutValues!( fTree::MetaDiGraph, grf::SimpleDiGraph )::Void
```
This function generates the cut values for the feasible tree `fTree` based on
the graph `grf`, and adds them as property `:cut` to the tree's edges.

This function returns `nothing`.
"""
generateCutValues!( fTree::MetaDiGraph, grf::SimpleDiGraph )::Void =
    generateCutValues!( fTree, MetaDiGraph( grf ) )


function addDummyNodes( mgrf::MetaDiGraph, ranks::Vector{Int} )::MetaDiGraph

    dgrf = deepcopy( mgrf )
    set_prop!.( dgrf, vertices( mgrf ), :rank, ranks )
    wt = weightfield( dgrf )

    # For non-tight edges, add the required number of dummy nodes and generate a
    #   chain of tight edges.
    for edge in edges( mgrf )
        nToAdd = ranks[ edge.dst ] - ranks[ edge.src ] - 1

        if nToAdd > 0
            nVerts = length( vertices( dgrf ) )
            add_vertices!( dgrf, nToAdd )
            tmpEdge = Edge( nVerts + nToAdd, edge.dst )
            add_edge!( dgrf, tmpEdge )

            for prop in keys( props( mgrf, edge ) )
                set_prop!( dgrf, tmpEdge, prop, get_prop( mgrf, edge, prop ) )
            end  # for prop in props( mgrf, edge )

            setEdgeWeight!( dgrf, tmpEdge, 3.0 * getEdgeWeight( mgrf, edge ) )

            for ii in 1:nToAdd
                jj = nVerts + ii
                set_prop!( dgrf, jj, :rank, ranks[ edge.src ] + ii )
                set_prop!( dgrf, jj, :dummy, true )
                set_prop!( dgrf, jj, :edge, ( edge.src, edge.dst ) )
                tmpEdge = Edge( ii == 1 ? edge.src : jj - 1, jj )
                add_edge!( dgrf, tmpEdge )

                for prop in keys( props( mgrf, edge ) )
                    set_prop!( dgrf, tmpEdge, prop,
                        get_prop( mgrf, edge, prop ) )
                end  # for prop in props( mgrf, edge )

                setEdgeWeight!( dgrf, tmpEdge, ( tmpEdge.src == edge.src ? 3.0 :
                    27.0 ) * getEdgeWeight( mgrf, edge ) )
            end  # for ii in 1:nToAdd

            rem_edge!( dgrf, edge )
            # set_prop!( dgrf, edge, :tight, false )
        elseif nToAdd < 0
            warn( "Something fishy's going on." )
        end  # if nToAdd > 0
    end  # for edge in edges( mgrf )

    return dgrf

end  # addDummyNodes( mgrf, ranks{Int} )


function validateSlacks( mgrf::MetaDiGraph, ranks::Vector{Int},
    grfEdges::Vector{Edge{Int}} )::Void

    while any( edge -> ranks[ edge.dst ] - ranks[ edge.src ] <
        getEdgeMinSlack( mgrf, edge ), grfEdges )
        slDefs = getEdgeMinSlack.( mgrf, grfEdges ) .-
            map( edge -> ranks[ edge.dst ] - ranks[ edge.src ], grfEdges )
        maxDef = maximum( slDefs )
        maxDefEdge = grfEdges[ findfirst( slDefs .== maxDef ) ]
        ranks[ maxDefEdge.dst ] += maxDef
        println( "Adjusted rank of node ", maxDefEdge.dst, " by ", maxDef )
    end  # while any( edge -> ...

    return

end  # validateSlacks( mgrf, ranks )