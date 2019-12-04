function plotGraph( mgrf::MetaDiGraph, nodeGap::Int = 20;
    plotName::String = "testPlot.svg", orientation::Symbol = :down,
    nodeLabel::Symbol = :none,
    nodeInfo::NTuple{4, Symbol} = ( :none, :none, :none, :none ) )::Void

    if nodeGap <= 0
        warn( "Value of minimum node separation must be > 0. Defaulting to 20." )
        nodeGap = 20
    end  # if nodeGap <= 0

    if !endswith( plotName, ".svg" )
        plotName = string( plotName, ".svg" )
    end  # if !endswith( plotName, ".svg" )

    plotName = joinpath( dirname( Base.source_path() ), plotName )

    if !ispath( dirname( plotName ) )
        info( "Creating required folder." )
        mkpath( dirname( plotName ) )
    end  # if !ispath( dirname( plotName ) )

    if orientation ∉ [ :up, :down, :left, :right ]
        warn( "Unknown graph orientation, defaulting to down orientation." )
        orientation = :down
    end  # if orientation ∉ [ :up, :down, :left, :right ]

    # Compute the size of the node boxes.
    nodeHeights, nodeWidths = computeNodeSizes!( mgrf, nodeLabel, nodeInfo )

    if orientation ∈ [ :right, :left ]
        nodeWidths, nodeHeights = nodeHeights, nodeWidths
    end  # if orientation ∈ [ :right, :left ]

    rankSep = maximum( nodeHeights )
    mult = orientation ∈ [ :up, :down ] ? 1.5 : 0.5
    rankSep = rankSep * mult > 70 ? Int( rankSep * ( 1 + mult ) ) : rankSep + 70

    # Compute the coordinates of every node.
    extComps, graphCompsNodes, width, height = computeNodeCoordinates( mgrf,
        nodeWidths, nodeGap, rankSep, orientation, nodeLabel )

    if orientation ∈ [ :right, :left ]
        width, height = height, width
    end  # if orientation ∈ [ :right, :left ]

    # Draw the graph.
    Drawing( width, height, :svg, plotName )
    background( "white" )

    for ii in eachindex( extComps )
        plotSubgraph( extComps[ ii ], graphCompsNodes[ ii ], nodeLabel,
            nodeInfo )
    end  # for ii in eachindex( extComps )

    finish()
    preview()

    return

end  # plotGraph( mgrf, nodeGap, plotName, orientation, nodeLabel )


function computeNodeSizes!( mgrf::MetaDiGraph, nodeLabel::Symbol,
    nodeInfo::NTuple{4, Symbol} )

    nVerts = length( vertices( mgrf ) )
    nodeWidths = Vector{Float64}( nVerts )
    nodeHeights = Vector{Float64}( nVerts )

    if nodeLabel === :none
        nodeWidths = fill( 30.0, nVerts )
        nodeHeights = fill( 30.0, nVerts )
    else
        Drawing( 0, 0, :svg )
        # XXX A drawing must be opened here so the required node sizes can be
        #   computed. The Luxor methods fontsize and textextents throw an error
        #   otherwise.

        for node in vertices( mgrf )
            label = has_prop( mgrf, node, nodeLabel ) ?
                get_prop( mgrf, node, nodeLabel ) : ""
            set_prop!( mgrf, node, nodeLabel, label )

            # Size of node text.
            fontsize( 14 )
            labelSize = textextents( label )
            labelWidth = floor( labelSize[ 3 ], -1 ) + 30.0

            # Size of extra information.
            fontsize( 9 )
            extraInfo = map( nInf -> has_prop( mgrf, node, nInf ) ?
                get_prop( mgrf, node, nInf ) : "", nodeInfo )
            extraInfoSize = textextents.( extraInfo )
            topInfoWidth = all( nodeInfo[ 1:2 ] .=== :none ) ? 0.0 : 40.0 +
                ceil( extraInfoSize[ 1 ][ 3 ] + extraInfoSize[ 2 ][ 3 ], -1 )
            bottomInfoWidth = all( nodeInfo[ 3:4 ] .=== :none ) ? 0.0 : 40.0 +
                ceil( extraInfoSize[ 3 ][ 3 ] + extraInfoSize[ 4 ][ 3 ], -1 )

            # Bounding box.
            nodeWidths[ node ] = max( labelWidth, topInfoWidth,
                bottomInfoWidth )
            nodeHeights[ node ] = floor( max( labelSize[ 4 ],
                - 2.0 * labelSize[ 2 ] ), -1 ) +
                ( all( isempty.( extraInfo ) ) ? 20.0 : 40.0 )
        end  # for node in vertices( dgrf )

        finish()
    end  # if nodeLabel === :none

    nodeWidths = ceil.( Int, nodeWidths )
    nodeHeights = ceil.( Int, nodeHeights )
    set_prop!.( mgrf, vertices( mgrf ), :xsize, nodeWidths )
    set_prop!.( mgrf, vertices( mgrf ), :ysize, nodeHeights )

    return nodeHeights, nodeWidths

end  # computeNodeSizes( mgrf, nodeLabel )


function computeNodeCoordinates( mgrf::MetaDiGraph, nodeWidths::Vector{Int},
    nodeGap::Int, rankSep::Int, orientation::Symbol, nodeLabel::Symbol )

    # Get all connected components of the graph.
    graphCompsNodes = weakly_connected_components( mgrf )
    sort!( graphCompsNodes, lt = ( a, b ) -> length( a ) < length( b ),
        rev = true )
    graphComps = map( compNodes -> induced_subgraph( mgrf, compNodes )[ 1 ],
        graphCompsNodes )

    extComps = Vector{MetaDiGraph}( length( graphComps ) )
    inrankOffset = 0
    rankSize = 0

    for ii in eachindex( extComps )
        nodeWidthsComp = nodeWidths[ graphCompsNodes[ ii ] ]
        dgrf = createSubgraph( graphComps[ ii ], nodeWidthsComp, nodeGap )
        inrankCoords, rankCoords, inrankOffset, rankSize =
            finaliseNodeCoords!( dgrf, nodeWidthsComp, nodeGap, rankSep,
                inrankOffset, rankSize, orientation )
        computeEdgePorts!( dgrf, orientation, nodeLabel )

        extComps[ ii ] = dgrf
    end  # for ii in eachindex( extComps )

    return extComps, graphCompsNodes, inrankOffset, rankSize

end  # computeNodeCoordinates( mgrf, nodeWidths, nodeHeights, nodeGap, rankSep,
     #   orientation, nodeLabel )


function createSubgraph( graphComp::MetaDiGraph,
    nodeWidths::Vector{Int}, nodeGap::Int )::MetaDiGraph

    removeCycles!( graphComp )
    dgrf = generateRanks( graphComp )
    order = orderNodes( dgrf )
    generateNodeCoordinates!( dgrf, order, nodeWidths, nodeGap )
    return dgrf

end  # createSubgraph( graphComp, nodeWidths )


function finaliseNodeCoords!( dgrf::MetaDiGraph, nodeWidths::Vector{Int},
    nodeGap::Int, rankSep::Int, inrankOffset::Int,
    rankSize::Int, orientation::Symbol )

    # Coordinates in every rank.
    inrankCoords = get_prop.( dgrf, vertices( dgrf ), :coord )
    minIRC = inrankCoords - map( node -> has_prop( dgrf, node, :dummy ) ? 0 :
        Int( nodeWidths[ node ] / 2 ), vertices( dgrf ) )
    inrankCoords += nodeGap - minimum( minIRC )
    maxIRC = inrankCoords + map( node -> has_prop( dgrf, node, :dummy ) ? 0 :
        Int( nodeWidths[ node ] / 2 ), vertices( dgrf ) )
    inrankCoords += inrankOffset
    inrankOffset += maximum( maxIRC ) + nodeGap

    # Coordinates of ranks.
    rankCoords = get_prop.( dgrf, vertices( dgrf ), :rank ) - 1

    if orientation ∈ [ :up, :left ]
        rankCoords = maximum( rankCoords ) - rankCoords
    end  # if orientation ∈ [ :up, :left ]

    rankCoords = rankCoords * rankSep + Int( ( rankSep - 70 ) / 2 ) + nodeGap

    if maximum( rankCoords ) > rankSize
        rankSize = maximum( rankCoords ) + Int( ( rankSep - 70 ) / 2 ) + nodeGap
    end  # if maximum( rankCoords ) > ysize

    # Attaching coordinates to graph.
    isVertical = orientation ∈ [ :down, :up ]

    for node in vertices( dgrf )
        set_prop!.( dgrf, node, isVertical ? [ :xcoord, :ycoord ] :
            [ :ycoord, :xcoord ],
            [ inrankCoords[ node ], rankCoords[ node ] ] )
    end  # for node in vertices( dgrf )

    return inrankCoords, rankCoords, inrankOffset, rankSize

end  # finaliseNodeCoords!( dgrf, nodeGap, rankSep, inrankOffset, orientation )


function computeEdgePorts!( dgrf::MetaDiGraph, orientation::Symbol,
    nodeLabel::Symbol )::Void

    nodePorts = ( nodeLabel !== :none ) .&
        has_prop.( dgrf, vertices( dgrf ), nodeLabel ) .&
        .!has_prop.( dgrf, vertices( dgrf ), :dummy )

    for edge in edges( dgrf )
        edgeDir = ( [ 1, -1 ]' *
            get_prop.( dgrf, [ edge.src, edge.dst ], :ycoord ) ) /
            ( [ 1, -1 ]' * get_prop.( dgrf, [ edge.src, edge.dst ], :xcoord ) )

        if nodePorts[ edge.src ]
        else
            set_prop!( dgrf, edge, :srcPort, ( 0, 0 ) )
        end  # if nodePorts[ edge.src ]

        if nodePorts[ edge.dst ]
        else
            set_prop!( dgrf, edge, :dstPort, ( 0, 0 ) )
        end  # if nodePorts[ edge.dst ]
    end  # for edge in edges( dgrf )

    return

end  # computeEdgePorts!( dgrf, orientation, nodeLabel )


function plotSubgraph( dgrf::MetaDiGraph, realNodes::Vector{Int},
    nodeLabel::Symbol, nodeInfo::NTuple{4, Symbol } )::Void

    drawNodes( dgrf, realNodes, nodeLabel, nodeInfo )
    drawEdges( dgrf, nodeLabel )
    return

end  # plotSubgraph( dgrf, realNodes, nodeLabel )


function drawNodes( dgrf::MetaDiGraph, realNodes::Vector{Int},
    nodeLabel::Symbol, nodeInfo::NTuple{4, Symbol} )::Void

    setline( 0.8mm )
    infoPosMods = [ -1 -1; 1 -1; -1 1; 1 1 ]
    hPos = [ :left, :right, :left, :right ]

    for node in filter( node -> .!has_prop( dgrf, node, :dummy ),
        vertices( dgrf ) )
        nodeCentre = Point( get_prop( dgrf, node, :xcoord ),
            get_prop( dgrf, node, :ycoord ) )
        label = nodeLabel === :none ? "" : get_prop( dgrf, node, nodeLabel )

        if isempty( label )
            sethue( "white" )
            circle( nodeCentre, 15, :fill )
            sethue( "black" )
            circle( nodeCentre, 15, :stroke )
        else
            # Box.
            sethue( "white" )
            xsize = get_prop( dgrf, node, :xsize )
            ysize = get_prop( dgrf, node, :ysize )
            box( nodeCentre, xsize, ysize, :fill )

            sethue( "black" )
            box( nodeCentre, xsize, ysize, :stroke )

            # Extra node text.
            extraInfo = map( nInf -> has_prop( dgrf, node, nInf ) ?
                get_prop( dgrf, node, nInf ) : "", nodeInfo )
            fontsize( 9 )
            xsize -= 10
            ysize -= 10
            xsize /= 2
            ysize /= 2

            for ii in 1:4
                anchorPoint = nodeCentre + ( xsize * infoPosMods[ ii, 1 ],
                    ysize * infoPosMods[ ii, 2 ] + ( ii < 3 ? 9 : 0 ) )
                text( extraInfo[ ii ], anchorPoint, halign = hPos[ ii ] )
            end  # for ii in 1:4
        end  # if ( nodeLabel === :none ) || ...

        # Node label.
        fontsize( 14 )
        label = isempty( label ) ? string( realNodes[ node ] ) : label
        text( label, nodeCentre - ( textextents( label )[ 1 ], 0.0 ),
            halign = :center, valign = :middle )
    end  # for node in realNodes

    return

end  # drawNodes( dgrf, realNodes, nodeLabel )


function drawEdges( dgrf::MetaDiGraph, nodeLabel::Symbol )::Void

    realNodes = filter( node -> !has_prop( dgrf, node, :dummy ),
        vertices( dgrf ) )

    setlinecap( "round" )

    for edge in edges( dgrf )
        setline( 0.5mm )
        sethue( "black" )
        startPoint, endPoint = generateLinePoints( edge, dgrf, nodeLabel )
        drawEdge( edge, startPoint, endPoint, dgrf )
    end  # for edge in edges( dgrf )

    return

end  # drawEdges( dgrf, nodeLabel )


function generateLinePoints( edge::Edge, dgrf::MetaDiGraph, nodeLabel::Symbol )

    # Get node centre points.
    startCentre = Point( get_prop( dgrf, edge.src, :xcoord ),
        get_prop( dgrf, edge.src, :ycoord ) )
    endCentre = Point( get_prop( dgrf, edge.dst, :xcoord ),
        get_prop( dgrf, edge.dst, :ycoord ) )

    # Get edge start and end points.  XXX to change for edge ports.
    startPoint, endPoint = startCentre, endCentre
    direction = getDirection( startPoint, endPoint )

    # If source node is a real node, adjust start point of edge.
    if !has_prop( dgrf, edge.src, :dummy )
        if ( nodeLabel === :none ) ||
            isempty( get_prop( dgrf, edge.src, nodeLabel ) )
            startPoint += 15.0 * direction
        else
            edges = 0.5 * get_prop.( dgrf, edge.src, [ :xsize, :ysize ] ) *
                [ -1, 1 ]'
            edges += [ startCentre.x - startPoint.x,
                startCentre.y - startPoint.y ] * [ 1, 1 ]'
            edges = edges ./ ( [ direction.x, direction.y ] * [ 1, 1 ]' )
            edges[ edges .< 0 ] = +Inf
            factor = findmin( edges )[ 1 ]
            startPoint += direction * factor
        end  # if ( nodeLabel === :none ) || ...
    end  # if !has_prop( dgrf, edge.src, :dummy )

    # If destination node is a real node, adjust end point of edge.
    if !has_prop( dgrf, edge.dst, :dummy )
        if ( nodeLabel === :none ) ||
            isempty( get_prop( dgrf, edge.dst, nodeLabel ) )
            endPoint -= 15.0 * direction
        else
            edges = 0.5 * get_prop.( dgrf, edge.dst, [ :xsize, :ysize ] ) *
                [ -1, 1 ]'
            edges += [ endCentre.x - endPoint.x,
                endCentre.y - endPoint.y ] * [ 1, 1 ]'
            edges = edges ./ ( [ direction.x, direction.y ] * [ 1, 1 ]' )
            edges[ edges .> 0 ] = -Inf
            factor = findmax( edges )[ 1 ]
            endPoint += direction * factor
        end  # if ( nodeLabel === :none ) || ...
    end  # if !has_prop( dgrf, edge.dst, :dummy )

    return startPoint, endPoint

end  # generateLinePoints( edge, dgrf, nodeLabel )


function getDirection( startPoint::Point, endPoint::Point )::Point

    direction = endPoint - startPoint
    edgeLen = sqrt( direction.x^2 + direction.y^2 )
    return direction / edgeLen

end  # getDirection( startPoint, endPoint )


function drawEdge( edge::Edge, startPoint::Point, endPoint::Point,
    dgrf::MetaDiGraph )::Void

    if !has_prop( dgrf, edge, :arrow )
        if !has_prop( dgrf, edge.dst, :dummy )
            arrow( startPoint, endPoint, linewidth = 0.5mm )
        else
            line( startPoint, endPoint, :stroke )
        end  # if edge.dst ∈ realNodes
    elseif get_prop( dgrf, edge, :arrow ) == "reverse"
        if !has_prop( dgrf, edge.src, :dummy )
            arrow( endPoint, startPoint, linewidth = 0.5mm )
        else
            line( endPoint, startPoint, :stroke )
        end  # if edge.src ∈ realNodes
    else
        line( startPoint, endPoint, :stroke )
    end  # if !has_prop( dgrf, edge, :arrow )

    return

end  # drawEdge( edge, startPoint, endPoint, dgrf )


plotGraph( grf::SimpleDiGraph, nodeGap::Int = 20;
    plotName::String = "testPlot.svg", orientation::Symbol = :down,
    nodeLabel::Symbol = :none )::Void =
    plotGraph( MetaDiGraph( grf ), nodeGap, plotName = plotName,
        orientation = orientation, nodeLabel = nodeLabel )


LightGraphs.weakly_connected_components( mgrf::MetaDiGraph )::Vector{Vector{Int}} =
    weakly_connected_components( mgrf.graph )
