export  setEdgeWeight!,
        setEdgeMinSlack!


function removeGraphCycles!( grf::MetaDiGraph )

    doubleEdges = Vector{Tuple{Edge, Dict{Symbol, Any}}}()

    for edge in filter( edge -> edge.src < edge.dst, collect( edges( grf ) ) )
        src = edge.src
        dst = edge.dst

        if has_edge( grf, dst, src )
            rn = rand()
            baseEdge = rn > 0.5 ? Edge( dst, src ) : Edge( src, dst )
            revEdge = rn > 0.5 ? Edge( src, dst ) : Edge( dst, src )
            push!( doubleEdges, ( baseEdge, props( grf, baseEdge ) ) )
            set_prop!( grf, revEdge, :arrow, "both" )
            setEdgeWeight!( grf, revEdge, getEdgeWeight( grf, baseEdge ) +
                getEdgeWeight( grf, revEdge ) )
            rem_edge!( grf, baseEdge )
        end  # if has_edge( grf, dst, src )
    end  # for edge in edges( grf )

    flippedEdges = Vector{Edge}()

    # Reversing one edge of a cycle, until the graph has no more cycles left.
    while is_cyclic( grf )
        grfCycle = simplecycles_iter( grf.graph, 1 )[ 1 ]
        startPoint = rand( 1:length( grfCycle ) )
        endPoint = startPoint == 1 ? length( grfCycle ) : startPoint - 1
        startPoint = grfCycle[ startPoint ]
        endPoint = grfCycle[ endPoint ]
        edge = Edge( startPoint, endPoint )
        revEdge = Edge( endPoint, startPoint )
        add_edge!( grf, edge )

        if has_prop( grf, revEdge, :arrow ) &&
            ( get_prop( grf, revEdge, :arrow ) == "reverse" )
            rem_prop!( grf, revEdge, :arrow )
        else
            set_prop!( grf, edge, :arrow, "reverse" )
        end  # if has_prop( grf, revEdge, :arrow ) &&

        for prop in keys( props( grf, revEdge ) )
            set_prop!( grf, edge, prop, get_prop( grf, revEdge, prop ) )
        end  # for prop in keys( props( grf, edge ) )

        rem_edge!( grf, revEdge )

        if revEdge ∈ flippedEdges
            deleteat!( flippedEdges, findfirst( map( e -> revEdge == e,
                flippedEdges ) ) )
        else
            push!( flippedEdges, edge )
        end  # if revEdge ∈ flippedEdges
    end  # while is_cyclic( grf )

    return doubleEdges, flippedEdges, grf

end  # removeGraphCycles!( grf::MetaDiGraph )

removeGraphCycles!( grf::SimpleDiGraph ) =
    removeGraphCycles!( MetaDiGraph( grf ) )


function getEdgeWeight( grf::MetaDiGraph, edge::Edge )

    if !has_edge( grf, edge )
        return 0.0
    end  # if !has_edge( grf, edge )

    if !has_prop( grf, edge, weightfield( grf ) )
        return grf.defaultweight
    end  # if !has_prop( grf, edge, weightfield( grf ) )

    return get_prop( grf, edge, weightfield( grf ) )

end  # getEdgeWeight( grf, edge )

getEdgeWeight( grf::MetaDiGraph, src::Int, dst::Int ) =
    getEdgeWeight( grf, Edge( src, dst ) )
getEdgeWeight( grf::MetaDiGraph, edge::Tuple{Int, Int} ) =
    getEdgeWeight( grf, Edge( edge ) )


function setEdgeWeight!( grf::MetaDiGraph, edge::Edge, weight::Real )

    if !has_edge( grf, edge ) || ( weight < 0.0 )
        return
    end  # if !has_edge( grf, edge )

    set_prop!( grf, edge, weightfield( grf ), Float64( weight ) )
    return

end  # setEdgeWeight!( grf, edge, weight )

setEdgeWeight!( grf::MetaDiGraph, src::Int, dst::Int, weight::Real ) =
    setEdgeWeight!( grf, Edge( src, dst ),  weight )
setEdgeWeight!( grf::MetaDiGraph, edge::Tuple{Int, Int}, weight::Real ) =
    setEdgeWeight!( grf, Edge( edge ), weight )


function getEdgeSlack( grf::MetaDiGraph, edge::Edge )

    if !( has_prop( grf, edge.src, :rank ) && has_prop( grf, edge.dst, :rank ) )
        return NaN
    end  # if !has_edge( grf, edge )

    return get_prop( grf, edge.dst, :rank ) - get_prop( grf, edge.src, :rank )

end  # getEdgeSlack( grf::MetaDiGraph, edge::Edge )

getEdgeSlack( grf::MetaDiGraph, src::Int, dst::Int ) =
    getEdgeSlack( grf, Edge( src, dst ) )
getEdgeSlack( grf::MetaDiGraph, edge::Tuple{Int, Int} ) =
    getEdgeSlack( grf, Edge( edge ) )


function getEdgeMinSlack( grf::MetaDiGraph, edge::Edge )

    if !has_edge( grf, edge )
        return 0
    end  # if !has_edge( grf, edge )

    return has_prop( grf, edge, :minSlack ) ?
        get_prop( grf, edge, :minSlack ) : 1

end  # getEdgeMinSlack( grf::MetaDiGraph, edge::Edge )

getEdgeMinSlack( grf::MetaDiGraph, src::Int, dst::Int ) =
    getEdgeMinSlack( grf, Edge( src, dst ) )
getEdgeMinSlack( grf::MetaDiGraph, edge::Tuple{Int, Int} ) =
    getEdgeMinSlack( grf, Edge( edge ) )


function setEdgeMinSlack!( grf::MetaDiGraph, edge::Edge, minSlack::Real )

    if !has_edge( grf, edge ) || ( minSlack < 0.0 )
        return
    end  # if !has_edge( grf, edge )

    set_prop!( grf, edge, :minSlack, Float64( minSlack ) )
    return

end  # setEdgeMinSlack!( grf, edge, minSlack )

setEdgeMinSlack!( grf::MetaDiGraph, src::Int, dst::Int, minSlack::Real ) =
    setEdgeMinSlack!( grf, Edge( src, dst ),  minSlack )
setEdgeMinSlack!( grf::MetaDiGraph, edge::Tuple{Int, Int}, minSlack::Real ) =
    setEdgeMinSlack!( grf, Edge( edge ), minSlack )