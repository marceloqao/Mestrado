

# get the list of features from a graph (igraph object)
# - g: a graph
features_from_graph = function(g, weighted=TRUE)
{
    # to avoid errors in functions with same name (e.g., degree)
    #unloadNamespace('bnlearn')

    # the features
    feats = list()

    if (weighted == TRUE)
    {
        type = 'weighted'
    }
    else
    {
        type = 'undirected'
    }

    # TODO: check if it is necessary some condition here
    
    # handling the case of 
    if (length(E(g)) >= 2)
    {
        # computing the features vector
        feats$vertices            = length(V(g))
        feats$edges               = length(E(g))
        if (weighted == TRUE)
        {
            feats$weight.avg        = mean(E(g)$weight)
            feats$weight.probloop   = sum(E(g)$weight[which_loop(g)])
        }
        else
        {
            feats$weight.avg        = 0
            feats$weight.probloop   = 0
        }
        feats$degree.avg          = mean(igraph::degree(g))
        feats$degree.max          = max(igraph::degree(g))
        feats$degree.min          = min(igraph::degree(g))
        feats$density             = edge_density(g, loops=TRUE)
        feats$distance.avg        = mean_distance(g, directed=TRUE)
        feats$diameter            = diameter(g)
        feats$transitivity.avg    = mean(transitivity(g, type=type), 
                                    na.rm=TRUE)
        feats$betweeness.edge.avg = mean(edge_betweenness(g))
        feats$betweeness.vert.avg = mean(betweenness(g))
        feats$closeness           = mean(closeness(g, mode='out'))
        feats$assortativity       = assortativity_degree(g)
        feats$reciprocity         = reciprocity(g, ignore.loops=FALSE)

        # adding more features

        # degree
        feats$degree.shannon = shannon_entropy(degree_distribution(g),
                                normalized=T)
        feats$degree.complexity = complexity(degree_distribution(g))
        feats$degree.fisher = fisher_information(degree_distribution(g))
        
        # transitivity (clustering coefficient)
        transitivity.dist = transitivity(g, type=type)
        transitivity.dist = transitivity.dist[!is.na(transitivity.dist)]
        if (sum(transitivity.dist) != 0)
        {
            transitivity.dist = transitivity.dist[!is.infinite(
                                 transitivity.dist)]
            transitivity.dist = transitivity.dist/sum(transitivity.dist)
            feats$transitivity.sha = shannon_entropy(transitivity.dist,
                                        normalized=T)
            feats$transitivity.com = complexity(transitivity.dist)
            feats$transitivity.fis = fisher_information(transitivity.dist)
        }
        else
        {
            feats$transitivity.sha = 0
            feats$transitivity.com = 0
            feats$transitivity.fis = 0
        }

        # edge betweenness
        edge_betweenness.dist     = edge_betweenness(g)
        if (sum(edge_betweenness.dist) != 0)
        {
            edge_betweenness.dist = edge_betweenness.dist[!is.na(
                                        edge_betweenness.dist)]
            edge_betweenness.dist = edge_betweenness.dist[!is.infinite(
                                        edge_betweenness.dist)]
            edge_betweenness.dist = edge_betweenness.dist/sum(
                                        edge_betweenness.dist)
            feats$betweeness.edge.sha = shannon_entropy(
                                            edge_betweenness.dist,
                                            normalized=T)
            feats$betweeness.edge.com = complexity(edge_betweenness.dist)
            feats$betweeness.edge.fis = fisher_information(
                                            edge_betweenness.dist)
        }
        else
        {
            feats$betweeness.edge.sha = 0
            feats$betweeness.edge.com = 0
            feats$betweeness.edge.fis = 0
        }

        # vertex betweenness
        betweenness.dist = betweenness(g)
        if (sum(betweenness.dist) != 0)
        {
            betweenness.dist = betweenness.dist[!is.na(betweenness.dist)]
            betweenness.dist = betweenness.dist[!is.infinite(
                                betweenness.dist)]
            betweenness.dist = betweenness.dist/sum(betweenness.dist)
            feats$betweeness.vert.sha = shannon_entropy(betweenness.dist,
                                        normalized=T)
            feats$betweeness.vert.com = complexity(betweenness.dist)
            feats$betweeness.vert.fis = fisher_information(betweenness.dist)
        }
        else
        {
            feats$betweeness.vert.sha = 0
            feats$betweeness.vert.com = 0
            feats$betweeness.vert.fis = 0
        }
        
        # closeness
        closeness.dist            = closeness(g, mode='out')
        if (sum(closeness.dist) != 0)
        {
            closeness.dist = closeness.dist[!is.na(closeness.dist)]
            closeness.dist = closeness.dist[!is.infinite(closeness.dist)]
            closeness.dist = closeness.dist/sum(closeness.dist)
            feats$closeness.vert.sha = shannon_entropy(closeness.dist,
                                        normalized=T)
            feats$closeness.vert.com = complexity(closeness.dist)
            feats$closeness.vert.fis = fisher_information(closeness.dist)
        }
        else
        {
            feats$closeness.vert.sha = 0
            feats$closeness.vert.com = 0
            feats$closeness.vert.fis = 0
        }

    }
    else
    {
        feats$vertices            = 0 # 4 
        feats$edges               = 0 # 5
        feats$weight.avg          = 0 # 6
        feats$weight.probloop     = 0 # 7
        feats$degree.avg          = 0 # 8
        feats$degree.max          = 0 # 9
        feats$degree.min          = 0 # 10
        feats$density             = 0 # 11
        feats$distance.avg        = 0 # 12
        feats$diameter            = 0 # 13
        feats$transitivity.avg    = 0 # 14
        feats$betweeness.edge.avg = 0 # 15
        feats$betweeness.vert.avg = 0 # 16
        feats$closeness           = 0 # 17
        feats$assortativity       = 0 # 18
        feats$reciprocity         = 0 # 19
        feats$degree.shannon      = 0 # 20
        feats$degree.complexity   = 0 # 21
        feats$degree.fisher       = 0 # 22
        feats$transitivity.sha    = 0 # 23
        feats$transitivity.com    = 0 # 24
        feats$transitivity.fis    = 0 # 25
        feats$betweeness.edge.sha = 0 # 26
        feats$betweeness.edge.com = 0 # 27
        feats$betweeness.edge.fis = 0 # 28
        feats$betweeness.vert.sha = 0 # 29
        feats$betweeness.vert.com = 0 # 30
        feats$betweeness.vert.fis = 0 # 31
        feats$closeness.vert.sha  = 0 # 32
        feats$closeness.vert.com  = 0 # 33
        feats$closeness.vert.fis  = 0 # 34
    }
    
    # converting to data.frame (???)
    feats = as.data.frame(feats)

    # printing the header (features names)
    #if (header==TRUE)
    #{
    #    cat(names(feats), '\n')
    #}

    #dist = distance_table(gx)
    #cat('max distance:', max(dist$res),'\n')
    #cat('min distance:', min(dist$res),'\n')

    # TODO: tvz nao tenha muito sentido essas metricas serem as medias,
    # é provavel que o mais adequado seja a entropia delas, como uma
    # distribuicao de uma dada quantidade/caracteristica pelos nós!!! 

    # clustering coefficient
    #cat('transitivity global:', mean(transitivity(gx, type='global'), na.rm=TRUE), '\n')
    # betweeness

    # ditribuicao dos graus dos nos?
    # distribuicao da betweeness?

    feats_res = as.numeric(feats)

    attr(feats_res, 'cols') = names(feats)

    return(feats_res)
}

