suppressMessages(library(igraph))

# convert a time series x to a visibility graph (igraph object)
vg = function(x)
{
    # the number of points in x
    m = length(x)

    # creating an empty graph
    g = make_empty_graph(n=m, directed=FALSE)

    # the adjacent nodes are visible to each other
    g = add_edges(g, c(rbind(1:(m-1),2:m)))

    for (ta in 1:(m-2))
    {
        for (tb in (ta+2):m)
        {
            ya = x[ta]
            yb = x[tb]

            # points placed between ta and tb
            tc = seq(ta+1, tb-1)
            yc = x[tc]

            # checking if any point between them obstructs the view
            view = ( yc >= yb + (ya - yb) * ( (tb - tc) / (tb - ta) ) )
            
            #cat('[',ta,tb,']: ',tc,'\n')
            #cat(view,'\n')
            
            # has visibility
            if (sum(view) == 0 && !are_adjacent(g, ta, tb))
            {
                g = add_edges(g, c(ta, tb))
            }
        }
    }

    return(g)
}


# convert a time series x to a visibility graph (adjacency matrix)
# - weighted: to compute the diff (weight) of the data points
vg.matrix = function(x, weighted=FALSE)
{
    # the number of points in x
    m = length(x)

    # the adjacency matrix
    M = matrix(0, nc=m, nr=m)

    # TODO: check if this difference, instead of 1, to assign a neighbor
    # point can be affected by two points with the same values (x-y=0)

    # the adjacent nodes are visible to each other
    if (weighted == TRUE)
    {
        diag(M[1:m, 2:m]) = x[2:m] - x[1:(m-1)] # + 1e-30
    }
    else
    {
        diag(M[1:m, 2:m]) = 1
    }

    for (ta in 1:(m-2))
    {
        for (tb in (ta+2):m)
        {
            ya = x[ta]
            yb = x[tb]

            # points placed between ta and tb
            tc = seq(ta+1, tb-1)
            yc = x[tc]

            # checking if any point between them obstructs the view
            view = ( yc >= yb + (ya - yb) * ( (tb - tc) / (tb - ta) ) )
            
            #cat('[',ta,tb,']: ',tc,'\n')
            #cat(view,'\n')
            
            # has visibility
            if (sum(view) == 0)
            {
                if (weighted == TRUE)
                {
                    M[ta,tb] =  x[tb] - x[ta] # + 1e-30
                }
                else
                {
                    M[ta,tb] = 1
                }
            }
        }
    }

    # to make it simetric
    M = (t(M) + M)

    return(M)
}

# Horizontal Visibility Graph (igraph object)
hvg = function(x)
{
    m = length(x)
    
    # creating an empty graph
    g = make_empty_graph(n=m, directed=FALSE)
    
    # the adjacent nodes are visible to each other
    g = add_edges(g, c(rbind(1:(m-1),2:m)))
    
    for (ta in 1:(m-2))
    {
        for (tb in (i+2):m)
        {
            n = sum(x[ta:tb] - min(x[c(ta,tb)]) > 0)

            if (n == 1 && !are_adjacent(g, ta, tb))
            {
                g = add_edges(g, c(ta, tb))
            }            
        }
    }

    return(g)
}

# Horizontal Visibility Graph (adjacency matrix)
hvg.matrix = function(x, weighted=FALSE)
{
    m = length(x)
    
    # the adjacency matrix
    M = matrix(0, nc=m, nr=m)

    # TODO: check if this difference, instead of 1, to assign a neighbor
    # point can be affected by two points with the same values (x-y=0)

    # the adjacent nodes are visible to each other
    if (weighted == TRUE)
    {
        diag(M[1:m, 2:m]) = x[2:m] - x[1:(m-1)] # + 1e-30
    }
    else
    {
        diag(M[1:m, 2:m]) = 1
    }
            
    for (ta in 1:(length(x)-2))
    {
        for (tb in (i+2):length(x))
        {
            n = sum(x[ta:tb] - min(x[c(ta,tb)]) > 0)

            if (n == 1)
            {
                if (weighted == TRUE)
                {
                    M[ta,tb] = x[tb] - x[ta] # + 1e-30
                }
                else
                {
                    M[ta,tb] = 1
                }
            }            
        }
    }

    # to make it simetric
    M = (t(M) + M)

    return(M)
}

# testing

# regular graph

#x = c(0.87, 0.49, 0.36, 0.83)
#x = rep(x, 50)
#g1 = vg(x)
#shannon_entropy(degree_distribution(g1), normalized=T)
#plot(log(degree_distribution(g1)))
#
#y = runif(10^3)
#g2 = vg(y)
#shannon_entropy(degree_distribution(g2), normalized=T)
#plot(log(degree_distribution(g2)))
#
#g3 = bandt_pompe_transition_graph(x, 3, 1)
#shannon_entropy(degree_distribution(g3), normalized=T)
#plot(log(degree_distribution(g3)))
#
#g4 = get.tg(y, 3, 1)
#shannon_entropy(degree_distribution(g4), normalized=T)
#plot(log(degree_distribution(g4)))
#
