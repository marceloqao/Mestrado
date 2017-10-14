suppressMessages(library(igraph))
require(e1071, quietly=TRUE)

# Implementation of the bandt-pope methodology

# Bandt-Pompe transformation
#
# data: the dataset (vector)
# D:    the embedding dimension (size of sliding window)
# tau:  the embedding delay ('step' value)
#
# return The list of symbols from the transformed dataset
bandt_pompe = function(data, D=4, tau=1, by=1)
{
    # the list of symbols to be returned
    symbols = c()
    
    # dicovering the sequences of order n
    for (s in seq(1, length(data)-(D-1)*tau, by=by))
    {
        # the indices for the subsequence
        ind = seq( s, s+(D-1)*tau, by=tau)
        
        # get the sub-timeseries (sliding window)
        sub = data[ind]

        # the current permutation pattern
        pattern = paste(order(sub), collapse='')

        # adding the current pattern to the list of symbols
        symbols = c(symbols, pattern)
    }

    return(symbols)
}

# Bandt-Pompe distribution
#
# Parameters:
# data: the dataset (vector)
# D:    the embedding dimension (size of sliding window)
# tau:  the embedding delay ('step' value)
# numred: numerosity reduction, similar to BOSS algorithm, do not count
#         repetitions of the same symbol
bandt_pompe_distribution = function(data, D=4, tau=1, numred=FALSE, by=1)
{
    # the list of symbols from the BP transformation
    symbols = bandt_pompe(data, D=D, tau=tau, by=by)

    # the distribution of permutations (pi)
    dpi = rep(0, factorial(D))
    
    # to get the index of the permutation pi
    perms = sort(apply(permutations(D), 1, paste, collapse=''))
    
    if (numred == FALSE)
    {
        # dicovering the sequences of order n
        for (i in 1:length(perms))
        {
            # counting the pattern
            dpi[i] = sum(symbols == perms[i])
        }
    }
    else
    {
        # only counts different subsequence of symbols
        oldsymbol = symbols[1]
        ind = which(perms == oldsymbol)
        dpi[ind] = dpi[ind] + 1
        
        for (i in 2:length(symbols))
        {
            if (oldsymbol != symbols[i])
            {
                ind = which(perms == symbols[i])
                dpi[ind] = dpi[ind] + 1
            }
            oldsymbol = symbols[i]
        }
    }
    
    # the returning format
    output = data.frame(patterns = perms, 
                        frequencies = dpi,
                        probabilities = dpi/sum(dpi))
    return(output)
}



# The Band-Pompe Transition (adjacency matrix)
#
# Parameters:
# data: the dataset (vector)
# D:    the embedding dimension (size of sliding window)
# tau:  the embedding delay ('step' value)
# loop: enbale (TRUE) the creation of loopings in the graph
# normalized: returns as percentage
# vect: returns as a vector instead of matrix
bandt_pompe_transition = function(data, D=4, tau=1, 
                                  normalized=TRUE, quiroz=FALSE,
                                  loop=TRUE, vect=FALSE, by=1)
{
    # the number of permutations
    dfact = factorial(D)

    # to get the index of the permutation pi
    perms = sort(apply(e1071::permutations(D), 1, paste, collapse=''))
    
    # the transitions matrix
    M = matrix(0, ncol=dfact, nrow=dfact)
    rownames(M) = perms
    colnames(M) = perms
    
    # the list of symbols from the BP transformation
    symbols = bandt_pompe(data, D=D, tau=tau, by=by)

    # dicovering the sequences of order n
    for (i in 2:length(symbols))
    {
        # the previous permutation pattern
        from = symbols[i-1]

        # the current permutation pattern
        to = symbols[i]
        
        # checking if the creation of loopings are allowed
        if (from != to | loop == TRUE)
        {
            # incrementing the counting for this transition
            M[from, to] = M[from, to] + 1
        }
    }

    if (normalized == TRUE)
    {
        # this is the normalization suggested by Quiroz (2015)
        # similar to a markov chain probabilities
        if (quiroz == TRUE)
        {
            M = t(apply(M, 1, function(x)
                            {
                                if(sum(x) > 0)
                                {
                                    x/sum(x)
                                } else {
                                    rep(0, length(x))
                                }
                            }
                            ))
        }
        else
        {
            # doing a general normalization
            M = M/sum(M)
        }
    }

    if (vect == TRUE)
    {
        M = c(M)
    }
    
    return(M)
}

# get the BP transition graph
# - D: embedded dimension
# - tau: embedded delay
# - empty: TRUE to remove the vertices with 0 degree
# - quiroz: normalization proposed by quiroz, similar to a markov-chain
bandt_pompe_transition_graph = function(x, D=4, tau=1, 
                                        empty=TRUE, quiroz=FALSE)
{
    # adjacency matrix from bandt pompe transition
    A = bandt_pompe_transition(x, D=D, tau=tau, quiroz=quiroz)
    
    # the graph
    gA = graph_from_adjacency_matrix(A, mode="directed", weighted=TRUE)
    
    # removing vertices without transition
    if (empty == TRUE)
    {
        gA = delete_vertices(gA, which(igraph::degree(gA) == 0))
    }

    return(gA)
}




# Ideas

bandt_pompe_distribution_old = function(data, D=4, tau=1,numred = F, by = 1)
{
    # the list of symbols from the BP transformation
    symbols = bandt_pompe(data, D=D, tau=tau)

    # the distribution of permutations (pi)
    dpi = rep(0, factorial(D))
    
    # to get the index of the permutation pi
    perms = sort(apply(permutations(D), 1, paste, collapse=''))
    
    # dicovering the sequences of order n
    for (s in (D*tau):length(data))
    {
        # get the sub-timeseries (sliding window)
        sub = data[seq( s-(D-1)*tau, s, by=tau)]

        # the current permutation pattern
        pattern = paste(order(sub), collapse='')
        
        # index of the permutation
        ind = which(perms == pattern)
        
        # incrementing the counting for this pattern
        dpi[ind] = dpi[ind] + 1
    }
    
    # the returning format
    output = data.frame(patterns = perms, 
                        frequencies = dpi,
                        probabilities = dpi/sum(dpi))
    return(output)
}

bandt_pompe_transition_old = function(data, D=4, tau=1, 
                                  normalized=TRUE, quiroz=FALSE,
                                  loop=TRUE, vect=FALSE)
{
    # the number of permutations
    dfact = factorial(D)

    # to get the index of the permutation pi
    perms = sort(apply(e1071::permutations(D), 1, paste, collapse=''))
    
    # the transitions matrix
    M = matrix(0, ncol=dfact, nrow=dfact)
    rownames(M) = perms
    colnames(M) = perms

    # the last pattern
    lastpat = NULL

    # dicovering the sequences of order n
    for (s in (D*tau):length(data))
    {
        # get the sub-timeseries
        sub = data[seq( s-(D-1)*tau, s, by=tau)]
        
        # the current perutation pattern
        pattern = paste(order(sub), collapse='')
        
        # if it is not the first pattern found
        if ( !is.null(lastpat) )
        {
            # checking if the creation of loopings are allowed
            if (lastpat != pattern | loop == TRUE)
            {
                # incrementing the counting for this transition
                M[lastpat, pattern] = M[lastpat, pattern] + 1
            }
        }
        
        # store the pattern to the next round
        lastpat = pattern
    }

    if (normalized == TRUE)
    {
        # TODO: verificar se a normalizacao das transicoes deve ser feita por
        # linha (ou coluna), para indicar a probabilidade de transicao de um
        # estado para os demais (incluindo ele) somando 1, e nao todas as
        # probabilidades somando 1.
        # Essa definicao foi feita na Thesis que o prof. Osvaldo enviou para
        # Heitor!
    
        if (quiroz == TRUE)
        {
            # this is the normalization suggested by Quiroz (2015)
            M = t(apply(M, 1, function(x)
                            {
                                if(sum(x) > 0)
                                {
                                    x/sum(x)
                                } else {
                                    rep(0, length(x))
                                }
                            }
                            ))
        }
        else
        {
            # TODO: aqui fazendo a normalizacao geral
            M = M/sum(M)
        }
    }

    if (vect == TRUE)
    {
        M = c(M)
    }
    
    return(M)
}


# tests with gaps
bandt_pompe_gap = function(data, D=4, tau=1)
{
    # the size of the time series
    M = length(data)
    
    # the distribution pi (permutation)
    dpi = rep(0, factorial(D))
    
    # to get the index of the permutation pi
    perms = sort(apply(e1071::permutations(D), 1, paste, collapse=''))
    #perms = apply(permutations(D, D, 1:factorial(D)), 1, paste, 
    #              collapse='')    

    # dicovering the sequences of order n
    for (s in (D*tau):M)
    {
        # get the sub-timeseries
        sub = data[seq( s-(D-1)*tau, s, by=tau)]

        # if there is some NA in sub
        if (sum(is.na(sub)) > 0)
        {
        #    # sub without NAs
        #    sub_nona = sub[!is.na(sub)]

        #    # for each NA value
        #    for (na_v in which(is.na(sub)))
        #    {
        #        # for each non-NA value
        #        # plus the min and max values
        #        for(na_w in seq(0, length(sub_nona)+1))
        #        {
        #            # TODO: parei aqui, esta ficando muito estranho
        #            # caso tenha mais de um NA na subsequencia eu ainda
        #            # nao sei o que fazer

        #        }
        #    }

            #print(D)
            #print('sub:')
            #print(sub)

            # no NAs
            sub_nona = sub[!is.na(sub)]
            
            napos = which(is.na(sub))
            
            ## NA in first position of sub
            #if (napos == 1)
            #{
            #    subna = c(
            #              sub[2]-0.01,
            #              sub[2],
            #              sub[2]+0.01
            #              )
            #}
            ## NA in last position of sub
            #else if (napos == D)
            #{
            #    subna = c(
            #              sub[D-1]-0.01,
            #              sub[D-1],
            #              sub[D-1]+0.01
            #              )
            #}
            ## NA in middle position of sub
            #else
            #{
            #    subna = c(
            #              sub[napos-1]-0.01,
            #              sub[napos-1],
            #              sub[napos+1],
            #              sub[napos+1]+0.01
            #              )
            #}
                
            #print('sub_nona:')
            #print(sub_nona)

            subna = c(
                      min(sub_nona)-0.01, 
                      sub_nona, 
                      max(sub_nona)+0.01
                      )
            
            #print('subna:')
            #print(subna)

            for(i in 1:length(subna))
            {
                #print(i)


                newsub = sub
                newsub[napos] = subna[i]
                

                #print(newsub)
                
                #next
                
                # the current perutation pattern
                pattern = paste(order(newsub), collapse='')
                
                # index of the permutation
                ind = which(perms == pattern)
                
                # incrementing the counting for this pattern
                dpi[ind] = dpi[ind] + 1

            }
        }
        else
        {
            # the current perutation pattern
            pattern = paste(order(sub), collapse='')
            
            # index of the permutation
            ind = which(perms == pattern)
            
            # incrementing the counting for this pattern
            dpi[ind] = dpi[ind] + 1
        }

    }
    
    # the returning format
    output = data.frame(patterns = perms, 
                        frequencies = dpi,
                        probabilities = dpi/sum(dpi))
    return(output)
}


# streamed version of bandt-pompe distribution
# - data is a vector
bandt_pompe_stream = function(data, D=4, tau=1, age=0)
{
    # TODO: to store the vector of shannon entropy and statistical
    # complexity
    H = c()
    J = c()
    
    # the size of the string to store the sequences
    n = D * tau - (tau - 1)

    # to store the strings of the bandt-pompe (online)
    s = rep(NA, n)

    # the size of the time series
    M = length(data)
    
    # the distribution pi (permutation)
    dpi = rep(0, factorial(D))

    # the aging matrix (used as a binary left shift aging)
    if (age > 0)
    {
        # TODO: confirmar aqui o numero de colunas
        # when the 1 gets to the first column, subtracts the freq
        A = matrix(0, nrow=factorial(D), ncol=age+1)
    }
    
    # to get the index of the permutation pi
    # using library e1071
    perms = sort(apply(e1071::permutations(D), 1, paste, collapse=''))
    
    for (i in 1:M)
    {
        # the string is at the beggining
        if (i < n)
        {
            s[i] = data[i]
        }
        else
        {
            # completing the string
            s[n] = data[i]

            # computing the permutation here

            # get the sub-timeseries
            sub = s[seq( 1, n, by=tau)]

            # the current perutation pattern
            pattern = paste(order(sub), collapse='')

            # index of the permutation
            ind = which(perms == pattern)
            
            # incrementing the counting for this pattern
            dpi[ind] = dpi[ind] + 1

            # left shift to the next insertion
            s[1:(n-1)] = s[2:n]

            if (age > 0)
            {
                # move left the Aging matrix
                A[,1:age] = A[,2:(age+1)]
                A[,age+1] = 0

                # remove the old frequencies
                dpi = sapply(dpi - A[,1], max, 0) # not accept < 0 values

                # counting the current pattern
                A[ind,age+1] = 1

                #print(A)
            }
        #}

            output = data.frame(patterns = perms, 
                            frequencies = dpi,
                            probabilities = dpi/sum(dpi))
            #print(output)

            # aqui pode ser computada a entropia e complexidade

            # shannon normalized entropy
            h = shannon_entropy(output$probabilities, normalized=TRUE)
            H = c(H, h)

            # statistical complexity
            j = complexity(output$probabilities, h)
            J = c(J, j[1])

        }

    }

    # the returning format
    output = data.frame(patterns = perms, 
                        frequencies = dpi,
                        probabilities = dpi/sum(dpi))

    # TODO: saving H and J as attributes
    attr(output, 'H') = H
    attr(output, 'J') = J

    return(output)
}

#bandt_pompe_stream(c(1, 8, 3, 6, 5, 4, 7, 2, 9), D=3, tau=1, age=3)


# computes the band-pompe transitions (online version)
# Parameters:
# data: the dataset (vector)
# D:    the order, embedding dimension
# tau:  the 'step' value
# normalized: returns as percentage
# vect: returns as a vector instead of matrix
bandt_pompe_transition_stream = function(data, D=4, tau=1, age=0,
                                  normalized=TRUE, vect=TRUE)
{
    # TODO: to store the vector of shannon entropy and statistical
    # complexity
    H = c()
    J = c()

    # the size of the string to store the sequences
    n = D * tau - (tau - 1)

    # to store the strings of the bandt-pompe (online)
    s = rep(NA, n)
    
    # the size of the time series
    M = length(data)

    # the number of permutations
    dfact = factorial(D)

    # to get the index of the permutation pi
    # using library e1071
    perms = sort(apply(e1071::permutations(D), 1, paste, collapse=''))
    
    # the transitions matrix
    tm = matrix(0, ncol=dfact, nrow=dfact)
    rownames(tm) = perms
    colnames(tm) = perms
    
    # the aging matrix (used as a binary left shift aging)
    if (age > 0)
    {
        # TODO: confirmar aqui o numero de colunas
        # when the 1 gets to the first column, subtracts the freq
        A = matrix(0, nrow=dfact*dfact, ncol=age+1)
    }

    # the last pattern
    lastpat = NULL

    # dicovering the sequences of order n
    #for (i in (D*tau):M)
    for (i in 1:M)
    {
        # the string is at the beggining
        if (i < n)
        {
            s[i] = data[i]
        }
        else
        {
            # completing the string
            s[n] = data[i]

            # computing the permutation here

            # get the sub-timeseries
            sub = s[seq( 1, n, by=tau)]

            # get the sub-timeseries
            #sub = data[seq( s-(D-1)*tau, s, by=tau)]
            
            # the current perutation pattern
            pattern = paste(order(sub), collapse='')
            
            # if it is not the first pattern found
            if ( !is.null(lastpat) )
            {
                #print(lastpat)
                #print(pattern)

                # incrementing the counting for this transition
                tm[lastpat, pattern] = tm[lastpat, pattern] + 1

                if (age > 0)
                {
                    # move left the Aging matrix
                    A[,1:age] = A[,2:(age+1)]
                    A[,age+1] = 0

                    # remove the old frequencies
                    tm = apply(tm - matrix(A[,1],dfact,dfact), c(1,2), max, 0)
                    # not accept < 0 values

                    # counting the current pattern
                    a = which(perms == lastpat) # row
                    b = which(perms == pattern) # col
                    ind = (b-1) * dfact + a
                    A[ind,age+1] = 1
                }

            #print(tm)
            #print(A)

            }
            
            # store the pattern to the next  round
            lastpat = pattern
            
            # left shift to the next insertion
            s[1:(n-1)] = s[2:n]

        }

        # the returning format
        output = data.frame(
                        frequencies = c(tm),
                        probabilities = c(tm)/sum(c(tm)))

        # shannon normalized entropy
        h = shannon_entropy(output$probabilities, normalized=TRUE)
        H = c(H, h)

        # statistical complexity
        j = complexity(output$probabilities, h)
        J = c(J, j[1])
    }

    #if (normalized == TRUE)
    #{
    #    tm = tm/sum(tm)
    #}

    #if (vect == TRUE)
    #{
    #    tm = c(tm)
    #}

    # TODO: saving H and J as attributes
    attr(output, 'H') = H
    attr(output, 'J') = J
    
    return(output)

}

#bandt_pompe_transition_stream(c(1, 8, 3, 6, 5, 4, 7, 2, 9), D=3, tau=1, age=3)

