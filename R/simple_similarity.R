#' @export

simple_similarity <-
  function(ratings, max_score=5, min_score=1, ac){

    #---------------------------------conditions
    #1 ratings
    if (missing(ratings) || !is.matrix(ratings)){
      stop("No ratings supplied.")
    }

    #2,3 max_score, min_score
    if (missing(max_score) || missing(min_score) ||
          !is.numeric(max_score) || !is.numeric(min_score) ||
          any(!is.finite(max_score)) || any(!is.finite(min_score)) ||
          any(max_score<=min_score)){
      stop("The score is invalidly specified.")
    }

    #4 ac
    if (missing(ac) || !is.numeric(ac) || any(ac<=0) || any(ac>dim(ratings)[2])){
      stop("No active_user specified.")
    }
    #---------------------------------conditions

    sim_i <- c(rep(NaN, dim(ratings)[2]))

    for (i in 1:dim(ratings)[2])
    {
      N_sim<-0

      sum_sim<-0

      dif <- c(rep(NaN, dim(ratings)[1]))

      sim_partial <- c(rep(NaN, dim(ratings)[1]))

      for (j in 1:dim(ratings)[1])
      {
        if (!is.na(ratings[j,ac]) && !is.na(ratings[j,i]))
        {
          dif[j]<-abs(ratings[j,ac]-ratings[j,i])

          #if(dif[j]<=(max_score-min_score)/2)
          #{
          sim_partial[j] <- ((-dif[j])/(max_score-min_score))+1

          sum_sim <- sum_sim + sim_partial[j]

          N_sim <- N_sim + 1
          #}
        }
      }#j

      sim_i[i] <- sum_sim/N_sim

    }#i

    sim_i[ac]<-NaN

    sort_sim <- sort(sim_i,na.last = TRUE,method = "radix",
                     decreasing = TRUE, index.return = TRUE)

    t <- length(sort_sim$x)

    for (k in 1:length(sort_sim$x))
    {
      if(is.na(sort_sim$x[k]))
      {
        sort_sim$ix[k] <- NaN

        t<-t-1
      }
    }

    obj_simple_similarity <- list( call = match.call(),
                                   sim_x=sort_sim$x[1:t],
                                   sim_index=sort_sim$ix[1:t])

    class(obj_simple_similarity) <- "simple_similarity"
    return(obj_simple_similarity)
  }
