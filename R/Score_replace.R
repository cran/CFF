#' @export

Score_replace <-
  function(ratings, sim_index, ac){

    #---------------------------------conditions
    #1 ratings
    if (missing(ratings) || !is.matrix(ratings)){
      stop("No ratings supplied.")
    }

    #2 sim_index
    if (missing(sim_index) || !is.vector(sim_index) || !is.numeric(sim_index)){
      stop("No sim_index supplied.")
    }

    #3 ac
    if (missing(ac) || !is.numeric(ac) || any(ac<=0) || any(ac>dim(ratings)[2]) ){
      stop("No active_user specified.")
    }
    #---------------------------------conditions

    ratings2 <- ratings

    flag1 <- c(rep(NaN, dim(ratings)[1]))

    for (h in 1:dim(ratings)[2])
    {
      for (j in 1:dim(ratings)[1])
      {
        if(is.na(ratings2[j,ac]) && !is.na(ratings[j,sim_index[h]]) && is.na(flag1[j]))
        {
          ratings2[j,ac] <- ratings[j,sim_index[h]]

          flag1[j] <- 1
        }
      }
    }

    return(ratings2)
  }

