
#' General Categorization Model Agent
#' stimuli: a trial by feature matrix
GCMagent <- function(stimuli){
    
    # init empty vector
    dangerous <- rep(NA, nrow(stimuli))
    
    # compute dangerous, if the first two columns are 1, then dangerous is 1
    for (trial in 1:nrow(stimuli)){
        if (stimuli[trial,1] == 1 & stimuli[trial,3] == 1){
            dangerous[trial] <- 1
        } else {
            dangerous[trial] <- 0
        }
    }

    ## identify which trial the agent encounters a new category (thereby having seen both)
    # create a lagged version of dangerous
    dangerous_lag <- c(NA, dangerous[-length(dangerous)])
    
    # calculate difference of vectors
    dangerous_diff <- dangerous - dangerous_lag

    # extract first index that contains either a 1 or -1 (used for knowing when the agent has seen both)i
    first_change <- which(dangerous_diff != 0)[1]

    for (trial in 1:32) {
        # check whether agent has seen both categories
        if (trial <= first_change){
            # make a random choice if the agent does not have an exemplar of both categories
            choice[trial] <- rbern(1, 0.5)
        }
        
        else {
            # identify current exemplars
            exemplars <- stimuli[1:trial,]
            dangerous_exemplars <- exemplars[dangerous[1:trial] == 1,]
            safe_exemplars <- exemplars[dangerous[1:trial] == 0,]
            
            # calculate euclidean distance between current stimulus and previous exemplars within each category
            dangerous_distances <- "SOMETHING"
        }
        
    }

    return(dangerous)
}


#' Simulate stimuli function
simulate_session <- function(){ 
    # create all combinations of 0 and 1 for six positions
    combinations_df <- expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1), c(0, 1))

    # convert data frame to a matrix
    combinations_matrix <- as.matrix(combinations_df)

    # shuffle rows randomly
    combinations_matrix <- combinations_matrix[sample(nrow(combinations_matrix)),]

    # add column names
    colnames(combinations_matrix) <- c("eyes", "legs", "spots", "arms", "color")

    return(combinations_matrix)    
}

# testing
stimuli <- simulate_session()

# run the agent
dangerous <- GCMagent(stimuli)

print(stimuli)
print(dangerous)