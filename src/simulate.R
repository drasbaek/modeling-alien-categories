# script for simulating the GCM model 

#' Euclidean distance
#' vector1 & vector2: vector of values
euclidean_distance <- function(vector1, vector2, weight_vector = c(1,1,1,1,1)){
    
    # compute weighted featurewise differences
    weighted_differences <- (vector1 - vector2)^2 * weight_vector
    
    # compute distance
    distance = sqrt(sum(weighted_differences))

    return(distance)
}


#' Similarity
#' distance: distance between two vectors
#' c: scaling parameter
similarity <- function(distance, c){    
    # compute similarity
    similarity = exp(-c * distance)
    
    return(similarity)
}

#' get the similarity sum between the current stimulus and all exemplars in a category
#' current_stimulus: features of the current stimulus (vector)
#' exemplars: features of the exemplars of some category (matrix)
#' c: scaling parameter (for similarity function)
get_similarity_sum <- function(current_stimulus, exemplars, c){
    # init empty vector
    similarities <- c()
    
    # loop over rows in matrix
    for (i in 1:nrow(exemplars)){

        # extract current exemplar
        exemplar = exemplars[i,]

        # compute
        distance = euclidean_distance(current_stimulus, exemplar)
        
        # calculate similarity between current stimulus and exemplar
        similarities[exemplar] <- similarity(distance, c)

    }
    # calculate sum of the similarity scores
    similarities_sum <- sum(similarities)

    return(similarities_sum)
}

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

    # init choice vector
    choices <- c()

    for (trial in 1:32) {
        # check whether agent has seen both categories
        if (trial <= first_change){
            # make a random choice if the agent does not have an exemplar of both categories
            choices[trial] <- rbern(1, 0.5)
        }
        
        else {
            # identify current exemplars (set the trial minus 1 to discount the one we are currently on)
            exemplars <- stimuli[1:trial-1,]
            dangerous_exemplars <- exemplars[dangerous[1:trial-1] == 1,]
            safe_exemplars <- exemplars[dangerous[1:trial-1] == 0,]

            # extract the current stimulus
            current_stimulus <- stimuli[trial,]
            
            # calculate similarity sum (which first computes distances, then similarities)
            dangerous_similarity_sum <- get_similarity_sum(current_stimulus, dangerous_exemplars, 1)
            safe_similarity_sum <- get_similarity_sum(current_stimulus, safe_exemplars, 1)

            # compute the rate  for selecting dangerous
            dangerous_rate <- dangerous_similarity_sum / (dangerous_similarity_sum + safe_similarity_sum)
            
            # make a choice based on the rate, 1: dangerous, 0: safe
            choices[trial] <- rbern(1, dangerous_rate)
     
        }
        
    }

    return(choices)
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