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
get_similarity_sum <- function(current_stimulus, exemplars, weight_vector, c){
    # init empty vector
    similarities <- c()
    
    # ensure that exemplars is treated as a matrix
    exemplars <- as.matrix(exemplars)

    # loop over rows in matrix
    for (i in 1:nrow(exemplars)){
        # extract current exemplar
        exemplar = exemplars[i,]

        # compute
        distance = euclidean_distance(current_stimulus, exemplar, weight_vector)
        
        # calculate similarity between current stimulus and exemplar
        similarities[exemplar] <- similarity(distance, c)

    }
    # calculate sum of the similarity scores
    similarities_sum <- sum(similarities)

    return(similarities_sum)
}

#' General Categorization Model Agent
#' stimuli: a trial by feature matrix
#' weight_vector: a vector of weights for each feature
#' c: scaling parameter for similarity function
GCMagent <- function(stimuli, weight_vector, c){
    
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
    rates <- c()

    for (trial in 1:32) {
        # check whether agent has seen both categories
        if (trial <= first_change){
            # make a random choice if the agent does not have an exemplar of both categories
            choices[trial] <- rbinom(1, 1, 0.5)
        }
        
        else {
            # identify current exemplars (set the trial minus 1 to discount the one we are currently on)
            exemplars <- stimuli[1:trial-1,]
            dangerous_exemplars <- exemplars[dangerous[1:trial-1] == 1,]
            safe_exemplars <- exemplars[dangerous[1:trial-1] == 0,]

            # extract the current stimulus
            current_stimulus <- stimuli[trial,]
            
            # calculate similarity sum (which first computes distances, then similarities)
            dangerous_similarity_sum <- get_similarity_sum(current_stimulus, dangerous_exemplars, weight_vector, c)
            safe_similarity_sum <- get_similarity_sum(current_stimulus, safe_exemplars, weight_vector, c)

            # compute the rate  for selecting dangerous
            dangerous_rate <- dangerous_similarity_sum / (dangerous_similarity_sum + safe_similarity_sum)

            rates[trial] <- dangerous_rate
            
            # make a choice based on the rate, 1: dangerous, 0: safe
            choices[trial] <- rbinom(1, 1, dangerous_rate)
        }
        
    }

    return(list(choices, dangerous, rates))
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

simulate_agents <- function(stimuli_list, weight_vector, c, agent_type){
    # get the number of agents from the length of the stimuli list
    n_agents <- length(stimuli_list)

    # init empty matrix for all data
    all_data <- data.frame()

    for (i in 1:n_agents){
        stimuli <- stimuli_list[[i]]
        result <- GCMagent(stimuli, weight_vector, c)
        choices <- result[[1]]
        dangerous <- result[[2]]
        rates <- result[[3]]
        accuracy <- ifelse(choices == dangerous, 1, 0)
        agent_types <- rep(agent_type, nrow(stimuli))

        data <- cbind(stimuli, choices, dangerous, accuracy, rates, agent_types)
        
        # add to all data
        all_data <- rbind(all_data, data)
    }
    return(all_data)
}

# testing
n_agent <- 50
seeds <- 1:n_agent
# simulate stimuli matrices and add to a list
stimuli_list <- list()
for (i in 1:n_agent){
    # set seed for reproducibility, but change for each shuffling of stimuli since it otherwise shuffles the same way each time
    set.seed(seeds[i])
    stimuli_list[[i]] <- simulate_session()
}

# run a sim with agents that have favorable weights
c <- 1
weights_both_good <- c(20,1,20,1,1)
weights_one_good <- c(20,1,1,1,1)
weights_neutral <- c(1,1,1,1,1)

both_good_data <- simulate_agents(stimuli_list, weights_both_good, c, "both_good")
one_good_data <- simulate_agents(stimuli_list, weights_one_good, c, "one_good")
neutral_data <- simulate_agents(stimuli_list, weights_neutral, c, "neutral")


# bind all data together with a column for the agent type
all_data <- rbind(both_good_data, one_good_data, neutral_data)

file_path <- file.path("data", "simulated_data.csv")
write.csv(all_data, file_path, row.names = FALSE)

