# script for simulating the GCM model 

#' Manhattan distance
#' vector1 & vector2: vector of values
#' weight_vector: vector of weights for each feature
manhattan_distance <- function(vector1, vector2, weight_vector = c(1,1,1,1,1)){
    
    # compute weighted featurewise differences
    weighted_differences <- weight_vector * abs(vector1 - vector2) 
    
    # compute distance
    distance = sum(weighted_differences)

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
        distance = manhattan_distance(current_stimulus, exemplar, weight_vector)
        
        # calculate similarity between current stimulus and exemplar
        similarities[exemplar] <- similarity(distance, c)

    }
    # calculate avg of the similarity scores
    similarities_avg <- mean(similarities)

    return(similarities_avg)
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

    # extract first index that contains either a 1 or -1 (used for knowing when the agent has seen both)
    first_change <- which(dangerous_diff != 0)[1]

    # init choice vector
    choices <- c()
    rates <- c()

    for (trial in 1:nrow(stimuli)) {
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

        # compute trial 
        trial <- 1:nrow(stimuli)
        agent_number <- i
        true_c <- rep(c, nrow(stimuli))

        data <- cbind(true_c, agent_number, trial, stimuli, choices, dangerous, accuracy, rates, agent_types)
        
        # add to all data
        all_data <- rbind(all_data, data)
    }
    return(all_data)
}

# testing
n_agent <- 50
n_sessions <- 3
seeds <- 1:(n_agent*n_sessions)

# simulate stimuli matrices and add to a list
stimuli_list <- list()
for (i in 1:n_agent){
    # set seed for reproducibility, but change for each shuffling of stimuli since it otherwise shuffles the same way each time
    # do first session
    seed1 <- seeds[n_sessions*i-2]
    set.seed(seed1)
    stimuli1 <- simulate_session()

    # do second session
    seed2 <- seeds[n_sessions*i-1]
    set.seed(seed2)
    stimuli2 <- simulate_session()

    # do third session
    seed3 <- seeds[n_sessions*i]
    set.seed(seed3)
    stimuli3 <- simulate_session()

    # bind the sessions
    stimuli <- rbind(stimuli1, stimuli2, stimuli3)

    # add to list
    print(paste("Saving stimuli with seeds", seed1, seed2, seed3))
    stimuli_list[[i]] <- stimuli
}

# set the weights we want to investigate
weights_both_good <- c(0.5,0,0.5,0,0)
weights_one_good <- c(0.5,0.125,0.125,0.125,0.125)
weights_neutral <- c(0.2,0.2,0.2,0.2,0.2)

# set the scalings we want to investigate
c <- c(0.1, 0.3, 0.5, 1, 2, 5)

# init empty df
all_data <- data.frame()

# simulate for all combinations
for (i in 1:length(c)){
    # simulate data for each agent type
    both_good_data <- simulate_agents(stimuli_list, weights_both_good, c[i], "both_good")
    one_good_data <- simulate_agents(stimuli_list, weights_one_good, c[i], "one_good")
    neutral_data <- simulate_agents(stimuli_list, weights_neutral, c[i], "neutral")

    # add to all data
    all_data <- rbind(all_data, both_good_data, one_good_data, neutral_data)
}
file_path <- file.path("data", "simulated_data.csv")
write.csv(all_data, file_path, row.names = FALSE)

