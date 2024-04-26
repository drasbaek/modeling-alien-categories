// generalized context model (GCM) 

data {
    int<lower=1> ntrials;  // n of trials
    int<lower=1> nfeatures;  // n of features
    array[ntrials] int<lower=0, upper=1> dangerous; // correct choice per trial
    array[ntrials] int<lower=0, upper=1> choices;  // choice per trial
    array[ntrials, nfeatures] real stimuli; // stimuli (vector of features)

    // priors
    vector[nfeatures] w_prior_values;  // concentration params (dirichlet distribution) 
    array[2] real c_prior_values;  // mean + variance (logit-normal distribution)
}

parameters {
    simplex[nfeatures] w; // feature weights (simplex as by nature sums to 1, so we don't have to ensure this)
    real logit_c;
}

transformed data {
   // ... declarations ... statements ... //
}

model{
    // model here //
}