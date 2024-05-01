// generalized context model (GCM) 

data {
    int<lower=1> ntrials;  // n of trials
    int<lower=1> nfeatures;  // n of features
    array[ntrials] int<lower=0, upper=1> dangerous; // correct classifications per trial
    array[ntrials] int<lower=0, upper=1> choices;  // choice per trial
    array[ntrials, nfeatures] real stimuli; // stimuli (vector of features)

    // priors
    vector[nfeatures] w_prior_values;  // concentration params (dirichlet distribution) 
    array[2] real c_prior_values;  // mean + variance (logit-normal distribution)
}

transformed data {
    array[ntrials] int<lower=0, upper=1> non_dangerous; // dummy var for category (non_dangerous)
    array[sum(dangerous)] int<lower=1, upper=ntrials> dangerous_idx; // stimuli that are cat = 1 (dangerous)
    array[ntrials-sum(dangerous)] int<lower=1, upper=ntrials> non_dangerous_idx; //  stimuli that are cat 2 (non_dangerous)

    // init counters for idx arrays 
    int count_idx_dangerous = 1; 
    int count_idx_non_dangerous = 1;

    for (i in 1:ntrials){
        non_dangerous[i] = abs(dangerous[i] - 1);

        if (dangerous[i] == 1){
            dangerous_idx[count_idx_dangerous] = i;
            count_idx_dangerous += 1;

        } else {
            non_dangerous_idx[count_idx_non_dangerous] = i;
            count_idx_non_dangerous += 1;
        }

    }

}

parameters {
    simplex[nfeatures] w; // feature weights (simplex as by nature sums to 1, so we don't have to ensure this)
    real logit_c; // scaling parameter on logit scale
}

transformed parameters {
    // define scaling parameter on true scale (inverse logit)
    real<lower=0, upper=5> c = inv_logit(logit_c) * 5; // multiply by 5 to bound between 0 and 5
    
    // parameter for rate 
    array[ntrials] real<lower=0.00001, upper=0.99999> rate; // 
    array[ntrials] real<lower=0, upper=1> real_rate;

    for (i in 1:ntrials){
        // compute distance from observations to all exemplars 
        array[(i-1)] real exemplar_similarities;  // array to store similarities to exemplars

        for (exemplar in 1:(i-1)) {
            array[nfeatures] real dist; // distance 
            for (feature in 1:nfeatures){
                dist[feature] = w[feature] * abs(stimuli[exemplar, feature] - stimuli[i, feature]);
            }
            // compute similarity for particular exemplar in loop
            exemplar_similarities[exemplar] = exp(-c * sum(dist));  
        }

        // if no exemplars have been seen in either of the categories, make a random choice
        if (sum(dangerous[:(i-1)])==0 || sum(non_dangerous[:(i-1)])==0) {
            rate[i] = 0.5;
        }
        else {
            // compute similarity
            array[2] real similarities;

            // identify indexes in exemplars for each of the two categories
            array[sum(dangerous[:(i-1)])] int temp_count_idx_dangerous = dangerous_idx[:sum(dangerous[:(i-1)])];
            array[sum(non_dangerous[:(i-1)])] int temp_count_idx_non_dangerous = non_dangerous_idx[:sum(dangerous[:(i-1)])];

            // compute similarity to the two categories
            similarities[1] = sum(exemplar_similarities[temp_count_idx_dangerous]);
            similarities[2] = sum(exemplar_similarities[temp_count_idx_non_dangerous]);

            // compute real rate at i 
            real_rate[i] = similarities[1] / (similarities[1] + similarities[2]);

            // compute rate at i (with a hacky fix)
            if (real_rate[i] >= 0.99999) {
                rate[i] = 0.99999;
            } else if (real_rate[i] <= 0.00001) {
                rate[i] = 0.00001;
            } else {
                rate[i] = real_rate[i];
            }
        }
    }
}

model{
    // define priors 
    target += dirichlet_lpdf(w | w_prior_values);
    target += normal_lpdf(logit_c | c_prior_values[1], c_prior_values[2]);

    // make choice 
    target += bernoulli_lpmf(choices | rate);
}

generated quantities {
    // define priors again
    simplex[nfeatures] w_prior = dirichlet_rng(w_prior_values);
    real logit_c_prior = normal_rng(c_prior_values[1], c_prior_values[2]);
    real<lower=0, upper=5> c_prior = inv_logit(logit_c_prior)*5;

    // prior predictive 
    array[ntrials] real<lower=0, upper=1> rate_prior;
    array[ntrials] real real_rate_prior;

    for (i in 1:ntrials){
        array[(i-1)] real exemplar_similarities;

        for (exemplar in 1:(i-1)){
            array[nfeatures] real dist; 
            
            for (feature in 1:nfeatures){
                dist[feature] = w_prior[feature]*abs(stimuli[exemplar, feature] - stimuli[i, feature]);
            }
            // compute similarity
            exemplar_similarities[exemplar] = exp(-c_prior * sum(dist));  
        }
    
        // if no exemplars have been seen in either of the categories, make a random choice
        if (sum(dangerous[:(i-1)])==0 || sum(non_dangerous[:(i-1)])==0) {
            rate_prior[i] = 0.5;
        }

        else {
        // compute similarity
        array[2] real similarities; 

    
        // identify indexes in exemplars for each of the two categories
        array[sum(dangerous[:(i-1)])] int temp_count_idx_dangerous = dangerous_idx[:sum(dangerous[:(i-1)])];
        array[sum(non_dangerous[:(i-1)])] int temp_count_idx_non_dangerous = non_dangerous_idx[:sum(dangerous[:(i-1)])];

         // compute similarity to the two categories
        similarities[1] = sum(exemplar_similarities[temp_count_idx_dangerous]);
        similarities[2] = sum(exemplar_similarities[temp_count_idx_non_dangerous]);

        // calculate real rate at i 
        real_rate_prior[i] = similarities[1] / (similarities[1] + similarities[2]);

        // compute rate at i (with a hacky fix)
        if (real_rate_prior[i] >= 0.99999) {
            rate_prior[i] = 0.99999;
        } else if (real_rate_prior[i] <= 0.00001) {
            rate_prior[i] = 0.00001;
        } else {
            rate_prior[i] = real_rate_prior[i];
            }

        }

    }
    // prior preds
    array[ntrials] int<lower=0, upper=1> priorpred = bernoulli_rng(rate_prior);

    // posterior preds
    array[ntrials] int<lower=0, upper=1> posteriorpred = bernoulli_rng(rate);
    array[ntrials] int<lower=0, upper=1> posteriorcorrect;
    
    for (i in 1:ntrials) {
        if (posteriorpred[i] == dangerous[i]) {
            posteriorcorrect[i] = 1;
        } else {
            posteriorcorrect[i] = 0;
        }
    }
    
    // get log likelihood
    array[ntrials] real log_lik; 

    for (i in 1:ntrials){
        log_lik[i] = bernoulli_lpmf(choices[i] | rate[i]);

    }
}
