# OBSERVED TRIGRAMS

# Assign probability to observed trigrams 
# We use the Maximum Likelihood Estimate (MLE). N-gram model predicts x_i based 
# on x_{i-(n-1)},…,x_{i-1}$. MLE assigns probability on each word that exist in
# corpus. The probability is the number of n-gram events divided by the number 
# of total n-gram events.

# Markov assumption: the conditional probability distribution of future states 
# of the process (conditional on both past and present states) depends only upon
# the present state, not on the sequence of events that preceded it.

# We use discounting to account for the probability of unobserved events
# (certain combinations of words). Some of the probability mass is taken from 
# observed n-grams and distributed to unobserved ones in order to estimate 
# probabilites of unseen n-grams. Here we perform an absolute discounting (γ) of
# 0.75

# Backed-off probability of observed trigram 
# q_bo(w_i∣w_{i−2},w_{i−1}) = (c(w_{i−2},w_{i−1},w) − γ) / c(w_{i−2},w_{i−1})

#returns the observed trigrams in the corpus that start with query and their frequency
get_observed_trigrams <- function(query, trigrams_df){
    output <- data.frame(ngram=vector(mode='character', length=0), 
                         freq=vector(mode='integer', length=0))
    trigram_index <- grep(paste0("^",gsub(" ", "_", query, fixed = TRUE),"_"), 
                          trigrams$ngram, value=FALSE)
    output <- trigrams_df[trigram_index,]
    return(output)
}

#returns the probability of the observed trigrams in the corpus that start with the query
get_observed_trigrams_prob <- function(observed_trigrams, bigrams_df, query, 
                                       gamma = 0.75){
    if(nrow(observed_trigrams) < 1) return(NULL)
    query = gsub(" ", "_", query, fixed = TRUE)
    obs_count <- bigrams[(bigrams$ngram == query),]$freq
    obs_trigrams_prob <- (observed_trigrams$freq - gamma)/obs_count
    output <- cbind(as.data.frame(observed_trigrams$ngram), obs_trigrams_prob)
    colnames(output) <- c("ngram", "prob")
    row.names(output) <- 1:nrow(output)
    return(output)
}

# UNOBSERVED TRIGRAMS

# Assign probability to unobserved trigrams
# The probability for an unobserved trigram is 
# q_bo(w_i∣w_{i−2},w_{i−1}) = α(w_{i−2},w_{i−1}) * q_bo(w_i∣w_{i−1}) / 
# (∑_{w⊆B(w_{i−2},w_{i−1}) * q_bo(w∣w_{i−1}) where
# α(w_{i−2},w_{i−1}) = 1− ∑_{w⊆A(w_{i−2},w_{i−1}) * (c(w_{i−2},w_{i−1},w) − γ) /
# c(w_{i−2},w_{i−1}) from observed trigrams

# returns words (unigrams) to complete unobserved trigrams
get_unobserved_trigrams <- function(observed_trigrams, unigrams){
    observedlast <- sapply(observed_trigrams$ngram, 
                           FUN = function(y) paste(tail(strsplit(as.character(y), "_")[[1]], 1)))
    output <- unigrams[!(unigrams$ngram %in% observedlast),]$ngram
    return(output)
}

# returns α(w_{i−1}), the discounted probability mass that will be distributed to unobserved bigram
get_alpha_bigrams <- function(query, bigrams, gamma=0.75){
    w_i_1 <- strsplit(query, "_")[[1]][2]
    w_i_1 <- unigrams[unigrams$ngram == w_i_1,]
    bigramcount <- bigrams[grep(paste0("^", w_i_1$ngram, "_"), bigrams$ngram),]
    if (nrow(bigramcount)<1) return(1)
    alpha_big <- 1 - sum((bigramcount$freq - gamma) / w_i_1$freq)
    return(alpha_big)
}

# Calculate q_bo for each bigram
get_bo_bigrams <- function(query, unobserved_trigrams){
    w_i_1 <- strsplit(query, "_")[[1]][2]
    output <- paste0(w_i_1,"_" , unobserved_trigrams)
    return(output)
}

get_obs_bo_bigrams <- function(bigrams, bo_bigrams){
    output <- bigrams[bigrams$ngram %in% bo_bigrams,]
    return(output)
}

get_unobs_bo_bigrams <- function(bigrams, bo_bigrams, obs_bo_bigrams){
    output <- bo_bigrams[!(bo_bigrams %in% obs_bo_bigrams$ngram)]
    return(output)
}

get_obs_bo_bigrams_prob <- function(query, obs_bo_bigrams, unigrams, gamma=0.75){
    w_i_1 <- strsplit(query, "_")[[1]][2]
    w_i_1 <- unigrams[unigrams$ngram == w_i_1,]
    output <- (obs_bo_bigrams$freq - gamma)/w_i_1$freq
    output <- data.frame(ngram = obs_bo_bigrams$ngram, prob = output)
    return(output)
}

get_unobs_bigrams_prob <- function(unobs_bo_bigrams, unigrams, alpha_big){
    unobs_bo_bigrams_tails <- sapply(unobs_bo_bigrams, 
                                     FUN = function(y) paste(tail(strsplit(as.character(y),
                                                                          "_")[[1]],1)))
    unobs_bo_bigrams_tails_table <- unigrams[unigrams$ngram %in% unobs_bo_bigrams_tails,]
    unobs_bo_bigrams2 <- sapply(unobs_bo_bigrams_tails_table$ngram,
                                FUN = function(y) paste("NA_", as.character(y), sep = ""))
    denom <- sum(unobs_bo_bigrams_tails_table$freq)
    output <- data.frame(ngram = unobs_bo_bigrams2, prob = (alpha_big * unobs_bo_bigrams_tails_table$freq / denom))
    return(output)
}


# returns α(w_{i−2},w_{i−1}), the discounted probability mass that will be distributed to unobserved trigrams
get_alpha_trigrams <- function(query, trigrams, gamma=0.75){
    trigramscount <- trigrams[grep(paste0("^", query,"_"), trigrams$ngram),]
    querycount <- trigrams[trigrams$ngram == query,]
    if (nrow(trigramscount)<1) return(1)
    alpha_trig <- 1 - sum((trigramscount$freq - gamma)/querycount$freq)
    return(alpha_trig)
}

# returns unobserved trigram probabilities
get_unobs_trigram_prob <- function(query, Qbo_bigrams, alpha_trig){
    sum_Qbo_bigrams <- sum(Qbo_bigrams$prob)
    unobs_trigrams <- paste(strsplit(query, "_")[[1]][1], Qbo_bigrams$ngram, sep="_")
    output <- alpha_trig * Qbo_bigrams$prob / sum_Qbo_bigrams
    output <- data.frame(ngram = Qbo_bigrams$ngram, prob=output)
    return(output)
}

# SELECT WORD WITH HIGHEST PROBABILITY
get_next_word <- function(obs_trigram_prob, unobs_trigram_prob, num_choices){
    Qbo_trigrams <- rbind(obs_trigram_prob, unobs_trigram_prob)
    Qbo_trigrams <- Qbo_trigrams[order(-Qbo_trigrams$prob),]
    Qbo_trigrams$ngram <- sapply(Qbo_trigrams$ngram, 
                                 FUN= function(y) paste(tail(strsplit(as.character(y), "_")[[1]],1)))
    output <- Qbo_trigrams[1:num_choices,]
    return(output)
}
