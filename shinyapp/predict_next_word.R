predict_next_word <- function(gamma, query, num_choices, unigrams, bigrams, trigrams){
    # keep the two last words of the query because it just uses trigrams
    query <- tolower(sub('.*?(\\w+\\W+\\w+)\\W*?$', '\\1',query))
    # calculate probability
    observed_trigrams <- get_observed_trigrams(query, trigrams)
    observed_trigrams_prob <- get_observed_trigrams_prob(observed_trigrams, bigrams, query, gamma)
    unobserved_trigrams <- get_unobserved_trigrams(observed_trigrams, unigrams)
    alpha_bigrams <- get_alpha_bigrams(query, bigrams, gamma)
    bo_bigrams <- get_bo_bigrams(query, unobserved_trigrams)
    observed_bo_bigrams <- get_obs_bo_bigrams(bigrams, bo_bigrams)
    unobserved_bo_bigrams <- get_unobs_bo_bigrams(bigrams, bo_bigrams, observed_bo_bigrams)
    observed_bo_bigrams_prob <- get_obs_bo_bigrams_prob(query, observed_bo_bigrams, unigrams, gamma)
    unobserved_bo_bigrams_prob <- get_unobs_bigrams_prob(unobserved_bo_bigrams, unigrams, alpha_bigrams)
    Qbo_bigrams <- rbind(observed_bo_bigrams_prob, unobserved_bo_bigrams_prob)
    alpha_trigrams <- get_alpha_trigrams(query, trigrams, gamma)
    unobserved_trigrams_prob <- get_unobs_trigram_prob(query, Qbo_bigrams, alpha_trigrams)
    output <- get_next_word(observed_trigrams_prob, unobserved_trigrams_prob, num_choices)
    return(output)
}
