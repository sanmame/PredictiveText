create_dfm <- function(corpus, n){
    require(quanteda)
    new_dfm <- dfm(corpus, what = "word",
                   remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE, 
                   remove_separators = TRUE, 
                   remove_twitter = TRUE,
                   remove_url = TRUE,
                   ngrams = n)
    return(new_dfm)
}
create_freq_dt <- function(corpus){
    require(data.table)
    dtf_freq <- docfreq(corpus)
    new_dt <- data.table(ngram=names(dtf_freq), freq=dtf_freq)
    return(new_dt)
}

