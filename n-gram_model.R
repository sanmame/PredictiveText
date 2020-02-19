# Download and unzip the data
if (!file.exists("raw_data.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                  destfile = "raw_data.zip", quiet = FALSE, method="auto")}

if (!file.exists("rawdata")){
    unzip(zipfile = "raw_data.zip", overwrite = TRUE)
}
res = file.rename("final","rawdata")

# Read the files
language = 'en_US' # ('de_DE','en_US','fi_FI','ru_RU')
sources = c('blogs','news','twitter')

require(readtext)
rdata = list()
for (src in sources){
    path = file.path("rawdata", language, paste(language,".",src,".txt",sep = "") )
    rdata[[src]] = readtext(path, encoding = 'UTF-8')
}

# Create and sample corpus
require(quanteda)
corpus_blogs <- quanteda::corpus_reshape(corpus(rdata$blogs), to = "sentences")
corpus_news <- quanteda::corpus_reshape(corpus(rdata$news), to = "sentences")
corpus_twitter <- quanteda::corpus_reshape(corpus(rdata$twitter), to = "sentences")

set.seed(236)
corpus_blogs <- corpus_sample(corpus_blogs, size=length(corpus_blogs$documents$texts)*0.005)
corpus_news <- corpus_sample(corpus_blogs, size=length(corpus_blogs$documents$texts)*0.005)
corpus_twitter <- corpus_sample(corpus_blogs, size=length(corpus_blogs$documents$texts)*0.005)

corpus <- corpus_blogs + corpus_news + corpus_twitter

# Remove profanity
# corpus <- filter_profanity(corpus)

#remove previous objects to free up memory
rm(rdata)
rm(corpus_blogs)
rm(corpus_news)
rm(corpus_twitter)

# Create data table for frequencies from dfm for n-grams
unigrams <- create_freq_dt(create_dfm(corpus, 1))
unigrams <- unigrams[order(-unigrams$freq),]
bigrams <- create_freq_dt(create_dfm(corpus, 2))
bigrams <- bigrams[order(-bigrams$freq),]
trigrams <- create_freq_dt(create_dfm(corpus, 3))
trigrams <- trigrams[order(-trigrams$freq),]

rm(corpus)

# predict next word
predict_next_word <- function(gamma, query, num_choices, unigrams, bigrams, trigrams){
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

query = "i love"

predict_next_word(gamma = 0.75, 
                  query = query, 
                  num_choices = 3,
                  unigrams = unigrams, 
                  bigrams = bigrams, 
                  trigrams = trigrams)