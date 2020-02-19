data_cleanup <- function(text){
    require(quanteda)
    # Helper functions
    removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
    removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
    
    # Data clean-up
    texts(text) <- lapply(texts(text), removeURL)
    texts(text) <- lapply(texts(text), removeSpecialChars)
}

data_cleanup_twitter <- function(text){
    require(quanteda)
    # Helper functions
    removeHashTags <- function(x) gsub("#\\S+", "", x)
    removeTwitterHandles <- function(x) gsub("@\\S+", "", x)
    
    # Remove handles and hashtags from the Twitter texts
    texts(text) <- lapply(texts(text), removeHashTags)
    texts(text) <- lapply(texts(text), removeTwitterHandles)
}