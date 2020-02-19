filter_profanity <- function(text){
    if (!file.exists("profanity.txt")) {
        download.file(url = "http://www.bannedwordlist.com/lists/swearWords.txt", 
                      destfile = "profanity.txt", quiet = FALSE, method="auto")
    }
    require(stringr)
    profanity <- readLines("profanity.txt", skipNul = TRUE, warn = FALSE)
    text = lapply(text, str_remove, profanity) #this is not working with corpus
    return(text)
}
