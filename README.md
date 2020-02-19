# PredictiveTextProduct

Capstone project for the Johns Hopkins' Data Science specialization.

The model makes typing on mobile devices easier by providing the choices with the highest probability to be typed by a user given the previous words and context of the sentence. The final data product is a Shiny app.

The data used to train the model consists of a corpus of texts collected from publicly available sources by a web crawler in English. Each entry is tagged with the type of entry, based on the type of website it is collected from (a newspaper, a blog or Twitter). Once the raw corpus was collected, it was parsed further, to remove duplicate entries and split into individual lines. The entries are anonymised. The final corpus is divided into blogs, news, and twitter source files and can be downloaded from [this site](https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html).

Link to Shiny app: https://sanmame.shinyapps.io/text_predictor/
