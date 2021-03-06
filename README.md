# Word Prediction App

This is one of my first forays into the world of word prediction.  The purpose of this project was to build a Shiny app to predict the next word for a given n-gram.  You can find the finished app [here]().

# Data
For this project I used publicly available social media [data](http://www.corpora.heliohost.org/index.html).  The data contains over 4 million lines of text and over 100 million words.  I sampled approximately 50% of the initial data, removed all non-alphabetic (numbers, punctuation, special characters) characters and converted to lowercase to elminate case sensitivity.  In addition, I removed profanity [words](https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en).

Once the data was cleaned, I primarily used the `tm` and `RWeka` libraries to perform the text analysis which consisted of extracting sequences and frequencies of words (2-, 3-, 4-, & 5-grams).

The data cleansing and and n-gram analysis can be found in the `extracting_n-grams.R` file located [here](https://github.com/bradleyboehmke/word_prediction_app).

# Prediction Algorithm
For the prediction algorithm used to predict the most likely word to follow the given n-gram I applied a [Simple Backoff Algorithm](http://en.wikipedia.org/wiki/Katz%27s_back-off_model).  In a Simple Backoff model the user provides character sequence (n-gram) which is passed to the algorithm.  The user input is then preprocessed in a similar manner as the training data (non-alphabetic characters/expressions removed, forces to lowercase, profanity filtered, etc.).  If th user input sequence contains more than four words only the final four words are selected.  The algorithm then identifies the length of user input and searches for an n-gram that matches.  If match is found, model selects highest probable word that follows.  If no matching n-gram exists, the algorithm "backs-off" by reducing the user input to $n-1$ gram and searches for matching n-gram.  If no match still exists after backing off to smallest n-gram possible, algorithm searches for partial n-gram matches (ie: "building xxx prediction app" and/or "building word xxx app").  If no partial matches exist, algorithm predicts most common single words found in data.

