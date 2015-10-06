# Word Prediction App

This is one of my first forays into the world of word prediction.  The purpose of this project was to build a Shiny app to predict the next word for a given n-gram.  

# Data
For this project I used publicly available social media [data](http://www.corpora.heliohost.org/index.html).  The data contains over 4 million lines of text and over 100 million words.  I sampled approximately 50% of the initial data, removed all non-alphabetic (numbers, punctuation, special characters) characters and converted to lowercase to elminate case sensitivity.  In addition, I removed profanity [words](https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en).

Once the data was cleaned, I primarily used the `tm` and `RWeka` libraries to perform the text analysis which consisted of extracting sequences and frequencies of words (2-, 3-, 4-, & 5-grams).

Th
    

