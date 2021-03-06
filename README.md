Word Prediction using NGrams
========================================================
author: Abdullah M. Mustafa
date: April 1st, 2020
autosize: true

Project Objective:
========================================================

- Auto-completion is a great feature that eases our daily writing tasks.
- We develop this predictive model using a corapora from SwiftKey.
- We built our Ngram model using a relatively large corpora from Blogs, News, and Twitter.
- This project uses Ngrams model to predict the most probable words based on its predecessors.
- The developed App outputs the most probable words and their associated probabilities.

Algorithm:
========================================================
- We start with the [SwiftKey Corpora] (https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip), and a [profanity list] (https://www.cs.cmu.edu/~biglou/resources/bad-words.txt).
- To generate the Ngrams (up to 5-grams):
    - We sample the corpus to reduce computation complexity.
    - we split the sampled corpus into sentences.
    - The sentnences are cleaned to reduce the number of unique words.
    - The cleaned sentences are then tokenized to get the desired Ngram model.
- To predict the next words, we use the [Katz's back-off model] (https://en.wikipedia.org/wiki/Katz%27s_back-off_model) for the obtained Ngrams.

Shiny App Description:
========================================================
- Our[word prediction app] (https://abdullahss.shinyapps.io/Word_Prediction_Ngrams/) is developed in R and published using Shiny servers.
- How to use the app:
    - Enter a sentence to be completed
    - Choose how many words to predict
    - Choose whether to predict common stop words (I, he, she, the, a,...)
- The top predictions are listed with their associated probabilities.
- For unknown words, no predictionsa are shown.

Shiny App Screenshot:
========================================================    
![Screenshot](Capture.PNG)

Conclusions & Future work:
========================================================    

- Based on the english SwiftKey corpora, a predictive Ngrams model was developed using Katz's back-off model.
- To improve the model performance, higher Ngrams should be added for deeper dependancies.
- For future work, the application of [Recurrent Neural Networks (RNN) language models] (https://docs.chainer.org/en/stable/examples/ptb.html) would improve the accuracy of our predictive model. 
