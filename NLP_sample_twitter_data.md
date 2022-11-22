Twitter textual data: scraping and analyzing
================

# Twitter textual data: scraping and analyzing

Renato Russo

### Loading all the required packages

``` r
library(googlesheets4)
library(academictwitteR)
library(widyr)
library(tidytext)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(corpus)
library(tidyr) # used for sentiment analysis plot
library(forcats)
library(tm)
```

    ## Loading required package: NLP

``` r
library(topicmodels) # visualization of topics in the NLP section
library(ggplot2) # chart of co-occurring terms
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     annotate

``` r
library(tidytext) # chart of co-occurring terms and get sentiment in seniment analysis
library(wordcloud) #for wordcloud
```

    ## Loading required package: RColorBrewer

``` r
library(igraph) # for the network plot of co-occurring words
```

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(ggraph) # for the network plot of co-occurring words
library(syuzhet) # for sentiment analysis
library(textdata) # for sentiment analysis
library(gridExtra) # for sentiment analysis grid charts
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(tidyr) # for sentiment analysis
```

In this project, we will be looking at three broader tasks of data
analysis and visualization applied to textual data. More specifically,
we will look at Twitter data, and see methods for **collecting
(scraping)**, **preparing**, and **analyzing** Twitter data.

### 1. Scrape data

The first thing you should do is obtaining data. Fortunately, Twitter
makes it relatively easy to scrape data. All you need to start is a
Twitter Developer Account so you can get access to Twitter API.
Actually, there are a few different types of Developer Account. For the
purpose of this project, I am using my Academic Research account. You
may want to check out [Twitter’s Developer
Portal](https://developer.twitter.com/) for more information. From this
point on, I will assume that you have your Twitter API credentials at
hand ;-)

#### Packages needed/recommended

There are a few packages that help us retrieve data from Twitter. For
this project, I am using
[academictwitteR](https://www.research.ed.ac.uk/en/publications/academictwitter-an-r-package-to-access-the-twitter-academic-resea),
but you may also want to check out
[twitteR](https://cran.r-project.org/web/packages/twitteR/twitteR.pdf)
and [rtweet](https://cran.rstudio.com/web/packages/rtweet/index.html). I
have already experienced a few glitches with the former, which seems to
be updated less frequently than rtweet. Another reason why I am using
academictwitteR now is because it has a more straight forward connection
with Twitter API V2, which we can use to retrieve “historical data” (the
previous API allowed retrieval of data from the past 8-10 days only,
among other limitations). So, let’s get on with the search:

#### Set up credentials

For this step, you will have to look at your profile on Twitter’s
developer portal. Now, go ahead and get your credentials. With
academictwitteR and API V2, I can use the Bearer Token only. The [API
documentation](https://developer.twitter.com/en/docs/authentication/oauth-2-0/bearer-tokens)
and the [package
repository](https://github.com/cjbarrie/academictwitteR) have more
information on that. Now, you have to call the function `set_bearer()`
to store that information for future use.

``` r
# set_bearer()
```

Once you follow the instructions above, you are ready to go ahead and
run your query!

#### Perform the search

Now, you’re finally able to run the search. For that, you will use the
`get_all_tweets()` function. In my case, I am interested in
investigating how sectors of the alt-right have “hijacked” narratives
related to the Capitol attack in January 2021, to assess what type of
vocabulary people were using to refer to different narratives related to
that event. For the purpose of this example, I will limit the number of
tweets retrieved to 5000.

``` r
# capitol_large <- get_all_tweets(query = "capitol", # search query
#                                    start_tweets = "2021-01-05T00:00:00Z", # search start date
#                                    end_tweets = "2021-01-06T23:59:00Z", # search end date
#                                    bearer_token = get_bearer(), # pulling stored bearer toke
#                                    data_path = "data", #where data is stored as series of JSON files
#                                    n = 50000, # total number of tweets retrieved
#                                    is_retweet = FALSE) # excluding retweets

# save(capitol_large, file = "capitol_large.Rda") # save resulting file as Rda

load("capitol_large.Rda") # loading file
capitol <- capitol_large
```

### 2. Prepare (pre-process) data

For this analysis, we will predominantly use the “text” column, that is,
the content of the actual tweet. We could use other data, for example
number of likes, or the timeline of a user, but let’s focus on the
content for now.

#### Removing capitalization, numbers, and punctuation.

The custom function below is used to remove capitalization, and to
remove numbers and punctuation:

``` r
# First, we define the function
clean_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[[:digit:]]+", "", text)
  text <- gsub("[[:punct:]]+", "", text)
  return(text)
}
# Then, apply it to the text content:
capitol$text <- clean_text(capitol$text)
```

We also want to remove stop words, that is, those that do not add
meaning to textual analysis. Before doing that, we will tokenize by
words, which is a process that will be useful in other steps further
ahead. \#### Tokenize words

``` r
capitol_tokens <- capitol %>%
  tidytext::unnest_tokens(word, text) %>% 
  count(id, word)
```

Below, I am using `tidytext`’s standard set of stop words:

``` r
capitol_tokens <- capitol_tokens %>%
  dplyr::anti_join(tidytext::get_stopwords())
```

    ## Joining, by = "word"

However, preliminary plotting showed that the word “capitol” was causing
noise in the charts because it obviously appears in every tweet, so I
decided to remove it from the corpus. I understand that this does not
harm the analysis, because we are not interested in the connections with
the word, but in the general meaning of tweets containing that word.

``` r
my_stopwords <- tibble(word = c(as.character(1:10),
                                    "capitol", "just", "now",
                                "right now", "get", "like"))
capitol_tokens <- capitol_tokens %>%
  anti_join(my_stopwords)
```

    ## Joining, by = "word"

I will also “stem” words, that is, combine words that share the same
“root.” This is important because preliminary analysis showed the
existence of words that have similar seemantic value like “storm” and
“storming”

``` r
capitol_tokens$word <- text_tokens(capitol_tokens$word, stemmer = "en") # english stemmer
```

#### Document-term matrix

The document-term matrix (dtm) is “a mathematical matrix that describes
the frequency of terms that occur in a collection of documents” \[3\].
In this case, each tweet is stored as a document, and each word in the
tweets is a term. In other words, the matrix shows which words appear in
each tweet.

``` r
DTM <- capitol_tokens %>%
  tidytext::cast_dtm(id, word, n)
DTM
```

    ## <<DocumentTermMatrix (documents: 49962, terms: 68859)>>
    ## Non-/sparse entries: 647486/3439685872
    ## Sparsity           : 100%
    ## Maximal term length: 254
    ## Weighting          : term frequency (tf)

Looking at the structure of the document-term matrix, we see that this
specific dtm has 49,962 documents (tweets) and 68,859 terms (unique
words). The dtm will be used for exploratory analysis in the next
section.

### 3. Analyze data

#### Exploratory analysis

First, let’s look at the frequency of words. For that, we apply
`group_by()` and `summarize()` to the tokens dataset to find out the
frequency:

``` r
capitol_tokens %>%
  group_by(word) %>%
  summarize(occurrence = sum(n)) %>%
  arrange(desc(occurrence))
```

    ## # A tibble: 68,859 × 2
    ##    word      occurrence
    ##    <list>         <int>
    ##  1 <chr [1]>      10533
    ##  2 <chr [1]>       9437
    ##  3 <chr [1]>       7857
    ##  4 <chr [1]>       7817
    ##  5 <chr [1]>       6956
    ##  6 <chr [1]>       6170
    ##  7 <chr [1]>       5998
    ##  8 <chr [1]>       4689
    ##  9 <chr [1]>       4248
    ## 10 <chr [1]>       4175
    ## # … with 68,849 more rows

As seen above, the resulting tibble shows only the description of the
words as list objects `<chr [1]>`, but not the words themselves, so I
create a new tibble that “reveals the words:”

``` r
tokens_words <- capitol_tokens %>% # creating a data frame with the frequencies
  group_by(word) %>%
  summarise(N = n())
class(tokens_words)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
tokens_words$word <- as.character(tokens_words$word) # turning the tokens into character objects
```

Below, we can see the list of words by frequency. We notice that there
is a “first tier” of frequency containing the words “trump” and “storm,”
then “build,” and “us,” as the most frequent ones. “Storm,” “build,” and
“polic” represent word stems, so words like “building” are within the
“build” term, for example.

``` r
(tokens_words %>%
  group_by(word) %>%
  arrange(desc(N))
)
```

    ## # A tibble: 68,859 × 2
    ## # Groups:   word [68,859]
    ##    word        N
    ##    <chr>   <int>
    ##  1 trump    9426
    ##  2 storm    9348
    ##  3 build    7593
    ##  4 us       7406
    ##  5 polic    6351
    ##  6 today    5899
    ##  7 peopl    5317
    ##  8 protest  4500
    ##  9 support  4034
    ## 10 shot     3996
    ## # … with 68,849 more rows

And we can plot the word frequency in a bar chart:

``` r
tokens_words %>%
  group_by(word) %>%
  filter(N > 3500) %>%
  ggplot(aes(x = fct_reorder(word, N, .desc = TRUE), y = N, fill = N)) +
  geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Term (stemmed)") +
  theme_classic()
```

![](NLP_sample_twitter_data_files/figure-gfm/word_frequency_chart-1.png)<!-- -->

Another way to visualize the frequency of words is with a wordcloud.
Although not always a good choice, in this case a wordcloud allows for
visualizations of other terms that don’t fit in the bar chart.

``` r
set.seed(1234) # for reproducibility

pal <- brewer.pal(9, "BuGn") # setting the color palette
pal <- pal[-(1:5)] # this improves readability by narrowing the range of shades of the palette showing in the chart

wordcloud(words = tokens_words$word,
          freq = tokens_words$N,
          min.freq = 1150,
          max.words=100,
          random.order=FALSE,
          rot.per=0.35,
          colors=pal)
```

![](NLP_sample_twitter_data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Natural language processing

##### Visualize topics with Latent Dirichlet Allocation (LDA)

Latent Dirichlet Allocation is an approach to statistical topic modeling
in which documents are represented as a set of topics and a topic is a
set of words. The topics are situated in a “latent layer.” Put in a very
simple way, this type of modeling compares the presence of a term in a
document and in a topic, and uses that comparison to establish the
probability that the document is part of the topic \[4\]. This type of
modeling is used here to identify the topics and the co-occurring terms,
that is, terms that appear together in the tweets. The code below
performs the topic allocation. Initial runs of this and the following
parts revealed that the topics were too similar, so I proceeded with the
creation of only 2, which seemed to capture the meaning in tweets
satisfactorily.

``` r
LDA <- topicmodels::LDA(DTM,
                        k = 2, # number of topics
                        control = list(seed = 123))

LDA_td <- tidytext::tidy(LDA) #tidying the LDA object
```

##### Visualize co-occurring terms

The code below creates a tibble containing topics with 5 terms each.

``` r
topTerms <- LDA_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>% # 5 main terms that will later be plotted
  arrange(topic, -beta) # arranging topics in descending order of beta coefficient
```

Then, we plot the topics.

``` r
topTerms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_x") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Topics in tweets containing the term 'capitol' in January 6, 2021")
```

![](NLP_sample_twitter_data_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The process of identifying the number of topics and terms in each topic
was iterative in this case. Preliminary plotting showed that there was
strong intersection between topics, so I kept reducing the number until
I found two that seemed relevant. Apparently, topic \#1 seems to refer
to police reaction to the attacks by the supporters, whereas topic \#2
refers to the process of protesters (stem “protest”) storming the
capitol building (stem “build”).

Another way to visualize the relationships in the corpus is by
identifying the frequency of word pairs present in the corpus. The code
below generates a data

``` r
word_pairs_capitol <- capitol_tokens %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
```

##### Network plot of co-occurring words

``` r
word_pairs_capitol %>%
  filter(n >= 800) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.5, "lines")) +
  theme_void() +
  labs(title = "Co-occurring terms in tweets containing the term 'capitol' in January 6, 2021")
```

![](NLP_sample_twitter_data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The plot above shows a few potentially interesting aspects of this
corpus, among which we highlight two. First, there is a connection
between the terms “terrorist” and “storm,” possibly because users were
describing the attacks as acts of terrorism. “Storm” is also strongly
connected with “Trump,” and a little less so with “build,” “US,”
“people,” and “support.” Another important cluster (at the top mid-left)
refers to the (woman who, unfortunately, died during the attack)
(<https://www.nytimes.com/2021/01/06/us/politics/woman-shot-capitol-dead.html>)
; that cluster is connected to “police” and and “people.” Another aspect
is the proximity of “trump” to the center of the graph, strongly
connected with “support” (possibly stemmed from “supporter/s”).
“Support” is also strongly connected with “build” and “storm.” This is
compatible with the pairwise chart, in which one of the topics is
possibly associated with the police reaction to the attacks.

##### Sentiment analysis

``` r
tweets <- iconv(capitol$text) # creating a tibble containing the text

sentiment <- get_nrc_sentiment(tweets) # this uses the nrc dictionary to assign sentiment scores for each tweet
```

    ## Warning: `spread_()` was deprecated in tidyr 1.2.0.
    ## ℹ Please use `spread()` instead.
    ## ℹ The deprecated feature was likely used in the syuzhet package.
    ##   Please report the issue to the authors.

Now that we have a tibble with the sentiment score for each tweet, we
can print the first results for sentiment and for tweets, if we want to
compare:

``` r
head(sentiment)
```

    ##   anger anticipation disgust fear joy sadness surprise trust negative positive
    ## 1     1            0       2    2   0       2        0     0        2        0
    ## 2     0            0       0    0   0       0        0     0        0        0
    ## 3     0            0       0    0   0       0        0     0        0        1
    ## 4     3            0       0    2   0       1        1     1        3        1
    ## 5     0            0       0    0   0       0        0     0        0        0
    ## 6     0            0       0    0   0       0        0     0        0        0

``` r
head(tweets)
```

    ## [1] "stunning and sickening shame on those in power who have stoked these flames httpstcoutiqpkwf"                                                                                                                                                                                                     
    ## [2] "could have nothing to do with the events at the capitol who knows httpstcoxmdslvavo"                                                                                                                                                                                                              
    ## [3] "an amazing story of how the terrorists organized this afternoon via rightwing social media\n\nhttpstcosaohppz\n\nsubscription may be necessary"                                                                                                                                                   
    ## [4] "trumpsupporters storm the capitolbuilding during protest \ndaalighttv breakingnews america capitolhill anarchy trump crazy maga wtf amerikkka  united states capitol httpstcojnwlovzlq"                                                                                                           
    ## [5] "يسارع أعضاء الكونغرس للاختباء بينما يحاول متظاهرون دخول غرفة مجلس النواب خلال جلسة مشتركة للتصديق على فوز بايدن وضباط حراسة الكابيتول يوجهون أسلحتهم صوب أحد الأبواب التي تتعرض للتخريب\nتابع آخر المستجدات الأميركية على الرابط\nhttpstcouihihdylb\nاندبندنتعربيةتغنيك\ncapitol httpstcoafnkeudp"
    ## [6] "asharangappa it sure was challenging i also had to monitor the crowds at the tx capitol"

We can also see the frequency of each sentiment:

``` r
sentiment_sums <- sentiment %>%
  summarise_all(sum)

sentiment_sums = gather(sentiment, key = "S_sum") %>%
    group_by(S_sum) %>%
    summarize(number = sum(value, na.rm = TRUE))
sentiment_sums
```

    ## # A tibble: 10 × 2
    ##    S_sum        number
    ##    <chr>         <dbl>
    ##  1 anger         44921
    ##  2 anticipation  20408
    ##  3 disgust       17959
    ##  4 fear          46277
    ##  5 joy           13690
    ##  6 negative      62327
    ##  7 positive      52953
    ##  8 sadness       27474
    ##  9 surprise      26642
    ## 10 trust         37676

And create a bar plot with that frequency:

``` r
ggplot(data = sentiment_sums,
       aes(x = fct_reorder(S_sum,
                           number,
                           .desc = TRUE),
           y = number,
           fill = number)) +
  geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Sentiment") +
  theme_classic() +
  coord_flip()
```

![](NLP_sample_twitter_data_files/figure-gfm/Sentiment_frequency_chart-1.png)<!-- -->

One other nlp task we can implement is identifying words associated with
each “polarity” (positive and negative):

``` r
nrc_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

tokens_words %>%
  inner_join(nrc_positive) %>%
  arrange(desc(N))
```

    ## Joining, by = "word"

    ## # A tibble: 574 × 3
    ##    word        N sentiment
    ##    <chr>   <int> <chr>    
    ##  1 build    7593 positive 
    ##  2 elect    1910 positive 
    ##  3 guard    1089 positive 
    ##  4 vote     1025 positive 
    ##  5 leader    740 positive 
    ##  6 protect   726 positive 
    ##  7 count     673 positive 
    ##  8 love      670 positive 
    ##  9 actual    664 positive 
    ## 10 govern    647 positive 
    ## # … with 564 more rows

Ok, now we see that “build” is classified as a positive term, and this
adds an unintended bias to the analysis. “Building” is classified as
positive by the NRC dictionary (possibly because of its verb form), and
this may not be true in this case. In this corpus, “building” is a
rather descriptive word, and apparently is mostly used as a noun.

So, let’s build a chart that shows the frequency of positive terms,
excluding the term “biuld.”

``` r
plot_positive <- tokens_words %>%
  inner_join(nrc_positive) %>%
  arrange(desc(N)) %>%
  filter(N  > 500 & N < 5000) %>%
  ggplot(aes(x = fct_reorder(word, N, .desc = TRUE), y = N, fill = N)) +
           geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Positive terms (stemmed)") +
  coord_flip()
```

    ## Joining, by = "word"

``` r
plot_positive
```

![](NLP_sample_twitter_data_files/figure-gfm/polarity_frequency_chart-1.png)<!-- -->
And we can do the same for negative:

``` r
nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

tokens_words %>%
  inner_join(nrc_negative) %>%
  arrange(desc(N))
```

    ## Joining, by = "word"

    ## # A tibble: 901 × 3
    ##    word          N sentiment
    ##    <chr>     <int> <chr>    
    ##  1 storm      9348 negative 
    ##  2 shot       3996 negative 
    ##  3 terrorist  3379 negative 
    ##  4 die        2558 negative 
    ##  5 mob        2332 negative 
    ##  6 riot       2124 negative 
    ##  7 breach     2013 negative 
    ##  8 attack     1999 negative 
    ##  9 arrest     1958 negative 
    ## 10 violent    1315 negative 
    ## # … with 891 more rows

``` r
tokens_words %>%
  inner_join(nrc_negative) %>%
  arrange(desc(N)) %>%
  filter(N  > 900) %>%
  ggplot(aes(x = fct_reorder(word, N, .desc = TRUE), y = N, fill = N)) +
           geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Negative terms (stemmed)") +
  coord_flip()
```

    ## Joining, by = "word"

![](NLP_sample_twitter_data_files/figure-gfm/polarity_negative-1.png)<!-- -->
Something slightly similar happens to “storm” among the negative terms:
It happens with strikingly higher frequency than the rest of the terms,
so, for this exercise, I’ll remove set the range so that the chart
includes most frequent words except for “storm:”

``` r
plot_negative <- tokens_words %>%
  inner_join(nrc_negative) %>%
  arrange(desc(N)) %>%
  filter(N  > 900 & N < 5000) %>%
  ggplot(aes(x = fct_reorder(word, N, .desc = TRUE), y = N, fill = N)) +
           geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Negative terms (stemmed)") +
  coord_flip()
```

    ## Joining, by = "word"

``` r
plot_negative
```

![](NLP_sample_twitter_data_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

As we see, the list of negative terms include words that are possibly
related to criticism towards the “methods” of the crowd invading the
capitol: they are possibly described as a **terrorist** **mob** that was
connected to someone being **shot** dead (**die**). Of course, more
precise conclusions would require deeper analysis, but these words give
a sense of the negative vocabulary used to describe the event.

We can present both negative and positive together using
`grid.arrange()`:

``` r
plot_negative <- tokens_words %>%
  inner_join(nrc_negative) %>%
  arrange(desc(N)) %>%
  filter(N  > 900 & N < 5000) %>%
  ggplot(aes(x = fct_reorder(word, N, .desc = TRUE), y = N, fill = N)) +
           geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Negative terms (stemmed)") +
  coord_flip()
```

    ## Joining, by = "word"

``` r
plot_positive <- tokens_words %>%
  inner_join(nrc_positive) %>%
  arrange(desc(N)) %>%
  filter(N  > 500 & N < 5000) %>%
  ggplot(aes(x = fct_reorder(word, N, .desc = TRUE), y = N, fill = N)) +
           geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Positive terms (stemmed)") +
  coord_flip()
```

    ## Joining, by = "word"

``` r
grid.arrange(plot_negative, plot_positive)
```

![](NLP_sample_twitter_data_files/figure-gfm/sentiment_grid_arrange-1.png)<!-- -->

### 4. Conclusion

With this project, I covered three main steps in textual analysis of
Twitter data. First, I used Twitter API V2 to scrape tweets of a topic
of interest within a time range. Second, I prepared the data by removing
punctuation, stemming, and stop words – both standard ones and those
which preliminary analysis had shown to cause overplotting in later
stages. Finally, I conduct some tasks in textual analysis. At that last
stage, I first conducted an exploratory analysis by identifying most
frequent terms in the corpus. I also used a word cloud to offer an
alternative visualization to word frequency. Then, I undertook natural
language processing tasks. Through Latent Dirichlet Allocation, I showed
two of the most salient topics in the data. Additionally, I plotted
bi-grams, which demonstrate strength of relationship between words, and
“clusters” where words concentrate. Finally, I implemented sentiment
analysis to identify terms and frequencies across sentiments described
by the `ncr` dictionary.

The most challenging part of carrying out this project was handling data
objects, and this is mainly because the object returned by the search is
a dataframe with other dataframes nested. Because of that, saving a csv
file was not as straightforward as I was used to. Therefore, it was a
little confusing to define a strategy to deal with files: should I use a
Rda file or save only the columns I needed as csv files? With saved
files, I was not sure how they would behave when someone opened the .rmd
file in another computer. I ended up sticking with the .rda file,
because it seemed the most natural choice as a format in which files
“carry” all the information needed for analysis.

##### References:

\[1\]
<https://jtr13.github.io/cc21/twitter-sentiment-analysis-in-r.html#word-frequency-plot>

\[2\]
<https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a>

\[3\] DTM:
<https://bookdown.org/Maxine/tidy-text-mining/tidying-a-document-term-matrix.html>

\[4\] LDA:
<https://blog.marketmuse.com/glossary/latent-dirichlet-allocation-definition/>

\[5\] Sentiment analysis:
<https://finnstats.com/index.php/2021/05/16/sentiment-analysis-in-r/>

\[6\] Sentiment analysis:
<https://www.tidytextmining.com/sentiment.html>

\[7\] THE NRC WORD-EMOTION ASSOCIATION LEXICON (aka NRC Emotion Lexicon,
aka EmoLex): <http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm>
