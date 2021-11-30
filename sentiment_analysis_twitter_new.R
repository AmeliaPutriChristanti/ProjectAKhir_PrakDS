library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)
library(twitteR)
library(wordcloud)
library(shiny)


## ambil API dari developer Twitter
api_key <- "nTAKExJAWArEXLXvA3dRaqIPm"
api_secret_key <- "xQKLmpMJiKuFwLvI0xOYzQd89FzLi5yBC4zrog26gFljxXaJwt"

## authenticate via web browser
token <- create_token(
  app = "project_rivanka_lia",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

#fungsi untuk mengatur folder
setwd("C:\\Users\\amel\\Documents\\Project\\")


#Cari tweet tentang topik hotel, 
#persempit jumlah tweet yang diinginkan dan putuskan untuk memasukkan retweet atau tidak.
#mencari dalam bahasa inggris
kata <- search_tweets("hotel", n=1000, include_rts = FALSE, lang ="en")


#simpan data kedalam format csv untuk dataset yang telah dikumpulkan
save_as_csv(kata, "dataset\\dataset_hotel.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
kata = read.csv("dataset\\dataset_hotel.csv")

#Proses setiap set tweet menjadi teks rapi atau objek corpus.
tweet.Kata = kata %>% select(screen_name, text)
tweet.Kata
head(tweet.Kata$text)
save_as_csv(tweet.Kata, "dataset\\dataset_hotel_corpus.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


#menghapus element http
tweet.Kata$stripped_text1 <- gsub("http\\S+","",tweet.Kata$text)

# menghapus element amp
tweet.Kata$stripped_text1 = gsub("&amp;","",tweet.Kata$stripped_text1)

#menghapus element baris baru
tweet.Kata$stripped_text1 <- gsub("\n","",tweet.Kata$stripped_text1)

#menghapus element RT
tweet.Kata$stripped_text1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweet.Kata$stripped_text1)

#menghapus element Quotes
tweet.Kata$stripped_text1 = gsub("'s|'s|[...]","",tweet.Kata$stripped_text1)

# menghapus element tag seseorang
tweet.Kata$stripped_text1 = gsub("@\\w+","",tweet.Kata$stripped_text1)

# menghapus element angka
tweet.Kata$stripped_text1 = gsub("[[:digit:]]","",tweet.Kata$stripped_text1)

# menghapus element tanda baca
tweet.Kata$stripped_text1 = gsub("[[:punct:]]","",tweet.Kata$stripped_text1)
 

try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
#mengubah dari upercase menjadi lower case
tweet.Kata$stripped_text1 = sapply(tweet.Kata$stripped_text1, try.error)

#menghapus NA
tweet.Kata$stripped_text1 = tweet.Kata$stripped_text1[!is.na(tweet.Kata$stripped_text1)]
names(tweet.Kata$stripped_text1) = NULL



#gunakan fungsi unnest_tokens() untuk membagi perkata
#hapus tanda baca, dan id untuk setiap tweet
tweet.Kata_stem <- tweet.Kata %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)
head(tweet.Kata_stem)


#hapus kata-kata stopwords dari daftar kata-kata
cleaned_tweets.Kata <- tweet.Kata_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Kata)
save_as_csv(tweet.Kata, "cleaning data\\clean_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
head(tweet.Kata$text)



# Naive bayes
library(e1071)
library(caret)
library(syuzhet)

#digunakan untuk membaca file csv yang sudah di cleaning data
hotel_dataset <-read.csv("cleaning data\\clean_data.csv",stringsAsFactors = FALSE)

#digunakan untuk mengeset variabel cloumn text menjadi char
review <- as.character(hotel_dataset$stripped_text1)

#memanggil sentimen dictionary untuk menghitung presentasi dari beberapa emotion dan mengubahnya ke dalam text file
get_nrc_sentiment('happy')
get_nrc_sentiment('excitement')
s<-get_nrc_sentiment(review)
review_combine<-cbind(hotel_dataset$stripped_text1,s)
par(mar=rep(3,4))
barplot(colSums(s),col=rainbow(10),ylab='count',main='sentiment analisis hotel')




#20 kata teratas di tweet hotel
cleaned_tweets.Kata %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique word",
       tittle="Unique word counts found in hotel Tweets")


#Untuk melakukan analisis sentimen menggunakan Bing di tweet Hotel, 
#perintah berikut ini mengembalikan sebuah tibble.
bing_kata = cleaned_tweets.Kata %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()


#visualisasi jumlah kata, 
#Anda dapat memfilter dan memplot kata-kata bersebelahan untuk membandingkan 
#emosi positif dan negatif.
bing_kata %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment))+ 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(title = "Tweets containing 'hotel'", y = "Contribution to sentiment", 
       x = NULL) + coord_flip() + theme_bw()


#Fungsi untuk mendapatkan skor sentimen untuk setiap tweet 
sentiment_bing = function(twt){
  twt_tbl = tibble(text = twt) %>%
    mutate(
      stripped_text = gsub("http\\S+","",text)
    )%>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment, sort = TRUE) %>%
    ungroup() %>%
    #buat kolom "skor", yang menetapkan -1 untuk semua kata negatif, 
    #dan 1 untuk kata positif
    mutate(
      score = case_when(
        sentiment == 'negative'~n*(-1),
        sentiment == 'positive'~n*1)
    )
  #menghitung total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #jika tidak ada kata, skor adalah 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #selainnya, jumlah positif dan negatif
  )
  #untuk melacak tweet mana yang tidak mengandung kata sama sekali dari daftar bing
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1: tidak ada kata sama sekali, zero = no
    nrow(twt_tbl)>0~"Type 2" #Type 2: nol berarti jumlah kata = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}



#menerapkan fungsi
#Fungsi lapply mengembalikan list semua skor sentimen, jenis, dan tabel tweet
kata_sent = lapply(kata$text, function(x){sentiment_bing(x)})   
kata_sent


#membuat tibble yang menentukan kata, skor, dan jenisnya
kata_sentiment = bind_rows(tibble(kata = 'hotel', 
                                  score = unlist(map(kata_sent, 'score')), 
                                  type = unlist(map(kata_sent, 'type'))))

#kita dapat melihat beberapa karakteristik sentimen di setiap kelompok. 
#Berikut adalah histogram sentimen tweet.
ggplot(kata_sentiment, aes(x=score, fill = kata)) + 
  geom_histogram(bins = 15, alpha= .6) + facet_grid(~kata) + theme_bw()

#Wordcloud
#Library untuk membuat wordcloud
library(wordcloud)
#Library untuk penggunaan corpus dalam cleaning data
library(tm)
library(RTextTools)
#Library yang terdapat sebuah algoritma naivebayes
library(e1071)
library(dplyr)
library(caret)

#Membaca data yang sudah dibersihkan 
df<-read.csv("C:/Users/amel/Document/Project/cleaning data/clean_data.cvs",stringsAsFactors = FALSE)
glimpse(df)

#Set the seed of R's random number generator, which is useful for creating simulations or random objects that can be reproduced.
set.seed(20)
df<-df[sample(nrow(df)),]
df<-df[sample(nrow(df)),]
glimpse(df)

corpus<-Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:10])

#Fungsinya untuk membersihkan data-data yang tidak dibutuhkan 
corpus.clean<-corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords,stopwords(kind="en"))%>%
  tm_map(stripWhitespace)
dtm<-DocumentTermMatrix(corpus.clean)

inspect(dtm[1:10,1:20])

df.train<-df[1:50,]
df.test<-df[51:100,]

dtm.train<-dtm[1:50,]
dtm.test<-dtm[51:100,]

corpus.clean.train<-corpus.clean[1:50]
corpus.clean.test<-corpus.clean[51:100]

dim(dtm.train)
fivefreq<-findFreqTerms(dtm.train,5)
length(fivefreq)

dtm.train.nb<-DocumentTermMatrix(corpus.clean.train,control = list(dictionary=fivefreq))

#dim(dtm.train.nb)

dtm.test.nb<-DocumentTermMatrix(corpus.clean.test,control = list(dictionary=fivefreq))

dim(dtm.test.nb)

convert_count <- function(x){
  y<-ifelse(x>0,1,0)
  y<-factor(y,levels=c(0,1),labels=c("no","yes"))
  y
}
trainNB<-apply(dtm.train.nb,2,convert_count)
testNB<-apply(dtm.test.nb,1,convert_count)

#Generate wordcloud
wordcloud(corpus.clean,min.freq = 4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))

  

  
#Tampilan Shiny
#membuka file csv
twitter <- read.csv(file="dataset\\dataset_hotel.csv", header=TRUE)
#membuka text file pada data frame twitter
tweet <- twitter$text

#UI
#bagian yang mengatur tampilan web
ui <- fluidPage(
  titlePanel("Penggunaan Kata Hotel Pada Twitter"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Scatterplot", plotOutput("scatterplot")),
                tabPanel("Data Twitter", DT::dataTableOutput("tbl")),
                tabPanel("Wordcloud", plotOutput("Wordcloud"))
    )
  )
)

#SERVER
#Bagian dimana data akan dianalisis dan diproses
server <- function(input, output){
  
  #output data
  output$tbl = DT::renderDataTable({
    DT::datatable(twitter, options = list(lenghtchange = FALSE))
  })
  
  #barplot
  output$scatterplot <- renderPlot({dataset_hotel<-read.csv("dataset\\dataset_hotel.csv",stringsAsFactors = FALSE)
  review < as.character(dataset_hotel$text)
  get_nrc_sentiment("happy")
  get_nrc_sentiment("excitement")
  s<-get_nrc_sentiment(review)
  review_combine<-cbind(dataset_hotel$text,s)
  par(mar=rep(3,4))
  barplot(colSums(s),col=rainbow(10),ylab='count', main='Sentimen Analisis')
  }, height=400)
  
  #wordcloud
  output$wordcl <- renderPlot({
    wordcloud(corpus.clean,min.freq = 4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))
  })
}

shinyApp(ui = ui, server = server)