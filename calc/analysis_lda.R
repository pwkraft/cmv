library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quanteda)
library(topicmodels)

## load data
load("out/cmv_data.Rdata")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))



##################################
### Topic overview of discussions
##################################

## prepare dfm
dfm_op <- data_op[,c("title","text")] %>% 
  apply(1, paste, collapse=" ") %>% corpus() %>% 
  tokens(remove_numbers = T, remove_punct = T, 
         remove_symbols = T, remove_twitter = T, 
         remove_hyphens = T, remove_url = T) %>%
  dfm(stem = T, remove = c("cmv", stopwords("english"))) %>%
  dfm_trim(., min_count = 10, min_docfreq = 10) %>%
  convert(to = "topicmodels")

## estimate topic model
lda <- LDA(dfm_op, k=20, method = "Gibbs",
           control=list(seed=123456, iter=2000, burnin=1000))

## prepare data frame for plotting
tibble(terms = apply(terms(lda, 10),2,paste, collapse=", "),
       props = apply(posterior(lda)$topics, 2, mean),
       maxtopic = table(topics(lda)),
       pmaxtopic = maxtopic/sum(maxtopic)) %>%
  ggplot(aes(y=reorder(terms, props), x=props)) + 
  geom_segment(aes(x=0, xend=props, yend=terms)) +
  geom_point() + 
  labs(y="") +
  plot_default