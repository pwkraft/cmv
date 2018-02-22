library(rjson)
library(dplyr)
library(purrr)

rm(list=ls())



############################################
### Paired data (only look at training set!)
############################################

## load paired data
raw_pair <- readLines("~/Dropbox/Uni/Data/CMV/pair_task/train_pair_data.jsonlist") %>%
  map(rjson::fromJSON)

## examine data
ls(raw_pair[[1]])

## op information
raw_pair[[1]]$op_author
raw_pair[[1]]$op_title
raw_pair[[1]]$op_text
raw_pair[[1]]$op_name

## information about response that changed view (positive)
ls(raw_pair[[1]]$positive)
raw_pair[[1]]$positive$author
raw_pair[[1]]$positive$ancestor
ls(raw_pair[[1]]$positive$comments[[1]])
raw_pair[[1]]$positive$comments[[1]]$body
raw_pair[[1]]$positive$comments[[1]]$author

## information about response that did not change view (negative)
ls(raw_pair[[1]]$negative)
raw_pair[[1]]$negative$author
raw_pair[[1]]$negative$ancestor
ls(raw_pair[[1]]$negative$comments[[1]])
raw_pair[[1]]$negative$comments[[1]]$author
raw_pair[[1]]$negative$comments[[1]]$body

## instances contain different numbers of replies
out <- matrix(NA, ncol=2, nrow=length(raw_pair))
for(i in 1:length(raw_pair)){
  out[i,] <- c(length(raw_pair[[i]]$positive$comments)
               , length(raw_pair[[i]]$negative$comments))
  
}
table(out[,1],out[,2])
which(out[,1]==4)[1]
raw_pair[[10]]$positive$comments

## all replies in one instance are by the same author -> combine all!
out <- rep(NA, length(raw_pair))
for(i in 1:length(raw_pair)){
  authors <- raw_pair[[i]]$positive$author
  for(j in 1:length(raw_pair[[i]]$positive$comments)){
    authors <- c(authors, raw_pair[[i]]$positive$comments[[j]]$author)
  }
  out[i] <- length(unique(authors))
}
table(out)

out <- rep(NA, length(raw_pair))
for(i in 1:length(raw_pair)){
  authors <- raw_pair[[i]]$negative$author
  for(j in 1:length(raw_pair[[i]]$negative$comments)){
    authors <- c(authors, raw_pair[[i]]$negative$comments[[j]]$author)
  }
  out[i] <- length(unique(authors))
}
table(out)

## creat dfm: each row is a single op + pos/neg responses (each combined into single string)
extractPair <- function(x){tibble(
  op_author = x$op_author, 
  op_title = tolower(x$op_title), 
  op_text = tolower(x$op_text),
  pos_author = x$positive$author,
  neg_author = x$negative$author,
  pos_text = map(x$positive$comments, "body") %>% 
    unlist %>% paste(collapse = "[newpost] ") %>% tolower(),
  neg_text = map(x$negative$comments, "body") %>% 
    unlist %>% paste(collapse = "[newpost] ") %>% tolower()
)}
data_pair <- raw_pair %>% map_dfr(extractPair)



########################################
### OP data (only look at training set!)
########################################

## load paired data
raw_op <- readLines("~/Dropbox/Uni/Data/CMV/op_task/train_op_data.jsonlist") %>%
  map(rjson::fromJSON)

## inspect op data
raw_op %>% map_int(length) %>% table()

extractOP <- function(x){tibble(
  name = x$name,
  title = tolower(x$title), 
  text = tolower(gsub("[^[:graph:]]", " ",x$selftext)),
  Delta = x$delta_label
)}
data_op <- raw_op %>% map_dfr(extractOP)



#############
### Save Data

save(data_pair, data_op, file="out/cmv_data.Rdata")

