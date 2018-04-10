library(rjson)
library(dplyr)
library(purrr)

rm(list=ls())

source("func.R")



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

## create dfm: each row is a single op + pos/neg responses (each combined into single string)
extractPair <- function(x){tibble(
  op_author = x$op_author, 
  op_title_raw = x$op_title, 
  op_text_raw = x$op_text,
  pos_author = x$positive$author,
  neg_author = x$negative$author,
  pos_text_raw = map(x$positive$comments, "body") %>% 
    unlist %>% paste(collapse = " [newpost] "),
  neg_text_raw = map(x$negative$comments, "body") %>% 
    unlist %>% paste(collapse = " [newpost] "),
  pos_root_raw = x$positive$comments[[1]]$body,
  neg_root_raw = x$negative$comments[[1]]$body
)}
data_pair <- raw_pair %>% map_dfr(extractPair) %>%
  mutate(op_title = cleanText(op_title_raw),
         op_text = cleanText(op_text_raw),
         pos_text = cleanText(pos_text_raw),
         neg_text = cleanText(neg_text_raw),
         pos_text_nterm = strsplit(pos_text, "\\s+") %>% map_int(length),
         neg_text_nterm = strsplit(neg_text, "\\s+") %>% map_int(length),
         pos_root = cleanText(pos_root_raw),
         neg_root = cleanText(neg_root_raw),
         pos_root_nterm = strsplit(pos_root, "\\s+") %>% map_int(length),
         neg_root_nterm = strsplit(neg_root, "\\s+") %>% map_int(length)
         )

## boolean indicating that positive comments are longer
pos_longer <- (data_pair$pos_root_nterm - data_pair$neg_root_nterm) > 0

## truncated positive posts
data_pair$pos_trunc <- data_pair$pos_root
for(i in which(pos_longer)){
  data_pair$pos_trunc[i] <- truncText(data_pair$pos_trunc[i], data_pair$neg_root_nterm[i])
}

## truncated negative posts
data_pair$neg_trunc <- data_pair$neg_root
for(i in which(!pos_longer)){
  data_pair$neg_trunc[i] <- truncText(data_pair$neg_trunc[i], data_pair$pos_root_nterm[i])
}



########################################
### OP data (only look at training set!)
########################################

## load paired data
raw_op <- readLines("~/Dropbox/Uni/Data/CMV/op_task/train_op_data.jsonlist") %>%
  map(rjson::fromJSON)

## inspect op data
raw_op %>% map_int(length) %>% table()

## create dfm: each observation is an original post
extractOP <- function(x){tibble(
  name = x$name,
  title_raw = x$title, 
  text_raw = gsub("[^[:graph:]]", " ",x$selftext),
  Delta = x$delta_label
)}
data_op <- raw_op %>% map_dfr(extractOP) %>%
  mutate(title = cleanText(title_raw),
         text = cleanText(text_raw))



#############
### Save Data

save(data_pair, data_op, file="out/cmv_data.Rdata")

