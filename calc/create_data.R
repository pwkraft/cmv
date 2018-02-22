library(rjson)
library(dplyr)
library(purrr)



############################################
### Paired data (only look at training set!)
############################################

## load paired data
test <- readLines("~/Dropbox/Uni/Data/CMV/pair_task/train_pair_data.jsonlist") %>%
  lapply(rjson::fromJSON)

## examine data
ls(test[[1]])

## op information
test[[1]]$op_author
test[[1]]$op_title
test[[1]]$op_text
test[[1]]$op_name

## information about response that changed view (positive)
ls(test[[1]]$positive)
test[[1]]$positive$author
test[[1]]$positive$ancestor
ls(test[[1]]$positive$comments[[1]])
test[[1]]$positive$comments[[1]]$body
test[[1]]$positive$comments[[1]]$author

## information about response that did not change view (negative)
ls(test[[1]]$negative)
test[[1]]$negative$author
test[[1]]$negative$ancestor
ls(test[[1]]$negative$comments[[1]])
test[[1]]$negative$comments[[1]]$author
test[[1]]$negative$comments[[1]]$body

## instances contain different numbers of replies
out <- matrix(NA, ncol=2, nrow=length(test))
for(i in 1:length(test)){
  out[i,] <- c(length(test[[i]]$positive$comments)
               , length(test[[i]]$negative$comments))
  
}
table(out[,1],out[,2])
which(out[,1]==4)[1]
test[[10]]$positive$comments

## all replies in one instance are by the same author -> combine all!
out <- rep(NA, length(test))
for(i in 1:length(test)){
  authors <- test[[i]]$positive$author
  for(j in 1:length(test[[i]]$positive$comments)){
    authors <- c(authors, test[[i]]$positive$comments[[j]]$author)
  }
  out[i] <- length(unique(authors))
}
table(out)

out <- rep(NA, length(test))
for(i in 1:length(test)){
  authors <- test[[i]]$negative$author
  for(j in 1:length(test[[i]]$negative$comments)){
    authors <- c(authors, test[[i]]$negative$comments[[j]]$author)
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
data_pair <- test %>% map_dfr(extractPair)




########################################
### OP data (only look at training set!)
########################################

data_op <- NULL

#############
### Save Data
#############

save(data_pair, data_op, file="out/cmv_data.Rdata")