library(rjson)
library(dplyr)
library(purrr)

# test <- readLines("~/Dropbox/Uni/Data/CMV/op_task/heldout_op_data.jsonlist") %>%
#   lapply(jsonlite::fromJSON)

convertJSON <- function(x){
  out <- data.frame(matrix(NA, nrow=length(x), ncol=5))
  colnames(out) <- names(x[[1]])
  for(i in 1:length(x)){
    out[i,] <- x[[i]]
  }  
  out
}

test[[1]]$op_author
test[[1]]$op_title
test[[1]]$op_text
test[[1]]$op_name
list(test[[1]])

test[[1]]$positive$author

length(test[[1]]$positive$comments)

test[[1]]$positive$comments[[1]]$body

out <- NULL
for(i in 1:length(test)){
  out <- c(out, length(test[[i]]$negative$comments))
}
table(out)

out <- NULL
for(i in 1:length(test)){
  out <- c(out, length(test[[i]]$positive$comments))
}
table(out)
which(out==4)[1]

test[[10]]$positive$comments[[1]]$body

test[[10]]$positive

test[[10]]$negative

convertPair <- function(x){
  
}

test <- readLines("~/Dropbox/Uni/Data/CMV/pair_task/train_pair_data.jsonlist") %>%
  lapply(rjson::fromJSON) %>% convertJSON()


