library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quanteda)
library(topicmodels)
library(stm)

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
  dfm(stem = T, remove = stopwords("english")) %>%
  dfm_trim(., min_count = 10, min_docfreq = 10) %>%
  convert(to = "topicmodels")

## estimate topic model
lda <- LDA(dfm_op, k=20, method = "Gibbs",
           control=list(seed=123456, iter=2000, burnin=1000))

## prepare data frame for plotting
tibble(terms = apply(terms(lda, 10),2,paste, collapse=", "),
       labels = c("Drugs/Guns", "Information", "Time/Life", "Travel", "Food",
                  "Opinions/Evaluations", "Entertainment", "Religion", "Gender/Sexuality", 
                  "Philosophy/Physics","Economy", "Social Issues", "People's Beliefs",
                  "Reddit/Misc.", "Education", "Nature/Climate", "Crime",
                  "Family/Parenting", "Domestic Politics", "International Politics"),
       props = apply(posterior(lda)$topics, 2, mean),
       maxtopic = table(topics(lda)),
       pmaxtopic = maxtopic/sum(maxtopic)) %>%
  ggplot(aes(y=reorder(labels, props), x=props, label = terms)) + 
  geom_segment(aes(x=0, xend=props, yend=labels)) +
  geom_text(hjust=0, position = position_nudge(0.002), size=2) + 
  geom_point(size=1) + plot_default + xlim(0,.11) +
  labs(y="Topic Label", x="Average Topic Proportion")
ggsave("fig/topics.pdf", width = 6.5, height = 3)
ggsave("fig/topics.png", width = 6.5, height = 3, dpi=400)



##################################
### STM analysis of topics in original post (Delta vs. No Delta)
##################################

## process for stm
processed <- textProcessor(apply(data_op[,c("title","text")], 1, paste, collapse=" "),
                           metadata = data.frame(delta = as.numeric(data_op$Delta)))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     lower.thresh = 10)

## stm fit with 20 topics
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
               , K=20, seed=12345)
summary(stm_fit)

## plot topic proportions
pdf("fig/stm_op_prop.pdf", width=11, height=6)
par(mar=c(4.2,0.5,0.5,0.5))
plot(stm_fit, n=10, labeltype = "prob", text.cex = 1, main = NA)
dev.off()

## estimate topic prevalence effects
prep <- estimateEffect(~ pos, stm_fit, meta = out$meta, uncertainty = "Global")

## sort topics by effect size
tmp <- tibble(estimate = sapply(summary(prep)[[3]], function(x) x["delta","Estimate"]),
              topics = prep$topics) %>% arrange(estimate)

## plot topic differences between negative and positive responses
pdf("fig/stm_op_diff.pdf", height=6, width=11)
par(mar=c(2.2,0.5,0.5,0.5))
plot.estimateEffect(prep, covariate = "pos", model = stm_fit, topics = tmp$topics
                    , method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=10, verbose.labels = F, width=100
                    , xlim = c(-.05,.012)
                    , xlab = "Difference in Topic Proportions (Change - No Change)")
dev.off()



##################################
### STM analysis of topics in positive/negative responses
##################################

## process for stm
processed <- textProcessor(c(data_pair$neg_text, data_pair$pos_text),
                           metadata = data.frame(pos = rep(c(0,1), each=nrow(data_pair))))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     lower.thresh = 10)

## stm fit with 20 topics
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
                   , K=20, seed=12345)
summary(stm_fit)

## plot topic proportions
pdf("fig/stm_pair_prop.pdf", width=11, height=6)
par(mar=c(4.2,0.5,0.5,0.5))
plot(stm_fit, n=10, labeltype = "prob", text.cex = 1, main = NA)
dev.off()

## estimate topic prevalence effects
prep <- estimateEffect(~ pos, stm_fit, meta = out$meta, uncertainty = "Global")

## sort topics by effect size
tmp <- tibble(estimate = sapply(summary(prep)[[3]], function(x) x["pos","Estimate"]),
              topics = prep$topics) %>% arrange(estimate)

## plot topic differences between negative and positive responses
pdf("fig/stm_pair_diff.pdf", height=6, width=11)
par(mar=c(2.2,0.5,0.5,0.5))
plot.estimateEffect(prep, covariate = "pos", model = stm_fit, topics = tmp$topics
                    , method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=10, verbose.labels = F, width=100
                    , xlim = c(-.05,.012)
                    , xlab = "Difference in Topic Proportions (Change - No Change)")
dev.off()

