### Function to compute percentage of moral word counts
countMFT <- function(x, dic){
  corpus <- corpus(x)
  dat <- dfm(corpus, dictionary = dic) %>%
    data.frame() %>%
    transmute(Care = HarmVirtue + HarmVice,
              Fairness = FairnessVirtue + FairnessVice,
              Loyalty = IngroupVirtue + IngroupVice,
              Authority = AuthorityVirtue + AuthorityVice,
              Sanctity = PurityVirtue + PurityVice,
              General = MoralityGeneral)
  dat/ntoken(corpus) * 100
}

### Function to compute percentage of virtue vs. vice moral terms
countVirtue <- function(x, dic){
  corpus <- corpus(x)
  dat <- dfm(corpus, dictionary = dic) %>%
    data.frame() %>%
    transmute(Virtue = HarmVirtue + FairnessVirtue + IngroupVirtue + AuthorityVirtue + PurityVirtue,
              Vice = HarmVice + FairnessVice + IngroupVice + AuthorityVice +  PurityVice,
              VVDiff = Virtue - Vice)
  dat/ntoken(corpus) * 100
}