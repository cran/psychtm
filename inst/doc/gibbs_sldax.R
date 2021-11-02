## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dplyr)   # For easier data manipulation with pipes `%>%`
library(lda)     # Needed if using prep_docs() function
library(psychtm)

data(teacher_rate)  # Synthetic student ratings of instructors

## -----------------------------------------------------------------------------
glimpse(teacher_rate)

## -----------------------------------------------------------------------------
docs_vocab <- prep_docs(teacher_rate, "doc")

## -----------------------------------------------------------------------------
str(docs_vocab$vocab)
vocab_len <- length(docs_vocab$vocab)

## -----------------------------------------------------------------------------
str(docs_vocab$documents)

## -----------------------------------------------------------------------------
print(docs_vocab$documents[1, 1:25])

## -----------------------------------------------------------------------------
docs_vocab$vocab[docs_vocab$documents[1, 1:17]]

## -----------------------------------------------------------------------------
set.seed(92850827)
fit_lda <- gibbs_sldax(m = 450, burn = 300, thin = 1,
                       docs = docs_vocab$documents,
                       V = vocab_len,
                       K = 3, model = "lda", display_progress = TRUE)

## -----------------------------------------------------------------------------
theta_hat <- est_theta(fit_lda)
head(theta_hat)

## -----------------------------------------------------------------------------
beta_hat <- est_beta(fit_lda)
colnames(beta_hat) <- docs_vocab$vocab
beta_hat[, 1:10]

## -----------------------------------------------------------------------------
get_topwords(beta_ = beta_hat, nwords = 10, docs_vocab$vocab, method = "prob") %>% 
  print(n = 30)

## -----------------------------------------------------------------------------
get_topwords(beta_hat, 15, docs_vocab$vocab, method = "termscore") %>%
  print(n = 30)

## -----------------------------------------------------------------------------
head(get_toptopics(theta = theta_hat, ntopics = 2))

## -----------------------------------------------------------------------------
get_coherence(beta_ = beta_hat, docs = docs_vocab$documents)

## -----------------------------------------------------------------------------
get_exclusivity(beta_ = beta_hat)

## -----------------------------------------------------------------------------
set.seed(44680835)
fit_sldax <- gibbs_sldax(rating ~ I(grade - 1), data = teacher_rate,
                         m = 450, burn = 300, thin = 1,
                         docs = docs_vocab$documents,
                         V = vocab_len,
                         K = 3, model = "sldax", display_progress = TRUE)

## -----------------------------------------------------------------------------
get_topwords(est_beta(fit_sldax), 15, docs_vocab$vocab, method = "termscore") %>%
  print(n = 30)

## -----------------------------------------------------------------------------
eta_post <- post_regression(fit_sldax)
summary(eta_post)

