## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  prompt = TRUE,
  comment = " "
)

## -----------------------------------------------------------------------------
library(ExactMed)

head(datamed)


## -----------------------------------------------------------------------------

as.logical(sum(is.na(datamed)))


## -----------------------------------------------------------------------------

results1 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', 
  a1 = 1, a0 = 0, interaction = FALSE
  )  

results1


## -----------------------------------------------------------------------------

results2 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0,  
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  interaction = FALSE
  )

results2


## -----------------------------------------------------------------------------

results3 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0,  
  m_cov = c('C1', 'C2'), y_cov = c('C1'), 
  interaction = FALSE
  )

results3


## -----------------------------------------------------------------------------

results4 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1'), 
  adjusted = FALSE, interaction = FALSE
  )

results4


## ---- results='hide'----------------------------------------------------------

results5 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  boot = TRUE, nboot = 100, bootseed = 1991, confcoef = 0.97
  )


## -----------------------------------------------------------------------------

results5


## ---- results='hide'----------------------------------------------------------

results6 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), Firth = TRUE, 
  boot = TRUE, nboot = 100, bootseed = 1991, confcoef = 0.97
  )


## -----------------------------------------------------------------------------

results6


## -----------------------------------------------------------------------------

results7 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = c(C1 = 0.1, C2 = 0.4), y_cov_cond = c(C1 = 0.1, C2 = 0.4)
  )

results7


## ---- error=TRUE, collapse=FALSE----------------------------------------------

exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = c(C1 = 0.3, C2 = 0.4), y_cov_cond = c(C1 = 0.1, C2 = 0.4)
 )



## -----------------------------------------------------------------------------

results8 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = c(C1 = 0.1), y_cov_cond = c(C1 = 0.1)
  )


## -----------------------------------------------------------------------------

 mc2 <- mean(datamed$C2)
 mc2

results9 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = c(C1 = 0.1, C2 = mc2), y_cov_cond = c(C1 = 0.1, C2 = mc2)
  )


## -----------------------------------------------------------------------------

all.equal(results8, results9)


## ----error=TRUE, collapse=FALSE-----------------------------------------------

exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = c(C1 = 0.1), y_cov_cond = c(C1 = 0.1, C2 = 0.4)
  )



## -----------------------------------------------------------------------------

cate <- factor(sample(c("a", "b", "c"), nrow(datamed), replace =TRUE))
datamed$C1 <- cate


## -----------------------------------------------------------------------------

results10 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = list(C1 = 'a', C2 = 0.4), y_cov_cond = list(C1 = 'a', C2 = 0.4)
  )

results10


## -----------------------------------------------------------------------------

results11 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  m_cov_cond = c(C2 = 0.4), y_cov_cond = c(C2 = 0.4)
  )

results11


## -----------------------------------------------------------------------------

results12 <- exactmed(
  data = datamed, a = 'X', m = 'M', y = 'Y', 
  a1 = 1, a0 = 0, interaction = FALSE, yprevalence = 0.1
  )

results12


## -----------------------------------------------------------------------------

library(ExactMed)

head(datamed_c)



## -----------------------------------------------------------------------------

results13 <- exactmed_c(
  data = datamed_c, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0,  
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  interaction = FALSE
  )

results13


## ---- results='hide'----------------------------------------------------------

results14 <- exactmed_c(
  data = datamed_c, a = 'X', m = 'M', y = 'Y', a1 = 1, a0 = 0, 
  m_cov = c('C1', 'C2'), y_cov = c('C1', 'C2'), 
  boot = TRUE, nboot = 100, bootseed = 1885, confcoef = 0.95,
  mf = 2
  )


## -----------------------------------------------------------------------------

results14


