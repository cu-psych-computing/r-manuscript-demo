###### R Club Manuscript Workshop ######

### Putting together some example code for what I call a set-up file


## Load bmlm package to access example dataset
library(bmlm)
library(papaja)
library(brms)
library(dplyr)
library(broom)
library(tidyr)
library(kableExtra)

### round to 3 decimal places

roundto3 <- function(x) {
  (ifelse(abs(x) < .01, 
          format(round(x, 3), nsmall = 3),
          format(round(x, 2), nsmall = 2)))
}


#### frequentist version
regtable <- function(x) {
  tidymerged <- cbind(tidy(x), tidy(confint(x)))
  # citidy <- tidy(confint(x))
  tidymerged$term <- row.names(tidymerged)
  tidymerged$var <- paste(x$terms)[2]
  tidymerged$df <- x$df.residual
  colnames(tidymerged) <- c("term", "estimate", "std.error", "t.value", 
                            "p.value", "term2", "lower", "upper", "var", "df")
  tidymerged$p.round <- ifelse(tidymerged$p.value < .001, "<.001", format(round(tidymerged$p.value, digits =3), nsmall = 3))
  tidymerged$estimate.round <-  format(round(tidymerged$estimate, digits =2), nsmall = 2)
  tidymerged$t.value.round <- format(round(tidymerged$t.value, digits =2), nsmall = 2)
  tidymerged$lower.round <- format(round(tidymerged$lower, digits =2), nsmall = 2)
  tidymerged$upper.round <- format(round(tidymerged$upper, digits = 2), nsmall = 2)
  tidymerged$std.error.round <- format(round(tidymerged$std.error, digits = 2), nsmall = 2)
  tidymerged$r <- sqrt(tidymerged$t.value^2/(tidymerged$t.value^2+tidymerged$df))
  tidymerged$err <- (1/sqrt(nobs(x)-3))
  tidymerged$r.round <- format(round(tidymerged$r, digits = 2), nsmall = 2)
  tidymerged$err.round <- format(round(tidymerged$err, digits = 2), nsmall = 2)
  tidymerged2 <- dplyr::select(tidymerged, var, term, estimate.round, std.error.round, t.value.round, df, p.round,
                               lower.round, upper.round, r.round, err.round)
  colnames(tidymerged2) <- c("var", "term", "estimate", "se", "t", "df", "p", "lwr", "upr", "r", "err")
  print(tidymerged2)
}


#### brms bayesian version

blm_table <- function(x, id) {
  library(broom)
  modtidy <- as.data.frame(fixef(x))
  modtidy$term <- row.names(modtidy)
  modtidy$var <- colnames(x$data)[1]
  
  modtidy$Estimate <- ifelse(abs(modtidy$Estimate) < .01, format(round(modtidy$Estimate, 3), nsmall = 3),
                             format(round(modtidy$Estimate, 2), nsmall = 2))
  
  modtidy$SE <- ifelse(abs(modtidy$Est.Error) < .01, format(round(modtidy$Est.Error, 3), nsmall = 3),
                       format(round(modtidy$Est.Error, 2), nsmall =2))
  
  modtidy$Lower <- ifelse(abs(modtidy$Q2.5) < .01, format(round(modtidy$Q2.5, 3), nsmall = 3), 
                          format(round(modtidy$Q2.5, 2), nsmall = 2))
  
  
  modtidy$Upper <- ifelse(abs(modtidy$Q97.5) < .01, format(round(modtidy$Q97.5, 3), nsmall = 3), 
                          format(round(modtidy$Q97.5, 2), nsmall = 2))
  
  modtidy90 <- as.data.frame(fixef(x, prob = c(.05, .95)))
  modtidy$Lower90 <- ifelse(abs(modtidy90$Q5) < .01, format(round(modtidy90$Q5, 3), nsmall = 3), 
                            format(round(modtidy90$Q5, 2), nsmall = 2))
  
  modtidy$Upper90 <- ifelse(abs(modtidy90$Q95) < .01, format(round(modtidy90$Q95, 3), nsmall = 3), 
                            format(round(modtidy90$Q95, 2), nsmall = 2))
  
  modtidy$N_Subj <- ifelse( length(unique(x$data$id)) == 0, nrow(x$data), length(unique(x$data$id)))
  modtidy$N_Obs <- nobs(x)
  modtidy <- dplyr::select(modtidy, var, term, Estimate, SE, Lower, Upper, Lower90, Upper90, N_Subj, N_Obs)
  
  row.names(modtidy) <- c()
  return(modtidy)
  
}


#### helper function for generating in-line reporting of stats
txt <- function(model, est) {
  b <- "*b* = "
  se <- "*SE* = "
  lwr <- "95% CI ["
  upr <- "]"
  bval <- fixef(model)[est, "Estimate"]
  seval <- fixef(model)[est, "Est.Error"]
  lwrval <- fixef(model)[est, "Q2.5"]
  uprval <- fixef(model)[est, "Q97.5"]
  
  noquote(paste(b, ifelse(abs(mean(bval)) < .01, 
                          format(round(mean(bval), 3), nsmall = 3), 
                          format(round(mean(bval), 2), nsmall = 2)),
                ", ",
                # se, ifelse(abs(mean(seval)) < .01, 
                #            format(round(mean(seval), 3), nsmall = 3), 
                #            format(round(mean(seval), 2), nsmall = 2)),
                # ", ", 
                
                lwr, ifelse(abs(mean(lwrval)) < .01, 
                            format(round(mean(lwrval), 3), nsmall = 3), 
                            format(round(mean(lwrval), 2), nsmall = 2)),
                ", ", 
                
                ifelse(abs(mean(uprval)) < .01, 
                       format(round(mean(uprval), 3), nsmall = 3), 
                       format(round(mean(uprval), 2), nsmall = 2)),   upr,
                
                sep = ""))
  
  
}




