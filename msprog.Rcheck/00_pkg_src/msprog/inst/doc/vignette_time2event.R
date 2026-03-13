## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>"
)

## ----wrap-hook, include=FALSE-------------------------------------------------
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = xfun::split_lines(x) #substring(x, 4, nchar(x)))
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste0(paste(x, collapse = '\n#> '), '\n')
  }
  hook_output(x, options)
})

## -----------------------------------------------------------------------------
library(msprog)
head(toydata_visits)
head(toydata_relapses)

## -----------------------------------------------------------------------------
output_edss <- MSprog(toydata_visits,
                 subj_col='id', value_col='EDSS', date_col='date',
                 outcome='edss',
                 event='firstCDW',   # <--- only detect first CDW event
                 relapse=toydata_relapses,
                 verbose=0)

## ----linewidth=90-------------------------------------------------------------
print(output_edss)

## -----------------------------------------------------------------------------
res <- output_edss$results
# print(res, row.names=FALSE)
DT::datatable(res, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## -----------------------------------------------------------------------------
survival_data <- res[c('id', 'time2event', 'nevent')]
# print(survival_data, row.names=FALSE)
DT::datatable(survival_data, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## ----eval=FALSE---------------------------------------------------------------
#  # library(dplyr)
#  # library(ggsurvfit)
#  survfit2(Surv(time2event, nevent) ~ 1, data=survival_data) %>%
#    ggsurvfit() +
#    labs(
#      x = "time (days)",
#      y = "survival probability"
#    )

## -----------------------------------------------------------------------------
vm <- value_milestone(toydata_visits, milestone=4.5,
                 subj_col='id', value_col='EDSS', date_col='date',
                 outcome='edss', relapse=toydata_relapses,
                 verbose=2)

## -----------------------------------------------------------------------------
print(toydata_visits[toydata_visits$id==2, c('date', 'EDSS')], row.names=FALSE)

## -----------------------------------------------------------------------------
print(toydata_visits[toydata_visits$id==4, c('date', 'EDSS')], row.names=FALSE)

## -----------------------------------------------------------------------------
# print(vm)
DT::datatable(vm, 
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## -----------------------------------------------------------------------------
output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss', relapse=toydata_relapses,
                event='multiple', baseline='roving',  # <--- detect multiple events with a roving baseline
                verbose=0)
survival_data <- output$results[c('id', 'nevent', 'time2event')]
# print(survival_data, row.names=FALSE)
DT::datatable(survival_data, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

