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

## -----------------------------------------------------------------------------
head(toydata_visits)

## -----------------------------------------------------------------------------
head(toydata_relapses)

## -----------------------------------------------------------------------------
output_edss <- MSprog(data=toydata_visits, # data on visits
                 subj_col='id', value_col='EDSS', date_col='date', # specify column names
                 outcome='edss', # specify outcome type
                 relapse=toydata_relapses) # data on relapses

## -----------------------------------------------------------------------------
output_sdmt <- MSprog(data=toydata_visits, # data on visits
                 subj_col='id', value_col='SDMT', date_col='visit_day', # specify column names
                 outcome='sdmt', # specify outcome type
                 relapse=toydata_relapses, # data on relapses
                 date_format='day') # specify that dates are given as days

## ----indent="     "-----------------------------------------------------------
# print(output_edss$results, row.names=FALSE)
DT::datatable(output_edss$results, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## ----indent="     "-----------------------------------------------------------
# print(output_edss$event_count)
DT::datatable(output_edss$event_count,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## ----linewidth=90-------------------------------------------------------------
print(output_edss)

## -----------------------------------------------------------------------------
print(compute_delta(4, outcome='edss'))

## -----------------------------------------------------------------------------
print(compute_delta(10, outcome='t25fw'))

## -----------------------------------------------------------------------------
my_sdmt_delta <- function(x) {min(c(x/10, 3))}

## -----------------------------------------------------------------------------
print(my_sdmt_delta(50)) # my delta
print(compute_delta(50, outcome='sdmt')) # default delta

## ----eval=FALSE---------------------------------------------------------------
#  output <- MSprog(...
#                  outcome='sdmt', delta_fun=my_sdmt_delta,
#                   ...)

## -----------------------------------------------------------------------------
print(toydata_visits[toydata_visits$id==4, c('date', 'EDSS')]) # EDSS visits

output <- MSprog(data=toydata_visits, 
                 subj_col='id', value_col='EDSS', date_col='date', outcome='edss', 
                 subjects=4,
                 relapse=toydata_relapses, 
                 event='multiple', baseline='fixed',  # <---
                 verbose=0)
# print(output$results, row.names=FALSE) # results
DT::datatable(output$results, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## -----------------------------------------------------------------------------
output <- MSprog(data=toydata_visits, 
                 subj_col='id', value_col='EDSS', date_col='date', outcome='edss', 
                 subjects=4,
                 relapse=toydata_relapses, 
                 event='multiple', baseline='roving',  # <---
                 verbose=0)
# print(output$results, row.names=FALSE) # results
DT::datatable(output$results, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## ----echo=FALSE, out.width="40%", fig.cap = "*Figure 2. Example of data with unscheduled visits.*"----
knitr::include_graphics(paste0(getwd(), 'unscheduled.png'))

## ----eval=FALSE---------------------------------------------------------------
#  output <- MSprog(...
#                   validconf_col='scheduled',
#                   ...)

## ----eval=FALSE---------------------------------------------------------------
#  output <- MSprog(...
#                   relapse_indep=relapse_indep_from_bounds(p0, p1, e0, e1, c0, c1),
#                   ...)

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 1. Relapse-free intervals characterising PIRA, as defined by arguments `p0`, `p1`, `e0`, `e1`, `c0`, `c1`.*"----
knitr::include_graphics(paste0(getwd(), 'relapse_indep_def.png'))

## -----------------------------------------------------------------------------
output <- MSprog(data=toydata_visits, 
                      subj_col='id', value_col='EDSS', date_col='date', 
                      outcome='edss', relapse=toydata_relapses, verbose=0,
                      include_dates=T, include_value=T, include_stable=F) # <---- !

# print(output$results, row.names=FALSE)
DT::datatable(output$results, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## -----------------------------------------------------------------------------
output <- MSprog(data=toydata_visits, 
                 subj_col='id', value_col='EDSS', date_col='date', outcome='edss',
                 event='multiple', baseline='roving', 
                 relapse=toydata_relapses, verbose=2)

## -----------------------------------------------------------------------------
# print(output$unconfirmed, row.names=FALSE)
DT::datatable(output$unconfirmed, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## -----------------------------------------------------------------------------
vm <- value_milestone(toydata_visits, milestone=6,
                 subj_col='id', value_col='EDSS', date_col='date',
                 outcome='edss', relapse=toydata_relapses,
                 verbose=0)

## -----------------------------------------------------------------------------
print(vm)

