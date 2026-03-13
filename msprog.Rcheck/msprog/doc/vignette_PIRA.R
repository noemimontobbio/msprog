## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>"
)

## ----echo=FALSE---------------------------------------------------------------
library(msprog)

## ----eval=FALSE---------------------------------------------------------------
#  output <- MSprog(...
#                   relapse_indep=relapse_indep_from_bounds(p0, p1, e0, e1, c0, c1, prec_type, use_end_dates),
#                   ...)

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 1. Relapse-free intervals characterising PIRA, as defined by arguments `p0`, `p1`, `e0`, `e1`, `c0`, `c1`.*"----
knitr::include_graphics(paste0(getwd(), 'relapse_indep_def.png'))

## -----------------------------------------------------------------------------
relapse_indep <- relapse_indep_from_bounds(p0=0, p1=0,   # baseline
                                          e0=90, e1=30, # event
                                          c0=90, c1=30) # confirmation

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 2. Relapse-free intervals as recommended in @muller2023.*"----
knitr::include_graphics('./relapse_indep_rev1.png')

## -----------------------------------------------------------------------------
relapse_indep <- relapse_indep_from_bounds(p0=0, p1=NULL,    # baseline
                                          e0=NULL, e1=NULL, # event
                                          c0=NULL, c1=0)    # confirmation

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 3. Relapse-free intervals as recommended in @muller2023 for high specificity.*"----
knitr::include_graphics('./relapse_indep_rev0.png')

## -----------------------------------------------------------------------------
relapse_indep <- relapse_indep_from_bounds(p0=0, p1=NULL,  # baseline
                                          e0=NULL, e1=30, # event
                                          c0=30, c1=30)   # confirmation

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 4. Relapse-free intervals as used in @kappos2020.*"----
knitr::include_graphics('./relapse_indep_kappos.png')

## -----------------------------------------------------------------------------
relapse_indep <- relapse_indep_from_bounds(p0=0, p1=0,     # baseline
                                          e0=90, e1=NULL, # event
                                          c0=NULL, c1=0)  # confirmation

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 5. Relapse-free intervals as used in @cagol2022.*"----
knitr::include_graphics('./relapse_indep_cagol.png')

## -----------------------------------------------------------------------------
relapse_indep <- relapse_indep_from_bounds(p0=0, p1=NULL,      # last visit before the event
                                          e0=NULL, e1=0, # event
                                          prec_type='last') 

## ----echo=FALSE, out.width="50%", fig.cap = "*Figure 6. Relapse-free intervals as recommended in @muller2025.*"----
knitr::include_graphics('./relapse_indep_muller2025.png')

## -----------------------------------------------------------------------------
head(toydata_visits)
head(toydata_relapses)

## ----indent="    "------------------------------------------------------------
output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
               relapse=toydata_relapses, 
               conf_days=c(7*12, 7*24), conf_tol_days=c(0, Inf),
               event='firstPIRA', baseline='roving',
               relapse_indep=relapse_indep_from_bounds(p0=0, p1=0, e0=90, e1=30, c0=90, c1=30),
               verbose=2)
# print(output$results, row.names=FALSE) # results
DT::datatable(output$results, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

## ----indent="    "------------------------------------------------------------
output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
               relapse=toydata_relapses,
               conf_days=c(7*12, 7*24), conf_tol_days=c(0, Inf), 
               event='firstPIRA', baseline='fixed', relapse_rebl=TRUE,
               relapse_indep=relapse_indep_from_bounds(p0=0, p1=NULL, e0=NULL, e1=30, c0=30, c1=30),
               verbose=2)
# print(output$results, row.names=FALSE) # results
DT::datatable(output$results, rownames=F,
              options = list(dom='t', scrollX=T, scrollY="200px", paging = FALSE)
              )

