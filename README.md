
<!-- README.md is generated from README.Rmd. Please only edit README.Rmd -->

<br />

> 🚧 **This repository is under active development. Please make sure you
> are using the latest version of the package (check by running
> `utils::packageVersion('msprog')`) – or at least v0.2.0, which is
> functionally stable, though some minor aspects may still change ahead
> of a full stable release on CRAN.** 🚧
>
> **Latest version:**
>
> ``` r
> utils::packageVersion('msprog')
> #> [1] '0.2.3'
> ```

# msprog: reproducible assessment of disability course in MS

<!-- badges: start -->

[![R-CMD-check](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<p align="center">

<img src="man/figures/logo_R.png" width="150"/>
</p>

`msprog` is an R package providing tools for exhaustive and reproducible
analysis of disability course in multiple sclerosis (MS) from
longitudinal data \[[1](#ref-msprog2024)\]. A [**Python
version**](https://github.com/noemimontobbio/pymsprog) of the package is
available as well.

Its core function, `MSprog()`, detects and characterises the evolution
of an outcome measure (Expanded Disability Status Scale, EDSS; Nine-Hole
Peg Test, NHPT; Timed 25-Foot Walk, T25FW; Symbol Digit Modalities Test,
SDMT; or any custom outcome measure) for one or more subjects, based on
repeated assessments through time and on the dates of acute episodes (if
any).

The package also provides two toy datasets for function testing:

- `toydata_visits`: artificially generated EDSS and SDMT assessments for
  a small cohort of patients;
- `toydata_relapses`: artificially generated relapse onset dates
  associated with the patients in `toydata_visits`.

Please refer to the documentation for function usage (e.g. `?MSprog`)
and data structure (e.g. `?toydata_visits`). The whole documentation can
be found in the [reference manual (PDF)](msprog.pdf). Additionally, a
detailed tutorial providing examples and best-practice tips is available
as a [package vignette](#vignette): *Analysing disability course in MS*.

The outcome computation can be run locally in R (see installation
instructions below), or online via a user-friendly [web
application](https://msprog.shinyapps.io/msprog/).

**If you use this package in your work, please cite it [as
below](#citation)**.

For any questions, requests for new features, or bug reporting, please
contact: **noemi.montobbio@unige.it**. Any feedback is highly
appreciated!

<a id="install"></a>

## Installation

You can install the development version of `msprog` from GitHub by
running the command below. If using RStudio, please make sure to restart
the R session right before running the installation.

``` r
# install.packages("devtools") # if not already installed
devtools::install_github("noemimontobbio/msprog", build_vignettes=TRUE)
```

## Getting started

The `MSprog()` function detects the events sequentially by scanning the
outcome values in chronological order.

The example below illustrates how to import toy data and apply
`MSprog()` to analyse EDSS course with the default settings.

``` r
library(msprog)

# Load toy data
data(toydata_visits)
data(toydata_relapses)

# Compute disability course
output <- MSprog(toydata_visits,                                      # provide data on visits
                 subj_col='id', value_col='EDSS', date_col='date',    # specify column names
                 outcome='edss',                                      # specify outcome type
                 relapse=toydata_relapses)                            # provide data on relapses
#> 
#> ---
#> Outcome: edss
#> Confirmation over: 84 days (-7 days, +730.5 days)
#> Baseline: fixed
#> Baseline skipped if: <30 days from last relapse
#> Event skipped if: -
#> Confirmation visit skipped if: <30 days from last relapse
#> Events detected: firstCDW
#> 
#> *Please use `print(output)` to display full info on event detection criteria*
#> 
#> ---
#> Total subjects: 6
#> ---
#> Subjects with CDW: 3 (PIRA: 2; RAW: 1)
```

Several qualitative and quantitative options for event detection are
given as arguments that can be set by the user and reported as a
complement to the results to ensure reproducibility. For example,
instead of only detecting the first confirmed disability worsening (CDW)
event for each subject, we can detect *all* disability events
sequentially by moving the baseline after each event
(`event='multiple', baseline='roving'`)\`:

``` r
output <- MSprog(toydata_visits,                                      # provide data on visits
                 subj_col='id', value_col='EDSS', date_col='date',    # specify column names
                 outcome='edss',                                      # specify outcome type
                 event='multiple', baseline='roving',                 # modify default options
                 relapse=toydata_relapses)                            # provide data on relapses
#> 
#> ---
#> Outcome: edss
#> Confirmation over: 84 days (-7 days, +730.5 days)
#> Baseline: roving
#> Baseline skipped if: <30 days from last relapse
#> Event skipped if: -
#> Confirmation visit skipped if: <30 days from last relapse
#> Events detected: multiple
#> 
#> *Please use `print(output)` to display full info on event detection criteria*
#> 
#> ---
#> Total subjects: 6
#> ---
#> Subjects with CDW: 4 (PIRA: 4; RAW: 1)
#> Subjects with CDI: 2
#> ---
#> CDW events: 5 (PIRA: 4; RAW: 1)
#> CDI events: 2
```

The function prints out a concise report of the results, and of the
options used to obtain them. Complete results are stored in an object of
class `MSprogOutput` with the following attributes.

1.  `event_count`: a `data.frame` containing the event sequence detected
    for each subject, and the counts for each event type.

    ``` r
    print(output$event_count)
    #>   event_sequence CDI CDW RAW PIRA undef_CDW
    #> 1           PIRA   0   1   0    1         0
    #> 2      RAW, PIRA   0   2   1    1         0
    #> 3                  0   0   0    0         0
    #> 4      CDI, PIRA   1   1   0    1         0
    #> 5           PIRA   0   1   0    1         0
    #> 6            CDI   1   0   0    0         0
    ```

    where: `event_sequence` specifies the order of the events; the other
    columns count the events of each type.

2.  `results`: extended info on each event for all subjects.

    ``` r
    print(output$results, row.names=FALSE)
    #>  id nevent event_type total_fu time2event bl2event conf84 PIRA_conf84 sust_days
    #>   1      1       PIRA      534        292      292      1           1       242
    #>   2      1        RAW      730        198      198      1           0        84
    #>   2      2       PIRA      730        539      257      1           1       191
    #>   3      0                 491        491      NaN      0           0         0
    #>   4      1        CDI      586         77       77      1           0        98
    #>   4      2       PIRA      586        304      129      1           1       282
    #>   5      1       PIRA      637        140      140      1           1       497
    #>   6      1        CDI      491        120      120      1           0       232
    #>  sust_last
    #>          1
    #>          0
    #>          1
    #>          0
    #>          0
    #>          1
    #>          1
    #>          0
    ```

    where: `nevent` is the cumulative event count for each subject;
    `event_type` characterises the event; `time2event` is the number of
    days from start of follow-up to event; `bl2event` is the number of
    days from current baseline to event; `conf84` reports whether the
    event was confirmed over 84 days (12 weeks); `sust_days` is the
    number of days for which the event was sustained; `sust_last`
    reports whether the event was sustained until the last visit.

Additionally, applying the `print` method to an object of class
`MSprogOutput` prints out the full list of function arguments, as well
as a short paragraph describing the complete set of criteria used to
obtain the output, **to be reported to ensure complete
reproducibility**:

``` r
print(output)
#> ---
#> msprog version: 0.2.3 
#> ---
#> MSprog() arguments:
#> outcome=edss, event=multiple, baseline=roving, proceed_from=firstconf, validconf_col=validconf, skip_local_extrema=none, conf_days=84, conf_tol_days=c(7, 730.5), require_sust_days=0, check_intermediate=TRUE, relapse_to_bl=c(30, 0), relapse_to_event=c(0, 0), relapse_to_conf=c(30, 0), relapse_assoc=c(90, 0), relapse_indep=list(prec = list(0, 0), event = list(90, 30), conf = list(90, 30), prec_type = "baseline"), renddate_col=NULL, sub_threshold_rebl=none, bl_geq=FALSE, relapse_rebl=FALSE, impute_last_visit=0, worsening=increase,
#> delta_fun=NULL
#> 
#> Textual description of applied criteria:
#> We detected all confirmed EDSS changes (in chronological order) confirmed over 84 days (with a lower tolerance of 7 days and an upper tolerance of 730.5 days). A visit could not be used as confirmation if occurring within 30 days after the onset of a relapse. A roving baseline scheme was applied where the reference value was updated after each confirmed worsening or improvement event. The new baseline was set at the first available confirmation visit for the event that triggered the re-baseline. Whenever the current baseline fell within 30 days after the onset of a relapse, it was moved to the next eligible visit. A confirmed EDSS worsening event was labelled as RAW if occurring within 90 days after the onset of a relapse. A confirmed EDSS worsening event was labelled as PIRA if no relapses started in the interval from 90 days before the event to 30 days after the event, or from 90 days before confirmation to 30 days after confirmation. 
#> ---
#> Clinically meaningful threshold for EDSS change (delta function): default for EDSS (as per function msprog::compute_delta(), see package docs).
```

<br />

<a id="vignette"></a>

### Vignettes

For detailed indications on usage and best practices, please refer to
the package vignettes (make sure to enable `build_vignettes` during
[installation](#install)).

``` r
browseVignettes('msprog')
```

<a id="citation"></a>

### Citation

If you use the `msprog` package, please use the `citation()` function to
obtain the correct reference:

``` r
citation('msprog')
#> To cite package 'msprog' in publications use:
#> 
#>   Montobbio N, Carmisciano L, Signori A, Ponzano M, Schiavetti I, Bovis
#>   F, Sormani MP (2024). "Creating an automated tool for a consistent
#>   and repeatable evaluation of disability progression in clinical
#>   studies for Multiple Sclerosis." _Mult Scler._, *30*(9), 1185-1192.
#>   doi:10.1177/13524585241243157
#>   <https://doi.org/10.1177/13524585241243157>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Creating an automated tool for a consistent and repeatable evaluation of disability progression in clinical studies for Multiple Sclerosis},
#>     author = {Noemi Montobbio and Luca Carmisciano and Alessio Signori and Marta Ponzano and Irene Schiavetti and Francesca Bovis and Maria Pia Sormani},
#>     journal = {Mult Scler.},
#>     year = {2024},
#>     volume = {30},
#>     number = {9},
#>     pages = {1185-1192},
#>     doi = {10.1177/13524585241243157},
#>   }
```

## References

<div id="refs" class="references csl-bib-body">

<div id="ref-msprog2024" class="csl-entry">

1\. Montobbio N, Carmisciano L, Signori A, Ponzano M, Schiavetti I,
Bovis F, et al. Creating an automated tool for a consistent and
repeatable evaluation of disability progression in clinical studies for
multiple sclerosis. Mult Scler. Department of Health Sciences (DISSAL),
University of Genoa, Genoa, Italy.; 2024;30:1185–92.
<https://doi.org/10.1177/13524585241243157>

</div>

</div>
