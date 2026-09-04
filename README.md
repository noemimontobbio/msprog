
<!-- README.md is generated from README.Rmd. Please only edit README.Rmd -->

<br>

> [!WARNING]
> **🚧 This repository is under active development. 🚧 <br> Please make sure you are using at least the latest stable version available on CRAN (1.0.0).
> Check your installed version by running `utils::packageVersion("msprog")`.**

<br>

> 📣 **What’s new**
>
> - Updated [citation](#citation) (v1.0.1) – **[new
>   paper](https://doi.org/10.1177/13524585261478492) out now!**
> - Info on initial baseline (first eligible baseline visit) in results
>   data frame (v1.0.1) <!-- ([development version](#devel)) -->
> - **Now available on CRAN** (supporting **R \>= 4.1.0**) (v1.0.0).

# msprog: reproducible assessment of disability course in MS

<!-- badges: start -->

[![R-CMD-check](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/msprog)](https://CRAN.R-project.org/package=msprog)
<!-- badges: end -->

<p align="center">

<img src="man/figures/logo_R.png" width="150"/>
</p>

[📦 **CRAN package
page**](https://cran.r-project.org/package=msprog)<br> [📖
**Documentation**](https://cran.r-project.org/web/packages/msprog/msprog.pdf)<br>
<!-- [🔍 **Vignettes**](link) -->

`msprog` is an R package providing tools for reproducible analysis of
disability course in multiple sclerosis (MS) from longitudinal data
\[[1](#ref-msprog2024)\]. A [**Python
version**](https://pymsprog.readthedocs.io/en/stable/) of the package is
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

Please refer to [**published
guidelines**](https://doi.org/10.1177/13524585261478492) on recommended
calculation settings according to study type and endpoint of interest.
These recommendations are the result of a consensus process involving
several international MS research groups and conducted under the
auspices of the International Advisory Committee on Clinical Trials in
MS (IACCTMS).

Refer to the documentation for function usage (e.g. `?MSprog`) and data
structure (e.g. `?toydata_visits`). The whole documentation can be found
in the [reference manual
(PDF)](https://cran.r-project.org/web/packages/msprog/msprog.pdf).
Detailed indications and examples on function usage are available in the
[package vignettes](#vignette).

The computation can be run locally in R (see [installation
instructions](#install) below), or online via a user-friendly [web
application](https://msprog.shinyapps.io/msprog/).

**If you use this package in your work, please cite it [as
below](#citation)**.

**For any questions, requests for new features, or bug reporting, please
contact: noemi.montobbio@unige.it**. Any feedback is highly appreciated!

<a id="install"></a>

## Installation

The **current stable release** of `msprog` is available on CRAN. To
install it, run:

``` r
install.packages("msprog")
```

<a id="devel"></a> Alternatively, you can install the **development
version** of `msprog` from GitHub as follows.

Using `remotes`:

``` r
# install.packages("remotes") # if not already installed
remotes::install_github("noemimontobbio/msprog")
```

or using `devtools`:

``` r
# install.packages("devtools") # if not already installed
devtools::install_github("noemimontobbio/msprog")
```

## 🚀 Getting started

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
                 subj_col="id", value_col="EDSS", date_col="date",    # specify column names
                 outcome="edss",                                      # specify outcome type
                 relapse=toydata_relapses)                            # provide data on relapses
```


    ---
    Outcome: edss
    Confirmation over: 84 days (-7 days, +730.5 days)
    Baseline: fixed
    Baseline skipped if: <30 days from last relapse
    Event skipped if: -
    Confirmation visit skipped if: <30 days from last relapse
    Events detected: firstCDW


    *Please use `print(output)` to display full info on event detection criteria*


    ---
    Total subjects: 7

    ---
    Subjects with CDW: 4

Several qualitative and quantitative options for event detection are
given as arguments that can be set by the user and reported as a
complement to the results to ensure reproducibility. For example,
instead of only detecting the first confirmed disability worsening (CDW)
event for each subject, we can detect *all* disability events
sequentially by moving the baseline after each event
(`event="multiple", baseline="roving"`)\`:

``` r
output <- MSprog(toydata_visits,                                      # provide data on visits
                 subj_col="id", value_col="EDSS", date_col="date",    # specify column names
                 outcome="edss",                                      # specify outcome type
                 event="multiple", baseline="roving",                 # modify default options
                 relapse=toydata_relapses)                            # provide data on relapses
```


    ---
    Outcome: edss
    Confirmation over: 84 days (-7 days, +730.5 days)
    Baseline: roving
    Baseline skipped if: <30 days from last relapse
    Event skipped if: -
    Confirmation visit skipped if: <30 days from last relapse
    Events detected: multiple


    *Please use `print(output)` to display full info on event detection criteria*


    ---
    Total subjects: 7

    ---
    Subjects with CDW: 5

    Subjects with CDI: 2

    ---
    CDW events: 6

    CDI events: 2

The function prints out a concise report of the results, and of the
options used to obtain them. Full tabulation of the results can be
accessed via the following attributes of the function output.

1.  `results`: detailed info on each event for all subjects.

    ``` r
    print(output$results, row.names=FALSE)
    ```

         id nevent event_type total_fu bl2event time2event sust_days sust_last
          1      1        CDW      534      292        292       242      TRUE
          2      1        CDW      730      198        198        84     FALSE
          2      2        CDW      730      257        539       191      TRUE
          3      0                 491      NaN        491       NaN     FALSE
          4      1        CDI      586       77         77        98     FALSE
          4      2        CDW      586      129        304       282      TRUE
          5      1        CDW      637      140        140       497      TRUE
          6      1        CDI      491      120        120       232     FALSE
          7      1        CDW      779      372        372       407      TRUE

    where: `nevent` is the cumulative event count for each subject;
    `event_type` and `CDW_type` characterise the event; `time2event` is
    the number of days from start of follow-up to event; `bl2event` is
    the number of days from current baseline to event; `sust_days` is
    the number of days for which the event was sustained; `sust_last`
    reports whether the event was sustained until the last visit.

2.  `event_count`: a data frame summarising event counts for each
    subject, and the event sequence (where relevant).

    ``` r
    print(output$event_count)
    ```

          event_sequence CDI CDW
        1            CDW   0   1
        2       CDW, CDW   0   2
        3                  0   0
        4       CDI, CDW   1   1
        5            CDW   0   1
        6            CDI   1   0
        7            CDW   0   1

    where: `event_sequence` specifies the order of the events; the other
    columns count the events of each type.

Additionally, applying the `print` method to the `MSprog()` output
prints out the full list of function arguments, as well as a short
paragraph describing the complete set of criteria used to obtain the
output, **to be reported to ensure complete reproducibility**:

``` r
print(output)
```

    ---
    msprog version: 1.0.1 
    ---
    MSprog() arguments:
    outcome=edss, event=multiple, RAW_PIRA=FALSE, baseline=roving, proceed_from=firstconf, validconf_col=validconf, skip_local_extrema=none, conf_days=84, conf_tol_days=c(7, 730.5), require_sust_days=0, check_intermediate=TRUE, relapse_to_bl=c(30, 0), relapse_to_event=c(0, 0), relapse_to_conf=c(30, 0), relapse_assoc=c(90, 0), relapse_indep=list(prec = list(0, 0), event = list(90, 30), conf = list(90, 30), prec_type = "baseline"), renddate_col=NULL, sub_threshold_rebl=none, bl_geq=FALSE, relapse_rebl=FALSE, impute_last_visit=0, worsening=increase,
    delta_fun=NULL

    Textual description of applied criteria:
    We detected all confirmed EDSS changes (in chronological order) confirmed over 84 days (with a lower tolerance of 7 days and an upper tolerance of 730.5 days). A visit could not be used as confirmation if occurring within 30 days after the onset of a relapse. A roving baseline scheme was applied where the reference value was updated after each confirmed worsening or improvement event. The new baseline was set at the first eligible confirmation visit for the event that triggered the re-baseline. Whenever the current baseline fell within 30 days after the onset of a relapse, it was moved to the next eligible visit. 
    ---
    Clinically meaningful threshold for EDSS change (delta function): default for EDSS (1.5 if baseline=0, 1.0 if 0.0<baseline<=5.0, 0.5 if baseline>5.0)

A complete tutorial on `MSprog()` usage is available as a [package
vignette](https://cran.r-project.org/web/packages/msprog/vignettes/vignette0.html).

<a id="vignette"></a>

## 🔍 Vignettes

Package vignettes provide detailed guidance on function usage and best
practices through examples. They can be accessed from the [package
webpage](https://cran.r-project.org/package=msprog), or by typing:

``` r
browseVignettes("msprog")
```

<a id="citation"></a>

## Citation

If you use the `msprog` package, please use the `citation()` function to
obtain the correct reference:

``` r
citation("msprog")
```

    To cite package 'msprog' in publications use:

      Montobbio N, Bovis F, Hofer L, Häring DA, Benkert P, Tur C, Kalincik
      T, Sharmin S, Masot Llima A, Hernández Soria D, Salter A, Morocz I,
      Wang Q, Kuhle J, Chappell S, Capra R, Cordioli C, D'Souza M, Coetzee
      T, Montalban X, Arnold DL, Calabresi PA, Sormani MP (2026). "A
      community-validated tool for clinical outcome calculation in multiple
      sclerosis: Consensus development and scenario-specific
      recommendations." _Multiple Sclerosis Journal_.
      doi:10.1177/13524585261478492
      <https://doi.org/10.1177/13524585261478492>.

    A BibTeX entry for LaTeX users is

      @Article{,
        title = {A community-validated tool for clinical outcome calculation in multiple sclerosis: Consensus development and scenario-specific recommendations},
        author = {Noemi Montobbio and Francesca Bovis and Lisa Hofer and Dieter A. Häring and Pascal Benkert and Carmen Tur and Tomas Kalincik and Sifat Sharmin and Ariadna {Masot Llima} and Daniel {Hernández Soria} and Amber Salter and Istvan Morocz and Qing Wang and Jens Kuhle and Steven Chappell and Ruggero Capra and Cinzia Cordioli and Marcus D'Souza and Timothy Coetzee and Xavier Montalban and Douglas L. Arnold and Peter A. Calabresi and Maria P. Sormani},
        journal = {Multiple Sclerosis Journal},
        year = {2026},
        doi = {10.1177/13524585261478492},
      }

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
