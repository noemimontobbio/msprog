
<!-- README.md is generated from README.Rmd. Please only edit README.Rmd -->

# msprog: reproducible assessment of disability course in MS

<!-- badges: start -->

[![R-CMD-check](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`msprog` is an R package providing tools for exhaustive and reproducible
analysis of disability progression in multiple sclerosis (MS) from
longitudinal data \[[1](#ref-msprog2024)\].

Its core function, `MSprog()`, detects and characterises the evolution
of an outcome measure (EDSS, NHPT, T25FW, SDMT; or any custom outcome
measure) for one or more subjects, based on repeated assessments through
time and on the dates of acute episodes (if any).

The package also provides two toy datasets for function testing:

- `toydata_visits`: artificially generated EDSS and SDMT assessments for
  four patients;
- `toydata_relapses`: artificially generated relapse dates associated
  with the patients in `toydata_visits`.

Please refer to the documentation for function usage (e.g. `?MSprog`)
and data structure (e.g. `?toydata_visits`). The whole documentation is
contained into the [reference manual (PDF)](msprog.pdf). Additionally, a
detailed tutorial providing examples and best-practice tips is available
as a [package vignette](#vignette): *Analysing disability course in MS*.

The outcome computation can be run locally on any computer with R
version $\geq$ 3.5.0 (see installation instructions below), or online
via our user-friendly [web
application](https://msprog.shinyapps.io/msprog/).

**If you use this package in your work, please cite it [as
below](#citation)**.

For any questions, requests for new features, or bug reporting, please
contact: noemi.montobbio@edu.unige.it.<br /> Any feedback is highly
appreciated!

<a id="install"></a>

## Installation

You can install the development version of `msprog` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("noemimontobbio/msprog", build_vignettes=TRUE)
```

## Usage

`MSprog()` detects the events sequentially by scanning the outcome
values in chronological order, and classifies progression events as
relapse-associated or relapse-independent based on their relative timing
with respect to the relapses
\[[2](#ref-lublin2014)–[4](#ref-silent2019)\].

Several qualitative and quantitative options for event detection are
given as arguments that can be set by the user and reported as a
complement to the results to ensure reproducibility. These include the
baseline scheme (fixed or roving), the events to be detected (first or
multiple, progression and/or improvement), the length of the relapse
influence window and of the event confirmation period(s) with the
relative tolerance.

The example below illustrates the function’s usage and output:

``` r
library(msprog)

# Load toy data
data(toydata_visits)
data(toydata_relapses)

# Compute progression
output <- MSprog(toydata_visits,                                      # provide data on visits
                 subj_col='id', value_col='EDSS', date_col='date',    # specify column names
                 outcome='edss',                                      # specify outcome type
                 event='multiple', baseline='roving',                 # modify default options
                 conf_tol_days=0, conf_unbounded_right=T,             # modify default options
                 relapse=toydata_relapses)                            # provide data on relapses
#> 
#> ---
#> Outcome: edss
#> Confirmation over: 12weeks (-0 days, +Inf days)
#> Baseline: roving
#> Relapse influence (baseline): 30 days
#> Relapse influence (event): 0 days
#> Relapse influence (confirmation): 30 days
#> Events detected: multiple
#> 
#> ---
#> Total subjects: 4
#> ---
#> Progressed subjects: 3 (PIRA: 3; RAW: 1)
#> Improved subjects: 1
#> ---
#> Progression events: 4 (PIRA: 3; RAW: 1)
#> Improvement events: 1
```

The function prints out a concise report of the results, and of the
options used to obtain them. Complete results are stored in an object of
class `MSprogOutput` with the following attributes.

1.  `event_count`: a `data.frame` containing the event sequence detected
    for each subject, and the counts for each event type.

    ``` r
    print(output$event_count)
    #>   event_sequence improvement progression RAW PIRA undefined_prog
    #> 1           PIRA           0           1   0    1              0
    #> 2      RAW, PIRA           0           2   1    1              0
    #> 3                          0           0   0    0              0
    #> 4     impr, PIRA           1           1   0    1              0
    ```

    where: `event_sequence` specifies the order of the events; the other
    columns count the events of each type.

2.  `results`: extended info on each event for all subjects.

    ``` r
    print(output$results, row.names=FALSE)
    #>  id nevent event_type total_fu time2event bl2event conf12 PIRA_conf12 sust_days
    #>   1      1       PIRA      534        292      292      0           0       242
    #>   2      1        RAW      730        198      198      0          NA        84
    #>   2      2       PIRA      730        539      257      0           0       191
    #>   3      0                 491        491       NA     NA          NA        NA
    #>   4      1       impr      586         77       77      1          NA        98
    #>   4      2       PIRA      586        304      129      0           0       282
    #>  sust_last
    #>          1
    #>          0
    #>          1
    #>         NA
    #>          0
    #>          1
    ```

    where: `nevent` is the cumulative event count for each subject;
    `event_type` characterises the event; `time2event` is the number of
    days from start of follow-up to event; `bl2event` is the number of
    days from current baseline to event; `conf12` reports whether the
    event was confirmed over 12 weeks; `sust_days` is the number of days
    for which the event was sustained; `sust_last` reports whether the
    event was sustained until the last visit.

Additionally, applying the `print` method to an object of class
`MSprogOutput` prints out a short paragraph describing the complete set
of criteria used to obtain the output, **to be reported to ensure
complete reproducibility**:

``` r
print(output)
#> For each subject, we detected all EDSS changes (in chronological order) confirmed over 12 weeks or more. A visit could not be used as confirmation if occurring within 30 days from the onset of a relapse. A roving baseline scheme was applied where the reference value was updated after each confirmed progression or improvement event. The new baseline was set as the first available confirmation visit. Whenever the current baseline fell within 30 days from the onset of a relapse, it was moved to the next available visit. A confirmed EDSS progression event was labelled as RAW if occurring within 90 days from the onset of a relapse. A confirmed EDSS progression event was labelled as PIRA if no relapses started in the interval from 90 days before the event to 30 days after the event, or from 90 days before confirmation to 30 days after confirmation.
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
#> 
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
Bovis F, et al. [Creating an automated tool for a consistent and
repeatable evaluation of disability progression in clinical studies for
multiple sclerosis.](https://doi.org/10.1177/13524585241243157) Mult
Scler. 2024;30:1185–92.

</div>

<div id="ref-lublin2014" class="csl-entry">

2\. Lublin FD, Reingold SC, Cohen JA, Cutter GR, Sørensen PS, Thompson
AJ, et al. Defining the clinical course of multiple sclerosis. Neurology
\[Internet\]. 2014;83:278–86. Available from:
<https://n.neurology.org/content/83/3/278>

</div>

<div id="ref-kappos2018" class="csl-entry">

3\. Kappos L, Butzkueven H, Wiendl H, Spelman T, Pellegrini F, Chen Y,
et al. Greater sensitivity to multiple sclerosis disability worsening
and progression events using a roving versus a fixed reference value in
a prospective cohort study. Multiple Sclerosis Journal \[Internet\].
2018;24:963–73. Available from:
<https://doi.org/10.1177/1352458517709619>

</div>

<div id="ref-silent2019" class="csl-entry">

4\. University of California SFM-ET, Cree BAC, Hollenbach JA, Bove R,
Kirkish G, Sacco S, et al. Silent progression in disease activity–free
relapsing multiple sclerosis. Annals of Neurology \[Internet\].
2019;85:653–66. Available from:
<https://onlinelibrary.wiley.com/doi/abs/10.1002/ana.25463>

</div>

</div>
