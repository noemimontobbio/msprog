
<!-- README.md is generated from README.Rmd. Please edit that file -->

# msprog: reproducible assessment of disability progression in MS

<!-- badges: start -->

[![R-CMD-check](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/noemimontobbio/msprog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`msprog` provides tools for exhaustive and reproducible analysis of
disability progression in multiple sclerosis (MS) from longitudinal
data.

Its core function, `MSprog()`, detects and characterises the progression
events of an outcome measure (EDSS, NHPT, T25FW, SDMT; or any custom
outcome measure) for one or more subjects, based on repeated assessments
through time and on the dates of acute episodes.

The package also provides two toy datasets for function testing:

- `toydata_visits`: artificially generated EDSS and SDMT assessments for
  four patients;
- `toydata_relapses`: artificially generated relapse dates associated
  with the patients in `toydata_visits`.

Please refer to the documentation for function usage (e.g. `?MSprog`)
and data structure (e.g. `?toydata_visits`). A detailed tutorial is
available as a package vignette: ***Computing MS progression from
longitudinal data***.

## Installation

You can install the development version of `msprog` from
[GitHub](https://github.com/noemimontobbio/msprog) with:

``` r
# install.packages("devtools")
devtools::install_github("noemimontobbio/msprog", build_vignettes=TRUE)
```

## Usage

`MSprog()` detects the events sequentially by scanning the outcome
values in chronological order, and classifies progression events as
relapse-associated or relapse-independent based on their relative timing
with respect to the relapses
\[[1](#ref-lublin2014)–[3](#ref-silent2019)\].

Several qualitative and quantitative options for event detection are
given as arguments that can be set by the user and reported as a
complement to the results to ensure reproducibility. These include the
baseline scheme (fixed or roving), the events to be detected (first or
multiple, progression and/or improvement), the length of the relapse
influence window and of the event confirmation period(s) with the
relative tolerance.

The example below illustrates the function’s usage and outputs:

``` r
library(msprog)

# Load toy data
data(toydata_visits)
data(toydata_relapses)

# Compute progression
output <- MSprog(toydata_visits,                                   # provide data on visits
                 subj_col='id', value_col='EDSS', date_col='date', # specify column names
                 outcome='edss',                                   # specify outcome type
                 event='multiple', baseline='roving',              # modify default options on event detection
                 relapse=toydata_relapses)                         # provide data on relapses
#> 
#> ---
#> Outcome:  edss 
#> Confirmation at:  12 mm (- 30 dd, + 30 dd)
#> Baseline:  roving   
#> Relapse influence (baseline):  30 dd
#> Relapse influence (event):  0 dd
#> Relapse influence (confirmation):  30 dd
#> Events detected:  multiple
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

The function prints out a concise report of the results, as well as
**the specific set of options used to obtain them**. Complete results
are stored in an object of class `MSprogOutput`. The following methods
are available for extracting info from the output.

1.  The `event_count()` method generates a `data.frame` containing the
    event sequence detected for each subject, and the counts for each
    event type:

    ``` r
    print(event_count(output))
    #>   event_sequence improvement progression RAW PIRA undefined_prog
    #> 1           PIRA           0           1   0    1              0
    #> 2      RAW, PIRA           0           2   1    1              0
    #> 3                          0           0   0    0              0
    #> 4     impr, PIRA           1           1   0    1              0
    ```

    where: `event_sequence` specifies the order of the events; the other
    columns count the events of each type.

2.  The `results` method generates extended info on each event for all
    subjects:

    ``` r
    print(results(output), row.names=FALSE)
    #>  id nevent event_type time2event bl2event conf12 PIRA_conf12 sust_days
    #>   1      1       PIRA        443      443      1           1        91
    #>   2      1        RAW        198      198      1          NA        84
    #>   2      2       PIRA        539      257      1           1       191
    #>   3      0                   491       NA     NA          NA        NA
    #>   4      1       impr         77       77      1          NA        98
    #>   4      2       PIRA        304      129      1           1       282
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
    event was confirmed at 12 weeks; `sust_days` is the number of days
    for which the event was sustained; `sust_last` reports whether the
    event was sustained until the last visit.

3.  The `criteria_text` method prints out a short paragraph describing
    the complete set of criteria used to obtain the output, to be
    reported to ensure complete reproducibility:

    ``` r
    criteria_text(output)
    #> For each subject, we detected all EDSS changes (in chronological order) confirmed at 12 weeks, with a tolerance of 30 days on both sides. A visit could only be used as confirmation if occurring at least 30 days from a relapse. A confirmed EDSS progression event was labelled as RAW if occurring within 90 days from a relapse. A confirmed EDSS progression event was labelled as PIRA if no relapses occurred in the interval from 90 days before the event to 30 days after the event, or from 90 days before confirmation to 30 days after confirmation. A roving baseline scheme was applied where the reference value was updated after each confirmed progression or improvement event. The new reference value was set as the EDSS value at the confirmation visit. Whenever the baseline fell within 30 days from a relapse, it was moved to the next available visit.
    ```

<br />

For detailed indications on usage and best practices, please refer to
the package vignettes.

``` r
browseVignettes('msprog')
```

## References

<div id="refs" class="references csl-bib-body">

<div id="ref-lublin2014" class="csl-entry">

1\. Lublin FD, Reingold SC, Cohen JA, Cutter GR, Sørensen PS, Thompson
AJ, et al. Defining the clinical course of multiple sclerosis. Neurology
\[Internet\]. 2014;83:278–86. Available from:
<https://n.neurology.org/content/83/3/278>

</div>

<div id="ref-kappos2018" class="csl-entry">

2\. Kappos L, Butzkueven H, Wiendl H, Spelman T, Pellegrini F, Chen Y,
et al. Greater sensitivity to multiple sclerosis disability worsening
and progression events using a roving versus a fixed reference value in
a prospective cohort study. Multiple Sclerosis Journal \[Internet\].
2018;24:963–73. Available from:
<https://doi.org/10.1177/1352458517709619>

</div>

<div id="ref-silent2019" class="csl-entry">

3\. University of California SFM-ET, Cree BAC, Hollenbach JA, Bove R,
Kirkish G, Sacco S, et al. Silent progression in disease activity–free
relapsing multiple sclerosis. Annals of Neurology \[Internet\].
2019;85:653–66. Available from:
<https://onlinelibrary.wiley.com/doi/abs/10.1002/ana.25463>

</div>

</div>
