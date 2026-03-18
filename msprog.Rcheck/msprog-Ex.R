pkgname <- "msprog"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "msprog-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('msprog')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MSprog")
### * MSprog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MSprog
### Title: Assess multiple sclerosis disability course from longitudinal
###   data.
### Aliases: MSprog

### ** Examples

# 1. EDSS course
output <- MSprog(toydata_visits, subj_col='id', value_col='EDSS', date_col='date', outcome='edss',
    relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
    event='multiple', baseline='roving', verbose=1)
print(output$results) # extended info on each event for all subjects
print(output$event_count) # summary of event sequence for each subject
# 2. SDMT course
output <- MSprog(toydata_visits, subj_col='id', value_col='SDMT', date_col='date', outcome='sdmt',
    relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
    event='multiple', baseline='roving', verbose=1)
print(output$results) # extended info on each event for all subjects
print(output$event_count) # summary of event sequence for each subject
# 3. SDMT course, with a custom delta function
my_sdmt_delta <- function(reference_value) {min(c(reference_value/10, 3))}
output <- MSprog(toydata_visits, subj_col='id', value_col='SDMT', date_col='date', outcome='sdmt',
    delta_fun=my_sdmt_delta,
    relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
    event='multiple', baseline='roving', verbose=1)
print(output$results) # extended info on each event for all subjects
print(output$event_count) # summary of event sequence for each subject



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MSprog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compute_delta")
### * compute_delta

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compute_delta
### Title: Definition of minimum clinically meaningful shift for different
###   scales.
### Aliases: compute_delta

### ** Examples

compute_delta(4.5) # default outcome is 'edss'
compute_delta(55, outcome='sdmt')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compute_delta", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_event")
### * is_event

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_event
### Title: Compare value to reference.
### Aliases: is_event

### ** Examples

is_event(x=4.5, baseline=4, type="wors", outcome="edss")
is_event(x=50, baseline=57, type="wors", outcome="sdmt")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_event", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.MSprogOutput")
### * print.MSprogOutput

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.MSprogOutput
### Title: Textual description of criteria used to assess disability
###   course.
### Aliases: print.MSprogOutput

### ** Examples

output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
    relapse=toydata_relapses, conf_days=7*12, conf_tol_days=30,
    event='multiple', baseline='roving', verbose=2)
print(output) # textual description of parameters used to obtain output



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.MSprogOutput", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("toydata_relapses")
### * toydata_relapses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: toydata_relapses
### Title: Synthetic Relapse Data
### Aliases: toydata_relapses
### Keywords: datasets

### ** Examples


data(toydata_relapses)
head(toydata_relapses)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("toydata_relapses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("toydata_visits")
### * toydata_visits

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: toydata_visits
### Title: Synthetic Longitudinal EDSS and SDMT Data
### Aliases: toydata_visits
### Keywords: datasets

### ** Examples


head(toydata_visits)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("toydata_visits", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
