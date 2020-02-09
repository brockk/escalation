
<!-- README.md is generated from README.Rmd. Please edit that file -->

# escalation <img src="man/figures/logo.png" align="right" height=140/>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/brockk/escalation.svg?branch=master)](https://travis-ci.org/brockk/escalation)
[![cran
version](http://www.r-pkg.org/badges/version/escalation)](https://cran.r-project.org/package=escalation)
![](https://cranlogs.r-pkg.org/badges/escalation)
![](https://cranlogs.r-pkg.org/badges/grand-total/escalation)
<!-- badges: end -->

by Kristian Brock

## Overview

`escalation` provides a grammar for dose-finding clinical trials.

It starts by providing functions to use dose-escalation methodologies
like CRM, BOIN, and 3+3. Largely, model-fitting code is imported from
existing packages:

  - `get_dfcrm()` uses `dfcrm`
  - `get_boin()` uses `BOIN`
  - `get_three_plus_three()`

These models are fit to trial outcomes to produce dose selector objects
that support a common interface. The two most important methods are
`recommended_dose()` to get the current dose selection, and `continue()`
to learn whether the model advocates continuing patient recruitment.

With a little bit of plumbing, a dose-selection function could be
imported from practically any dose-finding package in R and made to act
the same way.

`escalation` then adds optional embellishments to provide extra
desirable behaviour:

  - `dont_skip_doses()`
  - `stop_at_n()`
  - `stop_when_n_at_dose()`
  - `stop_when_too_toxic()`
  - `demand_n_at_dose()`

Each of these functions overrides the way doses are selected or when a
design decides to stop. A powerful feature of `escalation` is that all
of these objects supports exactly the same interface, so they can be
daisy-chained together using `%>%` operator from the `magrittr` package
to create dose-selectors that perform exactly how you want.

Furthermore, having defined this flexible interface for creating
dose-finding designs, it is simple to run simulations or calculate
dose-pathways for future cohorts of patients.

See [Usage](#usage)

# Installation

``` r
# Install the latest official version from CRAN with:
install.packages("escalation")

# Alternatively, install the latest code from GitHub:
devtools::install_github("brockk/escalation")
```

## Describing outcomes in dose-finding trials

`escalation` uses a succinct syntax for describing dose-finding
outcomes, described in Brock (2019) for the phase I setting and in Brock
et al. (2017) for the phase I/II setting.

In a phase I trial, we use the letters:

  - `T` to show that toxicity occurred in a patient;
  - `N` to show that toxicity did not occur in a patient.

In a joint phase I/II trial, like those supported by EffTox, where we
have coincident efficacy and toxicity outcomes, those relevant letters
are:

  - `T` to show that toxicity without efficacy occurred in a patient;
  - `E` to show that efficacy without toxicity occurred in a patient;
  - `N` to show that neither occurred;
  - `B` to show that both occurred.

These outcome letters are strewn behind integer dose-levels to show the
outcomes of patients in cohorts. To show that a cohort a three patients
was given dose 2, that the first two patients were without toxicity, but
the third patient experienced toxicity, we would use the outcome string:

``` r
outcomes <- '2NNT'
```

If that cohort was followed by another cohort of three, all of which
were without toxicity, the overall outcome string would be:

``` r
outcomes <- '2NNT 2NNN'
```

And so on. These strings are used in the `escalate` package to make it
easy to fit models to observed outcomes. There are many examples below.

## Usage

``` r
library(escalation)
```

    ## Loading required package: magrittr

### CRM

Let’s fit a continual reassessment method (CRM) (O’Quigley, Pepe, and
Fisher 1990) model to some outcomes using the code implementation in the
[`dfcrm`](https://CRAN.R-project.org/package=dfcrm) package by Cheung
(2013).

The very least information we need to provide is a dose-toxicity
skeleton, and our target toxicity level.

``` r
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

model <- get_dfcrm(skeleton = skeleton, target = target)
```

The `model` can then be fit to outcomes:

``` r
fit <- model %>% fit('2NNN')
```

and the `fit` object will tell you the dose recommended by the CRM model
to be administered next:

``` r
fit %>% recommended_dose()
```

    ## [1] 4

The model advocates skipping straight to dose 4. Clinicians are unlikely
to feel comfortable with this. We can respecify the model to expressly
not skip doses in escalation:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE)

fit <- model %>% fit('2NNN')
fit %>% recommended_dose()
```

    ## [1] 3

After the addition of `dont_skip_doses`, the two components are now
working in concert to conduct the dose-finding experiment. The net
effect here is that dose 3 is recommended for the next patient, rather
than being skipped.

We can ask whether the trial wants to keep going:

``` r
fit %>% continue()
```

    ## [1] TRUE

Naturally it wants to continue because `dfcrm` does not implement any
stopping rules. However, we can easily add some. Let us say that we want
to stop once the model has evaluated 18 patients, or at least 9 at the
dose being recommended, whichever occurs first. We specify this model by
adding more behaviours:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE) %>% 
  stop_at_n(n = 18) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 9)
```

Let’s fit this model to some more patients to see how this trial plays
out:

``` r
fit <- model %>% fit('2NNN 3TTN')
fit %>% recommended_dose()
```

    ## [1] 2

``` r
fit %>% continue()
```

    ## [1] TRUE

The trial wants to de-escalate and continue. Let’s do that:

``` r
fit <- model %>% fit('2NNN 3TTN 2TNN')
fit %>% recommended_dose()
```

    ## [1] 2

``` r
fit %>% continue()
```

    ## [1] TRUE

Again, the design wants to continue at dose 2. Let’s do that:

``` r
fit <- model %>% fit('2NNN 3TTN 2TNN 2NNT')
fit %>% recommended_dose()
```

    ## [1] 2

``` r
fit %>% continue()
```

    ## [1] FALSE

Now the design wants to stop and recommend dose 2.

``` r
fit %>% n_at_dose()
```

    ## [1] 0 9 3 0 0

This is because it has now seen 9 patients at dose 2 so that our
stopping criteria have been met.

``` r
fit %>% mean_prob_tox()
```

    ## [1] 0.1845713 0.2728713 0.4575229 0.5964102 0.7496662

We see that dose 2 is indeed the dose with posterior expected toxicity
rate closest to the target of 25%.

### BOIN

`escalate` also implements the BOIN dose-finding design by Liu and Yuan
(2015) via the [`BOIN`](https://CRAN.R-project.org/package=BOIN) package
(Yuan and Liu 2018).

In contrast to CRM, BOIN does not require a dose-toxicity skeleton. In
its simplest case, it requires merely the number of doses under
investigation and our target toxicity level:

``` r
target <- 0.25

model <- get_boin(num_doses = 5, target = target)
```

As before, we can fit the model to some observed outcomes:

``` r
fit <- model %>% fit('2NNN')
fit %>% recommended_dose()
```

    ## [1] 3

``` r
fit %>% continue()
```

    ## [1] TRUE

The BOIN dose selector natively implements stopping rules, as described
by Suyu & Yuan. For instance, if the bottom dose is too toxic, the
design will advise the trial halts:

``` r
fit <- model %>% fit('2NTN 1TTT')
fit %>% recommended_dose()
```

    ## [1] NA

``` r
fit %>% continue()
```

    ## [1] FALSE

Nevertheless, as with the CRM examples above, our BOIN selector can be
adorned with various rules to control sample size, etc:

``` r
model <- get_boin(num_doses = 5, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE) %>% 
  stop_at_n(n = 18) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 9)

fit <- model %>% fit('2NNN 3TTN 2TNN 2NNT')
fit %>% recommended_dose()
```

    ## [1] 2

``` r
fit %>% continue()
```

    ## [1] FALSE

## Future Plans

I plan to add model-fitting functions for:

  - CRM and EffTox via
    [trialr](https://cran.r-project.org/package=trialr)
  - CRM via [crmPack](https://cran.r-project.org/package=crmPack)
  - EWOC and mTPI once I discover how those designs are implemented in
    R.

I want to add stopping functions for each of the approaches investigated
by Zohar and Chevret (2001).

Finally, I want to add general functions to perform simulations and
calculate all possible future dose paths. Given the standard interface
implemented in `escalation`, each of these approaches will be opened up
to every specifiable dose selector. That is the beauty of
object-oriented programming.

## Getting help

This package is in its infancy. If you want help using it, please
contact me.

If you have found a bug, please drop me a line and also log it here:

<https://github.com/brockk/escalation/issues>

## References

<div id="refs" class="references">

<div id="ref-Brock2019">

Brock, Kristian. 2019. “trialr: Bayesian Clinical Trial Designs in R and
Stan.” *arXiv E-Prints*, June, arXiv:1907.00161.

</div>

<div id="ref-Brock2017a">

Brock, Kristian, Lucinda Billingham, Mhairi Copland, Shamyla Siddique,
Mirjana Sirovica, and Christina Yap. 2017. “Implementing the EffTox
Dose-Finding Design in the Matchpoint Trial.” *BMC Medical Research
Methodology* 17 (1): 112. <https://doi.org/10.1186/s12874-017-0381-x>.

</div>

<div id="ref-dfcrm">

Cheung, Ken. 2013. *Dfcrm: Dose-Finding by the Continual Reassessment
Method*. <https://CRAN.R-project.org/package=dfcrm>.

</div>

<div id="ref-liu_bayesian_2015">

Liu, Suyu, and Ying Yuan. 2015. “Bayesian Optimal Interval Designs for
Phase I Clinical Trials.” *Journal of the Royal Statistical Society:
Series C (Applied Statistics)* 64 (3): 507–23.
<https://doi.org/10.1111/rssc.12089>.

</div>

<div id="ref-OQuigley1990">

O’Quigley, J, M Pepe, and L Fisher. 1990. “Continual Reassessment
Method: A Practical Design for Phase 1 Clinical Trials in Cancer.”
*Biometrics* 46 (1): 33–48. <https://doi.org/10.2307/2531628>.

</div>

<div id="ref-BOIN">

Yuan, Ying, and Suyu Liu. 2018. *BOIN: Bayesian Optimal Interval (Boin)
Design for Single-Agent and Drug- Combination Phase I Clinical Trials*.
<https://CRAN.R-project.org/package=BOIN>.

</div>

<div id="ref-zohar_continual_2001">

Zohar, Sarah, and Sylvie Chevret. 2001. “The Continual Reassessment
Method: Comparison of Bayesian Stopping Rules for Dose-Ranging Studies.”
*Statistics in Medicine* 20 (19): 2827–43.
<https://doi.org/10.1002/sim.920>.

</div>

</div>
