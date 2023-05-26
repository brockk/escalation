
<!-- README.md is generated from README.Rmd. Please edit that file -->

# escalation <img src="man/figures/logo.png" align="right" height=140/>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/brockk/escalation.svg?branch=master)](https://travis-ci.org/brockk/escalation)
[![Codecov test
coverage](https://codecov.io/gh/brockk/escalation/branch/master/graph/badge.svg)](https://codecov.io/gh/brockk/escalation?branch=master)
[![cran
version](http://www.r-pkg.org/badges/version/escalation)](https://cran.r-project.org/package=escalation)
![](https://cranlogs.r-pkg.org/badges/escalation)
![](https://cranlogs.r-pkg.org/badges/grand-total/escalation)
<!-- badges: end -->

by Kristian Brock. Documentation is hosted at
<https://brockk.github.io/escalation/>

## Overview

`escalation` provides a grammar for dose-finding clinical trials.

It starts by providing functions to use dose-escalation methodologies
like the continual reassessment method (CRM), the Bayesian optimal
interval design (BOIN), and the perennial 3+3:

- `get_dfcrm()`
- `get_trialr_crm()`
- `get_trialr_nbg()`
- `get_tpi()`
- `get_mtpi()`
- `get_mtpi2()`
- `get_boin()`
- `get_trialr_efftox()`
- `get_wages_and_tait()`
- `get_three_plus_three()`

These functions fetch model fitting objects. Where possible, technical
implementations are imported from existing R packages like `dfcrm`,
`trialr`, and `BOIN`. Where no external implementations is available
however, methods are implemented natively in `escalation`.

These dose-finding approaches can then be augmented with extra
behaviours to specialise the dose selection process. For example, we can
add behaviours to prevent skipping doses, or to stop when we reach a
certain sample size. `escalation` supports the following behaviours:

- `dont_skip_doses()`
- `stop_at_n()`
- `stop_when_n_at_dose()`
- `stop_when_too_toxic()`
- `stop_when_tox_ci_covered()`
- `demand_n_at_dose()`
- `try_rescue_dose()`
- `follow_path()`
- `select_dose_by_cibp()`

Each of these functions overrides the way doses are selected or when a
design decides to stop the trial. The behaviours can be flexibly
combined using the `%>%` operator from the tidyverse.

These models are then fit to trial outcomes to produce dose
recommendations. No matter how the dose selection behaviours were
combined, the resulting model fits supports a standard interface. The
two most important methods are `recommended_dose()` to get the current
dose selection, and `continue()` to learn whether the model advocates
continuing patient recruitment.

Having defined this nomenclature for combining dose selection behaviours
and providing a standard interface for the resulting analyses, it is
simple to run simulations or calculate dose-pathways for future cohorts
of patients.

`escalation` provides an object-oriented approach to dose-escalation
clinical trials in R. See [Usage](#usage)

# Usage

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

## Dose selectors

A core class in the `escalation` package is the `selector`. It
encapsulates the notion that a general dose-escalation design is able to
recommend doses, keep track of how many patients have been treated at
what doses, what toxicity outcomes have been seen, and whether a trial
should continue. This general interface is true of model-based methods
like the CRM and rule-based methods like the 3+3. Irrespective the
particular approach used, the interface is consistent.

In this tutorial, we will demonstrate each of the types of `selector`
implemented in the package and how they can be combined to tailor
behaviour.

To begin, let us load `escalation`

``` r
library(escalation)
```

    ## Loading required package: magrittr

    ## Warning: package 'magrittr' was built under R version 4.0.5

At the core of the dose selection process is an algorithm or a model
that selects doses in responses to outcomes. The classes capable of
performing this core role are:

- `get_dfcrm()`, using the model-fitting code from
  [`dfcrm`](https://cran.r-project.org/package=dfcrm)
- `get_trialr_crm()` using the model-fitting code from
  [`trialr`](https://cran.r-project.org/package=trialr)
- `get_trialr_nbg()`
- `get_boin()` using the model-fitting code from
  [`BOIN`](https://cran.r-project.org/package=BOIN)
- `get_tpi()`
- `get_mtpi()`
- `get_trialr_efftox()`
- `get_wages_and_tait()`
- `get_three_plus_three()`
- and `follow_path()`

Where indicated these methods rely on external packages. Otherwise,
methods are implemented natively in `escalation`. We look at each now.

### get_dfcrm

The continual reassessment method (O’Quigley, Pepe, and Fisher 1990)
(CRM) is implemented in the
[`dfcrm`](https://CRAN.R-project.org/package=dfcrm) package by Cheung
(2013). The very least information we need to provide is a dose-toxicity
skeleton, and our target toxicity level. The skeleton represents our
prior beliefs on the probabilities of toxicity at each of the doses
under investigation. The model iteratively seeks a dose with toxicity
probability close to the target.

For illustration, let us say we have

``` r
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
```

We create a dose-selection model using:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target)
```

and we can fit this to outcomes using code like:

``` r
fit <- model %>% fit('2NNN')
```

The `fit` object will tell you the dose recommended by the CRM model to
be administered next. Depending on your preference for classic R or
tidyverse R, you might run:

``` r
recommended_dose(fit)
```

    ## [1] 4

or

``` r
fit %>% recommended_dose()
```

    ## [1] 4

Either way, you get the same answer. The model advocates skipping
straight to dose 4. Clinicians are unlikely to feel comfortable with
this. We can respecify the model to expressly not skip doses in
escalation. We will do that later on.

For now, let us return to our model fit. We can ask whether the trial
should keep going:

``` r
fit %>% continue()
```

    ## [1] TRUE

Naturally it wants to continue because `dfcrm` does not implement any
stopping rules. Again, we will add various stopping behaviours in
sections below.

The CRM-fitting function in `dfcrm` accepts many arguments to customise
the model form and these are passed onwards by `get_dfcrm` function via
the `...` parameter. For example, to use the one-parameter logit model
in `dfcrm` (rather than the default empiric model) with the intercept
term fixed to take the value 4, we can specify:

``` r
fit <- get_dfcrm(skeleton = skeleton, target = target, 
                 intcpt = 4, model = 'logistic') %>% 
  fit('2NNN 3TNN')

fit %>% recommended_dose()
```

    ## [1] 3

`intcpt` and `logistic` are the parameter names chosen by the authors of
`dfcrm`.

### get_trialr_crm

We could instead fit the CRM models above using the
[`trialr`](https://cran.r-project.org/package=trialr) package by (@
Brock 2019, 2020).

Reusing the `skeleton` and `target` variables defined above, we fit the
same empiric model

``` r
model <- get_trialr_crm(skeleton = skeleton, target = target, model = 'empiric',
                        beta_sd = sqrt(1.34))
fit <- model %>% fit('2NNN')
```

The `dfcrm` package, unless told otherwise, assumes that you want an
empiric model where the prior variance for $\beta$ is 1.34. In the
`trialr` package, no such assumptions are made so we had to specify
those variables.

All we have changed is the method of inference. `dfcrm` uses numerical
integration to calculate posterior statistics and plugs those into the
dose-toxicity function. In contrast, `trialr` fits the model using
Hamiltonian MCMC sampling via `Stan`. Thankfully, the two models agree
on the desired next dose:

``` r
fit %>% recommended_dose()
```

    ## [1] 4

and that the trial should continue:

``` r
fit %>% continue()
```

    ## [1] TRUE

The added bonus we get from the `trialr` fit, however, is those samples
from the posterior distribution:

``` r
fit %>% prob_tox_samples() %>% head(10)
```

    ## # A tibble: 10 × 6
    ##    .draw      `1`     `2`    `3`    `4`   `5`
    ##    <chr>    <dbl>   <dbl>  <dbl>  <dbl> <dbl>
    ##  1 1     0.00107  0.00519 0.0421 0.123  0.311
    ##  2 2     0.142    0.223   0.405  0.550  0.716
    ##  3 3     0.00190  0.00810 0.0550 0.147  0.344
    ##  4 4     0.0380   0.0810  0.220  0.368  0.573
    ##  5 5     0.0312   0.0696  0.201  0.346  0.554
    ##  6 6     0.000202 0.00144 0.0195 0.0741 0.234
    ##  7 7     0.000312 0.00202 0.0239 0.0847 0.252
    ##  8 8     0.0687   0.128   0.290  0.441  0.633
    ##  9 9     0.000361 0.00226 0.0255 0.0885 0.259
    ## 10 10    0.000615 0.00340 0.0327 0.104  0.283

That facilitates really flexible inference. For example, what is the
probability that toxicity at dose 3 is at least 5% greater than that at
dose 2? Simple to answer using the posterior samples:

``` r
library(dplyr)
fit %>% prob_tox_samples() %>% 
  summarise(prob = mean(`3` > `2` + 0.05))
```

    ## # A tibble: 1 × 1
    ##    prob
    ##   <dbl>
    ## 1 0.569

‘More likely than not’, is the answer.

See the Continual Reassessment Method vignette for more details.

### get_trialr_nbg

The two-parameter logistic dose-escalation method of Neuenschwander,
Branson, and Gsponer (2008) (NBG) is implemented in the
[`trialr`](https://CRAN.R-project.org/package=trialr) package by Brock
(2020).

The very least information we need to provide is a vector of the doses
under investigation, a reference dose-level $d^*$, our target toxicity
level, and priors on the logit model intercept, $\alpha$, and dose
gradient, $\beta$.

For illustration, let us reproduce the notorious example in Figure 1 of
Neuenschwander, Branson, and Gsponer (2008) with 15 doses:

``` r
dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
outcomes <- '1NNN 2NNNN 3NNNN 4NNNN 7TT'

fit <- get_trialr_nbg(real_doses = dose, d_star = 250, target = 0.3,
                      alpha_mean = 2.15, alpha_sd = 0.84,
                      beta_mean = 0.52, beta_sd = 0.8, seed = 2020) %>% 
  fit(outcomes)
```

Sticking at dose 7 is the recommendation:

``` r
fit %>% recommended_dose()
```

    ## [1] 7

However, we see that it is a close call as to which dose is closest to
the target toxicity level:

``` r
fit %>% mean_prob_tox()
```

    ##  [1] 0.01258690 0.03252256 0.06752086 0.13847209 0.20636157 0.26917718
    ##  [7] 0.32637010 0.37801151 0.46612242 0.53711249 0.66158702 0.73901450
    ## [13] 0.82594636 0.87171076 0.89929305

This is perhaps unsurprising in a situation with so many doses.

See the Neuenschwander, Branson & Gsponer vignette for more details.

### get_tpi

The Toxicity Probability Interval (TPI) method was introduced by Ji, Li,
and Bekele (2007). The model requires a few parameters:

``` r
model <- get_tpi(num_doses = 5, target = 0.25, k1 = 1, k2 = 1.5, 
                 exclusion_certainty = 0.95)
```

The model can be fit to outcomes in the usual way:

``` r
fit <- model %>% fit('1NNT') 
```

and the returned model fit obeys the same interface as the other classes
described here. For instance, the dose recommended for the next cohort
is:

``` r
fit %>% recommended_dose()
```

    ## [1] 1

See the Toxicity Probability Interval Design vignette for more
information.

### get_mtpi

The Modified Toxicity Probability Interval (mTPI) method was introduced
by Ji et al. (2010). It is generally simpler to implement than TPI
because the $\epsilon1$ and $\epsilon2$ parameters have the intuitive
interpretation of forming the bounds of the interval that we regard as
containing doses equivalent to the target dose. For instance, if we
target a dose with toxicity probability equal to 25%, but would judge
doses in the region (20%, 30%) to be satisfactorily toxic, we run:

``` r
model <- get_mtpi(num_doses = 5, target = 0.25, 
                  epsilon1 = 0.05, epsilon2 = 0.05, exclusion_certainty = 0.95)
```

In this parameterisation, we exclude doses if we are 95% a-posteriori
sure that the associated toxicity rate exceeds the target.

We fit the model to outcomes:

``` r
fit <- model %>% fit('1NNT') 
```

and learn that the recommended next dose is

``` r
fit %>% recommended_dose()
```

    ## [1] 1

dose 1, in accordance with Figure 2 of Ji et al. (2010).

See the Modified Toxicity Probability Interval Design vignette for more
information.

### get_boin

`escalate` also implements the Bayesian Optimal Interval (BOIN)
dose-finding design by Liu and Yuan (2015) via the
[`BOIN`](https://CRAN.R-project.org/package=BOIN) package (Yuan and Liu
2018).

In contrast to CRM, BOIN does not require a dose-toxicity skeleton. In
its simplest case, it requires merely the number of doses under
investigation and our target toxicity level. Continuing with our example
above:

``` r
target <- 0.25

model <- get_boin(num_doses = 5, target = target)
```

As before, we can fit the model to some observed outcomes:

``` r
fit <- model %>% fit('2NNN')
```

and ask the recommended dose:

``` r
fit %>% recommended_dose()
```

    ## [1] 3

The BOIN dose selector natively implements stopping rules, as described
by Liu & Yuan. For instance, if the bottom dose is too toxic, the design
will advise the trial halts:

``` r
fit <- model %>% fit('2NTN 1TTT')
fit %>% continue()
```

    ## [1] FALSE

Notice in this scenario that the recommended dose is `NA`:

``` r
fit %>% recommended_dose()
```

    ## [1] NA

This clarifies that no dose should be recommended for further study. In
this setting, this is because all doses are considered too toxic. This
is distinct from scenarios where a design advocates stopping a trial and
recommending a dose for further study. We will encounter situations like
that below.

Since `escalation` provides many flexible options for stopping, we have
made it possible to suppress BOIN’s native stopping rule via
`use_stopping_rule = TRUE`.

Similar to the method described above, extra parameters are passed to
the `get.boundary` function in the `BOIN` package to customise the
escalation procedure. For instance, the boundaries that guide changes in
dose are set to be 60% and 140% of the target toxicity rate, by default.
To instead use 30% and 170%, we could run:

``` r
get_boin(num_doses = 5, target = target, 
         p.saf = 0.3 * target, p.tox = 1.7 * target) %>% 
  fit('1NNN 2NNT') %>% 
  recommended_dose()
```

    ## [1] 2

To observe the effect of the change, note that the default values
suppress escalation in this scenario:

``` r
get_boin(num_doses = 5, target = target) %>% 
  fit('1NNN 2NNT') %>% 
  recommended_dose()
```

    ## [1] 1

The parameter names `p.saf` and `p.tox` were chosen by the authors of
the `BOIN` package.

See the Bayesian Optimal Interval Design vignette for more information.

### get_three_plus_three

The 3+3 method is an old method for dose-escalation that uses fixed
cohorts of three and pre-specified rules to govern dose-selection (Korn
et al. 1994; Le Tourneau, Lee, and Siu 2009).

To create a 3+3 design, we need no more information than the number of
doses under investigation:

``` r
model <- get_three_plus_three(num_doses = 5)
```

As usual, we can fit the model to some outcomes and learn the
recommended dose:

``` r
model %>% fit('2NTN') %>% recommended_dose()
```

    ## [1] 2

Korn et al. (1994) described a variant of 3+3 that permits deescalation
to ensure that six patients are treated at a dose before it is
recommended. To use that option in our model, we could have run:

``` r
model <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE)
```

The model would then advocate deescalation if at least two toxicities
are seen at a dose and the dose below has fewer than 6 treated patients:

``` r
model %>% fit('2NTT') %>% recommended_dose()
```

    ## [1] 1

### follow_path

The final dose selector in this section is not really a model at all, so
much as a pre-specified path to follow. Let us say that we would like to
escalate through the doses in the absence of toxicity, treating two
patients at each of the first two doses, and three at the other doses.
We can specify such a path in `escalation` using:

``` r
model <- follow_path('1NN 2NN 3NNN 4NNN 5NNN')
```

When fit to data, the method just returns whatever comes next in the
sequence:

``` r
model %>% fit('1NN 2N') %>% recommended_dose()
```

    ## [1] 2

``` r
model %>% fit('1NN 2NN') %>% recommended_dose()
```

    ## [1] 3

When the outcomes diverge from the pre-specified path, however, this
selector does not know what to do:

``` r
model %>% fit('1NN 2NT') %>% recommended_dose()
```

    ## [1] NA

That rather seems to limit its value. The point of this class is that we
sometimes want to specify what is occasionally referred to as *an
initial escalation plan*. When trial outcomes diverge from the initial
plan, another method takes over. This is a perfect opportunity to show
how different selectors can be joined together. Let us say that we wish
to follow the initial plan described above, but when the first toxicity
event is seen, we want a CRM model to take over. We simply join the
functions together using the pipe operator from `magrittr`:

``` r
model <- follow_path('1NN 2NN 3NNN 4NNN 5NNN') %>% 
  get_dfcrm(skeleton = skeleton, target = target)
```

Now, when trial outcomes diverge from the path, the CRM model analyses
all of the outcomes and recommends the next dose:

``` r
model %>% fit('1NN 2NT') %>% recommended_dose()
```

    ## [1] 2

This concludes our look at the core dose-selecting classes. We now turn
our attention to the ways in which these methods can be adapted using
extra behaviours.

### dont_skip_doses

We saw in the CRM example above that the design undesirably wanted to
skip straight to a high dose, without trying some of the lower doses. A
simple and very common constraint to impose in dose-finding trials is to
avoid skipping untested doses.

Resuming our CRM example, we suppress the skipping of untested doses in
escalation with:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE)
```

We then fit the model as before:

``` r
fit <- model %>% fit('2NNN')
fit %>% recommended_dose()
```

    ## [1] 3

This time, however, the model advocates dose 3. Previously, it wanted to
go straight to dose 4.

We prevented skipping dose in escalation. We could have prevented
skipping doses in deescalation with:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_deescalating = TRUE)
```

or both with:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE, when_deescalating = TRUE)
```

### stop_at_n

Let us now investigate some methods that facilitate stopping. The
simplest condition on which to stop is when the total sample size
reaches some pre-specified level. For instance, we might want to treat a
maximum of 15 patients and then stop. To do this, we call the
`stop_at_n` function and append it onto the end of a core dose selector,
like this:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  stop_at_n(n = 15)
```

When this design has seen fewer than 15 patients, it will select doses
and advocate that the trial continues. For instance:

``` r
fit <- model %>% fit('1NNN 2TNN 2NNN 3NNN')
fit %>% continue()
```

    ## [1] TRUE

The design advocates continuing at dose:

``` r
fit %>% recommended_dose()
```

    ## [1] 3

In contrast, once 15 patients are seen,

``` r
fit <- model %>% fit('1NNN 2TNN 2NNN 3NNN 3NTN')
fit %>% continue()
```

    ## [1] FALSE

the design advocates stopping. It is important to note that, even though
the design has stopped, it still recommends that a dose be studied at
the next trial phase:

``` r
fit %>% recommended_dose()
```

    ## [1] 3

This is in contrast to the scenario where a trial is stopped because all
doses are inappropriate. In this scenario, the dose recommendation would
be `NA`. We will encounter this in examples below.

### stop_when_n\_at_dose

Another common approach is to stop a dose-finding experiment when a
given number of patients have been treated at a particular dose.

Continuing with our CRM model, to stop when nine patients have been
treated at the dose that is about to be recommended again, we use:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  stop_when_n_at_dose(n = 9, dose = 'recommended')
```

We can observe how this alters the dose-selection model. Here we see six
patients treated at dose 2:

``` r
fit <- model %>% fit('1NNN 2TNN 2NTN')
```

The model recommends that dose 2 should be given to more patients:

``` r
fit %>% continue()
```

    ## [1] TRUE

``` r
fit %>% recommended_dose()
```

    ## [1] 2

If the next cohort results in dose 2 being recommended yet again,
i.e. to bring the total number of patients at dose 2 to nine or more,
the model stops:

``` r
fit <- model %>% fit('1NNN 2TNN 2NTN 2NNN')
fit %>% continue()
```

    ## [1] FALSE

``` r
fit %>% recommended_dose()
```

    ## [1] 2

In this scenario, dose 2 is the final recommended dose and the trial
stops gracefully at a pre-specified stopping rule.

This behaviour can also be configured to stop when any dose has been
given *n* times:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  stop_when_n_at_dose(n = 9, dose = 'any')
```

or when a particular dose-level has been given *n* times:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  stop_when_n_at_dose(n = 9, dose = 3)
```

Naturally, you can combine this behaviour with other behaviours. The
following model stops the trial when nine patients have been evaluated
at the recommended dose or when 21 patients have been treated in total,
whichever occurs first:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  stop_when_n_at_dose(n = 9, dose = 'recommended') %>% 
  stop_at_n(n = 21)
```

### stop_when_ci_covered

The two stopping mechanisms above scrutinise the number of patients
treated. In many situations, this will be valuable. However, in other
situations, we might want to stop when a threshold amount of statistical
information is obtained. One way to achieve this is to stop when the
confidence interval or credible interval for the probability of toxicity
at a dose is covered by a specified range.

For instance, we know that the BOIN design seeks a target toxicity
level, and we have used a target of 25% in our examples. We might say
that we are sure enough about the recommended dose when the associated
90% credible interval (because BOIN is a Bayesian design) of the
toxicity probability falls in the region 10% - 40%.

``` r
model <- get_boin(target = target, num_doses = 5) %>%
  stop_when_tox_ci_covered(dose = 'recommended', lower = 0.10, upper = 0.4)
```

Say that we observe the following trial path:

``` r
fit <- model %>% 
  fit('1NNN 2NTN 2TNN 2NNN 2NNT 2NTN 2NNN 2TNN')
```

The design recommends dose 2 and it also advocates stopping:

``` r
fit %>% recommended_dose()
```

    ## [1] 2

``` r
fit %>% continue()
```

    ## [1] FALSE

This is because the lower bound of the 90% interval for the probability
of toxicity at dose 2 is at least 10%:

``` r
fit %>% prob_tox_quantile(p = 0.05)
```

    ##   1   2   3   4   5 
    ## 0.0 0.1  NA  NA  NA

and the upper bound is no more than 40%:

``` r
fit %>% prob_tox_quantile(p = 0.95)
```

    ##   1   2   3   4   5 
    ## 0.1 0.4  NA  NA  NA

It may be intersting to note that our CRM model would not stop in this
scenario:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_tox_ci_covered(dose = 'recommended', lower = 0.10, upper = 0.4)

fit <- model %>% 
  fit('1NNN 2NTN 2TNN 2NNN 2NNT 2NTN 2NNN 2TNN')

fit %>% continue()
```

    ## [1] TRUE

``` r
fit %>% recommended_dose()
```

    ## [1] 2

This is because the lower bound of the 90% CI falls slightly outside the
sought range:

``` r
fit %>% prob_tox_quantile(p = 0.05)
```

    ## [1] 0.04876626 0.09809797 0.24712623 0.39695491 0.59744927

As before, we can specify `dose = 'recommended'`, `dose = 'any'`, or a
particular numerical dose-level with `dose = 3`, for example.

It should be appreciated that this approach only works when the
underlying model extends a way of calculating quantiles and uncertainty
intervals. The 3+3 lacks a statistical foundation and does not offer
quantiles:

``` r
get_three_plus_three(num_doses = 5) %>% 
  fit('1NNN 2NTN') %>% 
  prob_tox_quantile(p = 0.05)
```

    ## [1] NA NA NA NA NA

### stop_when_too_toxic

The stopping rules considered so far stop a trial and recommend a dose
once some critical threshold of information is obtained. We will
naturaly want to stop if all doses are too toxic.

We saw above that some model-based dose-finding approaches can calculate
quantiles. We can take this idea further and advocate stopping when
there is sufficient evidence that the toxicity probability at some dose
exceeds a critical threshold. In such circumstances, no dose will be
recommended because all doses of the treatment will be deemed to be
excessively toxic.

Let us set up a rule to stop and recommend no dose if the probability of
toxicity at the lowest dose is too high:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.7)
```

The above examples stops when 70% of the probability mass or posterior
distribution of the probability of toxicity at dose 1 exceeds 35%. With
an isolated toxicity incidence at dose 1, the model advocates continuing
at dose 1:

``` r
fit <- model %>% fit('1NTN')
fit %>% continue()
```

    ## [1] TRUE

``` r
fit %>% recommended_dose()
```

    ## [1] 1

This is because the probability that the toxicity rate exceeds 35% is
less than 70%:

``` r
fit %>% prob_tox_exceeds(0.35) %>% round(2)
```

    ## [1] 0.35 0.53 0.82 0.95 1.00

However, with material additional toxicity at dose 1, the design now
advocates stopping:

``` r
fit <- model %>% fit('1NTN 1TTT')
fit %>% continue()
```

    ## [1] FALSE

Furthermore, no dose is recommended:

``` r
fit %>% recommended_dose()
```

    ## [1] NA

This is because we are now at least 70% sure that the lowest dose is too
toxic:

``` r
fit %>% prob_tox_exceeds(0.35) %>% round(2)
```

    ## [1] 0.87 0.95 1.00 1.00 1.00

Once again, we can specify `dose = 'recommended'`, `dose = 'any'`, or a
particular numerical dose-level with `dose = 3`, for example. We also
require that the underlying model supports the calculation of quantiles.
BOIN supports this fucntionality:

``` r
model <- get_boin(target = target, num_doses = 5) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.7)

fit <- model %>% fit('1NTN 1TTT')
fit %>% continue()
```

    ## [1] FALSE

``` r
fit %>% recommended_dose()
```

    ## [1] NA

``` r
fit %>% prob_tox_exceeds(0.35) %>% round(2)
```

    ## [1] 0.95   NA   NA   NA   NA

but a non-statistical method like 3+3 does not.

### demand_n\_at_dose

We have looked at many behaviours that provide stopping. We can also
look at some behaviours that delay stopping.

We might want to guarantee that we treat at least *n* patients at a dose
before we permit a dose-finding trial to stop. For instance, we might
not feel comfortable recommending a dose for the next phase of study if
it has only been evaluated in a small number of patients.

It makes sense for this behaviour to be used with a design that would
otherwise stop. Let us say that we would normally like to stop after 18
patients have been treated. However, we will also demand that at least 6
patients be treated at the recommended dose before stopping is allowed,
irrespective the overall sample size. We specify:

``` r
model <- get_boin(target = target, num_doses = 5) %>% 
  stop_at_n(n = 18) %>% 
  demand_n_at_dose(n = 6, dose = 'recommended')
```

In the following situation:

``` r
fit <- model %>% fit('1NNN 2NNT 3NTN 3NNN 4TTN 3NTT')
fit %>% continue()
```

    ## [1] TRUE

``` r
fit %>% recommended_dose()
```

    ## [1] 2

the design advocates continuing at dose 2 even though 18 patients have
been evaluated. This is because the `demand_n_at_dose` function is
overriding the stopping behaviour of `stop_at_n`. It is requesting that
the trial continue at dose 2 instead of stopping with only three
patients treated at the nominal recommended dose.

It is important to recognise that the order of the functions matters. If
we flip the order of the constraints in the example above, the outcome
is different:

``` r
model <- get_boin(target = target, num_doses = 5) %>% 
  demand_n_at_dose(n = 6, dose = 'recommended') %>% 
  stop_at_n(n = 18)

fit <- model %>% fit('1NNN 2NNT 3NTN 3NNN 4TTN 3NTT')
fit %>% continue()
```

    ## [1] FALSE

``` r
fit %>% recommended_dose()
```

    ## [1] 2

Now the `stop_at_n` constraint overrides the action of
`demand_n_at_dose` to halt the trial when *n=18*, even though only three
patients have been evaluated at dose 2. It overrides because it comes
later in the decision chain. Users should be aware that commands that
come later take precedence.

Once again, we can specify `dose = 'recommended'`, `dose = 'any'`, or a
particular numerical dose-level with `dose = 3`, for example.

In summary, the `demand_n_at_dose` function delays stopping in a
scenario when a dose is being selected.

### try_rescue_dose

In contrast to `demand_n_at_dose`, the `try_rescue_dose` function delays
stopping in a scenario where no dose is going to be selected. It
overrides a decision to stop and recommend no dose when fewer than *n*
patients have been evaluated at a given dose. Thus, it provides a
facility to ensure that some “rescue” dose has been tried before
stopping is allowed.

This is another function where effective demonstration requires a design
that would normally stop. Let us say that we will stop if we are 80%
sure that the toxicity rate at the lowest dose exceeds 35%. But before
we stop, we want to ensure that at least two patients have been
evaluated at the lowest dose. We write:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8) %>%
  try_rescue_dose(dose = 1, n = 2)
```

Then, even when this design sees some major toxicity at dose 2:

``` r
fit <- model %>% fit('2TTT')
fit %>% continue()
```

    ## [1] TRUE

``` r
fit %>% recommended_dose()
```

    ## [1] 1

the design will not advocate stopping, even though the posterior
confidence that the tox rate at dose 1 exceeds 35% is greater than 80%:

``` r
fit %>% prob_tox_exceeds(0.35)
```

    ## [1] 0.8673669 0.9307674 0.9857421 0.9971830 0.9998310

Once two patients are seen at dose 1, stopping can be countenanced. If
those two patients tolerate treatment at dose 1:

``` r
fit <- model %>% fit('2TTT 1NN')
fit %>% continue()
```

    ## [1] TRUE

``` r
fit %>% recommended_dose()
```

    ## [1] 1

then stopping is not advocated because the posterior belief is now that
dose 1 is not excessively toxic:

``` r
fit %>% prob_tox_exceeds(0.35)
```

    ## [1] 0.6683818 0.8195981 0.9668375 0.9951862 0.9998694

However, if even one of those patients at dose 1 experiences toxicity:

``` r
fit <- model %>% fit('2TTT 1NT')
fit %>% continue()
```

    ## [1] FALSE

``` r
fit %>% recommended_dose()
```

    ## [1] NA

Then the trial stops and no dose is recommended.

The `try_rescue_dose` function allows researchers to rescue situations
where otherwise sensible stopping criteria may prove too sensitive to
chance events in very small sample sizes.

### select_dose_by_cibp

This function implements the convex infinite bounds penalisation (CIBP)
criterion of Mozgunov and Jaki (2020) that adjusts the way doses are
selected in CRM trials. Their method is mindful of the uncertainty in
the estimates of the probability of toxicity and uses an asymmetry
parameter, 0 \< *a* \< 2, to penalise escalation to risky doses. The
method alters the way doses are selected but not when the trial should
stop. For *a \< 1*, the criterion penalises toxic doses more heavily,
making escalation decisions more conservative.

To add the behaviour to a dose-finding design, we run:

``` r
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  select_dose_by_cibp(a = 0.3)
```

The model is then fit to outcomes in the usual way:

``` r
model %>% 
  fit('1NTN') %>% 
  recommended_dose()
```

    ## [1] 1

## Simulation and dose-paths

We have described at length above the flexible methods that `escalation`
provides to specify dose-escalation designs and tailor trial behaviour.
Once designs are specified, we can investigate their operating
characteristics by simulation using the `simulate_trials` function. We
can also exhaustively calculate dose recommendations for future cohorts
using the `get_dose_paths` function. Both of these topics are the topics
of full vignettes. Please check them out.

# Installation

``` r
# Install the latest official version from CRAN with:
install.packages("escalation")

# Alternatively, install the latest code from GitHub:
devtools::install_github("brockk/escalation")
```

# Future Plans

I plan to add model-fitting functions for EWOC via
[ewoc](https://cran.r-project.org/package=ewoc), further methods for
phase I/II designs, and perhaps also methods for dual agents.

I want to investigate adding some further stopping functions like those
researched by Zohar and Chevret (2001).

Finally, I will investigate adding time-to-event versions of the designs
presented here, the so-called TITE designs. These will require a
different approach to simulation because cohorts no longer apply.

## Getting help

This package is still in active development. There are thousands of unit
tests run each time the package code is updated. However, that certainly
does not mean that the code is bug free. You should always be on the
defensive. This software is offered with no guarantee at all. If you
have found a bug, please drop me a line and also log it here:

<https://github.com/brockk/escalation/issues>

If you want help using the package, feel free to contact me by email.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Brock2019" class="csl-entry">

Brock, Kristian. 2019. “<span class="nocase">trialr: Bayesian Clinical
Trial Designs in R and Stan</span>.” *arXiv e-Prints*, June,
arXiv:1907.00161. <https://arxiv.org/abs/1907.00161>.

</div>

<div id="ref-trialr" class="csl-entry">

———. 2020. *Trialr: Clinical Trial Designs in ’Rstan’*.
<https://cran.r-project.org/package=trialr>.

</div>

<div id="ref-Brock2017a" class="csl-entry">

Brock, Kristian, Lucinda Billingham, Mhairi Copland, Shamyla Siddique,
Mirjana Sirovica, and Christina Yap. 2017. “Implementing the EffTox
Dose-Finding Design in the Matchpoint Trial.” *BMC Medical Research
Methodology* 17 (1): 112. <https://doi.org/10.1186/s12874-017-0381-x>.

</div>

<div id="ref-dfcrm" class="csl-entry">

Cheung, Ken. 2013. *Dfcrm: Dose-Finding by the Continual Reassessment
Method*. <https://CRAN.R-project.org/package=dfcrm>.

</div>

<div id="ref-Ji2007" class="csl-entry">

Ji, Yuan, Yisheng Li, and B. Nebiyou Bekele. 2007. “<span
class="nocase">Dose-finding in phase I clinical trials based on toxicity
probability intervals</span>.” *Clinical Trials* 4 (3): 235–44.
<https://doi.org/10.1177/1740774507079442>.

</div>

<div id="ref-Ji2010" class="csl-entry">

Ji, Yuan, Ping Liu, Yisheng Li, and B. Nebiyou Bekele. 2010. “<span
class="nocase">A modified toxicity probability interval method for
dose-finding trials</span>.” *Clinical Trials* 7 (6): 653–63.
<https://doi.org/10.1177/1740774510382799>.

</div>

<div id="ref-korn1994" class="csl-entry">

Korn, Edward L., Douglas Midthune, T. Timothy Chen, Lawrence V.
Rubinstein, Michaele C. Christian, and Richard M. Simon. 1994. “A
Comparison of Two Phase I Trial Designs.” *Statistics in Medicine* 13
(18): 1799–1806. <https://doi.org/10.1002/sim.4780131802>.

</div>

<div id="ref-LeTourneau2009" class="csl-entry">

Le Tourneau, Christophe, J. Jack Lee, and Lillian L. Siu. 2009. “Dose
Escalation Methods in Phase i Cancer Clinical Trials.” *Journal of the
National Cancer Institute* 101 (10): 708–20.
<https://doi.org/10.1093/jnci/djp079>.

</div>

<div id="ref-liu_bayesian_2015" class="csl-entry">

Liu, Suyu, and Ying Yuan. 2015. “Bayesian Optimal Interval Designs for
Phase I Clinical Trials.” *Journal of the Royal Statistical Society:
Series C (Applied Statistics)* 64 (3): 507–23.
<https://doi.org/10.1111/rssc.12089>.

</div>

<div id="ref-mozgunov_improving_2020" class="csl-entry">

Mozgunov, Pavel, and Thomas Jaki. 2020. “Improving Safety of the
Continual Reassessment Method via a Modified Allocation Rule.”
*Statistics in Medicine* 39 (7): 906–22.
<https://doi.org/10.1002/sim.8450>.

</div>

<div id="ref-Neuenschwander2008" class="csl-entry">

Neuenschwander, Beat, Michael Branson, and Thomas Gsponer. 2008. “<span
class="nocase">Critical aspects of the Bayesian approach to phase I
cancer trials</span>.” *Statistics in Medicine* 27: 2420–39.
<https://doi.org/10.1002/sim.3230>.

</div>

<div id="ref-OQuigley1990" class="csl-entry">

O’Quigley, J, M Pepe, and L Fisher. 1990. “Continual Reassessment
Method: A Practical Design for Phase 1 Clinical Trials in Cancer.”
*Biometrics* 46 (1): 33–48. <https://doi.org/10.2307/2531628>.

</div>

<div id="ref-BOIN" class="csl-entry">

Yuan, Ying, and Suyu Liu. 2018. *BOIN: Bayesian Optimal INterval (BOIN)
Design for Single-Agent and Drug- Combination Phase i Clinical Trials*.
<https://CRAN.R-project.org/package=BOIN>.

</div>

<div id="ref-zohar_continual_2001" class="csl-entry">

Zohar, Sarah, and Sylvie Chevret. 2001. “The Continual Reassessment
Method: Comparison of Bayesian Stopping Rules for Dose-Ranging Studies.”
*Statistics in Medicine* 20 (19): 2827–43.
<https://doi.org/10.1002/sim.920>.

</div>

</div>
