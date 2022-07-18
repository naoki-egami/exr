exr: Quantifying Robustness to External Validity Bias
=====================================================

**Description:**

R package `exr` estimates robustness of experimental results to external
validity bias. Please read Devaux and Egami (2022+) for details about
the proposed method.

**Authors:**

-   [Naoki Egami](https://naokiegami.com) (Maintainer)
-   [Martin
    Devaux](https://polisci.columbia.edu/content/martin-samuel-devaux)

**Reference:**

-   Devaux and Egami. (2022+). [Quantifying Robustness to External
    Validity Bias.](https://naokiegami.com/paper/external_robust.pdf)
    (Working Paper).

**Overview:**

External validity of experimental results is essential in the social
sciences. Existing methods for external validity estimate causal effects
in a target population, called the target population average treatment
effect (T-PATE). However, these methods are sometimes difficult to
implement either because it is infeasible to obtain data for the target
population or because there is no target population that analysts and
skeptics can agree on. Devaux and Egami (2022+) consider a different
goal — quantifying how robust an experiment is to external validity
bias. In particular, Devaux and Egami (2022+) propose a measure of
*external robustness* by estimating how much different populations
should be from the experimental sample to explain away the T-PATE. This
R package `exr` allows users to estimate external robustness.

The three advantages of the proposed approach are as follows.

1.  Unlike the standard generalization approach, estimation of external
    robustness only requires experimental data and does not require any
    population data. In practice, researchers can estimate external
    robustness in any experimental study without collecting additional
    data.

2.  We prove that the proposed estimator is consistent to the true
    external robustness under common generalization assumptions and,
    more importantly, has simple interpretation even when those
    assumptions are violated.

3.  We provide simple default benchmarks to help interpret the degree of
    external robustness in each application.

Practical Guides
----------------

-   **Step 1**: Users specify `covariates` that are important for
    treatment effect heterogeneity. Importantly, researchers can use all
    covariates measured in the experimental data.

-   **Step 2**: Users can run `exr()` to estimate external robustness as
    a measure between 0 and 1.

-   **Step 3**: Users can run `summary()` to summarize results.
    `summary()` displays benchmarks (0.14 and 0.57) to help
    substantively interpret the degree of estimated external robustness.
    Users can visualize the distribution of CATEs and estimated external
    robustness using `plot()`.

Installation Instructions
-------------------------

You can install the most recent development version using the `devtools`
package. First you have to install `devtools` using the following code.
Note that you only have to do this once:

``` r
if(!require(devtools)) install.packages("devtools")
```

Then, load `devtools` and use the function `install_github()` to install
`exr`:

``` r
library(devtools)
install_github("naoki-egami/exr", dependencies = TRUE)
```

Get Started
-----------

Here, we provide an example to illustrate the use of function `exr()`.

### First Estimate the SATE.

Researchers can first use their favoriate approach to estimate the
sample average treatment effect (SATE).

``` r
library(exr)
data(LaLonde) # Use LaLonde data as an example

covariates <- c("age", "educ", "black","hisp", "marr", "nodegr", "log.re75","u75") 
for_sate <- as.formula(paste0("outcome ~ treat + ", paste(covariates, collapse = "+")))

lm_sate <- lm(for_sate, data = LaLonde)
sate_est <- summary(lm_sate)$coef["treat", c(1,2)]
sate_est
```

    ##   Estimate Std. Error 
    ## 0.06965018 0.03563844

### Estimate External Robustness

Then, researchers can use `exr()` to estimate external robustness. In
most scenarios, users only need to specify the following 5 arguments. We
describe other optinal arguments in the package
[manual](https://github.com/naoki-egami/exr/blob/master/manual/exr_0.1.0.pdf).

<table>
<colgroup>
<col style="width: 26%" />
<col style="width: 73%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Argument</th>
<th style="text-align: left;">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>outcome</code></td>
<td style="text-align: left;">A variable name in the data that corresponds to the outcome variable.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>treatment</code></td>
<td style="text-align: left;">A variable name in the data that corresponds to the treatment variable. The treatment variable needs to be binary (i.e., contains two levels). If users are interested in categorical treatments (e.g., control, treatment 1, and treatment 2), they can estimate external robustness by running <code>exr()</code> twice; using treatment 1 and treatment 2, respectively.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>covariates</code></td>
<td style="text-align: left;">A vector. Variable names in the data that correspond to covariates users adjust for.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">A data frame. The class should be <code>data.frame</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>sate_estimate</code></td>
<td style="text-align: left;">A vector of length 2. A point estimate of the SATE and its standard error. Default is <code>NULL</code>. When <code>sate_estimate = NULL</code>, the package internally estimates the SATE using a linear regression of the outcome on the treatment and all specified covariates.</td>
</tr>
</tbody>
</table>

``` r
exr_out <- exr(outcome = "outcome", 
               treatment = "treat", 
               covariates = c("age", "educ", "black","hisp", "marr", "nodegr", "log.re75","u75"), 
               data = LaLonde,
               sate_estimate = sate_est) 
```

    ## Estimating CATE with grf...
    ## Estimating External Robustness...

``` r
summary(exr_out)
```

    ## 
    ## CATE Estimator: grf
    ## 
    ## -------------------------
    ## External Robustness: 0.35
    ## -------------------------
    ## 
    ##    Estimate   With CI 
    ##    0.349225 *       0 
    ## ---
    ## Note: 0 ' ' 0.14 (Probability Surveys) '*' 0.57 (MTurk Samples) '**' 1

Use `plot` to visualize the estimated CATEs and estimated external
robustness.

``` r
plot(exr_out)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

When `uncertainty = TRUE`, we visualize the distribution of CATEs that
are shifted based on an adjusted threshold. For example, suppose the
SATE is positive and we consider the 95% confidence interval. Then, the
adjusted threshold is 1.96 x Standard Error. Therefore, we visualize the
distribution of CATEs - 1.96 x Standard Error.

``` r
plot(exr_out, uncertainty = TRUE)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
