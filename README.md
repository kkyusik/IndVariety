---
title: "dd"
layout: page
---
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IndVariety

## Industrial variety

Related variety of regional industry can be Jacobs externalities, which
means knowledge spillovers between related corporations or industries.
In contrast, the unrelated variety that means the extent of
dissimilarity within industries in a given region, which addresses the
effects of a portfolio, in fact, the higher unrelated variety means the
stronger from external economic shocks.

Each variety is calculated from the entropy index. The advantage of
using entropy index is that the index cannot cause multicollinearity in
linear regression model (FRENKENÃ, VAN OORTÃ, and VERBURG 2007). To
calculate related and unrelated variety I refer to the research of
FRENKENÃ, VAN OORTÃ, and VERBURG (2007), and the index can be calculated
as follow:

\[
P_g = \sum_{i \in S}P_i
\]

\(P_g\) is calculated by summation of shares of 5-digit sectors within
2-digit sectors \(S_g\). Based on \(P_g\), unrelated variety is derived
in 2-digit levels.

\[
UV = \sum_{g=1}^{G}P_g \log \left(\frac{1}{P_g}\right)
\]

Within each 2-digit level, related variety is weighted sum of entropy.

\[
RV = \sum_{g=1}^{G}P_g H_g
\]

And \(H_g\) is as follow,

\[
H_g = \sum_{i \in S_g}\frac{p_i}{P_g} \log_2 \left(\frac{1}{p_i/P_g}\right)
\]

## Install package

``` r
remotes::install_github("kkyusik/IndVariety")
```

## usage

UV(data, year) RV(data, year) employee\_number(data, year)

## parameters

`data` is dataframe of Korean Business Survey data. You are able to
download the data from [Microdata Integrated
Service](https://mdis.kostat.go.kr/index.do). `year` indicates what you
want to calculate year of data.

## example

``` r
library(IndVariety)

# Load dataframe
data <- read.table("your data", colClasses = "character")

# Calculation Unrelated Variety
UV(data = data, year = 2013)

# Calculation Related Variety
RV(data = data, year = 2013)

# Calculation the number of total workers
employee_number(data = data, year = 2013)
```

**Reference**

<div id="refs" class="references">

<div id="ref-frenken_2007">

FRENKENÃ, KOEN, FRANK VAN OORTÃ, and THIJS VERBURG. 2007. “Related
Variety, Unrelated Variety and Regional Economic Growth.” *Regional
Studies* 41: 685–97.

</div>

</div>
