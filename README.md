
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
linear regression model (Frenken, Van Oort, and Verburg 2007). To
calculate related and unrelated variety I refer to the research of
(Frenken, Van Oort, and Verburg 2007).

## Install package

``` r
remotes::install_github("kkyusik/IndVariety")
```

## Usage

``` r
UV(data, year)
RV(data, year)
employee_number(data, year)
```

## Parameters

`data` is dataframe of Korean Business Survey data. You are able to
download the data from [Microdata Integrated
Service](https://mdis.kostat.go.kr/index.do). `year` indicates what you
want to calculate year of data.

## Example

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

<div id="ref-frenken2007related">

Frenken, Koen, Frank Van Oort, and Thijs Verburg. 2007. “Related
Variety, Unrelated Variety and Regional Economic Growth.” *Regional
Studies* 41 (5): 685–97.

</div>

</div>
