---
title: "Pillar Negative Colors"
author: "Chase Clark"
date: "January 14, 2018"
output: 
  html_document: 
    keep_md: yes
---








```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.1     v dplyr   0.7.4
## v tidyr   0.7.2     v stringr 1.2.0
## v readr   1.1.1     v forcats 0.2.0
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```
## Warning: package 'tibble' was built under R version 3.4.3
```

```
## Warning: package 'tidyr' was built under R version 3.4.3
```

```
## Warning: package 'readr' was built under R version 3.4.3
```

```
## Warning: package 'purrr' was built under R version 3.4.3
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
```

```
## Warning: package 'stringr' was built under R version 3.4.3
```

```
## Warning: package 'forcats' was built under R version 3.4.3
```

```
## -- Conflicts ---------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
devtools::install_github("r-lib/pillar")
```

```
## Skipping install of 'pillar' from a github remote, the SHA1 (fe911937) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```r
library("pillar")
```

```
## 
## Attaching package: 'pillar'
```

```
## The following object is masked from 'package:dplyr':
## 
##     dim_desc
```



```r
keep_empty <- function(fun) {
  function(x) {
    ret <- rep_along(x, "")
    update <- which(is.na(x) | x != "")
    ret[update] <- fun(x[update])
    ret
  }
}
```




```r
# Set pillar.neg
options("pillar.neg"=FALSE)


# Test
style_neg <- keep_empty(function(x) {
  if (isTRUE(getOption("pillar.neg", TRUE))) {
    
    neg_color<-crayon::make_style(rgb(204,75,75,maxColorValue = 256),colors=16)
    
    neg_color(x)
  } else {
    x
  }
})

w<-style_neg("123")
cat(w)
```

```
## 123
```

```r
style_neg <- keep_empty(function(x) {
  if (isTRUE(getOption("pillar.neg", TRUE))) {
    
    neg_color<-crayon::make_style(rgb(204,75,75,maxColorValue = 256),colors=16)
    
    neg_color(x)
  } else {
    x
  }
})

w<-style_neg("123")
cat(w)
```

```
## 123
```

