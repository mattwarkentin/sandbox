Sandbox
================

Sandbox
=======

Author: Matthew T. Warketin Email: <warkentin@lunenfeld.ca>

This is some text to describe my sandbox repo. This repo contains my dev code for miscellaneous functions that I write in R and have documented as a R package.

Here is some of the important functions in this package:

-   IndexOfUnion
-   That's it

Index of Union
--------------

Here is some text to describe the `IndexOfUnion` function.

We have loaded the data so lets fit a ROC using `pROC`s function called `roc`.

``` r
library(pROC)
model <- glm(new ~ Sepal.Width, data = iris, family = 'binomial')
iris$pred <- predict(model, newdata = iris)
(rocauc <- roc(iris$new, iris$pred))
```

    ## 
    ## Call:
    ## roc.default(response = iris$new, predictor = iris$pred)
    ## 
    ## Data: iris$pred in 100 controls (iris$new 0) < 50 cases (iris$new 1).
    ## Area under the curve: 0.8796

The AUC for this logistic regression model is 0.88.
