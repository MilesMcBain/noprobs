# noprobs

Errors, warnings, messages... it's all just data innit?

```r
library(titanic)
library(tidyverse)
library(noprobs)

fitting_fn <- function(a_df){

  glm(Survived ~ ., family = binomial, data = a_df)

}

model_frame <- 
  titanic_train %>%
  group_by(Pclass) %>%
  nest() %>%
  mutate(result = map(data, ~no_problem(fitting_fn(.x)))) %>%
  unnest(result)

model_frame

## # A tibble: 3 x 6
##   Pclass data                .result   .warnings .messages .errors
##    <int> <list>              <list>    <list>    <list>    <list> 
## 1      3 <tibble [491 x 11]> <S3: glm> <chr [1]> <NULL>    <NULL> 
## 2      1 <tibble [216 x 11]> <S3: glm> <chr [1]> <NULL>    <NULL> 
## 3      2 <tibble [184 x 11]> <S3: glm> <chr [1]> <NULL>    <NULL> 

any_problems(model_frame)
## [1] TRUE

model_frame$.warnings
## [[1]]
## [1] "glm.fit: algorithm did not converge"

## [[2]]
## [1] "glm.fit: algorithm did not converge"

## [[3]]
## [1] "glm.fit: algorithm did not converge"
```
