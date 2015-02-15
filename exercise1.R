My first R Markdown file

===================================

My first file
line 2 
line 3  
line 4

load some data, then summary it

```{r}
library(datasets)
data(airquality)
summary(airquality)
```

now doing a pairs plot on it

```{r}
pairs(airquality)
```

now a regression model of ozone on wind, solar radiation, temperature

```{r}
library(stats)
fit <- lm(Ozone ~ Wind + Solar.R + Temp, data=airquality)
summary(fit)
```