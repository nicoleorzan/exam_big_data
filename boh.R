## How many people have ever lived on Earth?

```{r}
library(knitr)
c <- data.frame("Year" = c("8000 B.C.E", "1750", "1950", "2017"), 
                "Population" = c("5.000.000","795.000.000","2.516.000.000", "7.536.000.000"), 
                "Ever_Born_People" = c("1.137.789.769", "89.708.399.091", "100.045.075.171", "108.470.690.115") )
kable(c)
```