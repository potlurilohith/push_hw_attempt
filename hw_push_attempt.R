
#hello
---
  ```{r}
ana625_hw1_csv=read.csv('file:///C:/Users/arace/OneDrive/Documents/ANA625/ana625_incurance.csv')
```

```{r}
library(moments)
# Hi araceli , this is lohith, I bugged into your home work file and written this command 
```

```{r setup, include=FALSE}
####### Graphical Techniques
pairs(ana625_hw1_csv[c("age", "bmi", "children", "charges")])
#Scatterplots among variables
#age vs bmi- No correlation, however BMI stays constant betwwen 20-50
#age vs children- there is only 5 values for children, hence the staright lines
#age vs charges- generally, as age increases, charges increases as well
#bmi vs children- staright lines because of thelimited values for children
#bmi vs charges- no relationship between the variables
#children vs charges- staright lines because of values of charges
```

```{r}
#Box Plot for age
boxplot(ana625_hw1_csv$age, main="Boxplot for Insurance Age", ylab="Age", col="pink", horizontal=TRUE)
#Illustrartes 1st quartile at 27, second quartile (median) at 29, third quartile at 51 and fourth
#quartile at 64
```

```{r}
#Quartiles for Vriabel- Age
quantile(ana625_hw1_csv$age)
```

```{r}
#Histogram for Variable- Age
hist(ana625_hw1_csv$age, main = "Histogram of Insurance Age", xlab="Age",col="pink", breaks=5)
```

```{r}
qqnorm(ana625_hw1_csv$age, main= "Q-Q Plot for Age", col= "pink")
```
```{r}
plot(density(ana625_hw1_csv$age), main="Density Estimation for Age", col="pink")
```

```{r}
mean(ana625_hw1_csv$age)
```

```{r}
median(ana625_hw1_csv$age)
```

```{r}
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

Mode(ana625_hw1_csv$age)
```

```{r}
max(ana625_hw1_csv$age)
```

```{r}
min(ana625_hw1_csv$age)
```

```{r}
sd(ana625_hw1_csv$age)
```

```{r}
IQR(ana625_hw1_csv$age)
```

```{r}
skewness(ana625_hw1_csv$age, na.rm= FALSE)
```

```{r}
kurtosis(ana625_hw1_csv$age)
```



```{r}
#Box plot for BMI
boxplot(ana625_hw1_csv$bmi, main="Boxplot for Insurance BMI", ylab="BMI", col="blue", horizontal = TRUE)
#Illustrates 1st quartile at 26.30, 2nd (median) at 30.4, 3rd quartile at 34.60 and 4th quartile at 53.13 
```
```{r}
#Quartiles for variable- BMI
quantile(ana625_hw1_csv$bmi)
```

```{r}
#Histogram for variable-BMI
hist(ana625_hw1_csv$bmi, main="Histogram of Insurance- BMI", xlab="BMI", col="blue", breaks=5)
```

```{r}
qqnorm(ana625_hw1_csv$bmi, main= "Q-Q Plot for BMI", col= "blue")
```

```{r}
plot(density(ana625_hw1_csv$bmi), main="Density Estimation for BMI", col="blue")
```

```{r}
mean((ana625_hw1_csv$bmi))
```

```{r}
median(ana625_hw1_csv$bmi)
```
```{r}
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

Mode(ana625_hw1_csv$bmi)
```

```{r}
max(ana625_hw1_csv$bmi)
```

```{r}
min(ana625_hw1_csv$bmi)
```

```{r}
sd(ana625_hw1_csv$bmi)
```

```{r}
IQR(ana625_hw1_csv$bmi)
```

```{r}
skewness(ana625_hw1_csv$bmi, na.rm= FALSE)
```

```{r}
kurtosis(ana625_hw1_csv$bmi)
```

```{r}
#Box plot for Children
boxplot(ana625_hw1_csv$children, main="Boxplot for Insurance Children", ylab="Children", col="yellow",
        horizontal = TRUE)
# Illustrates 1st 25% at 0 value, 50% (median) at 1 value, 75% at 2 value and 100% at 5value (max)
```

```{r}
#Quartiles for variable- Children
quantile(ana625_hw1_csv$children)
```

```{r}
#Histogram for variable- Children
hist(ana625_hw1_csv$children, main="Histogram of Insurance- Children", xlab="Children", col="yellow", breaks = 5)
```

```{r}
qqnorm(ana625_hw1_csv$age, main= "Q-Q Plot for Children", col= "yellow")
```

```{r}
plot(density(ana625_hw1_csv$children), main="Density Estimation for Children", col="yellow")
```

```{r}
mean(ana625_hw1_csv$children)
```

```{r}
median(ana625_hw1_csv$children)
```

```{r}
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}
Mode(ana625_hw1_csv$children)
```

```{r}
max(ana625_hw1_csv$children)
```

```{r}
min(ana625_hw1_csv$children)
```

```{r}
sd(ana625_hw1_csv$children)
```

```{r}
IQR(ana625_hw1_csv$children)
```

```{r}
skewness(ana625_hw1_csv$children)
```

```{r}
kurtosis(ana625_hw1_csv$children, na.rm= FALSE)
```

```{r}
#Boxplot for Charges
boxplot(ana625_hw1_csv$charges, main="Boxplot for Insurance Charges", ylab="Charges", col="purple",
        horizontal = TRUE)
# Illustrates 1st quartile at $4740.29, 2nd quartile at $9382.03, 3rd quartile at $16639.91
# 4th quartile at $63770.42.
```

```{r}
#Quartiles for variable- Charges
quantile(ana625_hw1_csv$charges)
```


```{r}
#Histogram for variable- Charges
hist(ana625_hw1_csv$charges, main="Histogram of Insurance- Charges", xlab="Charges", col="purple",
     breaks= 10)
```

```{r}
qqnorm(ana625_hw1_csv$charges, main= "Q-Q Plot for Charges", col= "purple")
```

```{r}
plot(density(ana625_hw1_csv$charges), main="Density Estimation for Charges", col="purple")
```

```{r}
mean(ana625_hw1_csv$charges)
```

```{r}
median(ana625_hw1_csv$charges)
```

```{r}
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}
Mode(ana625_hw1_csv$charges)
```

```{r}
max(ana625_hw1_csv$charges)
```

```{r}
min(ana625_hw1_csv$charges)
```

```{r}
sd(ana625_hw1_csv$charges)
```

```{r}
IQR(ana625_hw1_csv$charges)
```

```{r}
skewness(ana625_hw1_csv$charges)
```

```{r}
skewness(ana625_hw1_csv$charges, na.rm= FALSE)
```

```{r}
?skewness
```














