---
title: "Exploring Okun's Law"
output: 
   rmarkdown::github_document
   
---




### By ELOIARM YOUNES  : younes.eloiarm@edu.uiz.ac.ma
Completion Date: January 04, 2022


## Setup
```{r, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,fig.align='center', message = FALSE, warning=FALSE, fig.width=11, fig.height = 4)
```

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2) ## V
require(R2jags)
library("rjags")
library(statsr)
library(BAS)
library(pander)
library(gridExtra)
library(GGally)
library(dplyr)
library(FactoMineR)
library(readr)
library(factoextra)
library(ade4)
library(corrplot)
library(dygraphs)
library(zoo)
library(lubridate)
library(TSstudio)
library(TSstudio)
library(plotly)
library(xts)
library(dplyr)
library(forecast)
```

<br>

## Load data
```{r}
Data <- read_csv("Data.csv")
```

* * *
## Introduction :



### Context
The importance of Okun's Law at the economic level especially when we talk about the problem of Unemployment, this model helps us to know how to remove or reduce the problem of unemployment. further, it can give us more information about the type of unemployment.
However, for this version of Data, I based on the several countries to know the special elements with them when we apply the same model. that's why I choose annual frequentists because we do have not the same volume of data for all countries that I had to choose it.

### Content
First of all, this data is available for everyone, just install wbdata package in your notebook.
So this data has 4 features that are really important to build Okun's Law model.
`GDP growth`: Gross Domestic Products growth (annual %)

`Unemployment_TLF`: Unemployment, total (% of total labor force) (modeled ILO estimate)

`Unemployment_AEF` : Unemployment with advanced education, female (% of female labor force with advanced education)

`Unemployment_AEM`: Unemployment with advanced education, male (% of male labor force with advanced education)

`Unemployment_AET` : Unemployment with advanced education (% of total labor force with advanced education)

#### Acknowledgements
Okun's Law model is like this :

$Unemployment$ $=$ $\beta$ * $GDP Growth$ $+$ $\alpha$

* * *

```{r}
head(Data)
```

### List of Countries :
```{r}
table(Data$country)
```

***

## Morocco :

### Load Data:

```{r}
morocco_df2<-Data %>%
        filter(country=="Morocco", date>="1991-01-01") 
morocco_df2<- arrange(morocco_df2,date)
morocco_df2
```

### GDP Growth:

```{r}
GDP_MAR_ts <- ts(data = morocco_df2$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_MAR_ts ,
        main = "GDP Growth of Morocco",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment Rate:

```{r}
UNM_MAR_ts <- ts(data = morocco_df2$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_MAR_ts,
        main = "Unemployment of Morocco",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualisation Aggregates:

```{r}
# Custom plot function for ggpairs scatterplot
my_fin <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + 
        geom_point() + 
        geom_smooth(method=loess, fill="red", color="red", ...) +
        geom_smooth(method=lm, fill="blue", color="blue", ...)
    p
}
```

```{r}
morocco_df2 %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```
### Building Okun's Law model :

```{r}
print("Correlation between GDP and Unemployment of Morocco :")
cor(morocco_df2$`GDP growth`,morocco_df2$Unemployment_TLF)
```
```{r}
plot(density(morocco_df2$Unemployment_TLF), main = "Density Function of Unemployment for Morocco")
```

So, we have seen, the density function of our unemployment is not normally distribution, but it is mixte. we can't apply simple regression model in this step.


```{r}
model_1 <- lm(`Unemployment_TLF`~`GDP growth`,data = morocco_df2)
summary(model_1)
```
So, we have knowen before this result, because the normal distribution for target Aggregate is important to apply linear regression.
the result for this step is like this;
the coefficient of correlation between GDP and Unemployment is this `-0.01495` is not significate, So we have to remove it.


***

## United States :


### Load Data:

```{r}
usa_df<-Data %>%
        filter(country=="United States", date>="1991-01-01") 
usa_df<- arrange(usa_df,date)
usa_df
```


### GDP Growth of United States:

```{r}
GDP_USA_ts <- ts(data = usa_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_USA_ts ,
        main = "GDP Growth of United States",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment of United States:
```{r}
UNM_USA_ts <- ts(data = usa_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_USA_ts,
        main = "Unemployment of United States",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualization :
```{r}
usa_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```
### Building Okun's Law model:
```{r}
print("Correlation between GDP and Unmployment of United States by Pearson approach")
cor(usa_df$`GDP growth`,usa_df$Unemployment_TLF)
```
```{r}
plot(density(usa_df$Unemployment_TLF), main = "Density Function of Unemployment for United States")
```
The density function of Unemployment of US is approximatly normal distribution, 
So, we have not any problem to apply the simple linear regression.

```{r}
model_2 <- lm(`Unemployment_TLF` ~`GDP growth`, data = usa_df)
summary(model_2)
```
So, Okun's Law in case USA is like this:

`Unemployment` $=$ 6.9428 $-$ 0.4390$*$`GDP Growth`

Summary of this equation, we assume the natural of unemployment for the USA is equal to `6.9428%`, and the power of the US economy can affect on Unemployment by `0.439` for each degree of `GDP Growth`.



## United Kingdom :

### Load Data:
```{r}
uk_df<-Data %>%
        filter(country=="United Kingdom", date>="1991-01-01") 
uk_df<- arrange(uk_df,date)
uk_df
```


### GDP Growth:

```{r}
GDP_UK_ts <- ts(data = uk_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_UK_ts ,
        main = "GDP Growth of United Kingdom",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```
### Unemployment Rate:
```{r}
UNM_UK_ts <- ts(data = uk_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_UK_ts,
        main = "Unemployment of United Kingdom",
        ylab = "percentage %") %>%
        dyRangeSelector()
```
### Visualization Correlation:
```{r}
uk_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```
### Building Okun's Law model:

```{r}
print("Correlation between GDP Growth and Unemployment rate :")
cor(uk_df$`GDP growth`,uk_df$Unemployment_TLF)
```
```{r}
plot(density(uk_df$Unemployment_TLF), main = "Density Function of Unemployment for  United Kingdom")
```

We have seen, Unemployment rate of  United Kingdom is not normal distribution, but is approximatly mixture.
So, We can't apply linear regression model.
```{r}
model_3 <- lm(`Unemployment_TLF` ~`GDP growth`, data = uk_df)
summary(model_3)
```
As we talk before the coefficient of regression is not significante,
So we reject this model.


## Japan :

### Load Data:
```{r}
japan_df<-Data %>%
        filter(country=="Japan", date>="1991-01-01") 
japan_df<- arrange(japan_df,date)
japan_df
```


### GDP Growth:
```{r}
GDP_japan_df_ts <- ts(data = japan_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_japan_df_ts ,
        main = "GDP Growth of Japan",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```

### Unemployment Rate:
```{r}
UNM_japan_ts <- ts(data = japan_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_japan_ts,
        main = "Unemployment of Japan",
        ylab = "percentage %") %>%
        dyRangeSelector()
```
### Visualization Correlation:
```{r}
japan_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```
### Building Okun's Law model:

```{r}
print("Correlation coefficient between GDP Growth and Unemployment rate ")
cor(japan_df$`GDP growth`,japan_df$Unemployment_TLF)
```
```{r}
plot(density(japan_df$Unemployment_TLF), main = "Density Function of Unemployment for  Japan")
```
We see that Unemployment has really beta distribution, not normal distribution, however, we will try if we can apply simple linear regression.
```{r}
model_4 <- lm(`GDP growth`~`Unemployment_TLF`, data = japan_df)
summary(model_4)
```
We can apply okun's law for Japan situation, because the normal distribution is really important for that equation.


## China :
### Load Data:
```{r}
China_df<-Data %>%
        filter(country=="China", date>="1991-01-01") 
China_df<- arrange(China_df,date)
China_df
```

### GDP Growth:
```{r}
GDP_China_df_ts <- ts(data = China_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_China_df_ts ,
        main = "GDP Growth of China",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment Rate :
```{r}
UNM_China_ts <- ts(data = China_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_China_ts,
        main = "Unemployment of China",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualization Correlation 
```{r}
China_df %>% 
  select(`GDP growth`,Unemployment_TLF) %>%#,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```


### Building Okun's Law model:
```{r}
print("Coreelation by Pearson Approach :")
cor(China_df$`GDP growth`,China_df$Unemployment_TLF)
```

```{r}
plot(density(China_df$Unemployment_TLF), main = "Density Function of Unemployment for  China")
```
China situation is perheps like others situation, because Unemployment rate of China has mixture distribution. 
```{r}
model_5 <- lm(`Unemployment_TLF`~`GDP growth`, data = China_df)
summary(model_5)
```
We assume one hypothesis about this problem of normal distribution, 
First, maybe the observations are not enough, if that is right, we need to change the annual frequency to monthly frequency. 
After that if the problem is still exist, we need to use other technique for mixture distribution.
However in China situation we can accept simple linear model with risk equal to `0.0118`.

***

## India:

### Load Data:

```{r}
India_df<-Data %>%
        filter(country=="India", date>="1991-01-01") 
India_df<- arrange(India_df,date)
India_df
```


### GDP Growth:
```{r}
GDP_India_df_ts <- ts(data = India_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_China_df_ts ,
        main = "GDP Growth of India",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment Rate:
```{r}
UNM_India_ts <- ts(data = India_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_India_ts,
        main = "Unemployment of India",
        ylab = "percentage %") %>%
        dyRangeSelector()
```

### Visualization Correlation:
```{r}
India_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```

### Chicking Okun's Law model:
```{r}
print("Correlation between GDP growth and Unemployment rate")
cor(India_df$`GDP growth`, India_df$Unemployment_TLF)
```

```{r}
plot(density(India_df$Unemployment_TLF), main = "Density Function of Unemployment for  India")
```
We can ignore this right picture, because it doesn't have high affect of our model.
So we assume Unemployment in this situation approximatly normal dstribution.


```{r}
model_6 <- lm(`Unemployment_TLF`~`GDP growth`, data = India_df)
summary(model_6)
```
Okun's Law in India situation exists, even if the destribution of Unemployment has usage in the right side.
So , the equation is like this 
`Unemployment` $=$ 6.05175 $-$ 0.06568 $*$ `GDP growth`



***


## Germany :
### Load Data:

```{r}
Germany_df<-Data %>%
        filter(country=="Germany", date>="1991-01-01") 
Germany_df<- arrange(Germany_df,date)
Germany_df
```
### GDP Growth:
```{r}
GDP_Germany_df_ts <- ts(data = Germany_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_China_df_ts ,
        main = "GDP Growth of Germany",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```
### Unemployment Rate:
```{r}
UNM_Germany_ts <- ts(data = Germany_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Germany_ts,
        main = "Unemployment of Germany",
        ylab = "percentage %") %>%
        dyRangeSelector()
```
### Visualization Correlation:
```{r}
Germany_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```
### Building Okun's Law model:

```{r}
print("Correlation between Unemployment Rate and GDP Growth")
cor(Germany_df$`GDP growth`, Germany_df$Unemployment_TLF,use = "everything",
    method = c("pearson", "kendall", "spearman"))
```
```{r}
plot(density(Germany_df$Unemployment_TLF), main = "Density Function of Unemployment for  Germany ")
```

```{r}
model_7 <- lm(`Unemployment_TLF`~`GDP growth`, data=Germany_df)
summary(model_7)
```
Noting, when we have big difference of density function, we know the linear regreession is not applying in that situation, like this situation.


***

## Israel :

### Load Data:
```{r}
Israel_df<-Data %>%
        filter(country=="Israel", date>="1996-01-01") 
Israel_df<- arrange(Israel_df,date)
Israel_df
```
### GDP Growth:

```{r}
GDP_Israel_df_ts <- ts(data = Israel_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_Israel_df_ts ,
        main = "GDP Growth of Israel",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```

### Unemployment Rate:

```{r}
UNM_Israel_ts <- ts(data = Israel_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Israel_ts,
        main = "Unemployment of Israel",
        ylab = "percentage %") %>%
        dyRangeSelector()
```

### Visualization Correlation:

```{r}
Israel_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```

### Building Okun Law's model:

```{r}
print("Correlation between Unemployment Rate and GDP Growth")
cor(Israel_df$`GDP growth`, Israel_df$Unemployment_TLF)
```


```{r}
plot(density(Israel_df$Unemployment_TLF), main = "Density Function of Unemployment for  Israel ")
```

```{r}
model_8 <- lm(`Unemployment_TLF`~`GDP growth`, data=Israel_df)
summary(model_8)
```


***

## France

### Load Data:

```{r}
France_df<-Data %>%
        filter(country=="France", date>="1991-01-01") 
France_df<- arrange(France_df,date)
France_df
```

### GDP Growth:

```{r}
GDP_France_df_ts <- ts(data = France_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_France_df_ts ,
        main = "GDP Growth of France",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```

### Unempoyment Rate :

```{r}
UNM_France_ts <- ts(data = France_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Israel_ts,
        main = "Unemployment of France",
        ylab = "percentage %") %>%
        dyRangeSelector()
```

### Visualization Correlation:

```{r}
France_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```

Look at this situation, the relationship between `Unemployment rate` and `Gross Domestic products Growth` are having positive direction, 
that's means in Frensh economy, GDP Growth go up when Unemployment rate go up, that's not logical of course.
We can say Frensh economy has problem or doesn't depend by Labor fource for creating his wealth.

### Building model :

```{r}
print("Correlation between Unemployment Rate and GDP Growth By Pearson Approach")
cor(France_df$`GDP growth`, France_df$Unemployment_TLF)
```


```{r}
plot(density(France_df$Unemployment_TLF), main = "Density Function of Unemployment for  France ")
```

```{r}
model_9 <- lm(`Unemployment_TLF`~`GDP growth`, data=France_df)
summary(model_9)
```

***

## Spain: 

### Load Data:

```{r}
Spain_df<-Data %>%
        filter(country=="Spain", date>="1991-01-01") 
Spain_df<- arrange(Spain_df,date)
Spain_df
```

### GDP Growth:

```{r}
GDP_Spain_df_ts <- ts(data = Spain_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_Spain_df_ts ,
        main = "GDP Growth of Spain",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment Rate:

```{r}
UNM_Spain_ts <- ts(data = Spain_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Spain_ts,
        main = "Unemployment of Spain",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualization Correlation:

```{r}
Spain_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```


### Building Okun's Law model :
```{r}
print("Correlation between Unemployment Rate and GDP Growth By Pearson Approach")
cor(Spain_df$`GDP growth`, Spain_df$Unemployment_TLF)
```
```{r}
plot(density(Spain_df$Unemployment_TLF), main = "Density Function of Unemployment for  Spain ")
```
```{r}
model_10 <- lm(`Unemployment_TLF`~`GDP growth`, data = Spain_df)
summary(model_10)
```


***

## Portugal :

### Load Data:

```{r}
Portugal_df<-Data %>%
        filter(country=="Portugal", date>="1991-01-01") 
Portugal_df<- arrange(Portugal_df,date)
Portugal_df
```

### GDP Gowth:

```{r}
GDP_Portugal_df_ts <- ts(data = Portugal_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(GDP_Portugal_df_ts ,
        main = "GDP Growth of Portugal",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment Rate:

```{r}
UNM_Portugal_ts <- ts(data =Portugal_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Portugal_ts,
        main = "Unemployment of Portugal",
        ylab = "percentage %") %>%
        dyRangeSelector()
```

### Visualization Correlation:

```{r}
Portugal_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```


### Building Model:
```{r}
print("Correlation between Unemployment Rate and GDP Growth By Pearson Approach")
cor(Portugal_df$`GDP growth`, Portugal_df$Unemployment_TLF)
```

```{r}
plot(density(Portugal_df$Unemployment_TLF), main = "Density Function of Unemployment for  Portugal ")
```

```{r}
model_11 <- lm(`Unemployment_TLF`~`GDP growth`, data = Portugal_df)
summary(model_11)
```

We can accept this model, never if the target variable is not normal distribution and no mixture distribution, but it has right skewed distribution, 
So right skewed or left skewed distribution doesn't affect on linear regression.
and we can ignore this case if we have it.

***

## Egypt, Arab Rep.

### Load Data:

```{r}
Egypt_df<-Data %>%
        filter(country=="Egypt, Arab Rep.", date>="1991-01-01") 
Egypt_df<- arrange(Egypt_df,date)
Egypt_df
```


### GDP Growth:

```{r}
Egypt_df_ts <- ts(data = Egypt_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(Egypt_df_ts ,
        main = "GDP Growth of Egypt",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```



### Unemployment rate:

```{r}
UNM_Egypt_ts <- ts(data =Egypt_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Egypt_ts,
        main = "Unemployment of Egypt",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualization Correlation

```{r}
Egypt_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```


### Building model:

```{r}
print("Correlation between Unemployment Rate and GDP Growth By Pearson Approach")
cor(Egypt_df$`GDP growth`, Egypt_df$Unemployment_TLF)
```
```{r}
plot(density(Egypt_df$Unemployment_TLF), main = "Density Function of Unemployment for  Egypt ")
```
```{r}
model_12 <- lm(`Unemployment_TLF`~`GDP growth`, data = Egypt_df)
summary(model_12)
```

***


## Tunisia :

### Load Data:

```{r}
Tunisia_df<-Data %>%
        filter(country=="Tunisia", date>="1991-01-01") 
Tunisia_df<- arrange(Tunisia_df,date)
Tunisia_df
```



### GDP Growth:

```{r}
Tunisia_df_ts <- ts(data = Tunisia_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(Tunisia_df_ts ,
        main = "GDP Growth of Tunisia",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```


### Unemployment Rate:

```{r}
UNM_Tunisia_ts <- ts(data =Tunisia_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Tunisia_ts,
        main = "Unemployment of Tunisia",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualization Correlation:

```{r}
Tunisia_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```


### Building model:
```{r}
print("Correlation between Unemployment Rate and GDP Growth By Pearson Approach")
cor(Tunisia_df$`GDP growth`, Tunisia_df$Unemployment_TLF)
```

```{r}
plot(density(Tunisia_df$Unemployment_TLF), main = "Density Function of Unemployment for  Tunisia ")
```

```{r}
model_13 <- lm(`Unemployment_TLF`~`GDP growth`, data = Tunisia_df)
summary(model_13)
```

***

## Turkey:

### Load Data:

```{r}
Turkey_df<-Data %>%
        filter(country=="Turkey", date>="1991-01-01") 
Turkey_df<- arrange(Turkey_df,date)
Turkey_df
```


### GDP Growth:

```{r}
Turkey_df_ts <- ts(data = Turkey_df$`GDP growth` ,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(Turkey_df_ts ,
        main = "GDP Growth of Turkey",
        ylab = "Growth %") %>%
                                   dyRangeSelector()
```

### Unemployment rate:

```{r}
UNM_Turkey_ts <- ts(data =Turkey_df$Unemployment_TLF,
start = c(1991,01,01), end = c(2020, 01, 01),
frequency =1 )
dygraph(UNM_Turkey_ts,
        main = "Unemployment of Turkey",
        ylab = "percentage %") %>%
        dyRangeSelector()
```


### Visualization Correlation:

```{r}
Turkey_df %>% 
  select(`GDP growth`,Unemployment_TLF,Unemployment_AEF,Unemployment_AEM,Unemployment_AET) %>%
  ggpairs(lower = list(continuous = my_fin))
```

### Bulding Model :
```{r}
print("Correlation between Unemployment Rate and GDP Growth By Pearson Approach")
cor(Turkey_df$`GDP growth`, Turkey_df$Unemployment_TLF)
```

```{r}
plot(density(Turkey_df$Unemployment_TLF), main = "Density Function of Unemployment for  Turkey ")
```

```{r}
model_14 <-lm(`Unemployment_TLF`~`GDP growth`, data = Turkey_df)
summary(model_14)
```

* * * 


# Conclusion :

In the conclusion of this step, we have seen the importance of normal distribution, or approximatly normally distribution for build the linear model, So many case of countries we can't apply Okun's Law model except `United Stated`, of course the origine of that model came from US economy, 
However other important thing is the type of distribution of Unemployment, that gives us one idea of the compenents of Unemployment, 
in general we can accept the majority of countries have many type of unemployment that we know it before such as `Cyclical` and `Sturctical`.
before goving to analyse theoritical thinking about this problem, 
we need to use Bayeisan approach for simulation mixture models.
Now, I can't use Jags language inside R in Kaggle Colab, if Any one have any idea to install jags in Kaggle , I will apprechiated him or her for this helping, I think in the next step of this reseach I will focus only on my country to know how can estimate the coefficients of Okun's Law with multimodal distribution.
I wish you are enjoy when you have read this notebook. it doesn't bad isn't it????
