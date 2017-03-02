# Labelling outliers in boxplot over factors

While reading ggplot2 (great book, by the way!), I came across a graph that looks like this:
![](/figure/2016-10-19-Labelling-Outliers_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

The data is the measurement of the height of 26 boys at 9 different scaled times, which are called "Occasions". This boxplot is a nice illustration of how easily ggplot handles things like this, because the command used to make this boxplot is given simply by


```r
ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
```

Aside: this post uses the following libraries:

```r
library(nlme) #where Oxboys is stored
library(plyr) #stack exchange solution
library(ggplot2)
```


I decided to push beyond what is being presented in the book. I decided I want to *color* the outliers according to the subject they represent, and provide a legend that says which color belongs to which subject. Basically, this:

![](/figure/2016-10-19-Labelling-Outliers_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

It seems to me that seeing at a glance which Subjects are the outliers, and that it is the same subject at each occasion, is information worth having. (OK, for this particular data set, it would be impossible for the outlier in occasions 4-10 to come from a different subject thant that in occasion 3, assuming that heights of the subjects were nondecreasing! But, in general, one would like to know whether it is the same subject, or different subjects at each occasion, right?) I guess one could argue that having the colored outliers distracts from what the main focus should be, which is how the rest of the data is behaving...

The book had already described how to get different points to show up as different colors, based on a factor, so I thought it shouldn't be too hard. I won't bore you or confuse you with all of the things that I tried, but let's just say that none of them worked. I eventually looked on stack exchange, where I found the technique that I used to create the graph in [this post](http://stackoverflow.com/questions/15273148/coloring-boxplot-outlier-points-in-ggplot2). The answer given used the plyr package, which I am not familiar with, so I decided to redo it on my own. 

The basic idea used to create the plot was that we need to create a new data frame, which contains the old data plus two new variables: lower.limit and upper.limit, which contain the lower [resp upper] limit of the whiskers in the boxplot *for the Occasion*. If this were the standard boxplot function, I could pull those values from


```r
boxplot(Oxboys$height~Oxboys$Occasion)$stats
```

![](/figure/2016-10-19-Labelling-Outliers_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```
##        [,1]  [,2]  [,3]   [,4]   [,5]   [,6]    [,7]   [,8]  [,9]
## [1,] 126.20 128.2 129.0 129.40 129.59 130.60 140.900 142.60 143.1
## [2,] 137.50 139.3 140.9 142.70 144.20 145.70 147.900 150.20 151.8
## [3,] 143.35 145.0 146.3 148.75 149.60 151.55 153.535 155.55 156.3
## [4,] 147.80 148.8 150.2 152.50 154.70 156.00 157.400 159.20 161.6
## [5,] 156.90 158.7 160.6 163.30 164.40 167.30 170.700 172.40 174.8
```

BUT, the whiskers are computed differently inside geom_boxplot than they are inside of boxplot. Look:


```r
boxplot(Oxboys$height~Oxboys$Occasion)
```

![](/figure/2016-10-19-Labelling-Outliers_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

This plot only has three outliers, whereas the one we are interested in has 8. I don't know how to use geom_boxplot to pull out the whiskers, but the documentation tells us that anything that falls more than 1.5 * IQR away from the 25th or 75th percentile is considered an outlier. So, that tells us how to compute our lower limits and our upper limits. 


```r
upper.limit <- tapply(Oxboys$height, Oxboys$Occasion, function(x) 1.5*(quantile(x, .75) - quantile(x,.25)) + quantile(x,.75))
lower.limit <- tapply(Oxboys$height, Oxboys$Occasion, function(x) -1.5*(quantile(x, .75) - quantile(x,.25)) + quantile(x,.25))
```

Now, we need to find the heights that fall below those values for the corresponding Occasions, and color them according to their Subject. This is the kind of thing that I feel like should be easy. But, I don't see how to do it super-easily. 

First, let's pull out the names of the factors from lower.limit and upper.limit in the following way

```r
attr(lower.limit, which = "dimnames")
```

```
## [[1]]
## [1] "1" "2" "3" "4" "5" "6" "7" "8" "9"
```

This allows me to combine which with some indexing to create the data we need to add to the data frame.

```r
N <- length(lower.limit)
lower.limits <- rep(0,nrow(Oxboys))
lower.limits[which(Oxboys$Occasion == attr(lower.limit, which = "dimnames")[[1]][1:N])] <- lower.limit[1:N]
upper.limits <- rep(0, nrow(Oxboys))
upper.limits[which(Oxboys$Occasion == attr(upper.limit, which = "dimnames")[[1]][1:N])] <- upper.limit[1:N]
Oxboys <- cbind(Oxboys, lower.limit = lower.limits, upper.limit = upper.limits)
```

Let's check:

```r
head(Oxboys)
```

```
##   Subject     age height Occasion lower.limit upper.limit
## 1       1 -1.0000  140.5        1    124.0750    161.8750
## 2       1 -0.7479  143.4        2    127.1375    161.6375
## 3       1 -0.4630  144.8        3    129.1125    162.8125
## 4       1 -0.1643  147.1        4    130.0500    165.4500
## 5       1 -0.0027  147.7        5    130.5750    168.1750
## 6       1  0.2466  150.2        6    131.8375    169.5375
```

Hmmm, weird that the upper limit at Occasion two is less than that at Occasion one. So, a person could not be an outlier in this group for being tall at age 10, not grow at all, and be an outlier for being tall at age 11. That means that the outlier identification procedure could be improved here, but I am going to forge on!

Anywho. Now we are ready to do the graph:


```r
ggplot() +  geom_boxplot(data = Oxboys, aes(x = Occasion, y = height)) +  geom_point(data = Oxboys[Oxboys$height > Oxboys$upper.limit | Oxboys$height < Oxboys$lower.limit,], aes(x = Occasion,y = height, color = Subject))
```

![](/figure/2016-10-19-Labelling-Outliers_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Let's compare what I did to the original solution on stack exchange (which was actually a solution to a slightly different problem, but certainly contained everything needed to write this solution):


```r
plot_Data <- plyr::ddply(Oxboys, .(Occasion), mutate, Q1=quantile(height, 1/4), Q3=quantile(height, 3/4), IQR=Q3-Q1, upper.limit=Q3+1.5*IQR, lower.limit=Q1-1.5*IQR)
ggplot() +  geom_boxplot(data = plot_Data, aes(x = Occasion, y = height)) +  geom_point(data = plot_Data[plot_Data$height > plot_Data$upper.limit | plot_Data$height < plot_Data$lower.limit,], aes(x = Occasion,y = height, color = Subject))
```

![](2016-10-19-Labelling-Outliers_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

My takeaways from this are:

1. Modulo the cryptic and intimidating .(Occasion) argument, the solution using ddply is more readable and intuitive. Looks like I may need to add learning about the plyr package to my to-do list.
2. Labelling outliers in boxplots is hard in R. It is tricky to do in the base package using boxplot, and it is tricky to do using ggplot. I wonder why this is. Do other people think that labelling outliers isn't useful enough to make easy?

