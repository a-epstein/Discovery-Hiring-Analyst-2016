Discovery-Hiring-Analyst-2016
================

------------------------------------------------------------------------

Task
====

You must create a reproducible report\* answering the following questions:
--------------------------------------------------------------------------

1.  What is our daily overall clickthrough rate? How does it vary between the groups?
2.  Which results do people tend to try first? How does it change day-to-day?
3.  What is our daily overall zero results rate? How does it vary between the groups?
4.  Let session length be approximately the time between the first event and the last event in a session. Choose a variable from the dataset and describe its relationship to session length. Visualize the relationship.
5.  Summarize your findings in an executive summary.

Data
====

> The dataset comes from a tracking schema that we use for assessing user satisfaction. Desktop users are randomly sampled to be anonymously tracked by this schema which uses a "I'm alive" pinging system that we can use to estimate how long our users stay on the pages they visit. The dataset contains just a little more than a week of EL data.

Column Value Description
------------------------

1.  uuid string Universally unique identifier (UUID) for backend event handling.
2.  timestamp integer The date and time (UTC) of the event, formatted as YYYYMMDDhhmmss.
3.  session\_id string A unique ID identifying individual sessions.
4.  group string A label ("a" or "b").
5.  action string Identifies in which the event was created. See below.
6.  checkin integer How many seconds the page has been open for.
7.  page\_id string A unique identifier for correlating page visits and check-ins.
8.  n\_results integer Number of hits returned to the user. Only shown for searchResultPage events.
9.  result\_position integer The position of the visited page's link on the search engine results page (SERP).

<https://github.com/wikimedia-research/Discovery-Hiring-Analyst-2016>

------------------------------------------------------------------------

Import libraries
================

``` r
# Import some libraries
library(ggplot2)
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

    ## The following object is masked from 'package:base':
    ## 
    ##     date

Import and clean data
=====================

``` r
# Import dataset CSV and explicitly cast each column type

events <- read.table(
  file = "C:/Users/Eirika/Documents/Code/Datasets/events_log/events_log.csv",
  sep = ",", header = TRUE,
  colClasses =  ( 
    c(
      "character", 
      "character", 
      "character", 
      "character", 
      "character",
      "integer", 
      "character",
      "integer", 
      "integer"
    )
  ))

# Use format mask for timestamps in UTC/GMT
# strptime(events$timestamp, "%Y%m%d%H%M%S", tz = "GMT")

# Using ymd_hms and date, convert timestamp column
events$timestamp <- ymd_hms(events$timestamp)
```

    ## Warning: 4 failed to parse.

``` r
events$date <- date(events$timestamp)
```

Troubleshooting
---------------

> Want to select all NA events that werent timestamped correctly and figure out what to do with them Then I decided I want to learn how to do loops in R to iterate through a vector of NA rows

``` r
# Troubleshooting

fails <- which(is.na(events$timestamp))

# From fails
# Extract first list element (vector size - (vector size - 1))
# events[first list element]
# Loop to the second list element and repeat

events[40953,]
```

    ##                                   uuid timestamp       session_id group
    ## 40953 1a5b663f6a1258f588dc4de65c90b5c0      <NA> 35a29e7a78ccc24b     a
    ##        action checkin          page_id n_results result_position date
    ## 40953 checkin     180 8f0d489715dd14b0        NA              NA <NA>

------------------------------------------------------------------------

1. What is our daily overall clickthrough rate? How does it vary between the groups?
------------------------------------------------------------------------------------

> The daily clickthrough rate varies from 6654 clickthroughs on 3/1/16 to 3323 clickthroughs on 3/5/16

> Group A makes up the bulk of the clickthrough events, whereas group B has far fewer.

``` r
# Pull out data only of visitPage clickthrough events

clickthrough <- events[events$action == 'visitPage',]

# Pull out clickthroughs that resulted from non-NA result position
clickthrough <- clickthrough[!is.na(clickthrough$result_position),]

# Chart by day

cpd <- ggplot(clickthrough, aes(x=as.POSIXct(clickthrough$timestamp, "%Y%m%d", tz = "GMT"))) + scale_x_datetime(date_breaks = "1 day") + labs(title= "Overall",x="Date", y="Clickthroughs") + geom_bar(fill="tomato") + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=0))

# Chart per group a and b

groupa <- clickthrough[clickthrough$group == 'a',]
groupb <- clickthrough[clickthrough$group == 'b',]

cpda <- ggplot(groupa, aes(x=as.POSIXct(groupa$timestamp, "%Y%m%d", tz = "GMT"))) + scale_x_datetime(date_breaks = "1 day") + labs(title= "Group A",x="Date", y="Clickthroughs") + geom_bar(fill="lawngreen") + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=0))


cpdb <- ggplot(groupb, aes(x=as.POSIXct(groupb$timestamp, "%Y%m%d", tz = "GMT"))) + scale_x_datetime(date_breaks = "1 day") + labs(title= "Group B", x="Date", y="Clickthroughs") + geom_bar(fill="steelblue") + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=0))


grid.arrange(cpd, cpda, cpdb, ncol=3)
```

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Overall daily clickthrough rate
#count(as.POSIXct(clickthrough$timestamp, "%Y%m%d", tz = "GMT"))

# Group A clickthrough rate
#count(as.POSIXct(groupa$timestamp, "%Y%m%d", tz = "GMT"))

# Group B clickthrough rate
#count(as.POSIXct(groupb$timestamp, "%Y%m%d", tz = "GMT"))

# How large is each group?
#groupsize <- count(events$group)
#print(groupsize)
```

------------------------------------------------------------------------

2. Which results do people tend to try first? How does it change day-to-day?
----------------------------------------------------------------------------

> Most people try the first result first. On 3/1/16, there are a cluster of search results that were around position 4000. This was not the case on any other day. 3/4/16 had the largest variability in search result position.

``` r
# Pull out all data that have a results_position that is not null

results <- events[!(is.na(events$result_position)),]

# count(results$result_position) # for freq table

# Graph of distribution of results_position
rtf <- ggplot(results, aes(x=results$result_position)) + geom_bar() + coord_cartesian(xlim=c(0,10)) + labs(title = "Results Clicked First", x="Result Position", y= "Frequency")

print(rtf)
```

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Zoom in on only result positions in the top 500

results_500 <- results[results$result_position < 500,]

days_500 <- ggplot(results_500, aes(x=as.POSIXct(results_500$timestamp, "%Y%m%d", tz = "GMT"), y=results_500$result_position, colour = results_500$result_position)) + geom_jitter() + labs(title = "Results Clicked First", x="Day", y= "Result Position", colour = "Result Position") + scale_colour_gradientn(colours=rainbow(4))

print(days_500)
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
# Zoom in on only result positions in the top 200

results_200 <- results_500[results_500$result_position < 200,]

days_200 <- ggplot(results_200, aes(x=as.POSIXct(results_200$timestamp, "%Y%m%d", tz = "GMT"), y=results_200$result_position, colour = results_200$result_position)) + geom_jitter() + labs(title = "Results Clicked First", x="Day", y= "Result Position", colour = "Result Position") + scale_colour_gradientn(colours=rainbow(4))

print(days_200)
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-5-3.png)

------------------------------------------------------------------------

3. What is our daily overall zero results rate? How does it vary between the groups?
------------------------------------------------------------------------------------

> The daily overall zero results rate ranges between 10 and 15% each day. Group A is closer to the overall trend whereas Group b as a population displays less variability in zero results rate.

``` r
# Pull out only search result page action
search_results <- events[events$action == "searchResultPage",]

# Pull out only n_results of 0, exclude N/A

not_na_results <- search_results[!is.na(search_results$n_results),]

zero_results <- not_na_results[not_na_results$n_results == 0,]

# Pull out group b and group A data

zero_a <- zero_results[zero_results$group == "a",]

zero_b <- zero_results[zero_results$group == "b",]


# Plot by day

zrpd <- ggplot(zero_results) + geom_bar(mapping = aes(x=as.POSIXct(zero_results$timestamp, "%Y%m%d", tz = "GMT"),y =..prop..), stat = "count", group = 1) + labs(title = "Frequency of Zero Results Per Day", x="Day", y= "Percentage of zero Results") + scale_y_continuous(labels = scales::percent_format())

# Plot by group

zra <- ggplot(zero_a) + geom_bar(mapping = aes(x=as.POSIXct(zero_a$timestamp, "%Y%m%d", tz = "GMT"),y =..prop..), stat = "count", group = 1) + labs(title = "Frequency of Zero Results Per Day", x="Day", y= "Percentage of zero Results") + scale_y_continuous(labels = scales::percent_format())

zrb <- ggplot(zero_b) + geom_bar(mapping = aes(x=as.POSIXct(zero_b$timestamp, "%Y%m%d", tz = "GMT"),y =..prop..), stat = "count", group = 1) + labs(title = "Frequency of Zero Results Per Day", x="Day", y= "Percentage of zero Results") + scale_y_continuous(labels = scales::percent_format())

print(zrpd)
```

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
print(zra)
```

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
print(zrb)
```

![](hiring_analyst_2016_files/figure-markdown_github/unnamed-chunk-6-3.png)

4. Let session length be approximately the time between the first event and the last event in a session. Choose a variable from the dataset and describe its relationship to session length. Visualize the relationship.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
