---
title: "Historical Totals By Semester"
date: "June 12, 2024"
execute:
  keep-md: true
  df-print: paged
  warning: false
format:
  html:
    code-fold: true
    code-line-numbers: true
editor_options: 
  chunk_output_type: console
---


::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(directlabels)
library(ggrepel)
library(gapminder)
library(ggthemes)
library(scales)

data <- read_csv("C:\\Users\\derek\\OneDrive - BYU-Idaho\\Data Science Society\\IBC Historical Data.csv")
```
:::

::: {.cell}

```{.r .cell-code}
per_dec <- function(x) {
  numeric_values <- as.numeric(gsub("%", "", x))
  
  numeric_values / 100
}
```
:::

::: {.cell}

```{.r .cell-code}
ibc <- data %>% 
  mutate(
    `Gross Margin` = per_dec(data$`Gross Margin`), 
    `Net Income Margin` = per_dec(data$`Net Income Margin`))

ibc <- ibc[-c(1, 5, 7:10, 12:21), ]

ibc <- rename(ibc, net_income = "Net Income")

ibc_semester <- ibc %>% 
  group_by(Smstr)

ibc_semester$Smstr <- factor(ibc_semester$Smstr, levels = c("Fall", "Winter", "Spring"))

ibc_category <- ibc %>% 
  group_by(Category)

ibc_year <- ibc %>% 
  group_by(Year)
```
:::


## Revenue vs. Semester


::: {.cell}

```{.r .cell-code}
semester_aov <- aov(Revenue ~ Smstr, ibc_semester)
summary(semester_aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df    Sum Sq  Mean Sq F value Pr(>F)
Smstr         2 6.578e+07 32890323   1.184  0.308
Residuals   186 5.167e+09 27779127               
```


:::

```{.r .cell-code}
par(mfrow = c(1,2))
plot(semester_aov, which = 1:2)
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
ggplot(ibc_semester, aes(x=Smstr, y=Revenue)) +
  geom_boxplot() +
  labs(x="Semester", y="Revenue") +
  scale_y_continuous(labels = dollar_format()) +
  theme(plot.margin = unit(c(2,1,2,2), "lines"), 
        plot.title = element_text(hjust = 0.5, vjust = 6, face = 'bold'),  
        panel.background = element_rect(fill = "white"), 
        axis.ticks.length.x = unit(0, 'pt'),  
        axis.ticks.length.y = unit(0, 'pt'), 
        panel.grid.major.y = element_line(color = "grey", linetype = 'solid'),
        axis.title.y = element_text(vjust = 6, size = 10), 
        axis.title.x = element_text(vjust = -6, size = 10))
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-5-1.png){width=672}
:::
:::



## Net Income vs. Semester


::: {.cell}

```{.r .cell-code}
seminc.aov <- aov(net_income ~ Smstr, ibc_semester)
summary(seminc.aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df    Sum Sq Mean Sq F value Pr(>F)
Smstr         2  12835887 6417943   1.931  0.148
Residuals   186 618074756 3322983               
```


:::

```{.r .cell-code}
par(mfrow = c(1,2))
plot(seminc.aov, which = 1:2)
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
ggplot(ibc_semester, aes(x=Smstr, y=net_income)) +
  geom_boxplot() +
  labs(x="Semester", y="Net Income") +
  scale_y_continuous(labels = dollar_format()) +
  theme(plot.margin = unit(c(2,1,2,2), "lines"), 
        plot.title = element_text(hjust = 0.5, vjust = 6, face = 'bold'),  
        panel.background = element_rect(fill = "white"), 
        axis.ticks.length.x = unit(0, 'pt'),  
        axis.ticks.length.y = unit(0, 'pt'), 
        panel.grid.major.y = element_line(color = "grey", linetype = 'solid'),
        axis.title.y = element_text(vjust = 6, size = 10), 
        axis.title.x = element_text(vjust = -6, size = 10))
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-7-1.png){width=672}
:::
:::



## Revenue vs. Category


::: {.cell}

```{.r .cell-code}
cat_aov <- aov(Revenue ~ Category, ibc_category)
summary(cat_aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df    Sum Sq   Mean Sq F value   Pr(>F)    
Category      3 4.901e+08 163354136   6.372 0.000392 ***
Residuals   185 4.743e+09  25635869                     
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
par(mfrow = c(1,2))
plot(cat_aov, which = 1:2)
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-8-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
ggplot(ibc_category, aes(x=Category, y=Revenue)) +
  geom_boxplot() +
  labs(x="Category", y="Revenue") +
  scale_y_continuous(labels = dollar_format()) +
  theme(plot.margin = unit(c(2,1,2,2), "lines"), 
        plot.title = element_text(hjust = 0.5, vjust = 6, face = 'bold'),  
        panel.background = element_rect(fill = "white"), 
        axis.ticks.length.x = unit(0, 'pt'),  
        axis.ticks.length.y = unit(0, 'pt'), 
        panel.grid.major.y = element_line(color = "grey", linetype = 'solid'),
        axis.title.y = element_text(vjust = 6, size = 10), 
        axis.title.x = element_text(vjust = -6, size = 10))
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-9-1.png){width=672}
:::
:::


## Net Income vs. Category


::: {.cell}

```{.r .cell-code}
catin.aov <- aov(net_income ~ Category, ibc_category)
summary(catin.aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df    Sum Sq Mean Sq F value Pr(>F)
Category      3  11239667 3746556   1.119  0.343
Residuals   185 619670977 3349573               
```


:::

```{.r .cell-code}
par(mfrow = c(1,2))
plot(catin.aov, which = 1:2)
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-10-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
ggplot(ibc_category, aes(x=Category, y=net_income)) +
  geom_boxplot() +
  labs(x="Category", y="Net Income") +
  scale_y_continuous(labels = dollar_format()) +
  theme(plot.margin = unit(c(2,1,2,2), "lines"), 
        plot.title = element_text(hjust = 0.5, vjust = 6, face = 'bold'),  
        panel.background = element_rect(fill = "white"), 
        axis.ticks.length.x = unit(0, 'pt'),  
        axis.ticks.length.y = unit(0, 'pt'), 
        panel.grid.major.y = element_line(color = "grey", linetype = 'solid'),
        axis.title.y = element_text(vjust = 6, size = 10), 
        axis.title.x = element_text(vjust = -6, size = 10))
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-11-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
ggplot(ibc_year, aes(x=Year, y=Revenue, group=Year)) +
  geom_boxplot() +
  labs(x="Year", y="Revenue") +
  scale_y_continuous(labels = dollar_format()) +
  theme(plot.margin = unit(c(2,1,2,2), "lines"), 
        plot.title = element_text(hjust = 0.5, vjust = 6, face = 'bold'),  
        panel.background = element_rect(fill = "white"), 
        axis.ticks.length.x = unit(0, 'pt'),  
        axis.ticks.length.y = unit(0, 'pt'), 
        panel.grid.major.y = element_line(color = "grey", linetype = 'solid'),
        axis.title.y = element_text(vjust = 6, size = 10), 
        axis.title.x = element_text(vjust = -6, size = 10))
```

::: {.cell-output-display}
![](HistoricalTotalsbySemester_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::
