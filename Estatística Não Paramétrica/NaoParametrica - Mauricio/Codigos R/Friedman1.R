> friedman.test(Y)

        Friedman rank sum test

data:  Y
Friedman chi-squared = 6.9, df = 3, p-value = 0.07515

> df=k-1;df
[1] 3
> 
> 
>  #### pela tabela 22 temos:
> 
> alfa=0.05
> 
> li=6.30;p2=0.094
> 
> ls=7.50;p1=0.052
> 
> nd=0.07515
> 
> nd<p2
[1] TRUE
> nd>p1
[1] TRUE
> 
> Y=c(bloco1,bloco2,bloco3,bloco4);Y
 [1] 3640 4200 4700 5300 4890 4550 6020 5900 4800 5320 5250 5150 4460 5500 5580
[16] 5560
> bloco=rep(1:4,each=4);bloco
 [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4
> bloco=factor(bloco)
> bloco=factor(bloco);bloco
 [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4
Levels: 1 2 3 4
> trat=rep(1:4,4)
> trat=factor(trat)
> trat
 [1] 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4
Levels: 1 2 3 4
> dad=cbind(bloco,trat,Y);dad
      bloco trat    Y
 [1,]     1    1 3640
 [2,]     1    2 4200
 [3,]     1    3 4700
 [4,]     1    4 5300
 [5,]     2    1 4890
 [6,]     2    2 4550
 [7,]     2    3 6020
 [8,]     2    4 5900
 [9,]     3    1 4800
[10,]     3    2 5320
[11,]     3    3 5250
[12,]     3    4 5150
[13,]     4    1 4460
[14,]     4    2 5500
[15,]     4    3 5580
[16,]     4    4 5560
> mod2=aov(Y~bloco +trat);mod2 
Call:
   aov(formula = Y ~ bloco + trat)

Terms:
                  bloco    trat Residuals
Sum of Squares  1956875 2737875   1416825
Deg. of Freedom       3       3         9

Residual standard error: 396.7682
Estimated effects may be unbalanced
> anova(mod2)
Analysis of Variance Table

Response: Y
          Df  Sum Sq Mean Sq F value  Pr(>F)  
bloco      3 1956875  652292  4.1435 0.04220 *
trat       3 2737875  912625  5.7972 0.01734 *
Residuals  9 1416825  157425                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> mod3=lm(Y ~bloco +trat)
> mod3

Call:
lm(formula = Y ~ bloco + trat)

Coefficients:
(Intercept)       bloco2       bloco3       bloco4        trat2        trat3  
       3856          880          670          815          445          940  
      trat4  
       1030  

> anova(mod2)
Analysis of Variance Table

Response: Y
          Df  Sum Sq Mean Sq F value  Pr(>F)  
bloco      3 1956875  652292  4.1435 0.04220 *
trat       3 2737875  912625  5.7972 0.01734 *
Residuals  9 1416825  157425                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> vcov(mod2)
            (Intercept)        bloco2        bloco3        bloco4         trat2
(Intercept)    68873.44 -3.935625e+04 -3.935625e+04 -3.935625e+04 -3.935625e+04
bloco2        -39356.25  7.871250e+04  3.935625e+04  3.935625e+04  2.087754e-11
bloco3        -39356.25  3.935625e+04  7.871250e+04  3.935625e+04  1.606171e-11
bloco4        -39356.25  3.935625e+04  3.935625e+04  7.871250e+04  2.008271e-11
trat2         -39356.25  2.087754e-11  1.606171e-11  2.008271e-11  7.871250e+04
trat3         -39356.25  8.826197e-12  3.965543e-12  9.268943e-12  3.935625e+04
trat4         -39356.25  1.591492e-11  1.580672e-11  2.626200e-11  3.935625e+04
                    trat3         trat4
(Intercept) -3.935625e+04 -3.935625e+04
bloco2       8.826197e-12  1.591492e-11
bloco3       3.965543e-12  1.580672e-11
bloco4       9.268943e-12  2.626200e-11
trat2        3.935625e+04  3.935625e+04
trat3        7.871250e+04  3.935625e+04
trat4        3.935625e+04  7.871250e+04
> round(vcov(mod2),2)
            (Intercept)    bloco2    bloco3    bloco4     trat2     trat3
(Intercept)    68873.44 -39356.25 -39356.25 -39356.25 -39356.25 -39356.25
bloco2        -39356.25  78712.50  39356.25  39356.25      0.00      0.00
bloco3        -39356.25  39356.25  78712.50  39356.25      0.00      0.00
bloco4        -39356.25  39356.25  39356.25  78712.50      0.00      0.00
trat2         -39356.25      0.00      0.00      0.00  78712.50  39356.25
trat3         -39356.25      0.00      0.00      0.00  39356.25  78712.50
trat4         -39356.25      0.00      0.00      0.00  39356.25  39356.25
                trat4
(Intercept) -39356.25
bloco2           0.00
bloco3           0.00
bloco4           0.00
trat2        39356.25
trat3        39356.25
trat4        78712.50
> model.matrix(mod2)
   (Intercept) bloco2 bloco3 bloco4 trat2 trat3 trat4
1            1      0      0      0     0     0     0
2            1      0      0      0     1     0     0
3            1      0      0      0     0     1     0
4            1      0      0      0     0     0     1
5            1      1      0      0     0     0     0
6            1      1      0      0     1     0     0
7            1      1      0      0     0     1     0
8            1      1      0      0     0     0     1
9            1      0      1      0     0     0     0
10           1      0      1      0     1     0     0
11           1      0      1      0     0     1     0
12           1      0      1      0     0     0     1
13           1      0      0      1     0     0     0
14           1      0      0      1     1     0     0
15           1      0      0      1     0     1     0
16           1      0      0      1     0     0     1
attr(,"assign")
[1] 0 1 1 1 2 2 2
attr(,"contrasts")
attr(,"contrasts")$bloco
[1] "contr.treatment"

attr(,"contrasts")$trat
[1] "contr.treatment"

> model.matrix(mod3)
   (Intercept) bloco2 bloco3 bloco4 trat2 trat3 trat4
1            1      0      0      0     0     0     0
2            1      0      0      0     1     0     0
3            1      0      0      0     0     1     0
4            1      0      0      0     0     0     1
5            1      1      0      0     0     0     0
6            1      1      0      0     1     0     0
7            1      1      0      0     0     1     0
8            1      1      0      0     0     0     1
9            1      0      1      0     0     0     0
10           1      0      1      0     1     0     0
11           1      0      1      0     0     1     0
12           1      0      1      0     0     0     1
13           1      0      0      1     0     0     0
14           1      0      0      1     1     0     0
15           1      0      0      1     0     1     0
16           1      0      0      1     0     0     1
attr(,"assign")
[1] 0 1 1 1 2 2 2
attr(,"contrasts")
attr(,"contrasts")$bloco
[1] "contr.treatment"

attr(,"contrasts")$trat
[1] "contr.treatment"

> summary(mod3)

Call:
lm(formula = Y ~ bloco + trat)

Residuals:
    Min      1Q  Median      3Q     Max 
-631.25 -212.50  -63.75  291.25  413.75 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   3856.2      262.4  14.694 1.35e-07 ***
bloco2         880.0      280.6   3.137  0.01199 *  
bloco3         670.0      280.6   2.388  0.04068 *  
bloco4         815.0      280.6   2.905  0.01745 *  
trat2          445.0      280.6   1.586  0.14717    
trat3          940.0      280.6   3.350  0.00852 ** 
trat4         1030.0      280.6   3.671  0.00514 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 396.8 on 9 degrees of freedom
Multiple R-squared:  0.7682,    Adjusted R-squared:  0.6136 
F-statistic:  4.97 on 6 and 9 DF,  p-value: 0.01635

> 
