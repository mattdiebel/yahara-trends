 summary(result1)
      yHat               SE             ConcHat      
 Min.   :-0.9256   Min.   :0.09419   Min.   :0.4305  
 1st Qu.: 0.3146   1st Qu.:0.19618   1st Qu.:1.4029  
 Median : 0.5286   Median :0.21150   Median :1.7302  
 Mean   : 0.4933   Mean   :0.21308   Mean   :1.7585  
 3rd Qu.: 0.7073   3rd Qu.:0.22922   3rd Qu.:2.0782  
 Max.   : 1.6854   Max.   :0.40710   Max.   :5.6566  
> summary(result3)
      yHat               SE            ConcHat      
 Min.   :-0.7233   Min.   :0.1547   Min.   :0.5085  
 1st Qu.: 0.2979   1st Qu.:0.1982   1st Qu.:1.3843  
 Median : 0.5175   Median :0.2185   Median :1.7120  
 Mean   : 0.4835   Mean   :0.2189   Mean   :1.7377  
 3rd Qu.: 0.6914   3rd Qu.:0.2389   3rd Qu.:2.0426  
 Max.   : 1.6393   Max.   :0.3067   Max.   :5.3737  
> summary(eList$Sample)
      Date                  Q                LogQ          ConcLow         ConcHigh         Uncen       
 Min.   :1984-10-02   Min.   :  28.09   Min.   :3.335   Min.   :0.068   Min.   :0.168   Min.   :0.0000  
 1st Qu.:1991-10-15   1st Qu.: 100.52   1st Qu.:4.610   1st Qu.:1.370   1st Qu.:1.370   1st Qu.:1.0000  
 Median :1997-04-22   Median : 234.18   Median :5.456   Median :1.700   Median :1.700   Median :1.0000  
 Mean   :1997-07-29   Mean   : 493.80   Mean   :5.531   Mean   :1.771   Mean   :1.772   Mean   :0.9948  
 3rd Qu.:2001-09-20   3rd Qu.: 526.69   3rd Qu.:6.267   3rd Qu.:2.080   3rd Qu.:2.080   3rd Qu.:1.0000  
 Max.   :2013-07-23   Max.   :9259.49   Max.   :9.133   Max.   :9.220   Max.   :9.220   Max.   :1.0000  
    ConcAve          Julian          Month             Day           DecYear        MonthSeq   
 Min.   :0.118   Min.   :49217   Min.   : 1.000   Min.   :  3.0   Min.   :1985   Min.   :1618  
 1st Qu.:1.370   1st Qu.:51786   1st Qu.: 4.000   1st Qu.:100.0   1st Qu.:1992   1st Qu.:1702  
 Median :1.700   Median :53802   Median : 7.000   Median :184.0   Median :1997   Median :1768  
 Mean   :1.771   Mean   :53900   Mean   : 6.551   Mean   :183.3   Mean   :1998   Mean   :1771  
 3rd Qu.:2.080   3rd Qu.:55414   3rd Qu.: 9.000   3rd Qu.:269.0   3rd Qu.:2002   3rd Qu.:1821  
 Max.   :9.220   Max.   :59738   Max.   :12.000   Max.   :366.0   Max.   :2014   Max.   :1963  
     SinDY              CosDY               yHat               SE            ConcHat      
 Min.   :-1.00000   Min.   :-1.00000   Min.   :-0.7233   Min.   :0.1547   Min.   :0.5085  
 1st Qu.:-0.76235   1st Qu.:-0.72661   1st Qu.: 0.2979   1st Qu.:0.1982   1st Qu.:1.3843  
 Median : 0.00000   Median :-0.10738   Median : 0.5175   Median :0.2185   Median :1.7120  
 Mean   :-0.01673   Mean   :-0.05536   Mean   : 0.4835   Mean   :0.2189   Mean   :1.7377  
 3rd Qu.: 0.72803   3rd Qu.: 0.61867   3rd Qu.: 0.6914   3rd Qu.:0.2389   3rd Qu.:2.0426  
 Max.   : 0.99999   Max.   : 0.99996   Max.   : 1.6393   Max.   :0.3067   Max.   :5.3737  
     resid         
 Min.   :-1.66829  
 1st Qu.:-0.12118  
 Median : 0.02050  
 Mean   : 0.02124  
 3rd Qu.: 0.16652  
 Max.   : 0.83968  
> summary(newSample)
      Date                 Q.x               LogQ          ConcLow         ConcHigh         Uncen       
 Min.   :1984-10-02   Min.   :  28.09   Min.   :3.335   Min.   :0.068   Min.   :0.168   Min.   :0.0000  
 1st Qu.:1991-10-15   1st Qu.: 100.52   1st Qu.:4.610   1st Qu.:1.370   1st Qu.:1.370   1st Qu.:1.0000  
 Median :1997-04-22   Median : 234.18   Median :5.456   Median :1.700   Median :1.700   Median :1.0000  
 Mean   :1997-07-29   Mean   : 493.80   Mean   :5.531   Mean   :1.771   Mean   :1.772   Mean   :0.9948  
 3rd Qu.:2001-09-20   3rd Qu.: 526.69   3rd Qu.:6.267   3rd Qu.:2.080   3rd Qu.:2.080   3rd Qu.:1.0000  
 Max.   :2013-07-23   Max.   :9259.49   Max.   :9.133   Max.   :9.220   Max.   :9.220   Max.   :1.0000  
    ConcAve          Julian          Month             Day           DecYear        MonthSeq   
 Min.   :0.118   Min.   :49217   Min.   : 1.000   Min.   :  3.0   Min.   :1985   Min.   :1618  
 1st Qu.:1.370   1st Qu.:51786   1st Qu.: 4.000   1st Qu.:100.0   1st Qu.:1992   1st Qu.:1702  
 Median :1.700   Median :53802   Median : 7.000   Median :184.0   Median :1997   Median :1768  
 Mean   :1.771   Mean   :53900   Mean   : 6.551   Mean   :183.3   Mean   :1998   Mean   :1771  
 3rd Qu.:2.080   3rd Qu.:55414   3rd Qu.: 9.000   3rd Qu.:269.0   3rd Qu.:2002   3rd Qu.:1821  
 Max.   :9.220   Max.   :59738   Max.   :12.000   Max.   :366.0   Max.   :2014   Max.   :1963  
     SinDY              CosDY               yHat               SE            ConcHat            Q.y         
 Min.   :-1.00000   Min.   :-1.00000   Min.   :-0.7233   Min.   :0.1547   Min.   :0.5085   Min.   :  28.09  
 1st Qu.:-0.76235   1st Qu.:-0.72661   1st Qu.: 0.2979   1st Qu.:0.1982   1st Qu.:1.3843   1st Qu.: 100.52  
 Median : 0.00000   Median :-0.10738   Median : 0.5175   Median :0.2185   Median :1.7120   Median : 234.18  
 Mean   :-0.01673   Mean   :-0.05536   Mean   : 0.4835   Mean   :0.2189   Mean   :1.7377   Mean   : 493.80  
 3rd Qu.: 0.72803   3rd Qu.: 0.61867   3rd Qu.: 0.6914   3rd Qu.:0.2389   3rd Qu.:2.0426   3rd Qu.: 526.69  
 Max.   : 0.99999   Max.   : 0.99996   Max.   : 1.6393   Max.   :0.3067   Max.   :5.3737   Max.   :9259.49  
       Qb                Qp                Q                Conc           LogQb           resid          
 Min.   :  20.95   Min.   :  8.009   Min.   :  28.09   Min.   :0.118   Min.   :3.042   Min.   :-1.361569  
 1st Qu.:  73.45   1st Qu.: 50.609   1st Qu.: 100.52   1st Qu.:1.370   1st Qu.:4.297   1st Qu.:-0.113544  
 Median : 157.37   Median : 72.082   Median : 234.18   Median :1.700   Median :5.059   Median : 0.005068  
 Mean   : 233.17   Mean   : 68.023   Mean   : 493.80   Mean   :1.771   Mean   :5.049   Mean   : 0.011485  
 3rd Qu.: 305.64   3rd Qu.: 89.511   3rd Qu.: 526.69   3rd Qu.:2.080   3rd Qu.:5.722   3rd Qu.: 0.132442  
 Max.   :1238.05   Max.   :100.000   Max.   :9259.49   Max.   :9.220   Max.   :7.121   Max.   : 0.708094  
> plot(newSample$resid,eList$Sample$resid)
> abline(a=0,b=1)
