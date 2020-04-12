| Learner                                                                 |    Weight| Min. Fold Risk| CV-Mean Risk| Max. Fold Risk|
|:------------------------------------------------------------------------|---------:|--------------:|------------:|--------------:|
| Intercept model                                                         | 0.0010146|      4.5582683|   10.6727821|     19.5866924|
| Generalized linear model                                                | 0.0000000|      1.4518634|    5.8182547|     22.3896415|
| Ridge regression (L2 penalty only)                                      | 0.0000000|      1.8218880|    6.3511082|     15.5949403|
| Lasso regression (L1 penalty only)                                      | 0.0000000|      1.4515412|    5.5360312|     20.8053263|
| Elastic net (25% L1, 75% L2 penalties)                                  | 0.0000000|      1.4456927|    5.5820900|     20.2182214|
| Elastic net (50% L1, 50% L2 penalties)                                  | 0.0000000|      1.4516456|    5.5644977|     21.1103607|
| Elastic net (75% L1, 25% L2 penalties)                                  | 0.0000000|      1.4506643|    5.5539900|     20.9665998|
| Custom Poisson regression                                               | 0.0132703|     14.8722760|  106.6813677|   1094.5608110|
| Random forest (50 trees)                                                | 0.0000000|      0.4933727|    1.4338410|      2.3962612|
| Random forest (100 trees)                                               | 0.0000000|      0.4473634|    1.4285483|      2.2896066|
| Random forest (500 trees)                                               | 0.0000000|      0.4540806|    1.4148094|      2.2735312|
| xgboost (50 iterations, max. tree depth = 2, learning rate = 0.001)     | 0.0000000|      4.2241126|   10.5028092|     20.2338284|
| xgboost (50 iterations, max. tree depth = 5, learning rate = 0.001)     | 0.0000000|      4.2087342|   10.5004241|     20.2018577|
| xgboost (50 iterations, max. tree depth = 8, learning rate = 0.001)     | 0.0000000|      4.2083754|   10.4938605|     20.2164001|
| xgboost (50 iterations, max. tree depth = 2, learning rate = 0.05)      | 0.0000000|      0.2717743|    0.7765784|      1.5221445|
| xgboost (50 iterations, max. tree depth = 5, learning rate = 0.05)      | 0.0000000|      0.1960830|    0.5038528|      0.8698730|
| xgboost (50 iterations, max. tree depth = 8, learning rate = 0.05)      | 0.0000000|      0.2009571|    0.5396907|      1.1164479|
| xgboost (50 iterations, max. tree depth = 2, learning rate = 0.20)      | 0.0708479|      0.2540515|    0.4859991|      0.7684363|
| xgboost (50 iterations, max. tree depth = 5, learning rate = 0.20)      | 0.0013957|      0.2103430|    0.3626901|      0.5478229|
| xgboost (50 iterations, max. tree depth = 8, learning rate = 0.20)      | 0.0025872|      0.2036223|    0.3743645|      0.5694282|
| xgboost (100 iterations, max. tree depth = 2, learning rate = 0.001)    | 0.0000000|      3.8618801|    9.6208054|     18.5797039|
| xgboost (100 iterations, max. tree depth = 5, learning rate = 0.001)    | 0.0000000|      3.8323963|    9.6139457|     18.5168594|
| xgboost (100 iterations, max. tree depth = 8, learning rate = 0.001)    | 0.0000000|      3.8317249|    9.6010220|     18.5439091|
| xgboost (100 iterations, max. tree depth = 2, learning rate = 0.05)     | 0.0868449|      0.2538900|    0.5042024|      0.8332305|
| xgboost (100 iterations, max. tree depth = 5, learning rate = 0.05)     | 0.3292875|      0.1964940|    0.3578047|      0.5367939|
| xgboost (100 iterations, max. tree depth = 8, learning rate = 0.05)     | 0.0250175|      0.2080751|    0.3862314|      0.6973792|
| xgboost (100 iterations, max. tree depth = 2, learning rate = 0.20)     | 0.0514476|      0.2480681|    0.4930272|      0.7028391|
| xgboost (100 iterations, max. tree depth = 5, learning rate = 0.20)     | 0.2567916|      0.2136044|    0.3666381|      0.5448544|
| xgboost (100 iterations, max. tree depth = 8, learning rate = 0.20)     | 0.1614951|      0.2037618|    0.3728807|      0.5621261|
| Stratified exponential smoothing                                        | 0.0000000|      1.1392641|    7.2331678|     15.2659706|
| Super Learner                                                           | 1.0000000|      0.1885469|    0.3182298|      0.4033458|
