# finance_playground

```bash
sudo docker build -f ~/finance_playground/Dockerfile ~/finance_playground --tag=finance_playground
sudo docker run -it -v ~/finance_playground:/home/finance_playground finance_playground bash
R -q
source("explore.R")
```

![S&P 500 Close](s&p500_close.png)

![S&P 500 Log Close](s&p500_log_close.png)

![S&P 500 Log Returns](s&p500_log_return.png)

![Test Set Log Likelihoods](test_set_log_likelihood.png)

![Test Set Histogram & Model Densities](test_set_log_returns_histogram_and_model_densities.png)

![Log Densities](model_log_densities.png)
