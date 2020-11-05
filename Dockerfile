FROM r-base

# Curl is needed for quantmod
# See https://github.com/docker/for-linux/issues/1131
RUN apt-get update && apt-get install -y curl libcurl4-openssl-dev -o APT::Immediate-Configure=0

RUN R -e "install.packages(c('ggplot2', 'mixtools', 'quantmod'))"

WORKDIR /home/finance_playground