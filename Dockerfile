FROM r-base

# Curl is needed for quantmod
# See https://github.com/docker/for-linux/issues/1131
# libuv1-dev is needed by the fs package (transitive dep of mixtools -> plotly -> htmlwidgets -> ... -> fs)
# -t unstable: r-base installs R (and with it libcurl4t64) from Debian unstable, so the -dev packages must come from
# unstable too; otherwise apt pairs testing's older libcurl4-openssl-dev with the newer installed libcurl4t64 and fails
RUN apt-get update && apt-get install -y -t unstable curl libcurl4-openssl-dev libuv1-dev -o APT::Immediate-Configure=0

RUN R -e "install.packages(c('ggplot2', 'mixtools', 'quantmod'))"

WORKDIR /home/finance_playground