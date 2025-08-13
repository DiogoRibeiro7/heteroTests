FROM rocker/r-base:latest

RUN apt-get update && \
    apt-get install -y r-cran-ggplot2 r-cran-testthat r-cran-knitr r-cran-rmarkdown

WORKDIR /pkg
COPY . /pkg
RUN R -q -e "install.packages(c('renv', 'remotes')); renv::restore(); remotes::install_local()"
CMD ["R"]
