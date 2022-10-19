FROM rocker/binder:3.6.0
LABEL maintainer='Emma Reich'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/egreich/GEDI_workshop/main && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-10-24/')); devtools::install_deps()"
