FROM rocker/binder:3.6.0
LABEL maintainer='Emma Reich'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

