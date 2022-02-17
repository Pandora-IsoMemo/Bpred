FROM inwt/r-shiny:4.0.1

RUN Rscript -e "devtools::install_github('INWTlab/shiny-matrix')" && \
  Rscript -e "devtools::install_github('INWTlab/rsync')"

ENV PKG mpiBpred

RUN apt-get update && \
  apt-get install -y git

COPY ssh_config /etc/ssh/ssh_config

COPY ssh_known_hosts /etc/ssh/ssh_known_hosts

COPY ssh_github_bpred /etc/ssh/id_github_bpred

RUN chmod 0400 /etc/ssh/id_github_bpred

COPY . .

RUN installPackage

CMD ["Rscript", "-e", "mpiBpred::startApplication(3838)"]
