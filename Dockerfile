FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "mpiBpred::startApplication(3838)"]
