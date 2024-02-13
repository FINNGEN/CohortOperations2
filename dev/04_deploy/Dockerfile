# syntax=docker/dockerfile:1
FROM --platform=linux/amd64 rocker/rstudio:4.3.0

# install OS dependencies including java and python 3
RUN apt-get update && apt-get install -y openjdk-11-jdk liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv git \
    # rjava
    libssl-dev libcurl4-openssl-dev  libpcre2-dev libicu-dev \
    # xml2
    libxml2-dev \
    # sodium
    libsodium-dev\
    # systemfonts
    libfontconfig1-dev \
    # textshaping
    libharfbuzz-dev libfribidi-dev\
    #ragg
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev\
&& R CMD javareconf \
&& rm -rf /var/lib/apt/lists/*

# install utility R packages
RUN install2.r \
	openssl \
	httr \
	xml2 \
	remotes \
	gitcreds \
&& rm -rf /tmp/download_packages/ /tmp/*.rds

# Create user and move to users directory
# Create a new user named "myuser" with UID and GID set to 1000
#RUN useradd -ms /bin/bash shiny
#USER shiny

# clone repo
RUN git clone https://github.com/FINNGEN/CohortOperations2.git /root/CohortOperations2

WORKDIR /root/CohortOperations2

COPY GITHUBPAT.txt /tmp/GITHUBPAT.txt

# install OHDSI HADES R packages from CRAN and GitHub, temporarily adding a GitHub Personal Access Token (PAT) to the Renviron file
#RUN R -e "renv::restore()"
# install OHDSI HADES R packages from CRAN and GitHub, temporarily adding a GitHub Personal Access Token (PAT) to the Renviron file
RUN --mount=type=secret,id=build_github_pat \
	cp /usr/local/lib/R/etc/Renviron ./Renviron \
        && echo "GITHUB_PAT=$(cat /tmp/GITHUBPAT.txt)" >> ./Renviron \
        && R -e "renv::restore()" \
        && R -e "renv::isolate()" \
        && rm ./Renviron

# install the jdbc drivers for database access using the OHDSI DatabaseConnector R package
RUN mkdir -p /home/rstudio/jdbc_drivers
ENV DATABASECONNECTOR_JAR_FOLDER="/home/rstudio/jdbc_drivers"
RUN R -e "DatabaseConnector::downloadJdbcDrivers('bigquery')" 

#
# FORCES REBUILD FROM THIS POINT
#
ARG CACHEBUST=1

# Update the code and dependencies
RUN git pull
RUN R -e "renv::restore()" \
    && R -e "renv::isolate()" 


EXPOSE 8888
ENV CO2CONFIGFILE = "/tmp/co2_config.yml"
CMD ["/usr/local/bin/R", "-e", "devtools::load_all('.');run_app(testthat::test_path('config', 'cohortOperationsConfig.yml'), Sys.getenv('CO2CONFIGFILE'), options = list(port=8888))"]
