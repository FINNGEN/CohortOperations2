

## Deployment 

### Build docker image





docker build . -t eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1 --build-arg CACHEBUST=$(date +%s)



















Docker image can be build from scratch or, to save time, it can be built using the pre-compiled dependencies built in  above section "Development in SandBox". 

Both methods use the same command:

```{bash, eval=FALSE}
cd <CohortOperationsShinyApp>
sudo docker build -t cohort_operations_shiny_app --build-arg GITHUB_PAT=<paste_PAT_token> .
```

If `renv::restore()` was run with option `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`. The cache is copied into the docker image during building. If so, you mush keep updated the cache if it changes during development. 

Alternatively, you can run `renv::restore()` or erase "./renv/cache" folder. In this case, all packages will be downloaded and install during the building process.  

### Load image into SandBox 
Built image can be moved into SandBox in two ways: (A) zipped and upload, or (B) through GCP Container Register. 

#### (A) Download and upload
Docker image can be save with: 

```{bash, eval=FALSE}
docker save --output cohort_operations_shiny_app.tar cohort_operations_shiny_app
```

Downloaded. For example using :

```{bash, eval=FALSE}
python3 -m http.server 8888
```

Uploaded and loaded into sanxbox:

```{bash, eval=FALSE}
docker load --input cohort_operations_shiny_app.tar
```

#### (B) Push and pull from GCP Container Register
Make sure you are [running docker without sudo](https://github.com/sindresorhus/guides/blob/main/docker-without-sudo.md). 

Authenticate with application-default login and configure docker. Use your FinnGen account
```{bash, eval=FALSE}
gcloud auth login
#gcloud auth application-default login #QUESTION: this was not working, is it ok the above way ??
gcloud auth configure-docker
# you can heck that credentials exist.
#cat ~/.config/gcloud/application_default_credentials.json
```

Tag the image.
```{bash, eval=FALSE}
docker tag cohort_operations_shiny_app eu.gcr.io/atlas-development-270609/cohort_operations_shiny_app:<version_tag>
```

Push newly tagged image to destination.
```{bash, eval=FALSE}
docker push eu.gcr.io/atlas-development-270609/cohort_operations_shiny_app:<version_tag>
```

Revoke credentials.
```{bash, eval=FALSE}
gcloud auth revoke
```

([other help link](https://cloud.google.com/container-registry/docs/advanced-authentication))

Request to humgen service desk to copy the image into SandBox container registry. 

Open a terminal inside SandBox. Pull the image :
```{bash, eval=FALSE}
docker pull eu.gcr.io/finngen-sandbox-v3-containers/cohort_operations_shiny_app:<version_tag>
```







