

## Deployment 

### Build docker image

Create a file named GITHUBPAT.txt with the GitHub personal access token in the same folder as the Dockerfile.

Docker image is build from the repository not form the local files. Make sure the repository is updated. 

```{bash, eval=FALSE}
docker build . -t eu.gcr.io/finngen-sandbox-v3-containers/co2:<version> --build-arg CACHEBUST=$(date +%s)
```

Flag `--build-arg CACHEBUST=$(date +%s)`  is used to force the pull the latest version from the repository and install it without having to install all the dependencies again.

 

### Push to sandbox

Authenticate with application-default login and configure docker. Use your FinnGen account

```bash
gcloud auth login 
```
   
Push image to destination.
```bash
docker push eu.gcr.io/finngen-sandbox-v3-containers/co2:<version>
```
 





