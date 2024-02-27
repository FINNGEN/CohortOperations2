

## Deployment 

### Build docker image

Create a file named GITHUBPAT.txt with the GitHub personal access token in the same folder as the Dockerfile.

Docker image is build from the repository not form the local files. Make sure the repository is updated. 

```{bash, eval=FALSE}
docker build . -t eu.gcr.io/finngen-sandbox-v3-containers/co2:<version> \
  --build-arg CACHEBUST=$(date +%s)\
  --build-arg BRANCH_NAME=$(git rev-parse --abbrev-ref HEAD)
```

Version tags 'latest' and 'dev' are reserved for the latest version and the development version respectively. 
Use any other tag for testing purposes.

Flag `--build-arg CACHEBUST=$(date +%s)`  is used to force the pull the latest version from the repository and install it without having to install all the dependencies again.

Flag `--build-arg BRANCH_NAME=$(git rev-parse --abbrev-ref HEAD)` is used to install the package from the current active branch.
If this command is called from a folder with in the project. Flag can be replace with 'main' or 'development' to build the image for tags 'latest' and 'dev' respectively.



 

### Push to sandbox

Authenticate with application-default login and configure docker. Use your FinnGen account

```bash
gcloud auth login 
```
   
Push image to destination.
```bash
docker push eu.gcr.io/finngen-sandbox-v3-containers/co2:<version>
```
 





