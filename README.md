``` bash
podman volume create r-site-library
# podman run -it --rm -v r-site-library:/usr/local/lib/R/site-library r-base
podman run -it --name r-base r-base
```

``` shell
podman run --name rstudio \
    -p 8787:8787 \
    -e PASSWORD=changeme \
    -v .:/workdir \
    docker.io/rocker/rstudio
``` 