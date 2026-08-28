# Bpred

Bayesian multivariate regression application

### Access to online version:
- MAIN version: https://isomemoapp.com/app/bpred
- BETA version: https://isomemoapp.com/app/bpred-beta

### Documenation
- https://pandora-isomemo.github.io/Bpred/

### Installation instructions
- https://pandora-isomemo.github.io/docs/apps.html#bpred---bayesian-prediction

### Release notes:
- see `NEWS.md`

### Folder for online models
- [`inst/app/predefinedModels`](https://github.com/Pandora-IsoMemo/bpred/tree/main/inst/app/predefinedModels)

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is build automatically via github action.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

When testing with a local docker container, please make sure to rebuild the docker image after changes in the R code or dependencies. You can do this from the root of the repository via:

```bash
docker build -t bpred-app:latest .
```

or for a full rebuild without cache:

```bash
docker build --no-cache -t bpred-app:latest .
```

After that, start the container as usual via:

```bash
docker run -p 3838:3838 bpred-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after stopping.
