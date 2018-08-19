#!/bin/bash

if [ "${TRAVIS_OS_NAME}" == "linux" ]; then 
  # Build site in docs/ to deploy to gh-pages
  Rscript -e 'source("build_site.R")'

  # Remove all comments in R/*.R to build ASCII-only R scripts
  # This mean removing all Roxygen documents in R/,
  # so can only be done remotely. 
  # The master branch is then push to branch: win-build after
  # executing this script.
  cd ./R
	
  bash clean_rscript.sh
  cp -r R_pars_dir/*.R .
  rm -r R_pars_dir/
	
  cd ..
fi
