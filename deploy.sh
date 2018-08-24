#!/bin/bash

if [ "${TRAVIS_OS_NAME}" == "linux" ]; then 

# Workflow: clean man/ -> build web page -> revert man/ -> clean R/ -> deploy 
 
  # PART 1: Build package website with chinese characters in man/
  cd ./man
  bash back_to_zh.sh  # Now *.Rd in man is in zh-tw
  cd .. 

  ## Build site in docs/ to deploy to gh-pages
  Rscript -e 'source("build_site.R")'  # Build pkgdown site with man/ in zh
  ### Add favicon links to every html head
  head=

  cd ./docs
    file=$(find . -name '*.html')  
    for i in $file; do
      cat "${i}" | sed "s,<head>,<head>${head},g" > "temp"
      cat "temp" > "${i}"
    done
    rm temp
  cd ..
  mv ./favicon/ ./docs/
  
 
  ## Copy backup(*.Rd in pingyin) files to man/
  cd ./man
  cp -r pingyin_dir/*.Rd . && rm -r pingyin_dir
  cd ..


  # PART 2: Remove unicode chracters in R/ (For fixing win-build error)

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

  echo "## Build for Windows" >> README.md
  echo "**This Branch is only for building and testing pttR for Windows. See [master branch](https://github.com/liao961120/pttR) instead.**" >> README.md
fi

