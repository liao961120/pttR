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
  head='<link rel="apple-touch-icon" sizes="180x180" href="https://liao961120.github.io/pttR/favicon/apple-touch-icon.png"><link rel="icon" type="image/png" sizes="32x32" href="https://liao961120.github.io/pttR/favicon/favicon-32x32.png"><link rel="icon" type="image/png" sizes="16x16" href="https://liao961120.github.io/pttR/favicon/favicon-16x16.png"><link rel="manifest" href="https://liao961120.github.io/pttR/favicon/site.webmanifest"><link rel="mask-icon" href="https://liao961120.github.io/pttR/favicon/safari-pinned-tab.svg" color="#d57700"><link rel="shortcut icon" href="https://liao961120.github.io/pttR/favicon/favicon.ico"><meta name="apple-mobile-web-app-title" content="pttR"><meta name="application-name" content="pttR"><meta name="msapplication-TileColor" content="#00a300"><meta name="msapplication-TileImage" content="https://liao961120.github.io/pttR/favicon/mstile-144x144.png"><meta name="msapplication-config" content="https://liao961120.github.io/pttR/favicon/browserconfig.xml"><meta name="theme-color" content="#ffffff">'
  
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

