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
  head='<link rel="apple-touch-icon" sizes="57x57" href="https://liao961120.github.io/pttR/favicon/apple-icon-57x57.png"><link rel="apple-touch-icon" sizes="60x60" href="https://liao961120.github.io/pttR/favicon/apple-icon-60x60.png"><link rel="apple-touch-icon" sizes="72x72" href="https://liao961120.github.io/pttR/favicon/apple-icon-72x72.png"><link rel="apple-touch-icon" sizes="76x76" href="https://liao961120.github.io/pttR/favicon/apple-icon-76x76.png"><link rel="apple-touch-icon" sizes="114x114" href="https://liao961120.github.io/pttR/favicon/apple-icon-114x114.png"><link rel="apple-touch-icon" sizes="120x120" href="https://liao961120.github.io/pttR/favicon/apple-icon-120x120.png"><link rel="apple-touch-icon" sizes="144x144" href="https://liao961120.github.io/pttR/favicon/apple-icon-144x144.png"><link rel="apple-touch-icon" sizes="152x152" href="https://liao961120.github.io/pttR/favicon/apple-icon-152x152.png"><link rel="apple-touch-icon" sizes="180x180" href="https://liao961120.github.io/pttR/favicon/apple-icon-180x180.png"><link rel="icon" type="image/png" sizes="192x192"  href="https://liao961120.github.io/pttR/favicon/android-icon-192x192.png"><link rel="icon" type="image/png" sizes="32x32" href="https://liao961120.github.io/pttR/favicon/favicon-32x32.png"><linkrel="icon" type="image/png" sizes="96x96" href="https://liao961120.github.io/pttR/favicon/favicon-96x96.png"><link rel="icon" type="image/png" sizes="16x16" href="https://liao961120.github.io/pttR/favicon/favicon-16x16.png"><link rel="manifest"href="https://liao961120.github.io/pttR/favicon/manifest.json"><meta name="msapplication-TileColor" content="#ffffff"><meta name="msapplication-TileImage" content="/ms-icon-144x144.png"><meta name="theme-color" content="#ffffff">'
  
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

