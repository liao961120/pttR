#!/bin/bash

if [ "${TRAVIS_OS_NAME}" == "linux" ]; then 

# Workflow: clean man/ -> build web page -> revert man/ -> clean R/ -> deploy 
 
  # PART 1: Preperation for Building pkg Web Site
  
  ## Chinese characters in man/
  cd ./man
    bash back_to_zh.sh  # Now *.Rd in man is in zh-tw
  cd .. 

  ## Temporary change DESCRIPTION: To use Chinese Descriptions for Web
  cp DESCRIPTION DESCRIPTION_ori
  curl https://raw.githubusercontent.com/liao961120/local_depend/master/pttR-remote-support/DESCRIPTION_ch > DESCRIPTION

  # PART2: Build Web Site in docs/
  Rscript -e 'source("build_site.R")'  # Build pkgdown site with man/ in zh
  
  ## Modify head region of built HTML for optimizing SEO
     # head: Replace link for supportting multi-platform favicon
     # head1, head2: Change image path to direcet URL (May be a bug in pkgdown), and rescale image
  head='<link rel="apple-touch-icon" sizes="180x180" href="https://liao961120.github.io/pttR/favicon/apple-touch-icon.png?v=alQmNKw6dL"><link rel="icon" type="image/png" sizes="32x32" href="https://liao961120.github.io/pttR/favicon/favicon-32x32.png?v=alQmNKw6dL"><link rel="icon" type="image/png" sizes="194x194" href="https://liao961120.github.io/pttR/favicon/favicon-194x194.png?v=alQmNKw6dL"><link rel="icon" type="image/png" sizes="192x192" href="https://liao961120.github.io/pttR/favicon/android-chrome-192x192.png?v=alQmNKw6dL"><link rel="icon" type="image/png" sizes="16x16" href="https://liao961120.github.io/pttR/favicon/favicon-16x16.png?v=alQmNKw6dL"><link rel="manifest" href="https://liao961120.github.io/pttR/favicon/site.webmanifest?v=alQmNKw6dL"><link rel="mask-icon" href="https://liao961120.github.io/pttR/favicon/safari-pinned-tab.svg?v=alQmNKw6dL" color="#d56000"><link rel="shortcut icon" href="https://liao961120.github.io/pttR/favicon/favicon.ico?v=alQmNKw6dL"><meta name="apple-mobile-web-app-title" content="pttR"><meta name="application-name" content="pttR"><meta name="msapplication-TileColor" content="#00a300"><meta name="msapplication-TileImage" content="https://liao961120.github.io/pttR/favicon/mstile-144x144.png?v=alQmNKw6dL"><meta name="msapplication-config" content="https://liao961120.github.io/pttR/favicon/browserconfig.xml?v=alQmNKw6dL"><meta name="theme-color" content="#ffffff">'
  og_img1='<meta property="og:image" content="/logo.png">'
  og_img2='<meta property="og:image" content="https://liao961120.github.io/pttR/favicon/og-image.jpg"><meta property="og:image:width" content="170"><meta property="og:image:height" content="170">'
  title_desc=$(cat DESCRIPTION | grep "Title: " | cut -f 2 -d : | xargs)
  og_title='<meta property="og:title" content="'${title_desc}'">'
  og_title2='<meta property="og:title" content="'${title_desc}' | pttR">'
  
  cd ./docs
    file=$(find . -name '*.html')  
    for i in $file; do
      cat "${i}" | sed "s,<head>,<head>${head},g" > "temp"
      cat "temp" > "${i}"
      
      cat "${i}" | sed "s,${og_img1},${og_img2},g" > "temp"
      cat "temp" > "${i}"
      
      cat "${i}" | sed "s,${og_title},${og_title2},g" > "temp"
      cat "temp" > "${i}"
    done
    rm temp
  cd ..
  mv ./favicon/ ./docs/
  rm ./docs/favicon.ico


  # PART 3: Prepare for deploy to win-build
  
  ## Revert chaange 1: Copy backup(*.Rd in pingyin) files to man/
  cd ./man
    cp -r pingyin_dir/*.Rd . && rm -r pingyin_dir
  cd ..

  ## Revert chaange 2: Get the original DESCRIPTION back
  cat DESCRIPTION_ori > DESCRIPTION
  rm DESCRIPTION_ori


  ## Remove unicode chracters in R/ (For fixing win-build error)
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
  echo "**This Branch is automatically built for dowloading. See [master branch](https://github.com/liao961120/pttR) instead.**" >> README.md
fi

