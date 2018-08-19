#!/bin/bash

if [ "${TRAVIS_OS_NAME}" == "linux" ]; then 
	Rscript -e 'source("build_site.R")'
fi

if [ "${TRAVIS_OS_NAME}" == "linux" ]; then
	cd ./R
	
	bash clean_rscript.sh
	cp -r R_pars_dir/*.R .
    rm -r R_pars_dir/
	
	cd ..
fi



