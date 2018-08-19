#!/bin/bash

if [[ ! -d R_pars_dir ]]; then 
  mkdir R_pars_dir
fi


for i in *.R; do
  cat "${i}" | sed -r 's/#.+//g' > R_pars_dir/"${i}"
done


# don't run locally: cp -r R_pars_dir/*.R .
