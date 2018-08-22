#!/bin/bash

if [[ ! -d translated ]]; then mkdir translated; fi

trans="../vignettes/translation.csv"
n_line=$(wc -l $trans | grep -o "[0-9]")

for (( i=1; i<=${n_line}; ++i )); do
	line=$(sed -n -e "$i"p $trans)
	zh=$(echo $line | cut -f 1 -d ,)
	pingyin=$(echo $line | cut -f 2 -d ,)
	
	for f in *.Rd; do
		cat "${f}" | sed -r "s/$pingyin/$zh/g" > translated/"${f}"
	done
done

cp -r translated/*.Rd .
rm -r translated
