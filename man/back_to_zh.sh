#!/bin/bash

mkdir pingyin_dir
cp -r *.Rd pingyin_dir  # Backup file for win-build


trans="../data-raw/translation.csv"
n_line=$(wc -l $trans | grep -o "[0-9]")

for f in *.Rd; do
	temp=
	for (( i=1; i<=${n_line}; ++i )); do
		line=$(sed -n -e "$i"p $trans)
		zh=$(echo $line | cut -f 1 -d ,)
		pingyin=$(echo $line | cut -f 2 -d ,)

		cat "${f}" | sed -r "s/$pingyin/$zh/g" > "temp"
		cat "temp" > ${f}
	done
done

rm temp
