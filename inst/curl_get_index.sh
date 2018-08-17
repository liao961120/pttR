#!/bin/bash

for i in {39425..39430}; do
	curl --cookie "over18=1" --limit-rate 20k -o Gossiping/index/index${i}.html https://www.ptt.cc/bbs/Gossiping/index${i}.html
	gzip Gossiping/index/index${i}.html
done

## Or parse directly
# https://github.com/ericchiang/pup
