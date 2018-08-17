#!/bin/bash

while read url; do
  echo "== $url =="
  curl -sL -O "$url"
done < list_of_urls.txt

gzip -r ../post


#	curl --cookie "over18=1" --limit-rate 20k -o Gossiping/index/index${i}.html https://www.ptt.cc/bbs/Gossiping/index${i}.html
