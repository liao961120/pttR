#!/bin/bash

Rscript data-raw/internal-ptt-dict-construct.R # Update dictioary file (read from github)
echo finish dict

Rscript data-raw/internal-board-info.R         # Update board info (scrape from PTT web)
echo finish board

Rscript data-raw/write-data2internal.R         # Write rds to internal data
echo finish write_internal

