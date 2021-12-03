#!/usr/bin/env bash

wget --no-verbose \
     --mirror \
     --adjust-extension \
     --convert-links \
     --force-directories \
     --backup-converted \
     --span-hosts \
     --no-parent \
     -e robots=off \
     --restrict-file-names=windows \
     --timeout=5 \
     --warc-file=archive.warc \
     --page-requisites \
     --no-check-certificate \
     --no-hsts \
     --exclude-directories=/ftp \
     --domains support.hdfgroup.org \
     "https://support.hdfgroup.org/"
