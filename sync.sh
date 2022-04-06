#!/bin/bash

set -x
path=$(dirname $0)

rsync -rav -e "ssh -p 22" --exclude=.git --exclude=.idea --exclude=**/.DS_Store --exclude=dump.rdb  --exclude=node_modules* $path root@114.67.218.152:/root/dsa4js
