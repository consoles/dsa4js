#!/bin/bash

set -x
path=$(dirname $0)

rsync -rav -e "ssh -p 3600" --exclude=.git --exclude=.idea --exclude=**/.DS_Store --exclude=dump.rdb  --exclude=node_modules* $path devel@192.168.3.118:/home/devel/dsa4js
