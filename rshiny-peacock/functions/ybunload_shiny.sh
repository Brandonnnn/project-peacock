#! /bin/bash

# shiny server
# export JAVA_HOME=/opt/jre1.8.0_211/
export YBPASSWORD=$(cat /home/shiny/.ybpassword)

# # local test
# export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
# export YBPASSWORD=$(cat /Users/bwang/projects/ds-shiny-tools/others/.ybpassword)

# local mode
download_dir=$1
yb_table_nm=$2
output_tbl_nm=$3
query=$4

mkdir $download_dir


# build up options
options=$(echo "-h orlpybvip01.catmktg.com \
        -d py1ussa1\
        --username r_shiny_app \
        --prefix $yb_table_nm \
        --max-file-size 10GB \
        --format text \
        --delimiter , \
        -o $download_dir \
        --truncate-existing \
        --logfile-log-level TRACE")

time /home/ybtools/bin/ybunload $options --select "$4;"

# change name (ls path is not the new created folder)
ls $download_dir/$yb_table_nm* | xargs -I '{}' mv {} $download_dir/$output_tbl_nm.txt
