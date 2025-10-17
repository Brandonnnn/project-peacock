#! /bin/bash

ssh -i /home/shiny/.ssh/id_rsa shinyuser@10.22.60.46 <<EOF
cd /cameo_portal/PROD_LIST_IMPORT
mv $1 $2
EOF
