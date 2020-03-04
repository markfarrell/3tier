#!/bin/sh
HOST=$1
FLOW_RECORD=$2
curl -G -v "http://$HOST/forward/flow" --data-urlencode "q=$FLOW_RECORD"
