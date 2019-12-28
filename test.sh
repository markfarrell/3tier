#!/bin/bash
forward_linux='http://0.0.0.0:3000/forward/linux/?entry='
rm -f *.db
cp templates/*.db .
./bin/centralized-logging-server-linux &
sleep 15s
cat ./data/linux/sample.log | ./bin/centralized-logging-forwarder-linux $forward_linux
kill %1
