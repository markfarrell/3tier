#!/bin/bash
forward_linux='http://0.0.0.0:3000/forward/linux/?entry='
forward_windows='http://0.0.0.0:3000/forward/windows/?entry='
rm -f *.db
cp templates/*.db .
./bin/centralized-logging-server-linux &
sleep 15s
cat ./data/linux/sample.log | ./bin/centralized-logging-forwarder-linux $forward_linux
cat ./data/windows/sample.csv | ./bin/centralized-logging-forwarder-linux $forward_windows
kill %1
