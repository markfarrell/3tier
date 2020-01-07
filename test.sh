#!/bin/bash
rm -f *.db
cp templates/*.db .
./bin/siem-logging-server-linux &
sleep 15s
cat ./data/linux/sample.log | ./bin/siem-logging-forwarder-linux 0.0.0.0:3000 linux
cat ./data/sensor/sample.csv | ./bin/siem-logging-forwarder-linux 0.0.0.0:3000 sensor
cat ./data/windows/sample.csv | ./bin/siem-logging-forwarder-linux 0.0.0.0:3000 windows
kill %1
