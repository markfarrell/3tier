#!/bin/sh
sample_octet() {
	echo "$(shuf -i 0-255 -n 1)"
}
sample_ipv4() {
	A=$(sample_octet)
	B=$(sample_octet)
	C=$(sample_octet)
	D=$(sample_octet)
	echo "$A.$B.$C.$D"
}
sample_port() {
	echo "$(shuf -i 0-65535 -n 1)"
}
sample_protocol() {
	echo "$(sample_octet)"
}
sample_flow_record() {
	SIP=$(sample_ipv4)
	DIP=$(sample_ipv4)
	SPORT=$(sample_port)
	DPORT=$(sample_port)
	PROTOCOL=$(sample_protocol)
	PACKETS="0"
	BYTES="0"
	FLAGS=""
	STIME="1970/01/01T00:00:00.000"
	DURATION="0.000"
	ETIME="1970/01/01T00:00:00.000"
	SENSOR=""
	echo "$SIP,$DIP,$SPORT,$DPORT,$PROTOCOL,$PACKETS,$BYTES,$FLAGS,$STIME,$DURATION,$ETIME,$SENSOR"
}
HOST=$1
N=$2
for i in $(seq $N); do
	FLOW_RECORD=$(sample_flow_record)
	START_TIME=$(date +%s%N | cut -b1-13)
	curl --fail -G -s "http://$HOST/forward/flow" --data-urlencode "q=$FLOW_RECORD"
	RESULT=$?
	END_TIME=$(date +%s%N | cut -b1-13)
	DURATION=$(($END_TIME - $START_TIME))
	echo "$RESULT,$DURATION"
done
