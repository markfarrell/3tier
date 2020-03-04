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
	FLAGS="FSPA"
	STIME="2020/01/01T00:00:00.000"
	DURATION="0.000"
	ETIME="2020/01/01T00:00:00.000"
	SENSOR="local"
	echo "$SIP,$DIP,$SPORT,$DPORT,$PROTOCOL,$PACKETS,$BYTES,$FLAGS,$STIME,$DURATION,$ETIME,$SENSOR"
}
N=$1
for i in $(seq $N); do
	FLOW_RECORD=$(sample_flow_record)
	echo $FLOW_RECORD
done
