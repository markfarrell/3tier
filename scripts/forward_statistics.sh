#!/bin/bash
function attempt () {
  $1 &> /dev/null
  if [[ "$?" == "0" ]]; then
    echo "[SUCCESS] $2"
  else
    echo "[FAILURE] $2"
	fi
}
forward_splunk() {
  curl -k https://192.168.2.20:8088/services/collector/event -H "Authorization: Splunk $1" -d "{ \"event\" : $2}"
}
forward_windows() {
  REPORT_STATISTICS_WINDOWS_TOKEN="246653a6-f9b5-4565-8ee1-bc61b4a32e86"
  WINDOWS=$(curl "http://localhost:3000/report/statistics?q=Windows")
  RESULT="$?"
  if [[ "$RESULT" == "0" ]]; then
    forward_splunk $REPORT_STATISTICS_WINDOWS_TOKEN $WINDOWS
  fi
  return $RESULT
}
forward_linux() {
  REPORT_STATISTICS_LINUX_TOKEN="7c290069-cf61-4ec0-9468-a1df79519a12"
  LINUX=$(curl "http://localhost:3000/report/statistics?q=Linux")
  RESULT="$?"
  if [[ "$RESULT" == "0" ]]; then
    forward_splunk $REPORT_STATISTICS_LINUX_TOKEN $LINUX
  fi
  return $RESULT
}
forward_sensor() {
  REPORT_STATISTICS_SENSOR_TOKEN="ca3bac5e-6ddd-4c08-a44a-516ccfae2c20"
  SENSOR=$(curl "http://localhost:3000/report/statistics?q=Sensor")
  RESULT="$?"
  if [[ "$RESULT" == "0" ]]; then
    forward_splunk $REPORT_STATISTICS_SENSOR_TOKEN $SENSOR
  fi
  return $RESULT
}
forward_audit() {
  REPORT_STATISTICS_AUDIT_TOKEN="5dcb7292-eb01-4419-bbdb-86e9cd5efb18"
  AUDIT=$(curl "http://localhost:3000/report/statistics?q=Audit")
  RESULT="$?"
  if [[ "$RESULT" == "0" ]]; then
    forward_splunk $REPORT_STATISTICS_AUDIT_TOKEN $AUDIT
  fi
  return $RESULT
}
forward_audit_success() {
  REPORT_STATISTICS_AUDIT_SUCCESS_TOKEN="96ae8806-c503-47c2-8b4d-898349682555"
  AUDIT_SUCCESS=$(curl "http://localhost:3000/report/statistics?q=(SELECT%20*%20FROM%20Audit%20WHERE%20EventType%3D%22SUCCESS%22)")
  RESULT="$?"
  if [[ "$RESULT" == "0" ]]; then
    forward_splunk $REPORT_STATISTICS_AUDIT_SUCCESS_TOKEN $AUDIT_SUCCESS
  fi
  return $RESULT
}
forward_audit_failure() {
  REPORT_STATISTICS_AUDIT_FAILURE_TOKEN="ddb812ac-8b0b-48a6-90ba-2b3e9b795db5"
  AUDIT_FAILURE=$(curl "http://localhost:3000/report/statistics?q=(SELECT%20*%20FROM%20Audit%20WHERE%20EventType%3D%22FAILURE%22)")
  RESULT="$?"
  if [[ "$RESULT" == "0" ]]; then
    forward_splunk $REPORT_STATISTICS_AUDIT_FAILURE_TOKEN $AUDIT_FAILURE
  fi
  return $RESULT
}
echo "Reporting statistics for forwarded log entries and audited applicaiton-layer events to Splunk."
attempt forward_windows "Report statistics for forwarded Windows Security Event Log entries to Splunk."
attempt forward_linux "Report statistics for forwarded Linux Auditing System log entries to Splunk."
attempt forward_sensor "Report statistics for forwarded SiLk data to Splunk."
attempt forward_audit "Report statistics for audited application-layer events to Splunk."
attempt forward_audit_success "Report statistics for successful audited application-layer events to Splunk."
attempt forward_audit_failure "Report statistics for failed audited application-layer events to Splunk."
echo "Done."
