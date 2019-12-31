#!/bin/bash
function attempt () {
  $1 &> /dev/null
  if [[ "$?" == "0" ]]; then
    echo "[SUCCESS] $2"
  else
    echo "[FAILURE] $2"
	fi
}
output_directory='bin'
echo "Building project to '$output_directory' ..."
attempt "rm -rf $output_directory && mkdir -p $output_directory" "Initialize '$output_directory' directory"
attempt "pulp build -m SIEM.Logging.Server --to bin/siem-logging-server.js" "Compile SIEM logging server" 
attempt "pulp build -m SIEM.Logging.LineForwarder --to bin/siem-logging-line-forwarder.js" "Compile SIEM logging line-forwarder"
cd $output_directory
attempt "pkg siem-logging-server.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for SIEM logging server"
attempt "pkg siem-logging-line-forwarder.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for SIEM logging line-forwarder" 
attempt "rm -f *.js" "Clean up '$output_directory'"
cd ..
echo "Done."
