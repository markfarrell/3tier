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
attempt "rm -rf $output_directory && mkdir $output_directory" "Initialize '$output_directory' directory"
attempt "pulp build -m Server --to bin/centralized-logging-server.js" "Compile centralized logging server" 
attempt "pulp build -m Client.ForwardWindows --to bin/windows-forwarder.js" "Compile Windows Security Event Logs forwarder"
attempt "pulp build -m Client.ForwardLinux --to bin/linux-forwarder.js" "Compile Linux Auditing System forwarder"
cd bin
attempt "pkg centralized-logging-server.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for centralized logging server"
attempt "pkg linux-forwarder.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for Windows Security Events Logs forwarder" 
attempt "pkg windows-forwarder.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for Linux Auditing System forwarder"
attempt "rm -f *.js" "Clean up '$output_directory'"
cd ..
echo "Done."
