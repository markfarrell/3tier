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
attempt "pulp build -m Client.Forwarder --to bin/centralized-logging-forwarder.js" "Compile centralized logging forwarder client"
cd bin
attempt "pkg centralized-logging-server.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for centralized logging server"
attempt "pkg centralized-logging-forwarder.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for centralized logging forwarder client" 
attempt "rm -f *.js" "Clean up '$output_directory'"
cd ..
echo "Done."
