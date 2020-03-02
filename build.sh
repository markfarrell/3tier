#!/bin/bash
function attempt () {
  $1 &> /dev/null
  if [[ "$?" == "0" ]]; then
    echo "[SUCCESS] $2"
  else
    echo "[FAILURE] $2"
	fi
}
build_directory='bin'
build_name="server"
echo "Building project to '$build_directory' ..."
attempt "rm -rf $build_directory && mkdir $build_directory" "Initialize '$build_directory' directory"
attempt "pulp build -m SIEM.Logging.Server --to bin/$build_name.js" "Compile $build_name" 
cd $build_directory
attempt "pkg $build_name.js -t node10-linux-x64,node10-macos-x64,node10-windows-x64" "Package binary executable(s) for $build_name"
attempt "rm -f *.js" "Clean up '$build_directory'"
cd ..
echo "Done."
