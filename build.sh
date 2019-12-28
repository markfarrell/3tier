#!/bin/bash
mkdir -p bin
pulp build -m Server --to bin/server.js
pulp build -m Client.InsertWindows --to bin/insert-windows.js
pulp build -m Client.InsertLinux --to bin/insert-linux.js
