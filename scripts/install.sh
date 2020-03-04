#!/bin/bash
git clone git@github.com:markfarrell/siem.git
cd siem
npm install -g pkg
npm install
bower install
./build.sh
