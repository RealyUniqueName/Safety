#!/bin/bash

rm safety.zip
zip -r safety.zip src README.md LICENSE haxelib.json > /dev/null
haxelib submit safety.zip