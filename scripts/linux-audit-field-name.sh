#!/bin/bash
function fieldNames() {
  IFS=","
	for entry in $(cat $1)
  do
    field=$(echo $entry) 
    fieldType=$(echo $field | sed 's/^./\U&\E/' | sed -E 's/-(.)/\U\1/g' | sed -E 's/_(.)/\U\1/g')
    echo "fieldName ($fieldType _) = \"$field\""
  done
}
input_file=$1
IFS=","
names=$(fieldNames $input_file)
echo $names
echo
