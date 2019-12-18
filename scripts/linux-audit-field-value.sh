#!/bin/bash
function fieldValues() {
  IFS=","
	for entry in $(cat $1)
  do
    field=$(echo $entry) 
    fieldType=$(echo $field | sed 's/^./\U&\E/' | sed -E 's/-(.)/\U\1/g' | sed -E 's/_(.)/\U\1/g')
    echo "fieldValue ($fieldType v) = v"
  done
}
input_file=$1
IFS=","
values=$(fieldValues $input_file)
echo $values
echo
