#!/bin/bash
function fieldTypes() {
  IFS=","
	for entry in $(cat $1)
  do
    field=$(echo $entry) 
    fieldType=$(echo $field | sed 's/^./\U&\E/' | sed -E 's/-(.)/\U\1/g' | sed -E 's/_(.)/\U\1/g')
    echo "  show ($fieldType v) = \"($fieldType \" <> show v <> \")\""
  done
}
input_file=$1
IFS=","
types=$(fieldTypes $input_file)
echo $types
echo
