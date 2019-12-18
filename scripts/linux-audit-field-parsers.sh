#!/bin/bash
function field_parsers() {
  IFS=","
	for entry in $(cat $1)
  do
    field=$(echo $entry) 
    fieldType=$(echo $field | sed 's/^./\U&\E/' | sed -E 's/-(.)/\U\1/g' | sed -E 's/_(.)/\U\1/g')
    echo "parse$fieldType :: Parser String Field"
    echo "parse$fieldType = do"
    echo "  _ <- string \"$field\""
    echo "  _ <- string \"=\""
    echo "  v <- parseValue"
    echo "  pure ($fieldType v)"
    echo
  done
  IFS=""
}
input_file=$1
IFS=","
parsers=$(field_parsers $input_file)
echo $parsers
echo
