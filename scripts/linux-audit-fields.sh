#!/bin/bash
function fields_in_lines() {
	for entry in $(cat $1)
	do
		IFS=" "
		for column in $entry
		do 
			IFS=" "
			param=($(echo $column | tr "=" " "))
			echo "${param[0]} TEXT NOT NULL"
		done
	done
	IFS=""
}
input_file=$1
fields=$(fields_in_lines $input_file | sort | uniq)
result=$(echo $fields)
echo $(echo $result | tr " " ",")
