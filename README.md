# Path Explorer

Interactive TUI for pruning your path.

## Bash Prototype


```bash
#!/bin/bash

if [[ ! "$1" ]]
then
	echo "Usage: path-explorer-2 COMMAND [-v]" 1>&2
	exit 1
fi

if [[ "-v" = "$2" ]]
then
	$1
fi


echo "$PATH" | tr ":" "\n" | while read -r segment
do
	if [[ "-v" = "$2" ]]
	then
		PATH="$segment" $1  && echo "OK $segment" || echo "FAIL $segment"
	else
		PATH="$segment" $1  2>/dev/null 1>/dev/null  && echo "OK $segment" || echo "FAIL $segment"
	fi
done
```
