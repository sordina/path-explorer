# Path Explorer

Interactive TUI for pruning your path.

![Path Explorer](https://github.com/sordina/path-explorer/blob/master/path-explorer.png?raw=true)

## Functions

* Show path
* Enable / disable path segments
* Reorder path
* Test command against WIP path
* Output PATH once done

## Limitations

Often your path will be constructed by referencing variables, but this works in expanded form.

## TODO

* [ ] Alllow mouse scrolling in output sections
* [ ] Help for keys
* [ ] Allow insertion of new path segements
* [ ] Command history

## Bash Prototype

A simple prototype in Bash to test the concept.

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
