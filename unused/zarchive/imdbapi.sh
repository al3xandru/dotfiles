#!/bin/bash
TITLE=`echo "$@" | tr ' ' '+'`
curl "http://imdbapi.org/?type=json&plot=full&limit=3&q=$TITLE" | python -mjson.tool
