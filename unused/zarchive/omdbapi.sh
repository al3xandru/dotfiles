#!/bin/bash
TITLE=`echo "$@" | tr ' ' '+'`
curl "http://www.omdbapi.com/?r=json&plot=full&t=$TITLE" | python -mjson.tool
