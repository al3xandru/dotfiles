#!/bin/bash
TITLE=`echo "$@" | tr ' ' '+'`
curl "http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey=3r2avcbv2fhn6gk2nhtxeke9&page_limit=3&q=$TITLE" | python -mjson.tool
