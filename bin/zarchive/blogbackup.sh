#!/bin/bash
cd $HOME/Documents/MyDocs/75-data/
DT=$(date +"%Y%m%d-%H%M")
tar -czf "$HOME/Documents/MyDocs/99-archives/$1-$DT.tar.gz" $1
