#!/bin/bash
cd $HOME/Transporter/MyDocs/75-data/
DT=$(date +"%Y%m%d-%H%M")
tar -czf "$HOME/Transporter/MyDocs/99-archives/$1-$DT.tar.gz" $1
