#!/bin/bash
cd $HOME/Transporter/MyDocs/75-data/
DT=$(date +"%Y%m%d-%H%M")
tar -czf "$HOME/Transporter/MyDocs/99-archives/nosql.mypopescu.com-$DT.tar.gz" nosql.mypopescu.com/
