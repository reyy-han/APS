#!/usr/bin/env bash

for file in Samples/*
	do
		echo $file
		./typrog.sh $file
	done
