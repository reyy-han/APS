#!/usr/bin/env bash

for file in Samples/*
	do
		echo $file
		./exeprog.sh $file
	done
