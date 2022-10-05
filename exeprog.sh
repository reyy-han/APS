#!/usr/bin/env bash

./prologTerm $1 | swipl -s Semantique/semantique2.pl -g main
