#!/usr/bin/env bash

./prologTerm $1 | swipl -s Typage/typage2.pl -g main
