#!/bin/bash

# NOTICE The ./BHeapTest program generates a random test and checks if the heap works correctly on it.
# Randseed is stored in a file called 'randseed' and has a simple format: 
#   The first line of a file contains an integer N, which is a number of all integers, used as a seed
#   Next N lines contain those integer (integer means an ocaml integer here).
#   When the test is done, new randseed is created and stored in the file.
#   So, ./BHeapTest can be called a huge number of times for testing on a huge number of different tests.

# the varible n is used to set a number of time to run the test here
n=1

nextNumber=0
while [ -r "testingResult$nextNumber" ]; do
    let nextNumber=$nextNumber+1;
done

for ((i=0; $i < $n; i=$i+1)); do
    d="testingResult$nextNumber"
    mkdir "$d"
    if ! "$1" &> "$d"/res; then
	echo "" > "$d/FAILURE"
	exit 1
    fi
    cp randseed "$d"
    let nextNumber=$nextNumber+1
done
