#!/bin/bash

for i in 0 1 2 3 4 5 6 7; do
  cpufreq-set -c $i -g ondemand
done
