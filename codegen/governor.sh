#!/bin/bash

if [ $# -eq 0 ]; then
  echo "Use '$0 read' to get the current governor on all CPUs."
  echo "Use '$0 set (ondemand|performance)' to get the current governor on all CPUs."
  exit 1
fi

CMD=$1
shift

# count number of processors
n=$(grep -c "^processor" /proc/cpuinfo)
let n1=$n-1

if [ "$CMD" == "read" ] || [ "$CMD" == "get" ]; then
  for i in `seq -f "%02g" 0 $n1`; do
    gov=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
    echo "CPU $i: $gov"
  done
elif [ "$CMD" == "set" ]; then
  if [ $# -eq 0 ]; then
    echo "Missing governor mode."
    exit 1
  fi
  mode=$1

  # check sudo
  if [[ $EUID > 0 ]]; then
    echo "Please run as root."
    exit 1
  fi

  for i in `seq 0 $n1`; do
    gov=$(cat /sys/devices/system/cpu/cpu$i/cpufreq/scaling_governor)
    echo "CPU $i: was $gov, setting to $mode"
    cpufreq-set -c $i -g $mode
    if [ $? -ne 0 ]; then
      echo "Aborting..."
      exit 1
    fi
  done

  echo "Done.  Set $n processors into $mode mode."
else
  echo "Invalid usage."
  exit 1
fi
