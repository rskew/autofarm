#!/bin/sh
#
# To Do
# ~~~~~
# - Loop through list of known USB Serial device "/dev/tty*" paths ...
#   - Count number of devices and if there is one device, then use it
#   - Otherwise, display index / devices allowing user to select one

MPF_SCRIPT=$1

mpfshell --reset -o ${AMPY_PORT:4} -s $MPF_SCRIPT
