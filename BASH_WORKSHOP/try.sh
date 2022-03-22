#!/bin/bash

welcome='Hello! $0 I am symbols'     #considers $0 as symbols
welcome2="Hello! $0 I am a variable"    #considers $0 as a variable, thi sis called interpreted string
echo welcome

echo $welcome
echo $welcome2
echo $0

testvalue="The ip address of this machine is: `ifconfig | grep 255.255.255.255`"
echo $testvalue
