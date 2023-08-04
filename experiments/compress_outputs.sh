#!/bin/bash

# ------------------------------------------------------------------
# Author: 		João L. Neto (https://github.com/jlnetosci/)
# Created: 		2023-08-03
# Description:	Checks sizes for specific outputs in this project; 
#		Compresses using 7z. If output is larger than 50Mb 
#		compresses in <50Mb parts (because of github).
# Arguments: 	None.
# Notes:	Depends on p7zip-full. Can be edited to run with 
#		specific patterns as arguments.
# ------------------------------------------------------------------

for var in $(ls experiment*/data_frames.obj); do
    if [ $(stat -c %s $var) -gt 50000000 ];
	then
		7z -v47M a ${var//.obj/.7z} $var
	else
		7z a ${var//.obj/.7z} $var
	fi
done
