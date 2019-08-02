#!/bin/bash

ls *.v | grep -v "main.v" | xargs -I {} cat {} >> other_code.v
cat main.v other_code.v >> popgenerator.v

v popgenerator.v

rm other_code.v
rm popgenerator.v
