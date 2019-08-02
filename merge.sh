#!/bin/bash

rm -f other_code.v
rm -f popgenerator.v

ls *.v | grep -v "main.v" | xargs -I {} cat {} >> other_code.v
cat main.v other_code.v >> popgenerator.v
