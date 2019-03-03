#!/usr/bin/env bash

sh main.sh | tail -n 6 | csvlook --no-inference -d ';'
