#!/bin/sh

while true; do
    # 1>&2 redirects stdout to stderr
    date 1>&2
    free -h 1>&2
    nvidia-smi 1>&2
    sleep 10
done
