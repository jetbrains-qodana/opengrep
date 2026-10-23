#!/bin/sh
cd /tmp/tc/structure
python3 /tmp/tc/structure/run.py new >> /tmp/tc/structure/run_new.log 2>&1
python3 /tmp/tc/structure/run.py old >> /tmp/tc/structure/run_old.log 2>&1
