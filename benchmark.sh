#!/bin/bash

# Run this script to benchmark the NBD server using nbd-client and hdparm.

set -eux

dd if=/dev/zero of=/tmp/test bs=1M count=100
_build/default/cli/main.exe serve --no-tls /tmp/test &
SERVER_PROCESS=$!
echo $SERVER_PROCESS

function stop_server {
  kill $(jobs -p)
}
trap stop_server EXIT


sudo modprobe nbd

sudo nbd-client -N test localhost /dev/nbd0

function stop_client {
  sudo nbd-client -d /dev/nbd0
  stop_server
}
trap stop_client EXIT

sudo hdparm -t /dev/nbd0
sudo hdparm -t /dev/nbd0
sudo hdparm -t /dev/nbd0
sudo hdparm -t /dev/nbd0
sudo hdparm -t /dev/nbd0
