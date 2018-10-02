#!/bin/sh

set -eux

CLI=$1

SCRATCH=$(mktemp -d)
EXPORT=$SCRATCH/test
OUTPUT=$SCRATCH/out

echo "**** Create a test file ****"
dd if=/dev/urandom of="$EXPORT" bs=1M count=40

echo "**** Serve it ****"
$CLI serve --exportname test --no-tls "$EXPORT" &
SERVER=$!
echo "**** Wait for the server to start the main loop ****"
sleep 0.1

stop_server() {
  kill -9 $SERVER
}
trap stop_server INT TERM EXIT

echo "**** Check that the server is running ****"
stat /proc/$SERVER

echo "**** Download it as raw from the server ****"
qemu-img -T 'nbd*' convert 'nbd:0.0.0.0:10809:exportname=test' -O raw "$OUTPUT"
echo "**** Check that the two files are the same ****"
cmp --silent "$EXPORT" "$OUTPUT"

echo "**** Download it as qcow2 from the server - qemu will use structured reads if possible ****"
QCOW2=$SCRATCH/qcow2
qemu-img -T 'nbd*' convert 'nbd:0.0.0.0:10809:exportname=test' -O qcow2 "$QCOW2"
echo "**** Convert the qcow2 to raw ****"
rm -f "$OUTPUT"
qemu-img convert "$QCOW2" "$OUTPUT"
echo "**** Check that the two files are the same ****"
cmp --silent "$EXPORT" "$OUTPUT"
