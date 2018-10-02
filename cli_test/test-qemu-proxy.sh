#!/bin/sh

set -eux

CLI=$1

SCRATCH=$(mktemp -d)
EXPORT=$SCRATCH/test
SOCKET=$SCRATCH/socket
OUTPUT=$SCRATCH/out
EXPECTED=$SCRATCH/expected

echo "**** Create a sparse qcow2 file ****"
qemu-img create -f qcow2 "$EXPORT" 4M
echo "**** Save it as raw ****"
qemu-img convert "$EXPORT" -O raw "$EXPECTED"

echo "**** Serve it with qemu-nbd ****"
echo "We do not need more than 1 connection at a time, so we set --shared=1"
echo "This is a good way of detecting disconnect issues and connection leaks"
qemu-nbd -T 'nbd*' --shared=1 --verbose --persistent --export-name qemu_node --socket "$SOCKET" "$EXPORT" &
QEMU_SERVER=$!
echo "**** Wait for the server to start ****"
sleep 0.1

stop_qemu_server() {
  kill -9 $QEMU_SERVER
}
trap stop_qemu_server INT TERM EXIT

echo "**** Proxy it ****"
URI="nbd:unix:$SOCKET:exportname=qemu_node"
$CLI proxy --exportname test --no-tls "$URI" &
SERVER=$!
echo "**** Wait for the server to start ****"
sleep 0.1
echo "**** Sanity check: verify that qemu's NBD server is still working ****"
rm -f $SCRATCH/tmp
timeout 2 $CLI download "$URI" 0 512 $SCRATCH/tmp

stop_servers() {
  kill -9 $SERVER
  stop_qemu_server
}
trap stop_servers INT TERM EXIT

echo "**** Check that the servers are running ****"
stat /proc/$QEMU_SERVER
stat /proc/$SERVER

echo "**** Download it as raw from the server ****"
qemu-img -T 'nbd*' convert 'nbd:0.0.0.0:10809:exportname=test' -O raw "$OUTPUT"
echo "**** Check that the two files are the same ****"
cmp "$EXPECTED" "$OUTPUT"

echo "**** Check that the servers are still running ****"
stat /proc/$QEMU_SERVER
stat /proc/$SERVER
echo "**** Sanity check: verify that qemu's NBD server is still working ****"
rm -f $SCRATCH/tmp
timeout 2 $CLI download "$URI" 0 512 $SCRATCH/tmp

echo "**** Download it as qcow2 from the server - qemu will use structured reads if possible ****"
QCOW2=$SCRATCH/qcow2
qemu-img -T 'nbd*' convert 'nbd:0.0.0.0:10809:exportname=test' -O qcow2 "$QCOW2"
echo "**** Convert the qcow2 to raw ****"
rm -f "$OUTPUT"
qemu-img convert "$QCOW2" "$OUTPUT"
echo "**** Check that the two files are the same ****"
cmp --silent "$EXPECTED" "$OUTPUT"
echo "**** Check that the qcow2 is sparse ****"
[ "$(stat -c%s "$QCOW2")" -lt 1000000 ]
