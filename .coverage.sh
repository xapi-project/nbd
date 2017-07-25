#!/bin/sh

set -ex

COVERAGE_DIR=.coverage
rm -rf $COVERAGE_DIR
mkdir -p $COVERAGE_DIR
pushd $COVERAGE_DIR
if [ -z "$KEEP" ]; then trap "popd; rm -rf $COVERAGE_DIR" EXIT; fi

$(which cp) -r ../* .

export COVERAGE=1
jbuilder runtest

mkdir _outs
$(find . | grep bisect.*.out | xargs mv -t _outs)
bisect-ppx-report -I _outs -text report _outs/bisect*.out
bisect-ppx-report -I _outs -summary-only -text summary _outs/bisect*.out
(cd _outs; bisect-ppx-report bisect*.out -html ../report-html)

if [ -n "$TRAVIS" ]; then
  echo "\$TRAVIS set; running ocveralls and sending to coveralls.io..."
  ocveralls --prefix _outs bisect*.out --send
else
  echo "\$TRAVIS not set; displaying results of bisect-report..."
  cat report
  cat summary
fi
