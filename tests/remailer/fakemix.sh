#!/bin/sh

# test-remailer3.el runs this script instead of the mixmaster/mixminion
# binary. Those executables accept '-t recipient' arguments and then get
# their message from stdin. Do something similar here so we can verify that
# the right arguments were provided

echo "fakemix" $*
cat

