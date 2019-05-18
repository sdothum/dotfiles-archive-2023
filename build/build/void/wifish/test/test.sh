#!/bin/sh
errors=0
errmsgs=""
test_int=Wifi
wscan_lines=23
waiting=" * Scanning For APs ..."
heading="SSID[[:space:]]+SIGNAL[[:space:]]+MAC[[:space:]]+FREQ[[:space:]]+CAPABILITIES"
tests=0
WIFISH_DEFAULT="list"

add_err() {
	err=$1
	errors=$((errors+1))
	errmsgs="$errmsgs
$err"
}

add_test() {
	tests=$((tests+1))
}

add_test
bashisms=$(checkbashisms -p wifish 2>&1)
if [ $? -ne 0 ];then
	add_err "$bashisms"
fi

add_test
failboat=$(WIFISH_DEFAULT=foo ./wifish 2>&1)
if [ $? -eq 0 ];then
	add_err "wifish should fail with WIFISH_DEFAULT as 'foo'"
fi

tfile=$(mktemp /tmp/$$.parsed.XXXXXX)
efile=$(mktemp /tmp/$$.parsed.err.XXXXXX)
trap 'rm -f $tfile' INT TERM EXIT
trap 'rm -f $efile' INT TERM EXIT
WIFISH_DEFAULT=list AWK_LOCATION=./awk _IN_TEST=true ./wifish >$tfile 2>$efile

lcount=$(cat $tfile|wc -l)
add_test
if [ $lcount -lt 2 ];then
	add_err "No Output from wifish! At line 43"
fi

add_test
if [ $lcount -ne $wscan_lines ];then
	add_err "Expected wifish to produce $wscan_lines lines, got $lcount"
fi

waitmsg="$(head -1 $efile)"
add_test
if [ "$waitmsg" != "$waiting" ];then
	add_err "Expected Wait Message to be '${waiting}', got '${waitmsg}'"
fi

interface="$(head -1 $tfile|awk '{print $(NF)}')"
add_test
if [ "$interface" != "'$test_int'" ];then
	add_err "Expected interface to be '$test_int', got '$interface'"
fi

header="$(head -2 $tfile|tail -1)"
echo "$header"|egrep -q "$heading"
add_test
if [ $? -ne 0 ];then
	add_err "Expected headers to match '$heading', got '$header'"
fi

echo "$errors Errors out of $tests Tests"

if [ $errors -gt 0 ];then
	echo "Details: $errmsgs"
fi
rm $tfile
exit $errors

