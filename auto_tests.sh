#!/bin/sh

make

echo "\n **************** Starting Tests *****************"
for test in $(ls tests/)
do
	echo "Testing $test :" `./main.native < tests/$test`
done;
