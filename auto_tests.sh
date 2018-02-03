#!/bin/sh

tests_dir="tests/"
sol_dir="solutions/"
make

found=true
echo "\n **************** Starting Tests *****************"
for test in $(ls $tests_dir)
do
	res=$(./main.native < $tests_dir/$test)
	printf "Testing %-10s : %10s" $test $res
	if [ -e $sol_dir/$test ]
	then
		solution=$(cat $sol_dir/$test)
		if [ $solution = $res ]
		then
			echo "\t\t--Right Answer"
		else
			echo -n "\t\t--Wrong Answer"
			printf " - should be \"%s\"\n" $solution 
		fi
	else
		echo "\t\tERROR : $sol_dir/$test"
	fi
done;
