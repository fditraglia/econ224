#!/bin/bash
anspath=$1
studpath="${anspath/.[Rr]md/_stud.Rmd}"
echo "$studpath"
if [ -f "$studpath" ] ; then
    rm "$studpath" ;
fi   
cp "$anspath" "$studpath"
ans_start_count=$(grep -o 'ANS_START' "$studpath" | wc -l)
ans_end_count=$(grep -o 'ANS_END' "$studpath" | wc -l)
echo $ans_start_count
echo $ans_end_count
if [ "$ans_start_count" -ne "$ans_end_count" ]; then
    echo "Check your script again. The number of occurences of ANS_START and ANS_END are not equal"
    exit 100 ;
fi
while grep 'ANS_START' "$studpath" > /dev/null;
do
    startLine=$(grep -n -m 1 'ANS_START' "$studpath" | sed 's/\([0-9]*\).*/\1/')
    stopLine=$(grep -n -m 1 'ANS_END' "$studpath" | sed 's/\([0-9]*\).*/\1/')
    echo "$startLine"
    echo "$stopLine"
    del="$startLine","$stopLine"d
    sed -i.bak -e $del "$studpath"
    add="$startLine"i'\# WRITE/CODE ANSWER HERE'
    echo $add
    sed -e "$add" -i.bak $studpath
    # cp temp.Rmd "$studpath"
done
rm "$studpath".bak
if [ -f temp.Rmd ] ; then
	rm temp.Rmd ;
fi
