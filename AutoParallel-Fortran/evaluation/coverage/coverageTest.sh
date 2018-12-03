#	TEST SCRIPT FOR LES CODEBASE

COMPILER="../../compiler/AutoParallel-Fortran"
CHANGESPY="changes.py"
RESULTDIR="compilerResult"
RESULTFILE="$RESULTDIR/compilerResults.txt"
OUTEXT=".console"

mkdir $RESULTDIR
echo "---" >> "$RESULTFILE"
date >> "$RESULTFILE"
python "$CHANGESPY" "-h" >> "$RESULTFILE"
for f in press.f95
do
	echo -e "Processing: $f"
    OUTFILE="${f%.*}_host.f95"
	$COMPILER "$f" "-main" "main.f95" "-out" "$RESULTDIR/" "-D" "NO_IO" -v > "$RESULTDIR/$OUTFILE$OUTEXT"
	if [ -e "$RESULTDIR/$OUTFILE" ]
		then
		# python "$CHANGESPY" "$f" "$RESULTDIR/$OUTFILE" >> "$RESULTFILE"
		python "$CHANGESPY" "$f" "$RESULTDIR/$OUTFILE" "-csv" >> "$RESULTFILE"
		#echo "---" >> "$RESULTFILE"
	fi
done
echo "---" >> "$RESULTFILE"
