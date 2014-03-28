BEGIN {print "@Relation Compare_Inequality_" FILENAME; quote="\""}
NR == 1 { 
	aline = gsub(quote,"",$0)
	ni = split($0,line,",") 
	for (i=1;i<= ni;i++) {
		print "@attribute " line[i] " NUMERIC"
	}
	print ""
	print "@data"
	next;
}

{
	aline = gsub(quote,"",$0)
	print $0
	next
	ni = split($0,linex,",")

	for (i=0;i< ni;i++) {
		printf(linex[i] ",")
	}
	print;
}