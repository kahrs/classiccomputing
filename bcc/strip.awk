/^\*/	{ print "// " substr($0, 2); }
/^[0-9]/	{ print line; line = ""; print "// " $0; }
/^CHECKBIT/ {}
/^\./	{ line = line "," substr($1,2) "=" $3; }
END	{ print line; }
