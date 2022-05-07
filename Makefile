miniL: miniL.lex miniL.y
	bison -d -v miniL.y
	flex miniL.lex
	gcc lex.yy.c miniL.tab.c -lfl -o miniL


clean:
	rm -f miniL miniL.tab.* miniL.output *~ lex.yy.c