









give_kudos(Nick, Kudos) :-
	kudos(Nick, Old),
	New is Old + Kudos,
	write(['kudos given ', New]), nl,
	retract(kudos(Nick,_)),
	asserta(kudos(Nick, New)),
	!.


give_kudos(Nick, Kudos) :-
	write(['nick added and kudos given ', Kudos]), nl,
	asserta(kudos(Nick, Kudos)),
	!.





/*

category(Category)
trivium(Category, Question, Type, AnswerStruct)	
	
Type should just be 'simple' for now


*/




savedb(Out) :-
	open(Out, write, Fd2, [alias(outfile)]),
	write(' writing database... '),
	(writelines;true),
	write(' done '),nl,nl,

	close(Fd2).
	
savedb_trivium :- trivium(X), write_canonical(outfile, X),write(outfile, '.'), nl(outfile), X = false.







