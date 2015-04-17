


:- dynamic  irc_input1/4.



?- consult('bot.pro').
?- consult('bothelp.pro').
% ?- consult('dbase.pro').
% ?- include('7cardstud.pro').



reload_config :-
	write('++ reloading config ++'), nl,
	reconsult('izbot.pro'),
	!.


read_sock(Str, []) :- at_end_of_stream(Str),!.
read_sock(Str, [Byte|Bytelist]) :-
	get_byte(Str, Byte),
	read_sock(Str, Bytelist).


read_line(Str, []) :- at_end_of_stream(Str),!.
read_line(Str, [C|Cl]) :-
	get_byte(Str, C),
	!,
	C > 13,
	read_line(Str, Cl).
	

irc_recv(Line) :-
	sock(R, _),  
	read_line_to_codes(R, Line),
	!,Line \= end_of_file. 
	/* read_line(R, Line). */


putstring([]) :- nl.
putstring([A | Rest]) :- 
	atom(A),
	tab(1),write(A),
	putstring(Rest).
	
putstring([A | Rest]) :- 
	not(atom(A)),
	put(A), putstring(Rest).


	

writestr([]) :- nl.
writestr([A | Rest]) :- write(A), putstring(Rest).

putstring(Stream, []) :- nl(Stream).
putstring(Stream, [A | Rest]) :- 
	atom(A),
	tab(Stream, 1), write(Stream, A),
	putstring(Stream, Rest).

putstring(Stream, [A | Rest]) :- 
	not(atom(A)),
	put(Stream, A), putstring(Stream, Rest).


putstringRaw(Stream, []) :- nl(Stream).
putstringRaw(Stream, [A | Rest]) :- 
	atom(A),
	write(Stream, A),
	putstringRaw(Stream, Rest).

putstringRaw(Stream, [A | Rest]) :- 
	not(atom(A)),
	put(Stream, A), putstringRaw(Stream, Rest).




irc_send(Msg) :-
	sock(_, W),
	putstring(W, Msg),
	flush_output(W). 

irc_sendRaw(Msg) :-
	sock(_, W),
	putstringRaw(W, Msg),
	flush_output(W). 
	
irc_nick(Nick) :-
	append("NICK ", Nick, S),
	irc_send(S).

	
irc_user(Nick, Mode, RealName) :-
	append("USER ", Nick, S),
	append(S, " ", T),
	append(T, Mode, U),
	append(U, " * ", V),
	append(V, RealName, W),
	irc_send(W).
	
	
irc_join(Chan) :-
	append("JOIN ", Chan, S),
	irc_send(S).

irc_quit :-
	irc_send("QUIT").
	
	
irc_pong(Seq) :-
	atom_codes(Seq, SS),
	append("PONG ", SS, X),
	irc_send(X).

 
irc_privmsg(To, Msg) :-
	atom_codes(To, T),
	append("PRIVMSG ", T, X),
	append(X, " ", Y),
	flatten(Msg, M),
	append(Y, M, Z),
	irc_send(Z), !.
	
irc_privmsgRaw(To, Msg) :-
	atom_codes(To, T),
	append("PRIVMSG ", T, X),
	append(X, " ", Y),
	flatten(Msg, M),
	write(M),nl,
	append(Y, M, Z),
	write(Z),nl,
	irc_sendRaw(Z), !.
	
irc_notice(To, Msg) :-
	atom_codes(To, T),
	append("NOTICE ", T, X),
	append(X, " ", Y),
	flatten(Msg, M),
	append(Y, M, Z),
	irc_send(Z), !.
	
irc_voice(User,Channel) :-
		atom_codes(User, U),
		atom_codes(Channel, C),
		append("MODE ", C, W),
		append(W, " +v ", X),
		append(X, U, Y),
		irc_send(Y).

irc_mute(User,Channel) :-
		atom_codes(User, U),
		atom_codes(Channel, C),
		append("MODE ", C, W),
		append(W, " -v ", X),
		append(X, U, Y),
		irc_send(Y).

irc_kick(User,Channel,Reason) :-
		atom_codes(User, U),
		atom_codes(Channel, C),
		atom_codes(Reason, R),
		append("KICK ", C, W),
		append(W, " ", X),
		append(X, U, Y),
		append(Y, " ", Z),
		append(Z, R, A),
		irc_send(A).


irc_connect(Server, Port) :-
	putstring("connecting...\n"),
	
	tcp_socket(Socket),
	tcp_connect(Socket, Server:Port),
	tcp_open_socket(Socket, Rs, Ws),
	
	asserta(sock(Rs, Ws)),
	asserta(sock(Socket)),
	asserta(connected(true)),
	putstring("connected\n")
	.

irc_disconnect :-
	sock(R, W),
	sock(S),
	retract(sock(R,W)),
	close(R), close(W),
	tcp_close_socket(S),
	putstring("disconnected\n"),!
	.

	
	
	
	
	
/* this is the main rule that starts the bot. */
irc_bot :-
	write(starting),nl,
	asserta(app_running(true)),
	assertz(kudos(_, 0)),
	irc_connect('irc.foonetic.net', 6667),
	irc_nick("Izbot-0001"),
	irc_user("Izbot-0001", "0", "Izzy"),
/*	write(sleeping),nl,
	sleep(2),
	write(awake),nl,*/
	
	irc_readloop,
		
	
	irc_quit,
	irc_disconnect.
	

	
	
	
prefix(P,L) :- append(P,_,L).	
suffix(S,L) :- append(_,S,L).
sublist(SubL,L) :- suffix(S,L), prefix(SubL,S).
first(List,Elem) :- nth0(0,List,Elem).


% ignore jobot
irc_input(['Jobot', _, _], _, _, _).

% log everything
irc_input([From, _,_], 'NOTICE', ['Izbot-0001'], ['remind', To | Msg]) :- !, 
	concat_atom(Msg, M),
	create_reminder(To, From, M),
	irc_privmsg(From, 'sure thing'),
	write('--reminder-- : '), write(To), nl,
	!.

irc_input(_, 'NOTICE', ['Izbot-0001'], ['kudos?', Nick|_]) :- !,
	atom_codes(Nick, K),
	write(' debug 1 '),
	kudos(Nick, Num),
	write(Num),nl,
	atom_codes(Num, N),
	append(K, " currently has ", A),
	append(A, N, B),
	append(B, " Kudos.", C),
	irc_privmsg('#ministryofsillywalks', C),
	write('--notice-- kudos? : '), write(Nick), nl,
	!.

irc_input(_, 'NOTICE', ['Izbot-0001'], ['kudos', Num, Nick | Reason]) :-
	Num =< 42,
	Num > 0,
	atom_codes(Num, N),
	atom_codes(Nick, K),
	give_kudos(Nick, Num),
	append(K, " was given ", A),
	append(A, N, B),
	append(B, " Kudos.", C),
	irc_privmsg('#ministryofsillywalks', C), 
	write('--notice-- kudos given'), nl,
	!.
	
irc_input(_, 'NOTICE', ['Izbot-0001'], ['kudos', Num, Nick | Reason]) :-
	atom_codes(Num, N),
	atom_codes(Nick, K),
	append(N, " is a rediculous number of Kudos to give ", A),
	append(A, K, B),
	append(B, ".", C),
	irc_privmsg('#ministryofsillywalks', C),
	write('--notice-- kudos give fail'), nl,
	!.


irc_input([From,_,_], 'PRIVMSG', _, ['izbot', 'help'|_]) :-
	irc_send_help(From), !.
	


	
irc_input(['Izzy'|_], 'PRIVMSG', _, ['izbot','kick', Nick, 'because' |Reason]) :-
	write('kick'),
	atomic_list_concat(Reason, ' ', X),
	irc_kick(Nick, '#ministryofsillywalks', X), !.

/*
irc_input(['Mal'|_], 'PRIVMSG', _, ['izbot','kick', Nick, 'because' |Reason]) :-
	write('kick'),
	atomic_list_concat(Reason, ' ', X),
	irc_kick(Nick, '#ministryofsillywalks', X), !.
*/

	
irc_input([Nick|_], 'PRIVMSG', _, Text) :- fail,
	atom_codes(X, "-_-"),
	member(X, Text),
	write('-- -- kick --'),
	irc_kick(Nick, '#ministryofsillywalks', 'That is a lame smiley. Do not use it.'), !.


	
irc_input1([Nick|_], 'PRIVMSG', _, ['postfix'|Text]) :-
	write('-- postfix --'),nl,
	postfilter(Text, Codes),
	write(Codes),nl,!,
	asserta(postfix_lastnick(Nick)),
	
	postfixer(Codes, R1, []),!,
	write(['after postfixer', R1]),nl,
	
	number(R1),
	atom_number(Result, R1),
	write(['result: ',Result]),nl,
	irc_privmsgRaw('#ministryofsillywalks', [Nick, ', ', Result]), 
	write('done'),nl,!.



re_postfilter_num([N|R]) --> 're_09_.-'(N), re_postfilter_num(R). 
re_postfilter_num([N]) --> 're_09_.-'(N).


're_09_.-'(X) --> [X],{char_type(X, digit);X=46;X=45}.
/*
're_azAZ_-.' --> [X],{char_type(X, csym);X=46;X=45}.
're_azAZ_-' --> [X],{char_type(X, csym);X=45}. 

*/

postfilter(['+'|T], ['+'|U]) :- postfilter(T, U).
postfilter(['-'|T], ['-'|U]) :- postfilter(T, U).
postfilter(['*'|T], ['*'|U]) :- postfilter(T, U).
postfilter(['/'|T], ['/'|U]) :- postfilter(T, U).
postfilter(['^'|T], ['^'|U]) :- postfilter(T, U).
postfilter(['mod'|T], ['mod'|U]) :- postfilter(T, U).
postfilter(['ln'|T], ['ln'|U]) :- postfilter(T, U).
postfilter(['e'|T], [2.718281828|U]) :- postfilter(T, U).
postfilter(['pi'|T], [3.141592653589793238462|U]) :- postfilter(T, U).
postfilter([H|T], [O|U]) :-   atom_codes(H, C), re_postfilter_num(X, C, Rest),!, Rest = [], number_codes(O, X), postfilter(T, U). % number(H), term_to_atom(O, H), postfilter(T, U).
postfilter([W|T], U) :- write('bad postfix'),nl,irc_privmsg('#ministryofsillywalks', ['wtf is ', W]), postfilter(T, U).
postfilter([], []).
	
	
postfixer(['+'|T], R, [A, B|S]) :-
	write('+'),
	R1 is A + B,
	postfixer(T, R, [R1|S]).


postfixer(['*'|T], R, [A, B|S]) :-
	write('*'),
	R1 is A * B,
	postfixer(T, R, [R1|S]).
	
postfixer(['/'|T], R, [A, B|S]) :-
	write('/'),
	A \= 0, B \= 0,
	R1 is A / B,
	postfixer(T, R, [R1|S]).
	
postfixer(['/'|T], R, [A, B|S]) :-
	write('-00/-kick-'),nl, 
	postfix_lastnick(Nick),write(Nick),
    irc_privmsg('#ministryofsillywalks', ['postfix = fuck you']), 
	irc_kick(Nick, '#ministryofsillywalks', 'Nice try; you fail').
	
postfixer(['-'|T], R, [A, B|S]) :-
	write('-'),
	R1 is A - B,
	postfixer(T, R, [R1|S]). 
	
postfixer(['^'|T], R, [A, B|S]) :-
	write('^'),
	R1 is A ** B,
	postfixer(T, R, [R1|S]). 
	
postfixer(['mod'|T], R, [A, B|S]) :-
	write('mod'),
	R1 is B mod A,
	postfixer(T, R, [R1|S]). 
	
postfixer(['ln'|T], R, [A|S]) :-
	write('ln'),
	R1 is log(A),
	postfixer(T, R, [R1|S]). 
	
postfixer([N|T], R, S) :-
	write('#'),
	postfixer(T, R, [N|S]).

postfixer([], R, [R]) :- write('here'),nl.
/* postfixer([], X, Y) :- write(['postfix -- non-empty stack error', X, Y]). */



	
	
irc_input(['Izzy'|_], 'PRIVMSG', [From], ['mute', Name|_]) :-
	write('-- mute -- '), write(Name),nl,
	irc_mute(Name,From), !.
	
irc_input(['Izzy',_,_], 'PRIVMSG', [From], ['unmute', Name|_]) :-
	irc_voice(Name,From), !.
	

irc_input1(_, 'PING', _, [D]) :- 
	write('--pong-- '), write(D), putstring("\n"),
	irc_pong(D), !.

irc_input(_, 'PRIVMSG', [From], Msg) :-
	/* write('--privmsg-- '), write(From), nl, */
	member('Izbot...?', Msg),
	irc_privmsg(From, "hello back to you. ( Jobot is a tosser. )"), !.
	

	
	
	
irc_input1(_, 'NOTICE', [To], Msg) :-
	To = 'Izbot-0001',
	prefix(['reload', 'config'], Msg),
	reload_config,!, fail.
	
irc_input1(['Izzy',_,_], 'NOTICE', [To], Msg) :-
	To = 'Izbot-0001',
	prefix(['go', 'away','password'], Msg),
	irc_privmsg('#ministryofsillywalks', ['goodbye, ','everyone.']),
	irc_quit,
	retract(app_running(true)),!, fail.
	
irc_input1(['Izzy',_,_], _, _, Msg) :-
	prefix(['izbot', 'get', 'lost'], Msg);
	prefix(['get', 'lost', 'izbot'], Msg),
	irc_privmsg('#ministryofsillywalks', ['goodbye, ','everyone.']),
	irc_quit,
	retract(app_running(true)),!, fail.
	
irc_input1(_, 'NOTICE' , [To], Msg) :-
	To = 'Izbot-0001',
	prefix(['say'], Msg),
	Msg = [_|X],
	write('--notice-- say '), nl,
	irc_privmsg('#ministryofsillywalks', X),
	!,fail.

/*:staticfree.foonetic.net 001 Izbot-0001 :Welcome to the Foonetic IRC Network Izbot-0001!Izbot-0001@hostname.com*/
	
/* :daemonic.foonetic.net 353 Izbot-0001 = #ministryofsillywalks :Izbot-0001 &mo +Krisx &Liberum_Vir &Izzy */
/*
irc_input('353', [Me, _, Channel, _ |Names) :-
	map_list(add_nick(), 
	write('-- added nicks: '), putstring([Me|Names]),nl,
	!.
	
add_nick(Nick) :-
	nicklist
	*/
	

irc_input(_, 1, _, _) :- 	
	write(' -- joi'),
	irc_join("#ministryofsillywalks"),
	write('ned -1-'),!.

irc_input(_, '001', _, _) :- 
	write(' -- joi'),
	irc_join("#ministryofsillywalks"),
	write('ned -2-'),!.



irc_input(_, _, _, _) :- /*
	write('unknown message recieved: '),
	writestr(MSG),
	tab(2),
	writestr(Details),putstring("\n"), */
	true,!.
irc_input1(_, _, _, _) :- /*
	write('unknown message recieved: '),
	writestr(MSG),
	tab(2),
	writestr(Details),putstring("\n"), */
	true,!.


/*
0x02 02 bold
0x03 03 color
0x1f 31 underline
*/




irc_proc([]) :- write('#0'),nl,!.

irc_proc(Prefix,[Msg|Details]) :-
	/* write('#1:'),tab(2), write(Msg),tab(1),write(Details),putstring("\n"),  */
	irc_input(Prefix,Msg,Details),!.
	



/* sleep then backtrack if there is no input. obsolete. */
idle([]) :- sleep(0.1), fail.
idle([_|_]).





irc_readiter(_, end_of_file) :- write('++idling++'),nl,sleep(0.01), !.
irc_readiter(Prefix, Line) :- 
		wordlist(AtomList, Line, []),!,
		irc_proc(Prefix, AtomList).

/* irc_readiter(_,_) :- write('++idling++'),nl,sleep(0.01), !. */
		
irc_readloop :-
	repeat,
		/* irc_rl2, */
		irc_newloop,
		not(app_running(true)),
		!.
	
	
irc_rl2 :-
	/* sleep(0.01), */
	irc_recv(RawLine),!,
	/* idle(Line),*/

	putstring(RawLine),nl,!,
	irc_msg_prefix(Prefix, RawLine, Line),!,
	/* irc_msg_parse(Prefix, Cmd, Opts, Rawline, Rest */ 
	atomify(Prefix, P),
	
	irc_readiter(P, Line),!.
	
	
irc_filter1(Prefix, Cmd, Opts, Line) :-
	irc_input1(Prefix, Cmd, Opts, Line),!.

irc_filter2(Prefix, Cmd, Opts, Line) :-
	/* write(Prefix),nl,
	write(Cmd), write(' - '), write(Opts),nl, write(Line),nl,nl, */
	irc_input(Prefix, Cmd, Opts, Line),!.

irc_filter3(Prefix, Cmd, Opts, Line) :-
	true.



irc_newloop :-
	
	irc_recv(RawLine),!,
	putstring(RawLine),!,
	/* irc_msg_prefix(P, RawLine, L),!, */

	irc_msg_parse(P, Cmd, Opts, RawLine, Rest),!,  
	atomify(P, Prefix),
	
	wordlist(AtomList, Rest, []),!,
	!,irc_filter1(Prefix, Cmd, Opts, AtomList), 

	lvl2wordlist(L2List, Rest, _), 
	!,irc_filter2(Prefix, Cmd, Opts, L2List), 

	
	fail,
	!,irc_filter3(Prefix, Cmd, Opts, _), 
	!.  









atomify([H|T], [A|B]) :- name(A,H), atomify(T,B).
atomify([L|[]],[A|[]]) :- name(A,L).





wordlistT([X|Y]) --> charlist(X), whitespace, wordlistT(Y).
wordlistT([X]) --> whitespace, wordlistT(X).
wordlistT([X]) --> charlist(X).
wordlistT([X]) --> charlist(X), whitespace.

wordlist([X|Y]) --> word(X), whitespace, wordlist(Y).
wordlist([X]) --> whitespace, wordlist(X).
wordlist([X]) --> word(X).
wordlist([X]) --> word(X), whitespace.
wordlist([]) --> whitespace.
wordlist([]) --> re_nothing.


word(W) --> charlist(X), {atom_codes(W,X)}.

charlist([X|Y]) --> chr(X), charlist(Y).
charlist([X]) --> chr(X).

chr(X) --> [X],{X>=33, X\=58}.

whitespace --> whsp, whitespace.
whitespace --> whsp.

whsp --> [X], {X<33}.





irc_msg_parse(Prefix, Cmd, Opts) --> irc_msg_prefix(Prefix), re_command(Cmd), wordlist([Opts]), 're_:'.
irc_msg_parse(Prefix, Cmd, []) --> irc_msg_prefix(Prefix), re_command(Cmd), 're_:'.
irc_msg_parse(Prefix, Cmd, []) --> irc_msg_prefix(Prefix), re_command(Cmd).
irc_msg_parse(Prefix, [], []) --> irc_msg_prefix(Prefix), re_nothing.
irc_msg_parse([[],[],[]], [], []) --> re_nothing.


re_command(C) --> re_nick(CC), {atom_codes(C,CC)}.


lvl2wordlist([X|Y]) --> lvl2word(X), re_punct, lvl2wordlist(Y).
lvl2wordlist([X]) --> re_punct, lvl2wordlist(X).
lvl2wordlist([X]) --> lvl2word(X).
lvl2wordlist([X]) --> lvl2word(X), re_punct.
lvl2wordlist([]) --> re_punct.
lvl2wordlist([]) --> re_nothing.



lvl2word(W) --> lvl2charlist(X), {name(W,X)}.

lvl2charlist([X|Y]) --> re_lvl2char(X), lvl2charlist(Y).
lvl2charlist([X]) --> re_lvl2char(X).

re_lvl2char(X) --> [X],{char_type(X, csym);X=45}.



re_punct --> re_punct_, re_punct.
re_punct --> re_punct_.
re_punct_ --> [X],{X=<44}.
re_punct_ --> [X],{X>=123}.
re_punct_ --> [X],{X=44;X=47;X=60}.
re_punct_ --> [X],{X>57,X<55}.
re_punct_ --> [X],{X>90,X<95}.




/*
DCG used for parsing apart the packet prefixes magically. 

:naos.foonetic.net 001 Izbot-0001 :Welcome to the Foonetic IRC Network Izbot-0001!Izbot-0001@hostname.com
*/

irc_msg_prefix([Nick,User,Host]) --> 're_:', re_nick(Nick), 're_!', re_user(User), 're_@', re_host(Host),re_whsp.
irc_msg_prefix([Nick,User,[]]) --> re_colon, re_nick(Nick), re_expoint, re_user(User), re_whsp.
irc_msg_prefix([Nick,[],[]]) --> re_colon, re_nick(Nick), re_whsp.
irc_msg_prefix([[],[],[]]) --> re_nothing.


re_nickchars(X) --> [X],{char_type(X, csym);X=45;X=46}.
re_userchars(X) --> [X],{char_type(X, csym);X=45;X=126}.
re_hostchars(X) --> [X],{char_type(X, csym);X=45;X=46}.

re_nick([N|R]) --> re_nickchars(N), re_nick(R). 
re_nick([N]) --> re_nickchars(N).

re_user([N|R]) --> re_userchars(N), re_user(R). 
re_user([N]) --> re_userchars(N).

re_host([N|R]) --> re_hostchars(N), re_host(R). 
re_host([N]) --> re_hostchars(N).




re_colon --> [X], {X = 58}.
re_expoint --> [X], {X = 33}.
re_atsign --> [X], {X = 64}.
re_whsp --> [X], {X<33}.
re_nothing --> [].


're_azAZ_.' --> [X],{char_type(X, csym);X=46}.
're_azAZ_-.' --> [X],{char_type(X, csym);X=46;X=45}.
're_azAZ_-' --> [X],{char_type(X, csym);X=45}.

're_:' --> [X],{X=58}.
're_!' --> [X],{X=33}.
're_@' --> [X],{X=64}.
're_+' --> [X],{X=43}.
're_%' --> [X],{X=37}.
're_&' --> [X],{X=38}.




/* * * * * * * * * * * * * * * * * */


/* filter chain of OR's   irc_rawinput ; irc_input ; irc_englishinput */

/* :daemonic.foonetic.net 353 Izbot-0001 = #ministryofsillywalks :Izbot-0001 &mo +Krisx &Liberum_Vir &Izzy */

irc_powernick([Power, Nick]) --> 're_&', re_nick(N), re_nothing, {Power=admin, name(Nick,N)}.
irc_powernick([Power, Nick]) --> 're_@', re_nick(N), re_nothing, {Power=op, name(Nick,N)}.
irc_powernick([Power, Nick]) --> 're_%', re_nick(N), re_nothing, {Power=halfop, name(Nick,N)}.
irc_powernick([Power, Nick]) --> 're_+', re_nick(N), re_nothing, {Power=voice, name(Nick,N)}.
irc_powernick([Power, Nick]) --> 're_:', irc_powernick([Power, Nick]).
irc_powernick([Power, Nick]) --> re_nick(N), re_nothing, {Power=user, name(Nick,N)}.




irc_adduser(Chan, User, Power) :-
	asserta(s_userlist(Chan, User, Power)),!.
irc_remuser(Chan, User) :-
	retractall(s_userlist(Chan, User, _)),!.

irc_userlist([H|T], Chan) :-
	is_list(H),
	irc_powernick([P,N], H, _),!,
	irc_adduser(Chan, N, P),
	irc_userlist(T, Chan).
	
irc_userlist([T], Chan) :-
	is_list(T),
	irc_powernick([P,N], T, _),!,
	irc_adduser(Chan, N, P).

irc_userlist([], _).	

	
/* irc_msg_text([Text]) :- re_colon, re_wordlist. */



re_anythingbut --> [X], {X\=45;X\=95}.
re_anythingbut --> [].

re_anylist --> re_anythingbut, re_anylist.
re_anylist --> re_anythingbut.



're_-_-' --> re_anylist, 're_-_list', 're__-'.
're__-' --> 're___list', 're_-_list', re_anylist.

're___list' --> 're__', 're___list'. 
're___list' --> 're__'. 

're_-_list' --> 're_-', 're_-_list'. 
're_-_list' --> 're_-'. 





're__' --> [X],{X=95}.
're_-' --> [X],{X=45}.





















