




irc_bot_helpmsg(A) :-
	A = [
	"-Izbot-0001 Help:",
	"-----------------------",
	"- ",
	"-     Display this help: /msg Izbot-0001 izbot help",
	"-",
	"-** Kudos System **",
	"-   Give Kudos:",
	"-     /notice Izbot-0001 kudos <number> <nick>",
	"- ",
	"-   Display current Kudos total:",
	"-     /notice Izbot-0001 kudos? <nick>",
	"- ",
	"-** Commands **",
	"-   Say something:",
	"-     /notice Izbot-0001 say <message>",
	"- ",
	"-** Auto-response Triggers **",
	"-     what does <nick> want?",
	
	"- "],
	!.




irc_send_help(To) :-
	write('--printing help--'),nl,
	irc_bot_helpmsg(M),
	irc_send_help(To, M),!.

irc_send_help(To, [H|T]) :-
	irc_privmsg(To, H),
	irc_send_help(To, T),!.
	
irc_send_help(To, [H]) :-
	irc_privmsg(To, H),!.















