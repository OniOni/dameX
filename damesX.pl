% Jeu de dames avec des trous et des blocs. Absolument.
% Blanc en haut

:- dynamic wh/2.
:- dynamic bl/2.

	wh(1, 0). wh(3, 0). wh(5, 0). wh(7, 0). wh(9, 0).
wh(0, 1). wh(2, 1). wh(4, 1). wh(6, 1). wh(8, 1).
	wh(1, 2). wh(3, 2). wh(5, 2). wh(7, 2). wh(9, 2).
wh(0, 3). wh(2, 3). wh(4, 3). wh(6, 3). wh(8, 3).

	bl(1, 6). bl(3, 6). bl(5, 6). bl(7, 6). bl(9, 6).
bl(0, 7). bl(2, 7). bl(4, 7). bl(6, 7). bl(8, 7).
	bl(1, 8). bl(3, 8). bl(5, 8). bl(7, 8). bl(9, 8).
bl(0, 9). bl(2, 9). bl(4, 9). bl(6, 9). bl(8, 9).

pion(X, Y) :- bl(X, Y).
pion(X, Y) :- wh(X, Y).

iscolor(C, X, Y) :- P =.. [C, X, Y], call(P).

%Recuperer la couleur d'un pion a une coordonnée
get_color(X, Y, C) :-
	bl(X, Y),
	C = bl.
get_color(X, Y, C) :-
	wh(X, Y),
	C = wh.

adv(wh, Res) :- Res = bl.
adv(bl, Res) :- Res = wh.

libre(X, Y) :- X > -1, X < 10, Y > -1, Y < 10, not(pion(X, Y)).
	
% can_eat(fromX, fromY, eaten_X, eaten_Y, dest_X, dest_Y, couleurmangeante)
can_eat(Xm, Ym, X, Y, ResX, ResY, wh) :- 
		Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, bl(X, Y),
		libre(Xp, Yp), ResX = Xp , ResY = Yp.
can_eat(Xp, Ym, X, Y, ResX, ResY, wh) :- 
		Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, bl(X, Y), 
		libre(Xm, Yp), ResX = Xm , ResY = Yp.
can_eat(Xm, Yp, X, Y, ResX, ResY, bl) :- 
		Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, wh(X, Y), 
		libre(Xp, Ym), ResX = Xp , ResY = Ym.
can_eat(Xp, Yp, X, Y, ResX, ResY, bl) :- 
		Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, wh(X, Y), 
		libre(Xm, Ym), ResX = Xm , ResY = Ym.
	
% Faire un pas dans une case libre (sans jumper)
% can_step_from(fromX, fromY, dest_X, dest_Y, _, couleurbougeante)
can_step_from(X, Y, ResX, ResY, bl) :- Xm is X - 1, Ym is Y - 1, libre(Xm, Ym), ResX = Xm , ResY = Ym.
can_step_from(X, Y, ResX, ResY, bl) :- Xp is X + 1, Ym is Y - 1, libre(Xp, Ym), ResX = Xp , ResY = Ym.
can_step_from(X, Y, ResX, ResY, wh) :- Xm is X - 1, Yp is Y + 1, libre(Xm, Yp), ResX = Xm , ResY = Yp.
can_step_from(X, Y, ResX, ResY, wh) :- Xp is X + 1, Yp is Y + 1, libre(Xp, Yp), ResX = Xp , ResY = Yp.

% Syntaxic sugar
% can_jump_from(fromX, fromY, dest_X, dest_Y, couleurbougeante)
can_jump_from(X, Y, ResX, ResY, Eaten, bl) :- Xm is X - 1, Ym is Y - 1, can_eat(X, Y, Xm, Ym, ResX, ResY, bl), Eaten = (Xm, Ym).
can_jump_from(X, Y, ResX, ResY, Eaten, bl) :- Xp is X + 1, Ym is Y - 1, can_eat(X, Y, Xp, Ym, ResX, ResY, bl), Eaten = (Xp, Ym).
can_jump_from(X, Y, ResX, ResY, Eaten, wh) :- Xm is X - 1, Yp is Y + 1, can_eat(X, Y, Xm, Yp, ResX, ResY, wh), Eaten = (Xm, Yp).
can_jump_from(X, Y, ResX, ResY, Eaten, wh) :- Xp is X + 1, Yp is Y + 1, can_eat(X, Y, Xp, Yp, ResX, ResY, wh), Eaten = (Xp, Yp).

% Pouvons-nous bouger si on se place à un endroit ? (Attention : on doit déjà y être)
can_move_from(X, Y, ResX, ResY, C, Eaten, eat) :- can_jump_from(X, Y, ResX, ResY, Eaten, C).
can_move_from(X, Y, ResX, ResY, C, _, step) :- can_step_from(X, Y, ResX, ResY, C).

one_move(X, Y, ResX, ResY, C, Eaten, MoveType) :- iscolor(C, X, Y), can_move_from(X, Y, ResX, ResY, C, Eaten, MoveType).

evaluate_situation(C, Value) :- findall((X, Y), iscolor(C, X, Y), Pieces), length(Pieces, Mine),
			    adv(C, AdvC), findall((X, Y), iscolor(AdvC, X, Y), PiecesA), length(PiecesA, Theirs),
				Value is Mine - Theirs.

% list des pions pouvant effectué une action
next_moves(C, Result) :- findall((X, Y, X2, Y2, Eaten, MoveType), one_move(X, Y, X2, Y2, C, Eaten, MoveType), Result).


try_one(C, MoveData, Res) :-
	(FromX, FromY, ToX, ToY, _, step) = MoveData,
	move(FromX, FromY, ToX, ToY, _, step),
	evaluate_situation(C, Res),
	move(ToX, ToY, FromX, FromY, _, step).
	
try_one(C, MoveData, Res) :-
	(FromX, FromY, ToX, ToY, ToEat, eat) = MoveData,
	move(FromX, FromY, ToX, ToY, ToEat, eat),
	evaluate_situation(C, Res),
	(X, Y) = ToEat,
	adv(C, A),
	NF=..[A, X, Y],
	assert(NF),
	move(ToX, ToY, FromX, FromY, _, step).

% Deplace un pion de from vers to
move_no_check(C, FromX, FromY, ToX, ToY) :-
	pion(FromX, FromY),
	F=..[C, FromX, FromY],
	retract(F),
	NF=..[C, ToX, ToY],
	assert(NF).

move(FromX, FromY, ToX, ToY, _, step) :-
	get_color(FromX, FromY, C),
	move_no_check(C, FromX, FromY, ToX, ToY).
	
move(FromX, FromY, ToX, ToY, ToEat, eat) :-
	get_color(FromX, FromY, C),
	adv(C, A),
	(X, Y) = ToEat,
	F=..[A, X, Y],
	retract(F),
	move_no_check(C, FromX, FromY, ToX, ToY).

first_moves(_, 0) :- true.

first_moves(C, I):-
	next_moves(C, Res), 
	[H | _] = Res, (FromX, FromY, ToX, ToY, ToEat, MoveType) = H, % Extract first move
	move(FromX, FromY, ToX, ToY, ToEat, MoveType),
	adv(C, A),
	I2 is I - 1,
	first_moves(A, I2).
	
get_best([], _, _) :- true. 
	
get_best([H | Reste], OldMax, BestMove) :- 
	(FromX, FromY, _, _, _, _) = H,
	get_color(FromX, FromY, C),
	try_one(C, H, ResLocal),
	(ResLocal > OldMax, NewMax = ResLocal, BestMove = H) ; (OldMax > ResLocal, true),
	get_best(Reste, NewMax, BestMove).
	
choose_best(_, 0) :- true.
	
choose_best(C, I):-
	next_moves(C, Res), 
	get_best(Res, -200, BestMove),
	(FromX, FromY, ToX, ToY, ToEat, MoveType) = BestMove,
	move(FromX, FromY, ToX, ToY, ToEat, MoveType),
	adv(C, A),
	I2 is I - 1,
	choose_best(A, I2).
	
draw :-
	writef('  0 1 2 3 4 5 6 7 8 9\n0'),
	draw_all(0, 0).

draw_one(X, Y) :-
	bl(X, Y),
	writef('|x').

draw_one(X, Y) :-
	wh(X, Y),
	writef('|o').

draw_one(_, _) :-
	writef('|.').


draw_all(9, 9) :-
	draw_one(9, 9),
	writef('|\n').

draw_all(9, Y) :-
	draw_one(9, Y),
	writef('|\n'),
	Yp is Y + 1,
	write(Yp),
	X2 is 0,
	Y2 is Y + 1,
	draw_all(X2, Y2).

draw_all(X, Y) :-
	draw_one(X, Y),
	X2 is X + 1,
	draw_all(X2, Y).

%clear all facts
clear_board :-
	retractall(wh(_,_)),
	retractall(bl(_,_)).

%Pack all game facts into two lists
pack(Whites, Blacks) :-
	findall((X,Y), wh(X, Y), Whites),
	findall((X1,Y1), bl(X1, Y1), Blacks).

unpack_inner([]) :- true.

unpack_inner([H|T]) :-
	(X,Y)=H, 
	F=..[wh, X, Y], 
	assert(F),
	unpack_inner(T).

%Unpack lists into game facts
unpack(Whites, Blacks) :-
	clear_board,
	unpack_inner(Whites),
	unpack_inner(Blacks).
	
game_loop(bl) :-
	write('\nJouons un coup :\n'),
	choose_best(bl, 1),	
	draw,
	game_loop(wh).
	
game_loop(wh) :-
	evaluate_situation(wh, V),
	write('Score : '), write(V),
	write('\nVotre pion :\n'),
	read(NextFrom),
	(Xnf, Ynf) = NextFrom,
	Wh(Xnf, Ynf),
	findall((X2, Y2), one_move(Xnf, Ynf, X2, Y2, wh, _, _), PossibleMovesTo),
	write('\nVous pouvez aller là :\n'),
	write(PossibleMovesTo),
	write('\nOù allez-vous ?\n'),
	read(NextTo),
	(Xnt, Ynt) = NextTo,
	findall((ToEat, MoveType), one_move(Xnf, Ynf, Xnt, Ynt, wh, ToEat, MoveType), PossibleMovesParams),
	[H | _] = PossibleMovesParams,
	(ToEat, MoveType) = H,
	move(Xnf, Ynf, Xnt, Ynt, ToEat, MoveType),
	draw,
	game_loop(bl).
	
start :-
	draw,
	game_loop(wh).

