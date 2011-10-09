% Jeu de dames avec des trous et des blocs. Absolument.
% Blanc en haut

:- dynamic wh/2.
:- dynamic bl/2.

wh(0, 0). wh(2, 0). wh(4, 0). wh(6, 0). wh(8, 0).
wh(1, 1). wh(3, 1). wh(5, 1). wh(7, 1). wh(9, 1).
wh(0, 2). wh(2, 2). wh(4, 2). wh(6, 2). wh(8, 2).

bl(0, 7). bl(2, 7). bl(4, 7). bl(6, 7). bl(8, 7).
bl(1, 8). bl(3, 8). bl(5, 8). bl(7, 8). bl(9, 8).
bl(0, 9). bl(2, 9). bl(4, 9). bl(6, 9). bl(8, 9).

pion(X, Y) :- bl(X, Y).
pion(X, Y) :- wh(X, Y).

iscolor(C, X, Y) :- P =.. [C, X, Y], call(P).

%Recuperer la couleur d'un pion a une coordonné
get_color(X, Y, C) :-
	bl(X, Y),
	C = bl.
get_color(X, Y, C) :-
	wh(X, Y),
	C = wh.

adv(wh, Res) :- Res = bl.
adv(bl, Res) :- Res = wh.

libre(X, Y) :- X > -1, X < 10, Y > -1, Y < 10, not(pion(X, Y)).
	
% can_eat(fromX, fromY, to_eatX, to_eatY, couleurmangeante)
can_eat(Xm, Ym, X, Y, ResX, ResY, wh) :- Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, bl(X, Y), libre(Xp, Yp), ResX = Xp , ResY = Yp.
can_eat(Xp, Ym, X, Y, ResX, ResY, wh) :- Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, bl(X, Y), libre(Xm, Yp), ResX = Xm , ResY = Yp.
can_eat(Xm, Yp, X, Y, ResX, ResY, bl) :- Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, wh(X, Y), libre(Xp, Ym), ResX = Xp , ResY = Ym.
can_eat(Xp, Yp, X, Y, ResX, ResY, bl) :- Xm is X - 1, Xp is X + 1, Ym is Y - 1, Yp is Y + 1, wh(X, Y), libre(Xm, Ym), ResX = Xm , ResY = Ym.
	
% Faire un pas dans une case libre (sans jumper)
can_step_from(X, Y, ResX, ResY, bl) :- Xm is X - 1, Ym is Y - 1, libre(Xm, Ym), ResX = Xm , ResY = Ym.
can_step_from(X, Y, ResX, ResY, bl) :- Xp is X + 1, Ym is Y - 1, libre(Xp, Ym), ResX = Xp , ResY = Ym.
can_step_from(X, Y, ResX, ResY, wh) :- Xm is X - 1, Yp is Y + 1, libre(Xm, Yp), ResX = Xm , ResY = Yp.
can_step_from(X, Y, ResX, ResY, wh) :- Xp is X + 1, Yp is Y + 1, libre(Xp, Yp), ResX = Xp , ResY = Yp.

% Syntaxic sugar
can_jump_from(X, Y, ResX, ResY, bl) :- Xm is X - 1, Ym is Y - 1, can_eat(X, Y, Xm, Ym, ResX, ResY, bl).
can_jump_from(X, Y, ResX, ResY, bl) :- Xp is X + 1, Ym is Y - 1, can_eat(X, Y, Xp, Ym, ResX, ResY, bl).
can_jump_from(X, Y, ResX, ResY, wh) :- Xm is X - 1, Yp is Y + 1, can_eat(X, Y, Xm, Yp, ResX, ResY, wh).
can_jump_from(X, Y, ResX, ResY, wh) :- Xp is X + 1, Yp is Y + 1, can_eat(X, Y, Xp, Yp, ResX, ResY, wh).

% Pouvons-nous bouger si on se place à un endroit ? (Attention : on doit déjà y être)
can_move_from(X, Y, ResX, ResY, C) :- can_jump_from(X, Y, ResX, ResY, C).
can_move_from(X, Y, ResX, ResY, C) :- can_step_from(X, Y, ResX, ResY, C).

one_move(X, Y, ResX, ResY, C) :- iscolor(C, X, Y), can_move_from(X, Y, ResX, ResY, C).

evaluate_situation(C, Value) :- findall((X, Y), iscolor(C, X, Y), Pieces), length(Pieces, Mine),
			    adv(C, AdvC), findall((X, Y), iscolor(AdvC, X, Y), PiecesA), length(PiecesA, Theirs),
				Value is Mine - Theirs.

% list des pions pouvant effectué une action
next_moves(C) :- findall((X, Y, X2, Y2), one_move(X, Y, X2, Y2, C), Result), write(Result).

% Deplace un pion de from vers to
move_no_check(C, FromX, FromY, ToX, ToY) :-
	pion(FromX, FromY),
	F=..[C, FromX, FromY],
	retract(F),
	NF=..[C, ToX, ToY],
	assert(NF).

move(FromX, FromY, ToX, ToY) :-
	get_color(FromX, FromY, C),
	move_no_check(C, FromX, FromY, ToX, ToY).


draw :-
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
	X2 is 0,
	Y2 is Y + 1,
	draw_all(X2, Y2).

draw_all(X, Y) :-
	draw_one(X, Y),
	X2 is X + 1,
	draw_all(X2, Y).
	


		