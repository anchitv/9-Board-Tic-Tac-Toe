%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Select the other player.  O <->  X
other(x, o).
other(o, x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  mark(+P,+M,+SubBoard0,-SubBoard1)
%  mark move M for player P on SubBoard0 to produce SubBoard1
%
mark(P, 1, [e|T], [P|T]).
mark(P, 2, [A, e|T], [A, P|T]).
mark(P, 3, [A, B, e|T], [A, B, P|T]).
mark(P, 4, [A, B, C, e|T], [A, B, C, P|T]).
mark(P, 5, [A, B, C, D, e|T], [A, B, C, D, P|T]).
mark(P, 6, [A, B, C, D, E, e|T], [A, B, C, D, E, P|T]).
mark(P, 7, [A, B, C, D, E, F, e|T], [A, B, C, D, E, F, P|T]).
mark(P, 8, [A, B, C, D, E, F, G, e, I], [A, B, C, D, E, F, G, P, I]).
mark(P,9,[A,B,C,D,E,F,G,H,e],[A,B,C,D,E,F,G,H,P]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  play(+P,+L,+M,+Board0,-Board1)
%  mark move M for player P on board L of Board0 to produce Board1
%
play(_P,0,Board,Board).
play(P,1,M,[B|T],[B1|T]) :- mark(P,M,B,B1).
play(P,2,M,[C,B|T],[C,B1|T]) :- mark(P,M,B,B1).
play(P,3,M,[C,D,B|T],[C,D,B1|T]) :- mark(P,M,B,B1).
play(P,4,M,[C,D,E,B|T],[C,D,E,B1|T]) :- mark(P,M,B,B1).
play(P,5,M,[C,D,E,F,B|T],[C,D,E,F,B1|T]) :- mark(P,M,B,B1).
play(P,6,M,[C,D,E,F,G,B|T],[C,D,E,F,G,B1|T]) :- mark(P,M,B,B1).
play(P,7,M,[C,D,E,F,G,H,B|T],[C,D,E,F,G,H,B1|T]) :- mark(P,M,B,B1).
play(P,8,M,[C,D,E,F,G,H,I,B,K],[C,D,E,F,G,H,I,B1,K]) :- mark(P,M,B,B1).
play(P,9,M,[C,D,E,F,G,H,I,J,B],[C,D,E,F,G,H,I,J,B1]) :- mark(P,M,B,B1).

% sub_board(L, Board, SubBoard);
sub_board(0, _C, []).
sub_board(1, [B|_T], B).
sub_board(2, [_C,B|_T], B).
sub_board(3, [_C,_D,B|_T], B).
sub_board(4, [_C,_D,_E,B|_T], B).
sub_board(5, [_C,_D,_E,_F,B|_T], B).
sub_board(6, [_C,_D,_E,_F,_G,B|_T], B).
sub_board(7, [_C,_D,_E,_F,_G,_H,B|_T], B).
sub_board(8, [_C,_D,_E,_F,_G,_H,_I,B,_K], B).
sub_board(9, [_C,_D,_E,_F,_G,_H,_I,_J,B], B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  empty(-M,+SubBoard)
%  check that cell M of SubBoard is empty
%
empty(1,[e|_]).
empty(2,[_,e|_]).
empty(3,[_,_,e|_]).
empty(4,[_,_,_,e|_]).
empty(5,[_,_,_,_,e|_]).
empty(6,[_,_,_,_,_,e|_]).
empty(7,[_,_,_,_,_,_,e|_]).
empty(8,[_,_,_,_,_,_,_,e,_]).
empty(9,[_,_,_,_,_,_,_,_,e]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  legal(+L,-M,+Board)
%  true if cell M of board L is legal
%
legal(1,M,[B|_]) :- empty(M,B).
legal(2,M,[_,B|_]) :- empty(M,B).
legal(3,M,[_,_,B|_]) :- empty(M,B).
legal(4,M,[_,_,_,B|_]) :- empty(M,B).
legal(5,M,[_,_,_,_,B|_]) :- empty(M,B).
legal(6,M,[_,_,_,_,_,B|_]) :- empty(M,B).
legal(7,M,[_,_,_,_,_,_,B|_]) :- empty(M,B).
legal(8,M,[_,_,_,_,_,_,_,B,_]) :- empty(M,B).
legal(9,M,[_,_,_,_,_,_,_,_,B]) :- empty(M,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  subwin(+P,SubBoard)
%  true if player P has achieved 3-in-a-row
%
subwin(P,[P,P,P|_]).
subwin(P,[_,_,_,P,P,P|_]).
subwin(P,[_,_,_,_,_,_,P,P,P]).
subwin(P,[P,_,_,P,_,_,P,_,_]).
subwin(P,[_,P,_,_,P,_,_,P,_]).
subwin(P,[_,_,P,_,_,P,_,_,P]).
subwin(P,[P,_,_,_,P,_,_,_,P]).
subwin(P,[_,_,P,_,P,_,P,_,_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  winning(+P,+L,+Board)
%  true if player P has achieved 3-in-a-row on board L
%
winning(P,1,[B|_]) :- subwin(P,B).
winning(P,2,[_,B|_]) :- subwin(P,B).
winning(P,3,[_,_,B|_]) :- subwin(P,B).
winning(P,4,[_,_,_,B|_]) :- subwin(P,B).
winning(P,5,[_,_,_,_,B|_]) :- subwin(P,B).
winning(P,6,[_,_,_,_,_,B|_]) :- subwin(P,B).
winning(P,7,[_,_,_,_,_,_,B|_]) :- subwin(P,B).
winning(P,8,[_,_,_,_,_,_,_,B,_]) :- subwin(P,B).
winning(P,9,[_,_,_,_,_,_,_,_,B]) :- subwin(P,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  open socket and establish TCP read/write streams
%
connect(Port) :-
   tcp_socket(Socket),
   gethostname(Host),
   tcp_connect(Socket,Host:Port),
   tcp_open_socket(Socket,INs,OUTs),
   assert(connectedReadStream(INs)),
   assert(connectedWriteStream(OUTs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  read next command and execute it
%
ttt :-
   connectedReadStream(IStream),
   read(IStream,Command),
   Command.

init :- ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  start(+P)
%  start a new game for player P
%
start(P) :-
   retractall(board(_ )),
   retractall(player(_ )),
   retractall(prev_move(_ )),
   assert(board(
   [[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],
    [e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],
    [e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e]])),
   assert(player(P)),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  second_move(+K,+L)
%  assume first move is board K, cell L
%  choose second move and write it
%
second_move(K,L) :-
   retract(board(Board0)),
   player(P), other(P,Q),
   play(Q,K,L,Board0,Board1),
   print_board(Board1),
   search(P,L,Board1,M),
   play(P,L,M,Board1,Board2),
   print_board(Board2),
   assert(board(Board2)),
   assert(prev_move(M)),
   write_output(M),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  third_Move(+J,+K,+L)
%  assume first move is board K, cell L,
%           second move is board L, cell M
%  choose third move and write it
%
third_move(J,K,L) :-
   retract(board(Board0)),
   player(P),
   play(P,J,K,Board0,Board1),
   print_board(Board1),
   other(P,Q),
   play(Q,K,L,Board1,Board2),
   print_board(Board2),
   search(P,L,Board2,M),
   play(P,L,M,Board2,Board3),
   print_board(Board3),
   assert(board(Board3)),
   assert(prev_move(M)),
   write_output(M),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  next_move(+L)
%  assume opponent move is L
%  choose (our) next move and write it
%
next_move(L) :-
   retract(prev_move(K)),
   retract(board(Board0)),
   player(P), other(P,Q),
   play(Q,K,L,Board0,Board1),
   print_board(Board1),
   search(P,L,Board1,M),
   play(P,L,M,Board1,Board2),
   print_board(Board2),
   assert(board(Board2)),
   assert(prev_move(M)),
   write_output(M),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if the player has 2 marks on the same row with third slot as empty.
% two_in_a_row(Player, SubBoard).
two_in_a_row(P,[P,P,e|_]).
two_in_a_row(P,[_,_,_,P,P,e|_]).
two_in_a_row(P,[_,_,_,_,_,_,P,P,e]).
two_in_a_row(P,[P,_,_,P,_,_,e,_,_]).
two_in_a_row(P,[_,P,_,_,P,_,_,e,_]).
two_in_a_row(P,[_,_,P,_,_,P,_,_,e]).
two_in_a_row(P,[P,_,_,_,P,_,_,_,e]).
two_in_a_row(P,[_,_,P,_,P,_,e,_,_]).
two_in_a_row(P,[e,P,P|_]).
two_in_a_row(P,[_,_,_,e,P,P|_]).
two_in_a_row(P,[_,_,_,_,_,_,e,P,P]).
two_in_a_row(P,[e,_,_,P,_,_,P,_,_]).
two_in_a_row(P,[_,e,_,_,P,_,_,P,_]).
two_in_a_row(P,[_,_,e,_,_,P,_,_,P]).
two_in_a_row(P,[e,_,_,_,P,_,_,_,P]).
two_in_a_row(P,[_,_,e,_,P,_,P,_,_]).
two_in_a_row(P,[P,e,P|_]).
two_in_a_row(P,[_,_,_,P,e,P|_]).
two_in_a_row(P,[_,_,_,_,_,_,P,e,P]).
two_in_a_row(P,[P,_,_,e,_,_,P,_,_]).
two_in_a_row(P,[_,P,_,_,e,_,_,P,_]).
two_in_a_row(P,[_,_,P,_,_,e,_,_,P]).
two_in_a_row(P,[P,_,_,_,e,_,_,_,P]).
two_in_a_row(P,[_,_,P,_,e,_,P,_,_]).

% Check if the player has 1 mark on the same row with other slots as empty.
% one_in_a_row(Player, SubBoard).
one_in_a_row(P,[P,e,e|_]).
one_in_a_row(P,[_,_,_,P,e,e|_]).
one_in_a_row(P,[_,_,_,_,_,_,P,e,e]).
one_in_a_row(P,[P,_,_,e,_,_,e,_,_]).
one_in_a_row(P,[_,P,_,_,e,_,_,e,_]).
one_in_a_row(P,[_,_,P,_,_,e,_,_,e]).
one_in_a_row(P,[P,_,_,_,e,_,_,_,e]).
one_in_a_row(P,[_,_,P,_,e,_,e,_,_]).
one_in_a_row(P,[e,P,e|_]).
one_in_a_row(P,[_,_,_,e,P,e|_]).
one_in_a_row(P,[_,_,_,_,_,_,e,P,e]).
one_in_a_row(P,[e,_,_,P,_,_,e,_,_]).
one_in_a_row(P,[_,e,_,_,P,_,_,e,_]).
one_in_a_row(P,[_,_,e,_,_,P,_,_,e]).
one_in_a_row(P,[e,_,_,_,P,_,_,_,e]).
one_in_a_row(P,[_,_,e,_,P,_,e,_,_]).
one_in_a_row(P,[e,e,P|_]).
one_in_a_row(P,[_,_,_,e,e,P|_]).
one_in_a_row(P,[_,_,_,_,_,_,e,e,P]).
one_in_a_row(P,[e,_,_,e,_,_,P,_,_]).
one_in_a_row(P,[_,e,_,_,e,_,_,P,_]).
one_in_a_row(P,[_,_,e,_,_,e,_,_,P]).
one_in_a_row(P,[e,_,_,_,e,_,_,_,P]).
one_in_a_row(P,[_,_,e,_,e,_,P,_,_]).
one_in_a_row(P,[e,e,P|_]).


% Calculate the heuristic value for a sub-board.
% Empty Board: -100
% Winning Board: 1000000
% Losing Board: -1000000
% Calculate the No. of Favourble marks in a row - No. of unfavourable marks [10*X2 + X1 - 10*O2 - O1] (Player is X)
% sub_value(_P,[e,e,e,e,e,e,e,e,e],-100).
sub_value(_P,[e,e,e,e,e,e,e,e,e],-100).

sub_value(P,SubBoard,1000000) :-
  subwin(P,SubBoard), !.

sub_value(P,SubBoard,-1000000) :-
  other(P,Q),
  subwin(Q,SubBoard), !.

sub_value(P,SubBoard,Subval) :-
  findall(SubBoard,two_in_a_row(P,SubBoard),Moves),
  findall(SubBoard,one_in_a_row(P,SubBoard),Moves1),
  length(Moves,X2),
  length(Moves1, X1),
  other(P,Q),
  findall(SubBoard,two_in_a_row(Q,SubBoard),Moves2),
  findall(SubBoard,one_in_a_row(Q,SubBoard),Moves3),
  length(Moves2,O2),
  length(Moves3, O1),
  Subval is (10*X2 + X1 - 10*O2 - O1).


% Calculate the Heauristic value for the entire board from the heuristic values of sub-boards.
% value(Player, L, Board, HValue)

% value(P,L,Board,500) :-
%   winning(P,L,Board), !.

% value(P,L,Board,-1000) :-
%   other(P,Q),
%   winning(Q,L,Board), !.

value(P,_L,Board,Val) :-
   other(P,Q),
   sub_board(1, Board, SubBoard1),
   % Heuristic Value for Player winning in sub_board
   sub_value(P, SubBoard1, Subval1),
   % Heuristic Value for Opponent winning in sub_board
   sub_value(Q, SubBoard1, Subval1Q),
   sub_board(2, Board, SubBoard2),
   sub_value(P, SubBoard2, Subval2),
   sub_value(Q, SubBoard2, Subval2Q),
   sub_board(3, Board, SubBoard3),
   sub_value(P, SubBoard3, Subval3),
   sub_value(Q, SubBoard3, Subval3Q),
   sub_board(4, Board, SubBoard4),
   sub_value(P, SubBoard4, Subval4),
   sub_value(Q, SubBoard4, Subval4Q),
   sub_board(5, Board, SubBoard5),
   sub_value(P, SubBoard5, Subval5),
   sub_value(Q, SubBoard5, Subval5Q),
   sub_board(6, Board, SubBoard6),
   sub_value(P, SubBoard6, Subval6),
   sub_value(Q, SubBoard6, Subval6Q),
   sub_board(7, Board, SubBoard7),
   sub_value(P, SubBoard7, Subval7),
   sub_value(Q, SubBoard7, Subval7Q),
   sub_board(8, Board, SubBoard8),
   sub_value(P, SubBoard8, Subval8),
   sub_value(Q, SubBoard8, Subval8Q),
   sub_board(9, Board, SubBoard9),
   sub_value(P, SubBoard9, Subval9),
   sub_value(Q, SubBoard9, Subval9Q),
   Val1 is Subval1+Subval2+Subval3+Subval4+Subval5+Subval6+Subval7+Subval8+Subval9,
   Val2 is Subval1Q+Subval2Q+Subval3Q+Subval4Q+Subval5Q+Subval6Q+Subval7Q+Subval8Q+Subval9Q,
   Val is Val1 - Val2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  search(+P,+L,+Board,-M)
%  choose Move M for player P on board L, by alpha-beta search
%
search(P, L, Board, Move) :-
   alpha_beta(P, 5, L, Board, -1000000, 1000000, Move, _Value).
   % write('BOARD: '), write(L), write('    MOVE: '), write(Move), write('   VALUE:'), write(Value), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  alpha_beta(+P,+D,+Board,+Alpha,+Beta,-Move,-Value)
%  perform alpha-beta search to depth D for player P,
%  assuming P is about to move on Board. Return Value
%  of current Board position, and best Move for P.

% if other player has won, Value is -900000
alpha_beta(P,_D,L,Board,_Alpha,_Beta,0,-900000) :-
   other(P,Q),
   winning(Q,L,Board), !.

% if player has won, Value is 900000
alpha_beta(P,_D,L,Board,_Alpha,_Beta,0,900000) :-
   winning(P,L,Board), !.

% if depth limit exceeded/terminatl nodes: use heuristic estimate
alpha_beta(P,0,L,Board,_Alpha,_Beta,0,Value) :-
   value(P,L,Board,Value), ! .

% Evaluate and choose all legal moves in this position
alpha_beta(P,D,L,Board,Alpha,Beta,Move,Value) :-
   D > 0,
   findall(M,legal(L,M,Board),Moves),
   Moves \= [], !,
   Alpha1 is -Beta,
   Beta1 is -Alpha,
   D1 is D-1,
   eval_choose(P,Moves,L,Board,D1,Alpha1,Beta1,0,Move,Value).

% if no available moves, it must be a draw
alpha_beta(_P, _D, _L, _Board, _Alpha, _Beta, 0, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  eval_choose(+P,+Moves,+L, +Board,+D,+Alpha,+Beta,+BestMove
%              -ChosenMove,-Value)
%  Evaluate list of Moves and determine Value of position
%  as well as ChosenMove for this Board position
% (taking account of current BestMove for this position)

% if no more Moves, BestMove becomes ChosenMove and Value is Alpha
eval_choose(_P,[],_L,_Board,_D,Alpha,_Beta,BestMove,BestMove,Alpha).

% evaluate Moves, find Value of Board Position, and ChosenMove for P
eval_choose(P,[M|Moves],L,Board,D,Alpha,Beta,BestMove,ChosenMove,Value) :-
   play(P,L,M,Board,Board1),
   other(P,Q),
   alpha_beta(Q,D,M,Board1,Alpha,Beta,_Move1,Value1),
   V is -Value1,
   cutoff(P,Moves,L,Board,D,Alpha,Beta,BestMove,M,V,ChosenMove,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  cutoff(+P,+Moves,+L,+Board,+D,+Alpha,+Beta,+BestMove,+M,+V,
%              -ChosenMove,-Value)
%  Compare move M (with value V) to Alpha and Beta,
%  and compute Value and ChosenMove appropriately.

% cut off the search, ChosenMove is M and Value is V
cutoff(_P,_Moves,_L,_Board,_D,_Alpha,Beta,_Move0,M,V,M,V) :-
   V >= Beta.

% Alpha increases to V, BestMove is M, continue search
cutoff(P,Moves,L,Board,D,Alpha,Beta,_BestMove,M,V,ChosenMove,Value) :-
   Alpha < V, V < Beta,
   eval_choose(P,Moves,L,Board,D,V,Beta,M,ChosenMove,Value).

% keep searching, with same Alpha, Beta, BestMove
cutoff(P,Moves,L,Board,D,Alpha,Beta,BestMove,_M,V,ChosenMove,Value) :-
   V =< Alpha,
   eval_choose(P,Moves,L,Board,D,Alpha,Beta,BestMove,ChosenMove,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  write_output(+M)
%  transmit the chosen move (M)
%
write_output(M) :-
   connectedWriteStream(OStream),
   write(OStream,M),
   nl(OStream), flush_output(OStream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  print_board()
%
print_board([A,B,C,D,E,F,G,H,I]) :-
   print3boards(A,B,C),
   write('------+-------+------'),nl,
   print3boards(D,E,F),
   write('------+-------+------'),nl,
   print3boards(G,H,I),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  print_board()
%
print3boards([A1,A2,A3,A4,A5,A6,A7,A8,A9],
             [B1,B2,B3,B4,B5,B6,B7,B8,B9],
             [C1,C2,C3,C4,C5,C6,C7,C8,C9]) :-
   print_line(A1,A2,A3,B1,B2,B3,C1,C2,C3),
   print_line(A4,A5,A6,B4,B5,B6,C4,C5,C6),
   print_line(A7,A8,A9,B7,B8,B9,C7,C8,C9).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  print_line()
%
print_line(A,B,C,D,E,F,G,H,I) :-
   write_wrapper(A),write_wrapper(' '),write_wrapper(B),write_wrapper(' '),write_wrapper(C),write_wrapper(' | '),
   write_wrapper(D),write_wrapper(' '),write_wrapper(E),write_wrapper(' '),write_wrapper(F),write_wrapper(' | '),
   write_wrapper(G),write_wrapper(' '),write_wrapper(H),write_wrapper(' '),write_wrapper(I),nl.

write_wrapper(e):-
   write('.').

write_wrapper(x):-
   write('X').

write_wrapper(o):-
   write('O').

write_wrapper(X):-
   write(X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  last_move(+L)
%
last_move(L) :-
   retract(prev_move(K)),
   retract(board(Board0)),
   player(P), other(P,Q),
   play(Q,K,L,Board0,Board1),
   print_board(Board1),
   ttt.

win(_)  :- write('win'), nl,ttt.
loss(_) :- write('loss'),nl,ttt.
draw(_) :- write('draw'),nl,ttt.

end :- halt.
