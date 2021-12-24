:- use_module(library(socket)).
:- set_prolog_flag(double_quotes, chars).

gen_state([ball([sprite(ball), position(0,0)])]).

main :-
    tcp_connect('127.0.0.1':7878, Stream, []),
    format(Stream, "init\n", []),
    gen_state(State),
    loop(Stream, State).

loop(Stream, State0) :-
    ask_input(Stream, Input),
    update_state(Input, State0, State),
    phrase(render_state(State), RenderCs),
    string_chars(RenderStr, RenderCs),
    format(Stream, "~s\n", [RenderStr]),
    loop(Stream, State).

ask_input(Stream, Input) :-
    format(Stream, "input?\n", []),
    flush_output(Stream),
    read_string(Stream, "\n", " \r", _, String),
    string_chars(String, Line),
    once(phrase(input_(Input), Line)).

input_([up-Up,down-Down,left-Left,right-Right]) -->
    "up=",
    bool(Up),
    " down=",
    bool(Down),
    " left=",
    bool(Left),
    " right=",
    bool(Right).

bool(true) -->
    "true".
bool(false) -->
    "false".

update_state(Input, [ball(Options0)|Xs0], [ball(Options2)|Xs1]) :-
    select(position(X0, Y0), Options0, Options1),
    (member(up-true, Input) -> Y1 is Y0 -10;Y1 = Y0),
    (member(down-true, Input) -> Y2 is Y1 + 10;Y2 = Y1),
    (member(left-true, Input) -> X1 is X0-10;X1 = X0),
    (member(right-true, Input) -> X2 is X1+10;X2 = X1),
    Options2 = [position(X2, Y2)|Options1],
    update_state(Xs0, Xs1).

update_state([], []).

render_state([]) --> [].
render_state([X|Xs]) -->
    {
        arg(1, X, Options),
        member(position(PosX, PosY), Options),
        member(sprite(Sprite), Options),
        atom_chars(Sprite, SpriteCs),
        number_chars(PosX, PosXCs),
        number_chars(PosY, PosYCs)
    },
    "setpos ",
    SpriteCs,
    " ",
    PosXCs,
    " ",
    PosYCs,
    "\n",
    render_state(Xs).

read_line_to_chars(Stream, Cs0, Cs) :-
        get_char(Stream, Char),
        write(Char),
        (   Char == [] -> Cs0 = Cs
        ;   Char = [C],
            Cs0 = [C|Rest],
            (   C == '\n' -> Rest = Cs
            ;   read_line_to_chars(Stream, Rest, Cs)
            )
        ).

:- initialization(main).