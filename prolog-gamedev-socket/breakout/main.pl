:- use_module(library(sockets)).
:- use_module(library(format)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(pairs)).

gen_state([
    ball([sprite(ball), position(340, 430), speed(5, -5)]),
    paddle([sprite(paddle), position(300, 450)]),
    block([sprite(block), position(10,10)]),
    block([sprite(block), position(75, 10)]),
    block([sprite(block), position(140, 10)]),
    block([sprite(block), position(205, 10)]),
    block([sprite(block), position(270, 10)]),
    block([sprite(block), position(335, 10)]),
    block([sprite(block), position(400, 10)]),
    block([sprite(block), position(465, 10)]),
    block([sprite(block), position(530, 10)])
]).

main :-
    socket_client_open('127.0.0.1':7878, Stream, [type(text)]),
    format(Stream, "init\n", []),
    gen_state(State),
    loop(Stream, State).

loop(Stream, State0) :-
    ask_input(Stream, Input),
    update_paddle(Input, State0, State1),
    update_ball(Input, State1, State),
    phrase_to_stream(render_state(State), Stream),
    loop(Stream, State).

ask_input(Stream, Input) :-
    format(Stream, "input?\n", []),
    read_line_to_chars(Stream, Line, []),
    once(phrase(input_(Input), Line)).

input_([up-Up,down-Down,left-Left,right-Right,delta-Dt]) -->
    "up=",
    bool(Up),
    " down=",
    bool(Down),
    " left=",
    bool(Left),
    " right=",
    bool(Right),
    " delta=",
    number(Delta),
    {
        Dt is Delta / 10000000
    },
    "\n".

bool(true) -->
    "true".
bool(false) -->
    "false".

number(D) -->
    number_(Cs),
    {
        number_chars(D, Cs)
    }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.
digit(D) --> [D], { D = '.'}.

update_ball(Input, State, State3) :-
    select(ball(Options0), State, State1),
    select(position(X0, Y0), Options0, Options1),
    select(speed(SpeedX0, SpeedY0), Options1, Options2),
    member(delta-Dt, Input),
    X1 is X0 + SpeedX0*Dt,
    Y1 is Y0 + SpeedY0*Dt,
    ((X1 < 0; X1 > 618) -> SpeedX1 is SpeedX0 * -1 ; SpeedX1 = SpeedX0),
    ((Y1 < 0; Y1 > 458) -> SpeedY1 is SpeedY0 * -1 ; SpeedY1 = SpeedY0),
    (
        (member(Block, State), collision_block(X1, Y1, Block, Side)) ->
            select(Block, State1, State2),
            (Side = vertical ->
                SpeedX2 is SpeedX1 * -1, SpeedY2 = SpeedY1
            ;   SpeedY2 is SpeedY1 * -1, SpeedX2 = SpeedX1
            )
    ;   SpeedX2 = SpeedX1, SpeedY2 = SpeedY1, State1 = State2
    ),
    Options3 = [position(X1, Y1), speed(SpeedX2, SpeedY2)|Options2],
    State3 = [ball(Options3)|State2].

update_paddle(Input, State, State2) :-
    select(paddle(Options0), State, State1),
    select(position(X0, Y), Options0, Options1),
    member(delta-Dt, Input),
    (member(left-true, Input) -> X1 is X0-2*Dt;X1 = X0),
    (member(right-true, Input) -> X2 is X1+2*Dt;X2 = X1),
    X is min(536, max(0, X2)),
    Options2 = [position(X, Y)|Options1],
    State2 = [paddle(Options2)|State1].

collision_block(X, Y, block(Options), Side) :-
    member(position(BlockX, BlockY), Options),
    DistX is abs(BlockX + 32 - X),
    DistY is abs(BlockY + 16 - Y + 11),
    DistX < 11,
    DistY < 11,
    (   DistX < DistY ->
        Side = vertical
    ;   Side = horizontal
    ).




render_state(Xs) -->
    {
        maplist(sprite_state, Xs, Pairs),
        group_pairs_by_key(Pairs, Groups)
    },
    render_state_group(Groups).

render_state_group([Group|Groups]) -->
    render_group(Group, 0),
    render_state_group(Groups).

render_state_group([]) --> [].

render_group(sprite(Sprite)-[position(PosX, PosY)|PXs], N0) -->
    {
        SetPosX is floor(PosX),
        SetPosY is floor(PosY)
    },
    format_("setpos ~q ~d ~d ~d\n", [Sprite, N0, SetPosX, SetPosY]),
    {
        N is N0 + 1
    },
    render_group(sprite(Sprite)-PXs, N).

render_group(_-[], _) --> [].

sprite_state(X, sprite(Sprite)-position(PosX, PosY)) :-
    arg(1, X, Options),
    member(position(PosX, PosY), Options),
    member(sprite(Sprite), Options).