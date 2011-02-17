%%
%% Credits: Vikas Gorur <vikas@gluster.com>, Anand Avati <avati@gluster.com>
%%    Bugs: Bad green tea
%%

-module(sudoku).
-export([start/0, getpid/2, setpid/2, cell_init/2, get_initial_value/3]).

-define(RANGE_9, [1, 2, 3, 4, 5, 6, 7, 8, 9]).

-define(TEMPLATE, [[7, 0, 0,  0, 9, 0,  0, 0, 3],
                   [0, 0, 5,  8, 0, 2,  6, 0, 0],
                   [0, 8, 0,  3, 0, 1,  0, 9, 0],

                   [0, 5, 0,  7, 0, 4,  0, 1, 0],
                   [3, 0, 0,  0, 0, 0,  0, 0, 4],
                   [0, 4, 0,  5, 0, 9,  0, 8, 0],

                   [0, 2, 0,  9, 0, 8,  0, 5, 0],
                   [0, 0, 9,  6, 0, 7,  4, 0, 0],
                   [5, 0, 0,  0, 2, 0,  0, 0, 8]]).


indices_to_name(I, J) ->
    list_to_atom("p" ++ integer_to_list(I) ++ "_" ++ integer_to_list(J)).

getpid(I, J) when ((I < 1) or (J < 1) or (I > 9) or (J > 9)) -> out_of_bounds;
getpid(I, J) ->
    PName = indices_to_name(I, J),
    whereis(PName).

setpid(I, J) ->
    PName = indices_to_name(I, J),
    register(PName, self()).

get_initial_value(Template, I, J) ->
    lists:nth (J, lists:nth (I, Template)).

block_num(I, J) ->
    (trunc ((I-1) / 3) * 3) + (trunc ((J-1) / 3)).

propagate(Not_list, MyI, MyJ, MyBlockID) ->

    lists:map (fun (I) ->
                       getpid (I, MyJ) ! {Not_list}
               end,
               ?RANGE_9 -- [MyI]),

    lists:map (fun (J) ->
                       getpid (MyI, J) ! {Not_list}
               end,
               ?RANGE_9 -- [MyJ]),

    lists:map (fun (I) ->
                       lists:map (fun (J) ->
                                          BlockNum = block_num (I, J),
                                          if ((BlockNum == MyBlockID)
                                              and
                                              (not ((I == MyI) and (J == MyJ)))) ->
                                                  getpid (I, J) ! {Not_list};
                                             true -> true
                                          end
                                  end,
                                  ?RANGE_9)
               end,
               ?RANGE_9).

next_cell(I, 9) ->
    getpid(I+1, 1);
next_cell(I, J) ->
    getpid(I, J+1).

cell_loop(MyI, MyJ, MyBlockID, MyViable, MyValue) ->
    receive
        {Not_list} ->
            MyNewViable = MyViable -- Not_list,
            case MyNewViable of
                [V] -> %io:format ("propagating ~p to neighbors ", [V]),
                    if (MyValue == unknown) ->
%                            io:format ("[~p ~p] fixed as ~p~n", [MyI, MyJ, V]),
                            propagate ([V], MyI, MyJ, MyBlockID);
                       true -> true
                    end,
                    cell_loop (MyI, MyJ, MyBlockID, MyNewViable, V);

                _ -> cell_loop (MyI, MyJ, MyBlockID, MyNewViable, unknown)
            end;

        print -> io:format ("~p ", [MyValue]),
                 if (MyJ == 9) ->
                         io:format ("~n");
                    true -> true
                 end,
                 if not ((MyI == 9) and (MyJ == 9)) -> next_cell (MyI, MyJ) ! print;
                    true -> true
                 end
    end.

cell_init(I, J) ->
    BlockID = block_num (I, J),
    setpid (I, J),
    cell_loop (I, J, BlockID, ?RANGE_9, unknown).

create_row(I) ->
    CreateCell = fun(J) ->
                         spawn (sudoku, cell_init, [I, J])
                 end,
    lists:map (CreateCell, ?RANGE_9).

init_row(I) ->
    InitCell = fun(J) ->
                       InitialValue = get_initial_value (?TEMPLATE, I, J),
%                       io:format ("~p", [InitialValue]),
                       if
                           (not (InitialValue == 0)) -> getpid(I, J) ! {?RANGE_9 -- [InitialValue]};
                           true -> true
                       end
               end,
    lists:map (InitCell, ?RANGE_9).

create_cells() ->
    lists:map (fun (I) -> create_row (I) end, ?RANGE_9),

%% kwak!
    timer:sleep (108),

    lists:map (fun (I) -> init_row (I) end, ?RANGE_9).

start() ->
    create_cells(),

%%  kwak!
    timer:sleep (108),

    getpid(1, 1) ! print.
