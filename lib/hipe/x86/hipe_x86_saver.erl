% hipe_x86_saver.erl
% This module writes a erlang atom to disk.
% Intented to write an object file given a binary code.

-module(hipe_x86_saver).
-export([save/3]).

save( Bin, Dir, Mod ) ->
    file:write_file(Dir ++ atom_to_list(Mod) ++ ".hipe.x86",Bin).
