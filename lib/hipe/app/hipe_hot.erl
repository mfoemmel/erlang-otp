%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		LOCATE THE HOT FUNCTIONS OF THE SYSTEM
%
% Our basic idea of application-scale optimization is this:
% * first, profile the application to find the frequently called functions;
%   these form the system core
% * second, native compile the core (w. profiling enabled) and run the app
%   again
% * third, reoptimize the system core using the profile info
%
% This module handles the first phase: 

-module(hipe_hot).
-export([app/0,app_stat/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Returns [{ModName,[MFA]}], a list of the 'hot modules' with their
% 'hot functions'. The functions _not_ in this set are regarded as
% not worth compiling into JAM code.

app() ->
    app(default_cut_off()).

default_cut_off() -> 0.1.

app(N) ->
    Funs = loaded_functions(),
    Call_freqs = number_of_calls_list(Funs),
    app_core(Call_freqs,N).

app_stat() ->
    Funs = loaded_functions(),
    Call_freqs = number_of_calls_list(Funs),
    part_per_module(Call_freqs).

number_of_calls_list([]) -> [];
number_of_calls_list([MFA|MFAs]) ->
    number_of_calls(MFA,number_of_calls_list(MFAs)).

app_quartiles() ->
    Funs = loaded_functions(),
    Call_freqs = number_of_calls_list(Funs),
    {_,N} = max_calls(Call_freqs),
    bin_list( [ {K,MFA} || {MFA,K} <- Call_freqs ],
	     empty_bins(quartile_bins(N))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Collect the set of loaded modules in the system.

loaded_functions() ->
    lists:append( [ ordsets:from_list(M:module_info()) 
		   || M <- active_modules() ] ).

active_modules() ->
    [ Mod || {Mod,Path} <- code:all_loaded() ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Find the system core functions. Then partition them into 'per-module'
% lists.

app_core(Funs,N) ->
    Hot_Funs = core_functions(Funs,N),
    part_per_module(Hot_Funs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The tricky part here is that we must decide what
% 'frequently executed' means. Then, we must filter out those that aren't
% 'frequent enough'.
% 
% - I don't think there is an absolute value
%   that can be used:
% - if there is such a value, a long-running system will have to compile
%   more and more functions as even infrequent functions pass that limit.
% 
% *** UNFINISHED ***
% - stubbed

core_functions([],_) -> [];
core_functions(Funs,K) ->
    {MFA,N} = max_calls(Funs),
    CutOff = cutoff(K,N),         % must have at least 10% of calls?
    Bins = core_ins_list(Funs,empty_bins(bins([0,CutOff,N+1000]))),
    bin_value(N,Bins).

core_ins_list([],Bins) -> Bins;
core_ins_list([{MFA,N}|Xs],Bins) ->
    core_ins_list(Xs,bin({N,MFA},Bins)).

cutoff(K,N) -> round(N*K).

%%%%%%%%%%%%%%%%%%%%
%
% Return the MFA that is most called, and the value of that call.

max_calls([{MFA,N}|Xs]) ->
    maxc(Xs,MFA,N).

maxc([],MFA,N) -> {MFA,N};
maxc([{NewMFA,NewN}|Xs],MFA,N) ->
    if
	NewN < N ->
	    maxc(Xs,MFA,N);
	true ->
	    maxc(Xs,NewMFA,NewN)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Count the number of items that map to a given value or a given bucket

empty_histogram() ->
    hash:empty().

histogram(N,Hist) ->
    New = case hash:lookup(N,Hist) of
	      not_found ->
		  1;
	      {found,K} ->
		  K+1
	  end,
    hash:update(N,New,Hist).

histogram_list([],H) -> H;
histogram_list([X|Xs],H) ->
    histogram_list(Xs,histogram(X,H)).

list_histogram(Hist) ->
    hash:list(Hist).

% A bucket histogram collects the data into a number of buckets.
% A bucket is {Lo,Hi}; for X, if Lo =< X < Hi, then X goes into that bucket.
%  The number of buckets is fixed at creation time.

empty_bucket_histogram(Buckets) ->
    {enum(Buckets,1),hipe_vectors:init(length(Buckets),0)}.

enum([],N) -> [];
enum([B|Bs],N) ->
    [{B,N}|enum(Bs,N+1)].

bucket_histogram(Key,{Buckets,Hist}) ->
    B = bucket_ix(Key,Buckets),
    N = hipe_vectors:get(Hist,B),
    {Buckets,hipe_vectors:set(Hist,B,N+1)}.

bucket_histogram_list([],B) -> B;
bucket_histogram_list([X|Xs],B) ->
    bucket_histogram_list(Xs,bucket_histogram(X,B)).

% if there is no bucket, crash

bucket_ix(X,[{{Lo,Hi},Ix}|_]) when Lo =< X, X < Hi -> Ix;
bucket_ix(X,[_|Bs]) -> bucket_ix(X,Bs).

list_bucket_histogram({Buckets,Hist}) ->
    [ {Bucket,hipe_vectors:get(Hist,Ix)} || {Bucket,Ix} <- Buckets ].

bucket_value(Key,{Buckets,Hist}) ->
    B = bucket_ix(Key,Buckets),
    hipe_vectors:get(Hist,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Bins are used just as bucket histograms, except they put the Value
% in the bin of the Key.

empty_bins(Bins) ->
    {enum(Bins,1),hipe_vectors:init(length(Bins),[])}.

bin({Key,Value},{Bins,Hist}) ->
    B = bin_ix(Key,Bins),
    Lst = hipe_vectors:get(Hist,B),
    {Bins,hipe_vectors:set(Hist,B,[Value|Lst])}.

bin_list([],B) -> B;
bin_list([X|Xs],B) ->
    bin_list(Xs,bin(X,B)).

% if there is no bin, crash

bin_ix(X,[{{Lo,Hi},Ix}|_]) when Lo =< X, X < Hi -> Ix;
bin_ix(X,[_|Bs]) -> bin_ix(X,Bs).

list_bins({Bins,Hist}) ->
    [ {Bin,hipe_vectors:get(Hist,Ix)} || {Bin,Ix} <- Bins ].

bins(Xs) -> buckets(Xs).

bin_value(Key,{Bins,Hist}) ->
    B = bin_ix(Key,Bins),
    hipe_vectors:get(Hist,B).
    
quartile_bins(N) ->
    bins([0,round(0.25*N),round(0.5*N),round(0.75*N),N+100]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buckets([X|Xs]) ->
    buckets(X,Xs).

buckets(X,[]) -> [];
buckets(X,[Y|Ys]) ->
    [{X,Y}|buckets(Y,Ys)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

part_per_module(Funs) ->
    hash:list(ppm(Funs,hash:empty())).

ppm([],Tab) -> Tab;
ppm([MFA_N|MFAs],Tab) ->
  MFA = 
    case MFA_N of
      {MFA0,N} -> MFA0;
      MFA0 -> MFA0
    end,
  New = case hash:lookup(module_of(MFA),Tab) of
	  not_found ->
	    [MFA_N];
	  {found,Old} ->
	    [MFA_N|Old]
	end,
  NewTab = hash:update(module_of(MFA),New,Tab),
  ppm(MFAs,NewTab).

module_of({M,_,_}) -> M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

number_of_calls(MFA,Rest) ->
    case catch hipe_bifs:call_count_get(MFA) of
	{'EXIT',_} ->
	    Rest;
	0 ->
	    Rest;
	N when is_integer(N) ->
	    [{MFA,N}|Rest]
    end.
