%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%                                                                        

-module(cover_web).
-author('marting@erix.ericsson.se').
-behaviour(gen_server).

%%Export of configuration function
-export([configData/0]).
%% External exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start_link/0,start/0,stop/0]).
-export([calls/2,coverage/2,source_file/2,compile/2,clear/2]).
-export([default_selection/2,default_result/2]).


-include_lib("kernel/include/file.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, webcover_server},cover_web, [], []).

start()->
    gen_server:start({local,webcover_server},cover_web,[],[]).
    
stop()->
    gen_server:call(webcover_server,stop,1000).


compile(Env,Input)->
    gen_server:call(webcover_server,{compile,Env,Input}).

clear(Env,Input)-> 
    gen_server:call(webcover_server,{clear,Env,Input}).

calls(Env,Input)->
    gen_server:call(webcover_server,{calls,Env,Input}).

coverage(Env,Input)->
    gen_server:call(webcover_server,{coverage,Env,Input}).

source_file(Env,Input)->
    gen_server:call(webcover_server,{source,Env,Input}).


default_selection(Env,Input)->
    gen_server:call(webcover_server,{work,Env,Input}).

default_result(Env,Input)->
    gen_server:call(webcover_server,{def_result,Env,Input}).

			    

configData()->
    {webcover,[{web_data,{"WebCover","/webcover"}},
               {alias,{"/webcover",code:priv_dir(tools)}},
	       {alias,{erl_alias,"/webcover/erl",[cover_web]}},
	       {start,{child,{{local,webcover_server},
			      {cover_web,start_link,[]},
			      permanent,100,worker,[cover_web]}}}
	      ]}.


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, From, State) ->
    {stop,ordered, State};

handle_call({compile,Env,Input},From,State)->
    {reply,compile_page(Env,Input),State};

handle_call({clear,Env,Input},From,State)->
    {reply,clear_page(Env,Input),State};

handle_call({calls,Env,Input},From,State)->
    {reply,call_page(Env,Input),State};

handle_call({coverage,Env,Input},From,State) ->
    {reply,coverage_page(Env,Input),State};

handle_call({source,Env,Input},From,State) ->
    {reply,source_page(Env,Input),State};

handle_call({work,Env,Input},From,State)->
    {reply,default_selection_page(Env,Input),State};

handle_call({def_result,Env,Input},From,State)->
    {reply,default_result_page(Env,Input),State};




handle_call(Request, From, State) ->
    Reply = bad_request,
    {reply, Reply, State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%The functions that creates the whole pages by collecting all the    %%
%%neccessary data for each page. This functions is the public interface%
%5                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Returns the page to the left frame                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
default_selection_page(Env,Input)->
    [header(),html_header(head),"<BODY id=\"def_body\" BGCOLOR=\"#FFFFFF\">",selection_body(Env),html_end()].

%%----------------------------------------------------------------------
%% Creates the page where the user can cover compile modules
%%----------------------------------------------------------------------

compile_page(Env,Input)->
    [header(),html_header(head),compile_body(Env,Input),html_end()].

%%----------------------------------------------------------------------
%%The beginning of the page that clear the cover information on a cover
%%compiled module
%%----------------------------------------------------------------------
clear_page(Env,Input)->
    cover_clear(Input),
    [header(),html_header(tot),selection_body(Env),html_end()].

default_result_page(Env,Input)->
    [header(),html_header(tot),"Default result",html_end()].

call_page(Env,Input)->
    [header(),html_header(tot),result_script("calls"),call_result(Input),html_end()].

coverage_page(Env,Input)->
    [header(),html_header(tot),result_script("coverage"),coverage_result(Input),html_end()].

source_page(Env,Input)->
    [header(),html_header(tot),source_file_body(Input),html_end()].
                           



%----------------------------------------------------------------------
% The functions that build the body of the page that shows the 
% Selctions page
%----------------------------------------------------------------------
selection_body(Env)->
    case browser(Env) of
	netscape ->
	    menu_script() ++ work_selection() ++ module_list(netscape) ++ menu();
	ie ->
	    menu_script(ie) ++ work_selection() ++ module_list(ie) ++ menu(ie)
    end.

browser(Env)->
    case lists:keysearch(http_user_agent,1,Env) of
	{value,{_,AgentString}}->
	    case mod_browser:getBrowser(AgentString) of
		{{msie,_},_}->
		    ie;
		{{mozilla,_},_}->
		    netscape;
		{{netscape,_},_} ->
		    netscape;
		_ ->
		    ie
	    end;
	_ ->
	    ie
    end.


compile_script()->
    "<SCRIPT>
         function submitCompile()
         {
            var File,Options; 
            File=document.compile_form.path1.value;
            Options=document.compile_form.options.value;
            document.compile_form.reset();
            document.compile_form.path.value=File;
            document.compile_form.options.value=Options;
            document.compile_form.submit();
         }
    </SCRIPT>".

menu_script(ie)->
    "<SCRIPT>
        var module;
        function showMenu(item)
        {
          module=item.id;
          menu_layer.style.top=item.offsetTop+12;
          menu_layer.style.left=30;
          menu_layer.style.visibility=\"visible\";
        }
        function hideMenu()
        { 
          X=event.x;
          Y=event.y;
          MinX=menu_layer.style.left;
          MaxX=menu_layer.style.left+menu_layer.style.width;
          MinY=menu_layer.style.top;
          MaxY=menu_layer.style.top+menu_layer.style.height;
          if(Y<=MinY||Y>=MaxY)
            menu_layer.style.visibility=\"hidden\";
         if(X<=MinX||X>=MaxX)
           menu_layer.style.visibility=\"hidden\";
           //alert(\"X: \"+X+\" Y:\"+Y + \"MinX:\"+MinX+\" MaxX:\"+MaxX+\" MinY:\"+MinY +\" MaxY:\"+MaxY)                 
        }
        function clearStat()
        {
          window.location=\"./clear?module=\" + module ;
        }
        function showCalls()
        {
         parent.frames.result.location = \"./calls?module=\" + module;
        }
        function showCoverage()
        {
         parent.frames.result.location = \"./coverage?module=\" + module;
        }
        function showFile()
        {
         parent.frames.result.location = \"./source_file?module=\" + module;
        }
     </SCRIPT>".


menu_script()->
    "<SCRIPT>
        var module;
        function showMenu(item)
        {
          module=item.name
          window.document.menu_layer.left=item.left+10;
          window.document.menu_layer.top=item.top+10;
          window.document.menu_layer.visibility=\"visible\";
        }
        function hideMenu(e)
        {
           window.document.menu_layer.visibility=\"hidden\";                    
           false;
        }

        function clearStat(e)
        {
          window.location=\"./clear?module=\" + module ;
        }
        function showCalls(e)
        {
         parent.frames.result.location = \"./calls?module=\" + module;
         return false;       
        }
        function showCoverage(e)
        {
         parent.frames.result.location = \"./coverage?module=\" + module;
        }
        function showFile(e)
        {
         parent.frames.result.location = \"./source_file?module=\" + module;
        }
     </SCRIPT>".

work_selection()->
     "<BODY id=Body BGCOLOR=\"#FFFFFF\"> 
       <BR>
       <FONT SIZE=4>Task:</FONT> 
            <A STYLE=\"color=black \" HREF=\"./compile\" TARGET=\"result\">
             Compile</A>".

module_list(Browser)->
    "<BR><BR>
     <FONT SIZE=4>Compiled modules</FONT>
     <BR>
       <UL>" ++
	  create_module_list(cover:modules(),Browser) ++
       "</UL>".

create_module_list([],_)->
    [];
create_module_list([Mod|Rest],netscape) ->
   " <LI> <LAYER NAME=\"" ++ atom_to_list(Mod) ++ "\"  onMouseOver=\"showMenu(this)\" >" ++ 
                 atom_to_list(Mod) ++ "</LAYER></LI>\n" ++
	create_module_list(Rest,netscape);

create_module_list([Mod|Rest],ie) ->
   " <LI ID=\"" ++ atom_to_list(Mod) ++ "\"  onMouseOver=\"showMenu(this)\" >" 
         ++ atom_to_list(Mod) ++ "</LI>\n" ++
	create_module_list(Rest,ie).


menu()->
    "<LAYER NAME=\"menu_layer\" ID=\"men_lay\" LEFT=500 TOP=500 VISIBILITY=\"hidden\" 
      BGCOLOR=\"#8899AA\" HEIGHT=120 WIDTH=150 onMouseOut=\"hideMenu()\" >
      <TABLE  WIDTH=\"100%\" HEIGHT=\"100%\">
      <TR><TD BGCOLOR=\"#AABBCC\" NAME=\"row1\" >
 	<A HREF=\"./default_selection\" NAME=\"call\" onClick=\"showCalls();return false;\"> Show Calls</A>
    </TD></TR>
    <TR><TD BGCOLOR=\"#AABBCC\"  NAME=\"row2\">     
    <A HREF=\"./default_selection\" NAME=\"coverage\" onClick=\"showCoverage();return false;\"> Show Coverage</A>
    </TD></TR> 
    <TR><TD BGCOLOR=\"#AABBCC\"  NAME=\"row3\">
     <A HREF=\"./default_selection\" NAME=\"source\" onClick=\"showFile();return false;\"> Source File</A>
    </TD></TR> 
    <TR><TD BGCOLOR=\"#AABBCC\"  NAME=\"row4\"> 
         <A HREF=\"./default_selection\" NAME=\"clear\" onClick=\"clearStat();return false;\"> Clear stat</A>
    </TD></TR> 
 </TABLE>
 </LAYER>".  

menu(ie)->
    Style="\"visibility:hidden; left:500; top:500; background-color:#8899AA; height:120; width:150; position:absolute\"",
    "<DIV ID=\"menu_layer\" NAME=\"men_lay\" STYLE=" ++ Style ++  "onMouseOut=\"hideMenu()\" >
  <TABLE WIDTH=\"100%\" HEIGHT=\"100%\">
    <TR><TD BGCOLOR=\"#AABBCC\" NAME=\"row1\">
	<A HREF=\"./default_selection\" onMouseDown=\"showCalls()\"> Show Calls</A>
   </TD></TR>
   <TR><TD BGCOLOR=\"#AABBCC\"  NAME=\"row2\" >
        <A HREF=\"./default_selection\" onMouseDown=\"showCoverage()\"> Show Coverage</A>
    </TD></TR> 
    <TR><TD BGCOLOR=\"#AABBCC\"  NAME=\"row3\">
         <A HREF=\"./default_selection\" onMouseDown=\"showFile()\"> Source File</A>
    </TD></TR> 
    <TR><TD BGCOLOR=\"#AABBCC\"  NAME=\"row4\"> 
         <A HREF=\"./default_selection\" onMouseDown=\"clearStat()\"> Clear stat</A>
    </TD></TR> 
 </TABLE> 
</DIV>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that is used when the user wants to compile something %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_body(Env,Input)->
    case control_input_data(Input,compile) of
	{true,File_or_dir}->
	    compile_javascripts(compile_result(File_or_dir,Input)) ++ compile_script()++ compile_form(file) ++ 
	       directory_list(Input);
	false->
	    compile_script()++compile_form(noFile)++directory_list(Input)
    end.
directory_list(Input)->
    case get_dir(Input)of
	{ok,Dir}->
	     format_dir_list(Dir);
	{error,Not_dir}->
	    Not_dir ++ "Is not a directory or has no read acces to it"
    end.

format_dir_list(Dir)->
    case file:list_dir(Dir) of
	{ok,Files}->
	    "<BR><TABLE BORDER=1> 
            <TR><TD COLSPAN=2 BGCOLOR=\"#8899AA\">
	    <FONT SIZE=4>Erlang Files in " ++ Dir ++ "</FONT></TD></TR>" ++ 
             list_erlang_files(Files)++ "</TABLE>";
	_ ->
	    []
    end.

list_erlang_files(File_list)->
    %%Sort out the erlang files
    Erlang_files=lists:filter(fun(File)->
				      case string:rstr(File,".erl") of
					  0->
					      false;
					  _->
					      string:rstr(File,".erl")+ 3 == string:len(File)
				      end
			      end,File_list),
    %%Transform them to table elements
    {TabData,Acc}=lists:mapfoldl(fun(Erl_file,NewRow)->		     
					 case NewRow of
					     2->
						 {"<TD>"++Erl_file++"</TD></TR>",1};
					     1 ->
						 {"<TR><TD>"++Erl_file++"</TD>",2}
					 end
				 end,1,Erlang_files),
    %%Control that the table is closed  
    case Acc of
       1 ->
	    lists:flatten(TabData);
	_ ->
	    (lists:flatten(TabData))++"<TD>&nbsp;</TD></TR>" 
    end.




get_dir(Input)->
     case control_input_data(Input,dir) of
	{ok,Dir}->
	    case control_input_data(Input,action) of
		{ok,change}->
		    file:set_cwd(Dir),
		    {ok,Dir};
		_->
		    {ok,Dir}
	    end;
	 {error,nodir} ->
	     get_current_dir();
	 {error,Not_dir} ->
	     {error,Not_dir}
     end.

get_current_dir()->
    case file:get_cwd() of
	{ok,Dir}->
	    {ok,Dir};
	_ ->
	    {error,"."}
    end.

compile_result(File_or_dir,Input)->
    case cover_compile(File_or_dir,Input) of 
	{ok,Mod}->
	    format_compilation_result({ok,Mod});
	{error,Reason}->
	    format_compilation_result({error,Reason});
	{okDir,Res_list} ->
	    "Compilation of dir : " ++ File_or_dir ++ " Succeded";
	{errorDir,Reason}->
	    "Result of compilation:\\n"++ 
		format_compilation_error({errorDir,Reason});
	{errorDir2,{_,eacces}}->
	    "No acces to directory :" ++ File_or_dir;
	{errorDir2,Reason}->
	    "Cant find directory:" ++ File_or_dir ; 
	false->
	    []
    end.
format_compilation_result(Reason)->
    case Reason of
	{ok,Module} when list(Module)->
	    "Compilation of module " ++ Module ++ " Succeded\\n";
	{ok,Module} when atom(Module)->
	    "Compilation of module " ++ atom_to_list(Module) ++ " Succeded\\n";
	{error,File} when list( File) ->
	    "Compilation of module " ++ File ++ " Failed\\n";
	{error,File} when atom(File)->
	    "Compilation of module " ++ atom_to_list(File) ++ " Failed\\n";
	{error,{_,File,enoent}}->
		"File don't exists: " ++ File ++ " \\n";
	{error,{_,File,eaccess}} ->
	    "Can't acess file: " ++ File  ++  "\\n";
	_->
	    io_lib:format("Unknown error: ~p \\n",[Reason])
    end.

format_compilation_error({errorDir,Result_list})->
    lists:flatten(lists:map(fun format_compilation_result/1,Result_list)).


compile_form(CompiledFile)->
    Body = case CompiledFile of
	       noFile ->
		   "<BODY BGCOLOR=\"#FFFFFF\">";
	       _->
		   "<BODY onLoad=reloadCompiledList() BGCOLOR=\"#FFFFFF\">"
	   end,
    Form= case webtool:is_localhost() of 
	      false->
		  "<FONT SIZE=5>Compile</FONT><TABLE  BORDER=1>
                   <TR><TD>
                   <FORM name=compile_form ACTION= \"./compile\">
	           <TABLE BORDER=\"0\">
                   <TR><TD> Module or directory to compile</TD></TR>
	           <TR><TD><INPUT TYPE=\"text\" NAME=\"path\" SIZE=30></TD></TR>
	           <TR ROWSPAN=2><TD>Compile options</TD></TR>
	           <TR><TD><INPUT TYPE=\"TEXT\" NAME=\"options\" SIZE=30 ></TD></TR>
	           <TR><TD><FONT SIZE=2>&nbsp;</FONT></TD></TR>
	           <TR><TD ALIGN=\"center\"><INPUT TYPE=\"Submit\" VALUE=\"compile\"></TD></TR>
                   </TABLE>
                   </FORM>
                </TD>";
	      true ->
		  "<FONT SIZE=5>Compile</FONT><TABLE  BORDER=1>
                   <TR><TD>
                   <FORM name=compile_form ACTION= \"./compile\">
	           <TABLE BORDER=\"0\">
                   <TR><TD> Module or directory to compile</TD></TR>
	           <TR><TD><INPUT TYPE=\"file\" NAME=\"path1\" SIZE=20><INPUT TYPE=\"hidden\" NAME=\"path\"></TD></TR>
	           <TR ROWSPAN=2><TD>Compile options</TD></TR>
	           <TR><TD><INPUT TYPE=\"TEXT\" NAME=\"options\" SIZE=30 ></TD></TR>
	           <TR><TD><FONT SIZE=2>&nbsp;</FONT></TD></TR>
	           <TR><TD ALIGN=\"center\"><INPUT TYPE=\"Button\" onClick=submitCompile() VALUE=\"compile\"></TD></TR>
                   </TABLE>
                   </FORM>
                </TD>"
	      end,
    Body++Form++
          "<TD>
          <FORM name=direcory_form ACTION= \"./compile\">
	   <TABLE  BORDER=\"0\"> 
 	      <TR><TD COLSPAN=4>
		 Directory 
               </TD></TR> 
	      <TR><TD COLSPAN=4> 
		  <INPUT TYPE=\"TEXT\" NAME=\"dir\" SIZE=30> 
               </TD></TR>
 	      <TR><TD COLSPAN=4>&nbsp;
                </TD></TR>
	       <TR>
                  <TD WIDTH=20> <INPUT TYPE=\"radio\" NAME=\"action\" VALUE=\"list\" CHECKED=\"true\" ></TD><TD>List directory</TD>
                  <TD WIDTH=20> <INPUT TYPE=\"radio\" NAME=\"action\" VALUE=\"change\" ></TD><TD>Change directory</TD>
               </TD></TR>
	       <TR><TD COLSPAN=4>
	          <FONT SIZE=2>&nbsp;</FONT>
                </TD></TR>
 	      <TR><TD COLSPAN=4 ALIGN=\"center\">
	 	  <INPUT TYPE=\"Submit\" VALUE=\"Perform\">
               </TD></TR>
             </TABLE>
          </FORM>
     </TD></TR>
 </TABLE>".


cover_compile(File_or_dir,Input)->
%Add control for compiler options in the input data
    Options=get_options(Input),
    case file_type(File_or_dir) of
	{true,file}->
	    cover_compile_file(File_or_dir,Options);
	{true,dir}->
	    cover_compile_dir(File_or_dir,Options);
	_->
	    false
    end.

get_options(Input)->
    case control_input_data(Input,options) of
	{ok,Options}->
	   parse_options(Options);
	_ ->
	    []
    end.

parse_options(Options)->
    case erl_scan:string(Options ++".") of
	{ok,Tokens,Line} ->
	    case erl_parse:parse_exprs(Tokens) of
		{ok,X}->
		    lists:map({erl_parse,normalise},X);
		_ ->
		    []
	    end;
	_ ->
	    []
    end.
	    

cover_compile_file(File,[])->
    cover_compile_file(File,[verbose,report_errors,report_warnings]);

cover_compile_file(File,Options)->
    %io:format("~n--------------------------~n"),
    %io:format("~p",[File]),
    %io:format("~n--------------------------~n"),
    case cover:compile_module(File,Options) of
	{ok,Mod}->
	    {ok,File};
	{error,Reason}->
	    {error,Reason}
    end.


cover_compile_dir(Dir,[])->
    cover_compile_dir(Dir,[verbose,report_errors,report_warnings]);
cover_compile_dir(Dir,Options)->
    control_result(cover:compile_directory(Dir,Options)).



control_result(Result_list)when list(Result_list)->
    case lists:keymember(error,1,Result_list) of
	true->
	    {errorDir,Result_list};
	false->
	    {okDir,Result_list}
    end;

control_result(Res)->
    {errorDir2,Res}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that clear the cover database from data about the     %
% selected module                                                     %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cover_clear(Input)->
    case  control_input_data(Input,module) of
	{true,Module}->    
	    cover:reset(Module);
	_ ->
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the page that shows the calls %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_result(Input)->
    case control_input_data(Input, module) of
	{true,Mod}->
	    case cover:analyze(Mod,calls) of
		{error,_}->
		    error_body();
		{ok,_} ->
		    call_result2(Mod,Input)
	    end;
	_ ->
	    error_body()
    end.

call_result2(Mod,Input)->
    case control_input_data(Input,resultchoice) of
	{true,mod}->
	    result_choice("mod",Mod) ++ call_result(mod,Mod);
	{true,func}->
	    result_choice("func",Mod) ++ call_result(func,Mod);
	{true,clause}->
	    result_choice("clause",Mod) ++ call_result(clause,Mod);
	_->
	    result_choice("all",Mod) ++ call_result(all,Mod)
    end.

result_choice(Selected,Mod)->
    "<FORM NAME=\"reload_form\" ><INPUT TYPE=\"hidden\" NAME=\"module\" VALUE=\"" ++ atom_to_list(Mod)  ++  
      "\"><TABLE WIDTH=100%><TR>" ++ print_radio_buttons([{"all","All Data"},{"mod","Module"},
                        {"func","Function"},{"clause","Clause"}],Selected) ++ "</TR></TABLE><FORM>".

call_result(Mode,Module)->
    Content = case Mode of
		  mod->
		      format_cover_call(cover:analyse(Module,calls,module),mod);
		  func->
		      format_cover_call(cover:analyse(Module,calls,function),func);
		  clause->
		      format_cover_call(cover:analyse(Module,calls,clause),clause);
		  _->
		      format_cover_call(cover:analyse(Module,calls,module),mod) ++
		      format_cover_call(cover:analyse(Module,calls,function),func)++
		      format_cover_call(cover:analyse(Module,calls,clause),clause)
	      end,
    getModDate(Module,date())++"<BR>"++
    "<TABLE WIDTH=\"100%\" BORDER=1>"
                ++ Content ++"</TABLE>".


format_cover_call({error,_},_)->
     "<TR><TD>
      <BR><BR><BR><BR>
      <FONT SIZE=5>The selected module is not Cover Compiled</FONT>
      <BR>  
      </TD></TR>";

format_cover_call({ok,{Mod,Calls}},mod)->
    "<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=5><B>Module calls</B></TD></TR>" ++ 
    "<TR><TD COLSPAN=4><I>Module</I></TD><TD ALIGN=\"right\"><I>Number of calls</I> </TD> </TR>" ++
    "<TR><TD COLSPAN=4>" ++ atom_to_list(Mod) ++"</TD> <TD ALIGN=\"right\">" ++ integer_to_list(Calls)  ++" </TD></TR>";

format_cover_call({ok,Calls},func)->
    "<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=5><B>Function calls</B></TD></TR>" ++ 
    "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD><TD COLSPAN=2 ALIGN=\"right\"><I>Arity</I></TD>
     <TD ALIGN=\"right\"><I>Number of calls </I></TD> </TR>" ++
    lists:flatten(lists:map(fun({{Mod,Func,Arity},Nr_of_calls})->
				    "<TR><TD WIDTH=\"20%\">"++ atom_to_list(Mod) ++" </TD>  
                                     <TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++" </TD> 
                                    <TD COLSPAN=2 WIDTH=\"40%\" ALIGN=\"right\" >" ++ integer_to_list(Arity) ++" </TD>  
                                    <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Nr_of_calls) ++" </TD>  </TR>"
			    end,Calls));
format_cover_call({ok,Calls},clause)->
    "<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=5><B>Clause calls</B></TD></TR>" ++
    "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD><TD ALIGN=\"right\"><I>Arity</I></TD>
     <TD ALIGN=\"right\"><I>Ordinal</I></TD><TD ALIGN=\"right\"><I>Number of calls</I></TD> </TR>" ++
    lists:flatten(lists:map(fun({{Mod,Func,Arity,Ord},Nr_of_calls})->
				    "<TR><TD WIDTH=\"20%\" >"++ atom_to_list(Mod) ++" </TD>  
                                     <TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++" </TD> 
                                    <TD WIDTH=\"20%\" ALIGN=\"right\" >" ++ integer_to_list(Arity) ++" </TD>  
                                    <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Ord) ++" </TD>  
                                    <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Nr_of_calls) ++" </TD>  </TR>"
			    end,Calls)).
result_script(What)->
    "<SCRIPT>
     function reload_page()
     {
        window.location=\"./" ++ What ++  "?module=\" + document.reload_form.module.value  +\"&present=\" + get_mode() ; 
     } 
     function get_mode() 
     {
       for (i= 0; i < document.reload_form.mode.length; i++)
        {
          if (document.reload_form.mode[i].checked)
            return(document.reload_form.mode[i].value);
        }
      return(\"all\");
     }
    </SCRIPT>".



%%Print the radiobuttons. if the mode is the one the current radio-but%%
%% set the radio button to checked                                    %%

print_radio_buttons([],_)->
    [];
print_radio_buttons([{Mode,Name}|Rest],Mode)->
    "<TD><INPUT TYPE=\"radio\" NAME=\"mode\" CHECKED=\"true\" VALUE=\""++ Mode ++
	"\" onClick=\"reload_page()\">&nbsp;&nbsp;" ++Name ++  "</TD>\n" ++
	print_radio_buttons(Rest,Mode);
print_radio_buttons([{Mode1,Name}|Rest],Mode)->
    "<TD><INPUT TYPE=\"radio\" NAME=\"mode\" VALUE=\""++ Mode1 ++ 
        "\" onClick=\"reload_page()\">&nbsp;&nbsp;" ++Name ++  "</TD>\n" ++
	print_radio_buttons(Rest,Mode).




error_body()->
    "<TABLE WIDTH=\"100%\" BORDER=1>
     <TR ALIGN=\"center\">
     <TD>
     <BR><BR><BR><BR><BR><BR>
     <FONT SIZE=5>The selected module is not Cover Compiled</FONT>
     <BR>  
     </TD>
     </TR>
     </TABLE>".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the page that shows coverage  %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
coverage_result(Input)->
    case control_input_data(Input, module) of
	{true,Mod}->
	    case cover:analyze(Mod,coverage) of
		{error,_}->
		    error_body();
		{ok,_} ->
		    coverage_result2(Mod,Input)
	    end;
	_ ->
	    error_body()
    end.

coverage_result2(Mod,Input)->
    case control_input_data(Input,resultchoice) of
	{true,mod}->
	    result_choice("mod",Mod) ++ coverage_result(mod,Mod);
	{true,func}->
	    result_choice("func",Mod) ++ coverage_result(func,Mod);
	{true,clause}->
	    result_choice("clause",Mod) ++ coverage_result(clause,Mod);
	_->
	    result_choice("all",Mod) ++ coverage_result(all,Mod)
    end.

coverage_result(Mode,Module)->
    Content = case Mode of
		  mod->
		      format_cover_coverage(cover:analyze(Module,coverage,module),mod);
		  func->
		      format_cover_coverage(cover:analyse(Module,coverage,function),func);
		  clause->
		      format_cover_coverage(cover:analyse(Module,coverage,clause),clause);
		  _->
		      format_cover_coverage(cover:analyse(Module,coverage,module),mod) ++
		      format_cover_coverage(cover:analyse(Module,coverage,function),func)++
		      format_cover_coverage(cover:analyse(Module,coverage,clause),clause)
	      end,
    getModDate(Module,date())++"<BR>"++
    "<TABLE WIDTH=\"100%\" BORDER=1>"
                ++ Content ++"</TABLE>".

getModDate(Module,{Year,Mon,Day})->
    "<TABLE>
      <TR>
        <TD>Module:</TD> 
        <TD>" ++ atom_to_list(Module) ++ "</TD> 
      </TR>
     <TR>
        <TD>Date:</TD> 
        <TD>" ++ integer_to_list(Day) ++ "/" ++ 
                 integer_to_list(Mon) ++"&nbsp;-&nbsp;"++ 
                 integer_to_list(Year)  ++ 
       "</TD> 
     </TR>
   </TABLE>".


format_cover_coverage({error,_},_)->
     "<TR><TD>
      <BR><BR><BR><BR>
      <FONT SIZE=5>The selected module is not Cover Compiled</FONT>
      <BR>  
      </TD></TR>";


format_cover_coverage({ok,{Mod,{Cov,Not_cov}}},mod)->
    "<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6><B>Module coverage</B></TD></TR>" ++
     "<TR><TD COLSPAN=4><I>Module</I></TD>
      <TD ALIGN=\"right\"><I>Covered</I> </TD>
      <TD ALIGN=\"RIGHT\" NOWRAP=\"true\"><I>Not Covered</I></TD> 
      </TR>" ++
      "<TR><TD COLSPAN=4>" ++ atom_to_list(Mod) ++"</TD> 
           <TD ALIGN=\"right\">" ++ integer_to_list(Cov)  ++" </TD>
           <TD ALIGN=\"right\" >" ++ integer_to_list(Not_cov)  ++" </TD></TR>";

format_cover_coverage({ok,Cov_res},func)->
    "<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6><B>Function coverage</B></TD></TR>" ++ 
    "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD><TD ALIGN=\"right\"><I>Arity</I></TD>
     <TD COLSPAN=2 ALIGN=\"right\"><I>Covered</I></TD> 
     <TD ALIGN=\"right\" STYLE=\"white-space:nowrap\"><I>Not Covered</I></TD> </TR>" ++
    lists:flatten(lists:map(fun({{Mod,Func,Arity},{Cov,Not_cov}})->
				    "<TR><TD WIDTH=\"20%\" >"++ atom_to_list(Mod) ++" </TD>  
                                     <TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++" </TD> 
                                    <TD WIDTH=\"40%\" ALIGN=\"right\">" ++ integer_to_list(Arity) ++" </TD>  
                                    <TD WIDTH=\"40%\" ALIGN=\"right\" COLSPAN=2>" ++ integer_to_list(Cov) ++" </TD>  
                                    <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Not_cov) ++" </TD>  </TR>"
			    end,Cov_res));

format_cover_coverage({ok,Cov_res},clause)->
    "<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6><B>Clause coverage</B></TD></TR>" ++ 
    "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD><TD ALIGN=\"right\"><I>Arity</I></TD>
     <TD ALIGN=\"right\"><I>Ordinal<I></TD>
         <TD ALIGN=\"right\">Covered</TD> <TD ALIGN=\"right\" STYLE=\"white-space:nowrap\">Not Covered</TD> </TR>" ++ 
    lists:flatten(lists:map(fun({{Mod,Func,Arity,Ord},{Cov,Not_cov}})->
				    "<TR><TD WIDTH=\"20%\" >"++ atom_to_list(Mod) ++" </TD>  
                                     <TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++" </TD> 
                                     <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Arity) ++" </TD>  
                                     <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Ord) ++" </TD>  
                                     <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Cov) ++" </TD>  
                                     <TD WIDTH=\"20%\" ALIGN=\"right\">" ++ integer_to_list(Not_cov) ++" </TD>  </TR>"
			    end,Cov_res)).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the page that shows the       %
% source file	                                                      %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source_file_body(Input)->
    case control_input_data(Input,module) of
	{true,Mod}->
	    case cover:is_compiled(Mod) of
		{file,Path}->
		    get_code_header(date()) ++ get_code_body(Mod);
		_->
		    error_body()
	    end;
	_ ->
	    error_body()
    end.

get_code_header({Year,Mon,Day})->
    "<TABLE>
     <TR>
        <TD>Date:</TD> 
        <TD>" ++ integer_to_list(Day) ++ "/" ++ 
                 integer_to_list(Mon) ++"&nbsp;-&nbsp;"++ 
                 integer_to_list(Year)  ++ 
       "</TD> 
     </TR>
   </TABLE>".

get_code_body(Path) when atom(Path)->
    case cover:analyze_to_file(Path) of
	{ok,OutFile} ->
	    case file:read_file(OutFile) of
		{ok,File}->
		    file:delete(OutFile),
		    "<PRE>" ++ binary_to_list(File)  ++ "</PRE>";
		_ ->
		    "Can not read file" ++ OutFile
	    end;
	{error,Reason} ->
	    "Could not read the analyze file." ++ Path
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% Different private helper functions                                   %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Adds a javascript that reloads the frame when the page is reloaded %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_javascripts(CompileRes)->
    "<script>
        function reloadCompiledList()
        {
           parent.work.document.location.href=\"/webcover/erl/cover_web/default_selection\";
           alert(\""++ CompileRes ++"\");
        }
     </script>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Create the Header for the page If we now the mimetype use that type %%
%%otherwise use text                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
header() ->
    header("text/html").
header(MimeType) ->
    "Pragma:no-cache\r\n" ++
    "Content-type: " ++ MimeType ++ "\r\n\r\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%%Create the Htmlheader set the title of the side to nothing if      %%       
%%we dont know the name of the side                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
html_header(head)->
   html_header("",head);

html_header(tot)->
   html_header("",tot).

html_header(Part,tot) ->    
    "<HTML>\n" ++
	"<HEAD>\n" ++
	"<TITLE>" ++ Part ++  "</TITLE>\n" ++
	"</HEAD>\n<BODY BGCOLOR=\"#FFFFFF\">";

html_header(Part,head) ->    
    "<HTML>\n" ++
	"<HEAD>\n" ++
	"<TITLE>" ++ Part ++  "</TITLE>\n" ++
	"</HEAD>\n".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Close the Html tag and if neccessay add some clean upp              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
html_end()->
    "</BODY></HTML>".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Control the input data and return the intresting values or error   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
control_input_data(Input_list,options)->
    case get_value(httpd:parse_query(Input_list),"options") of
	{true,Options}->
	    {true,Options};
	_ ->
	    []
    end;

control_input_data(Input_list,action)->
    case get_value(httpd:parse_query(Input_list),"action") of
	{true,Action}->
	    case Action of
		"change" ->
		    {ok,change};    
		_->
		    {ok,list}
	    end;
	_ ->
	        {ok,list}
    end;

control_input_data(Input,dir)->
    case get_value(httpd:parse_query(Input),"dir") of
	{true,""}->
	    {error,nodir};
	{true,Dir}->
	    control_file_type(Dir);
	_->
	    {error,nodir}
    end;

control_input_data(Input_list,compile)->
    case get_value(httpd:parse_query(Input_list),"path") of
	{true,File}->
	    %io:format("~p",[File]),
	    {true,File};    
	_->
	    false
    end;

control_input_data(Input_list,resultchoice)->
    case get_value(httpd:parse_query(Input_list),"present") of
	{true,Choice}->
	    {true,list_to_atom(Choice)};    
	_->
	    false
    end;

control_input_data(Input_list,module)->
    case get_value(httpd:parse_query(Input_list),"module") of
	{true,Module}->
	    {true,list_to_atom(Module)};   
	_->
	    false
    end.


get_value(Tuple_list,Key)->
    case lists:keysearch(Key,1,Tuple_list) of
	{value,{_,Val}}->
	    {true,Val};
	_->
	   false
    end.

control_file_type(Dir)->
    case file_type(Dir) of
	{true,dir}->
	    {ok,Dir};
	_->
	    {error,Dir}
    end.


%----------------------------------------------------------------------
% control_type(Dir_or_file,Type)->true|false
% Dir_or_file a string that shall be controlled if its a plain file or a dir
%----------------------------------------------------------------------

file_type(Dir_or_file)->
    case file:read_file_info(Dir_or_file) of
	{ok,Info}->
	    case  Info#file_info.type of
		regular->
		    {true,file};
		directory->
		    {true,dir};
		_->
		    false
	    end;
	_->
	    false
    end.
	    
    
	
	
























