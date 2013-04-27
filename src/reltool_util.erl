%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==reltool Utility Functions==
%%% All the functions here are probably considered unorthodox, but
%%% are useful for runtime usage of applications and releases.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(reltool_util).
-author('mjtruog [at] gmail (dot) com').

-export([application_start/1,
         application_start/2,
         application_stop/1,
         script_start/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually.===
%% @end
%%-------------------------------------------------------------------------

application_start(Application) ->
    application_start(Application, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually with a specific configuration.===
%% @end
%%-------------------------------------------------------------------------

application_start(Application, Env)
    when is_atom(Application), is_list(Env) ->
    ok = application_loaded(Application),
    lists:foreach(fun({K, V}) ->
        ok = application:set_env(Application, K, V)
    end, Env),
    lists:foreach(fun(A) ->
        ok = application_started(A)
    end, applications_dependencies(Application)),
    ok = application_started(Application),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop an application and its dependencies.===
%% @end
%%-------------------------------------------------------------------------

application_stop(Application) when is_atom(Application) ->
    StopAs0 = applications_dependencies(Application),
    StopAs1 = delete_all(kernel, StopAs0),
    StopAs2 = delete_all(stdlib, StopAs1),
    Apps = application:loaded_applications(),
    {value, _, OtherApps0} = lists:keytake(Application, 1, Apps),
    OtherAppsN = lists:foldl(fun(A, As) ->
        {value, _, NextAs} = lists:keytake(A, 1, As),
        NextAs
    end, OtherApps0, StopAs2),
    RequiredAs = lists:foldl(fun({A, _, _}, As) ->
        sets:union(As, sets:from_list(applications_dependencies(A)))
    end, sets:new(), OtherAppsN),
    StopAsN = lists:foldl(fun(A, As) ->
        delete_all(A, As)
    end, StopAs2, sets:to_list(RequiredAs)),
    ok = application_stopped(Application),
    lists:foreach(fun(A) ->
        ok = application_stopped(A)
    end, lists:reverse(StopAsN)),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Start everything specified within a script file.===
%% A script file is the input used when creating a boot file, which is the
%% file used when first starting the Erlang VM.  This function checks
%% all applications to determine if they are already running with the
%% expected versions.  All modules are checked to make sure they have
%% been loaded, if they are expected to have been loaded. Normally,
%% the script is only used in the binary boot file format and only a single
%% boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The script file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

script_start(FilePath) ->
    true = lists:suffix(".script", FilePath),
    % system name and version are ignored
    {ok, [{script, {_Name, _Vsn}, Instructions}]} = file:consult(FilePath),
    Dir = filename:dirname(FilePath),
    % expects the typical directory structure produced by reltool
    DirNames = filename:split(Dir),
    Root = lists:sublist(DirNames, erlang:length(DirNames) - 2),
    true = filelib:is_dir(filename:join(Root ++ ["lib"])),
    script_instructions(Instructions, Root).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

application_loaded(A) ->
    case application:load(A) of
         ok ->
             ok;
        {error, {already_loaded, A}} ->
             ok
     end.

application_started(A) ->
    case application:start(A, temporary) of
        ok ->
            ok;
        {error, {already_started, A}} ->
            ok
    end.

application_stopped(A) ->
    case application:stop(A) of
        ok ->
            ok;
        {error, {not_started, A}} ->
            ok
    end.

applications_dependencies(A) ->
    {ok, As} = application:get_key(A, applications),
    applications_dependencies(As, As).

applications_dependencies([], As) ->
    As;
applications_dependencies([A | Rest], As) ->
    ok = application_loaded(A),
    NewAs = case application:get_key(A, applications) of
        {ok, []} ->
            As;
        {ok, NextAs} ->
            applications_dependencies(NextAs, NextAs ++ As)
    end,
    applications_dependencies(Rest, NewAs).

script_instructions(L, Root) ->
    Apps = application:loaded_applications(),
    script_instructions(L, preload, Root, Apps).

script_instructions([], started, _, _) ->
    ok;
script_instructions([{progress, Progress} | L], _, Root, Apps) ->
    script_instructions(L, Progress, Root, Apps);
script_instructions([{preLoaded, _} | L],
                    preload, Root, Apps) ->
    script_instructions(L, preload, Root, Apps);
script_instructions([{kernel_load_completed} | L],
                    preloaded, Root, Apps) ->
    script_instructions(L, kernel_load_completed, Root, Apps);
script_instructions([{path, Paths} | L],
                    preloaded, Root, Apps) ->
    true = ensure_code_paths(Paths, Apps),
    script_instructions(L, preloaded, Root, Apps);
script_instructions([{primLoad, Modules} | L],
                    preloaded, Root, Apps) ->
    true = lists:all(fun(M) ->
        is_module_loaded(M) =:= true
    end, Modules),
    script_instructions(L, preloaded, Root, Apps);
script_instructions([{kernel_load_completed} | L],
                    kernel_load_completed, Root, Apps) ->
    script_instructions(L, kernel_load_completed, Root, Apps);
script_instructions([{primLoad, Modules} | L],
                    kernel_load_completed, Root, Apps) ->
    true = ensure_all_loaded(Modules),
    script_instructions(L, kernel_load_completed, Root, Apps);
script_instructions([{path, Paths} | L],
                    kernel_load_completed, Root, Apps) ->
    lists:foreach(fun(P) ->
        ["$ROOT", "lib", NameVSN, "ebin"] = filename:split(P),
        code:add_pathz(filename:join([Root, "lib", NameVSN, "ebin"]))
    end, Paths),
    script_instructions(L, kernel_load_completed, Root, Apps);
script_instructions([{path, _} | L],
                    modules_loaded, Root, Apps) ->
    script_instructions(L, modules_loaded, Root, Apps);
script_instructions([{kernelProcess, _, _} | L],
                    modules_loaded, Root, Apps) ->
    script_instructions(L, modules_loaded, Root, Apps);
script_instructions([{apply, {application, load, [AppDescr]}} | L],
                    init_kernel_started, Root, Apps) ->
    {application, A, [_ | _] = AppSpecKeys} = AppDescr,
    case lists:keyfind(A, 1, Apps) of
        {A, _, VSN} ->
            {vsn, RequestedVSN} = lists:keyfind(vsn, 1, AppSpecKeys),
            true = VSN == RequestedVSN;
        false ->
            ok = application:load(AppDescr)
    end,
    script_instructions(L, init_kernel_started, Root, Apps);
script_instructions([{apply, {application, start_boot, [A, permanent]}} | L],
                    applications_loaded, Root, Apps)
    when A =:= kernel; A =:= stdlib ->
    % if this code is being used, kernel and stdlib should have already
    % been started with the boot file that was used to start the Erlang VM
    script_instructions(L, applications_loaded, Root, Apps);
script_instructions([{apply, {application, start_boot, [A, permanent]}} | L],
                    applications_loaded, Root, Apps) ->
    ok = application_started(A),
    script_instructions(L, applications_loaded, Root, Apps);
script_instructions([{apply, {c, erlangrc, []}} | L],
                    applications_loaded, Root, Apps) ->
    script_instructions(L, applications_loaded, Root, Apps).

ensure_all_loaded([]) ->
    true;
ensure_all_loaded([Module | Modules]) ->
    case is_module_loaded(Module) of
        true ->
            lists:all(fun(M) ->
                is_module_loaded(M) =:= true
            end, Modules);
        false ->
            Load = lists:all(fun(M) ->
                is_module_loaded(M) =:= false
            end, Modules),
            if
                Load ->
                    lists:foreach(fun(M) ->
                        {module, M} = code:load_file(M)
                    end, [Module | Modules]),
                    true;
                true ->
                    false
            end
    end.

ensure_code_paths(Paths, Apps) ->
    lists:all(fun(P) ->
        ["$ROOT", "lib", NameVSN, "ebin"] = filename:split(P),
        {Name, VSN} = split_name_vsn(NameVSN),
        Application = erlang:list_to_existing_atom(Name),
        case lists:keyfind(Application, 1, Apps) of
            {Application, _, VSN} ->
                true;
            _ ->
                false
        end
    end, Paths).

is_module_loaded(Module) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            true;
        false ->
            false
    end.

split_name_vsn(NameVSN) ->
    split_name_vsn([], [], NameVSN).
split_name_vsn([_ | _] = Name, VSN, []) ->
    [$- | FinalVSN] = lists:reverse(VSN),
    {Name, FinalVSN};
split_name_vsn(Name, NameSegment, [$- | L]) ->
    split_name_vsn(lists:reverse(NameSegment) ++ Name, [$-], L);
split_name_vsn(Name, VSN, [C | L]) ->
    split_name_vsn(Name, [C | VSN], L).

delete_all(Elem, List) when is_list(List) ->
    delete_all(Elem, [], List).
delete_all(Elem, L, [Elem | T]) ->
    delete_all(Elem, L, T);
delete_all(Elem, L, [H | T]) ->
    delete_all(Elem, [H | L], T);
delete_all(_, L, []) ->
    lists:reverse(L).
