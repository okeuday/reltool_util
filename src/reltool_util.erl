%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==reltool Utility Functions==
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
         application_stop/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually.===
%% @end
%%-------------------------------------------------------------------------

application_start(A) ->
    application_start(A, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually with a specific configuration.===
%% @end
%%-------------------------------------------------------------------------

application_start(A, Env) when is_atom(A), is_list(Env) ->
    ok = application_loaded(A),
    lists:foreach(fun({K, V}) ->
        ok = application:set_env(A, K, V)
    end, Env),
    {ok, As} = application:get_key(A, applications),
    AllAs = applications_start(As, As ++ [A]),
    lists:foreach(fun(Application) ->
        ok = application_started(Application)
    end, AllAs),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop an application and its dependencies.===
%% @end
%%-------------------------------------------------------------------------

application_stop(A) when is_atom(A) ->
    application:stop(A).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

application_started(A) ->
    case application:start(A, temporary) of
        ok ->
            ok;
        {error, {already_started, A}} ->
            ok
    end.

application_loaded(A) ->
    case application:load(A) of
        ok ->
            ok;
        {error, {already_loaded, A}} ->
            ok
    end.

applications_start([], As) ->
    As;
applications_start([A | Rest], As) ->
    ok = application_loaded(A),
    NewAs = case application:get_key(A, applications) of
        {ok, []} ->
            As;
        {ok, NextAs} ->
            applications_start(NextAs, NextAs ++ As)
    end,
    applications_start(Rest, NewAs).

