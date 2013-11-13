
-module(ntf_gui).

-behaviour(gen_server).

%% Client API
-export([start_link/0]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-define(menuID_QUIT, 400).

-record(state,
        {
          icon,
          menu,
          taskbaricon
        }).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
    process_flag(trap_exit, true),

    _Wx = wx:new([{debug, verbose}]),

    _Panel = wxPanel:new(),

  %% Initialize Task Bar Icon
    IconFile = filename:join("priv", "icon.jpg"),
    true = filelib:is_regular(IconFile),
    Icon = wxIcon:new(IconFile, [{type, ?wxBITMAP_TYPE_JPEG}]),
    TaskBarIcon = wxTaskBarIcon:new(),
    true = wxTaskBarIcon:setIcon(TaskBarIcon, Icon, [{tooltip, "Notification"}]),

    %% Initialize popup menu
    Menu = wxMenu:new(),
    Quit = wxMenuItem:new ([{id, ?menuID_QUIT},{text, "&Quit"}]),
    wxMenu:append (Menu, Quit),
    wxMenu:connect(Menu, command_menu_selected),

    %% Connect menu to Taskbar Icon
    wxTaskBarIcon:connect(TaskBarIcon, taskbar_left_down),
    wxTaskBarIcon:connect(TaskBarIcon, taskbar_right_down), 

    {ok, #state{icon = Icon, menu = Menu, 
                     taskbaricon = TaskBarIcon}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Callbacks handled as normal gen_server callbacks
handle_info(#wx{obj = TaskBarIcon,
                 event = #wxTaskBarIcon{type = EventType}},
	    #state{menu = _Menu, icon = Icon, taskbaricon = TaskBarIcon} = State) 
  when EventType == taskbar_left_down;
       EventType == taskbar_right_down ->
    io:format("Got TaskBarIcon Click ~n", []),   
    wxTaskBarIcon:setIcon(TaskBarIcon, Icon, [{tooltip, "Powered by @Erlang"}]),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Unknown message ~p~n", [Msg]),
    {noreply, State}.

handle_event(Msg, State) ->
    io:format("Got Event ~p~n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{}) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, #state{icon = _Icon, menu = Menu, 
                     taskbaricon = TaskBarIcon}) ->
    wxMenu:destroy(Menu),
    wxTaskBarIcon:destroy(TaskBarIcon),
    wx:destroy(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

