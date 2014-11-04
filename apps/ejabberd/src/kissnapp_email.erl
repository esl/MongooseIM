%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% 通过mailgun (https://mailgun.com/cp) 提供的免费https接口发送邮件
%%% @end
%%% Created : 20. 八月 2014 下午2:53
%%%-------------------------------------------------------------------
-module(kissnapp_email).
-author("yoolo").


%% API
-export([send_text/3,send_html/3,send_register_validate/3,test_sendtext/0,test_sendhtml/0,test_send_register_validate/0]).

%% 发送文本内容邮件
%% Subject 邮件标题
%% ToEmail 收件人
%% Text    邮件内容
send_text(Subject,ToEmail,Text) ->
  inets:start(),
  ssl:start(),
  %io:format("ssl Status:~p~n",[SSLStatus]),
  %application:start(inets),
  Method = post,
  URL = "https://api:key-c3fa23e26ecd8d5f61c0397932e29fbc@api.mailgun.net/v2/yoolosoft.com/messages",
  Header = [],
  Type = "application/x-www-form-urlencoded",
  Body = lists:concat(["from=postmaster@yoolosoft.com" ,"&to=", ToEmail ,"&subject=", Subject ,"&text=", Text]),
  %Body ="from=postmaster@yoolosoft.com&to=jaypkwyl@163.com&subject=Kissnapp Validate Email&text=Hello,World!",
  HTTPOptions = [],
  Options = [],
  %R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  %R=httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options),
  %io:format("Send Result ~p~n ~p~n",[Body, R]).
  httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options).

%% 发送HTML内容邮件
%% Subject 邮件标题
%% ToEmail 收件人
%% Html    邮件内容
send_html(Subject, ToEmail, Html) ->
  inets:start(),
  ssl:start(),
  %io:format("ssl Status:~p~n",[SSLStatus]),
  %application:start(inets),
  Method = post,
  URL = "https://api:key-c3fa23e26ecd8d5f61c0397932e29fbc@api.mailgun.net/v2/yoolosoft.com/messages",
  Header = [],
  Type = "application/x-www-form-urlencoded",
  Body = lists:concat(["from=postmaster@yoolosoft.com" ,"&to=", ToEmail ,"&subject=", Subject ,"&html=", Html]),
  %Body ="from=postmaster@yoolosoft.com&to=jaypkwyl@163.com&subject=Kissnapp Validate Email&text=Hello,World!",
  HTTPOptions = [],
  Options = [],
  %R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  %R=httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options),
  %io:format("Send Result ~p~n ~p~n",[Body, R]).
  httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options).


%% 发送注册验证接口
%% Subject 邮件标题
%% ToEmail 收件人
%% Content 链接地址
send_register_validate(Subject,ToEmail,Content)->
  Chtml="<!DOCTYPE html>
  <html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
  </head>
  <body>
  <div style=\"width:100%;font-size:20px;text-align:center\">
  Welcome to register  Kissnapp <br/> <br/>
  <a style=\"color:blue\" href=\""++Content++"\">Click Validate Email</a>
  </div>
  </body>
  </html>",
  send_html(Subject,ToEmail,Chtml).

% 测试发送
test_sendtext()->
  send_text("Kissnapp Validate Email","jaypkwyl@163.com","Hello,World!").

% 测试发送邮件html
test_sendhtml()->
  send_html("Kissnapp Validate Email","jaypkwyl@163.com","Hello,World!").

% 测试发送邮件注册验证邮件
test_send_register_validate()->
  send_register_validate("Kissnapp Register Vilidate","jaypkwyl@163.com","http://www.baidu.com").
