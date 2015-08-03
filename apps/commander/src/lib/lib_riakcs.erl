%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 七月 2015 下午4:34
%%%-------------------------------------------------------------------
-module(lib_riakcs).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
%% API
-export([riakcs_init/4]).
-export([insert/5]).
-export([delete/3]).
-export([select/3]).
-export([create_riakcs_bucket/2]).


-spec riakcs_init(string(), string(), string(), integer()) -> aws_config().
riakcs_init(ACCESS_KEY_ID, SECRET_ACCESS_KEY, S3_HOST, S3_PORT)
    when is_list(ACCESS_KEY_ID), is_list(SECRET_ACCESS_KEY), is_list(S3_HOST), is_integer(S3_PORT) ->
    erlcloud_s3:new(ACCESS_KEY_ID, SECRET_ACCESS_KEY, S3_HOST, S3_PORT).

-spec create_riakcs_bucket(string(), aws_config()) -> ok.
create_riakcs_bucket(BucketName, Aws_config)
    when is_integer(BucketName), is_record(Aws_config, aws_config) ->
    erlcloud_s3:create_bucket(BucketName, Aws_config).

-spec insert(string(), string(), iolist(), [{string(), string()}], aws_config()) -> proplists:proplist().
insert(BucketName, Key, Value, HTTPHeaders, Aws_config)
    when is_list(BucketName), is_list(Key), is_record(Aws_config, aws_config) ->
    erlcloud_s3:put_object(BucketName, Key, Value, [], HTTPHeaders, Aws_config).

-spec delete(string(), string(), aws_config()) -> proplists:proplist().
delete(BucketName, Key, Aws_config)
    when is_list(BucketName), is_list(Key), is_record(Aws_config, aws_config) ->
    erlcloud_s3:delete_object(BucketName, Key, Aws_config).

-spec select(string(), string(), aws_config()) -> proplists:proplist().
select(BucketName, Key, Aws_config)
    when is_list(BucketName), is_list(Key), is_record(Aws_config, aws_config) ->
    erlcloud_s3:get_object(BucketName, Key, Aws_config).






