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
-export([initiate_upload/4]).
-export([upload_part/7]).
-export([complete_upload/5]).
-export([abort_upload/4]).


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



-spec initiate_upload(BucketName :: list(), Key :: list(), CType :: list(), Config :: aws_config()) -> list().
initiate_upload(BucketName, Key, CType, Config) ->
    initiate_upload(lib_util:to_list(BucketName), lib_util:to_list(Key), [], [{"content-type", lib_util:to_list(CType)}], Config).

initiate_upload(BucketName, Key, Options, HTTPHeaders0, Config) when is_list(BucketName), is_list(Key), is_list(Options) ->
    {ContentType, HTTPHeaders} = case lists:keytake("content-type", 1, HTTPHeaders0) of
                                     {value, {_, CType}, Rest} -> {CType, Rest};
                                     false -> {"application/octet-stream", HTTPHeaders0}
                                 end,
    RequestHeaders = [{"x-amz-acl", proplists:get_value(acl, Options)} | HTTPHeaders]
        ++ [{["x-amz-meta-" | string:to_lower(MKey)], MValue} ||
            {MKey, MValue} <- proplists:get_value(meta, Options, [])],
    _ReturnResponse = proplists:get_value(return_response, Options, false),
    erlcloud_s3_multipart:upload_id(erlcloud_s3_multipart:initiate_upload(BucketName, Key, ContentType, RequestHeaders, Config)).

-spec upload_part(BucketName :: list(), Key :: list(), UploadId :: list(), PartNum :: integer(), Body :: binary(), Config :: aws_config(), EtagList :: list()) -> list().
upload_part(BucketName, Key, UploadId, PartNum, Body, Config, EtagList) ->
    {RespHeaders, _UploadRes} = erlcloud_s3_multipart:upload_part(BucketName, Key, UploadId, PartNum, Body, Config),
    PartEtag = proplists:get_value("ETag", RespHeaders),
    [{PartNum, PartEtag} | EtagList].

-spec complete_upload(BucketName :: list(), Key :: list(), UploadId :: list(), Config :: aws_config(), EtagList :: list()) -> ok.
complete_upload(BucketName, Key, UploadId, Config, EtagList) ->
    erlcloud_s3_multipart:complete_upload(BucketName, Key, UploadId, EtagList, Config).

-spec abort_upload(string(), string(), string(), Config :: aws_config()) -> ok.
abort_upload(BucketName, Key, UploadId, Config) ->
    erlcloud_s3_multipart:abort_upload(BucketName, Key, UploadId, Config).


