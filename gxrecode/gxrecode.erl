#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    io:format("Usage: gxrecode <--in-file file> [--in-encoding encoding> <--out-encoding encoding> [--out-file file]~n");

main(["--help"]) ->
    io:format("Usage: gxrecode gxrecode <--in-file file> [--in-encoding encoding> <--out-encoding encoding> [--out-file file]~n");

main(["--in-file", InFile, "--out-encoding", OutEncoding]) ->
    core([InFile, unknown, OutEncoding, InFile]);
main(["--in-file", InFile, "--in-encoding", InEncoding, "--out-encoding", OutEncoding]) ->
    core([InFile, InEncoding, OutEncoding, InFile]);
main(["--in-file", InFile, "--out-encoding", OutEncoding, "--out-file", OutFile]) ->
    core([InFile, unknown, OutEncoding, OutFile]);
main(["--in-file", InFile, "--in-encoding", InEncoding, "--out-encoding", OutEncoding, "--out-file", OutFile]) ->
    core([InFile, InEncoding, OutEncoding, OutFile]).

check_input([InEncoding, OutEncoding]) ->
    if
	(InEncoding =/= unknown) -> 
	    In = encoding_char_to_atom(InEncoding);
	true ->
	    In = unknown
    end,
    Out = encoding_char_to_atom(OutEncoding),
    if 
	In =:= error ->
	    io:format("Unsupported in-encoding,use utf-8/16{le|be}/32{le|be}.~n"),
	    error;
	Out =:= error ->
	    io:format("Unsupported out-encoding,use utf-8/16{le|be}/32{le|be}.~n"),
	    error;
	true ->
	    {ok, In, Out}
    end.


read_file(InFile) ->
     case file:read_file(InFile) of
	 {ok, Content} ->
	     {X, _} =  unicode:bom_to_encoding(Content),
	     In1 = case X of
		       utf8 -> utf8;
		       {utf16,little} ->  {utf16,little};
		       {utf16,big} ->  {utf16,big};
		       {utf32,little} -> {utf32,little};
		       {utf32,big} ->  {utf32,big};
		       _ ->  unknown
		   end,
	     {ok,Content,In1};
	 {error, enoent} -> 
	     io:format("in-file does not exist.~n"),
	     error;
	 {error, eacces} ->
	     io:format("Missing permission for reading the file, or for searching one of the parent directories.~n"),
	     error;
	 {error, eisdir} ->
	     io:format("The named file is a directory.~n"),
	     error;
	 {error, enotdir} ->
	     io:format("A component of the filename is not a directory.~n"),
	     error;
	 {error, enomem} ->
	     io:format("There is not enough memory for the contents of the file.~n"),
	     error
     end.

write_file([Content, InEncoding, InEncoding1, OutEncoding, OutFile]) ->
    if 
	((InEncoding =:= unknown) and (InEncoding1 =/= unknown)) ->
	    Result = unicode:characters_to_binary(Content, InEncoding1, OutEncoding),
	    file:write_file(OutFile, Result),
	    ok;
	(InEncoding =/= unknown) ->
	    Result = unicode:characters_to_binary(Content, InEncoding, OutEncoding),
	    file:write_file(OutFile, Result),
	    ok;
	((InEncoding =:= unknown) and (InEncoding1 =:= unknown)) ->
	    error
    end.


core([InFile, InEncoding, OutEncoding, OutFile]) ->
    case  check_input([InEncoding, OutEncoding]) of
	error ->
	    ignore;
        {ok, In, Out} ->
	    case read_file(InFile) of
		{ok, Content, In1} -> 
		    case write_file([Content, In, In1, Out, OutFile]) of
			ok ->
			    io:format("file transcoding successful.~n");
			error ->
			    io:format("file transcoding failed.~n");
			true ->
			    ignore
		    end;
		error ->
		    ignore		   
	    end;    
	true ->
	    ignore
    end.


encoding_char_to_atom(X) ->
    case string:to_lower(X) of
	"utf-8" -> utf8;
	"utf-16le" -> {utf16,little};
	"utf-16be" -> {utf16,big};
	"utf-16" -> utf16;
	"utf-32le" -> {utf32,little};
	"utf-32be" -> {utf32,big};
	"utf-32" -> utf32;
	_ -> error			     
    end.
