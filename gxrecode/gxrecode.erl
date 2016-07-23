#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    io:format("Usage: gxrecode <in-file> <in-encoding> <out-encoding> <out-file>~n");

main(["--help"]) ->
    io:format("Usage: gxrecode <in-file> <in-encoding> <out-encoding> <out-file>~n");

main([InFile, OutEncoding]) ->
    core([InFile, unknown, OutEncoding, InFile]);
main([InFile, InEncoding, OutEncoding]) ->
    core([InFile, InEncoding, OutEncoding, InFile]);
main([InFile, OutEncoding, OutFile]) ->
    core([InFile, unknown, OutEncoding, OutFile]);
main([InFile, InEncoding, OutEncoding, OutFile]) ->
    core([InFile, InEncoding, OutEncoding, OutFile]).

core([InFile, InEncoding, OutEncoding, OutFile]) ->
    if 
	(InEncoding =/= unknown) -> 
	    In = encoding_char_to_atom(InEncoding);
	true ->
	    In = unknown
    end,
    Out = encoding_char_to_atom(OutEncoding),
    if 
	In =:= error ->
	    io:format("Unsupported in-encoding,use utf-8/16{le|be}/32{le|be}.~n");
	Out =:= error ->
	    io:format("Unsupported out-encoding,use utf-8/16{le|be}/32{le|be}.~n");
	true ->
	    case file:read_file(InFile) of
		{ok, Content} ->
		    {X, _} =  unicode:bom_to_encoding(Content),
		    In1 = case X of
			      utf8 -> utf8;
			      {utf16,little} ->  {utf16,little};
			      {utf16,big} ->  {utf16,big};
			      {utf32,little} -> {utf32,little};
			      {utf32,big} ->  {utf32,big};
			      _ ->  error
			  end,
		    if 
			((In =:= unknown) and (In1 =/= error)) ->
			    Result = unicode:characters_to_binary(Content, In1, Out),
			    file:write_file(OutFile, Result),
			    io:format("file transcoding successful.~n");
			(In =/= unknown) ->
			    Result = unicode:characters_to_binary(Content, In, Out),
			    file:write_file(OutFile, Result),
			    io:format("file transcoding successful.~n");
			((In =:= unknown) and (In1 =:= error)) ->
			    io:format("aaaaaaaaaaaa.~n")
		    end;
		{error, enoent} -> 
		    io:format("in-file does not exist.~n");
		{error, eacces} ->
		    io:format("Missing permission for reading the file, or for searching one of the parent directories.~n");
		{error, eisdir} ->
		    io:format("The named file is a directory.~n");
		{error, enotdir} ->
		    io:format("A component of the filename is not a directory.~n");
		{error, enomem} ->
		    io:format("There is not enough memory for the contents of the file.~n")
	    end    
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
