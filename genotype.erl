-module(genotype).
-compile(export_all).

save_genotype(FileName,GenoType) ->
	TId = ets:new(FileName,[public,set,{kepypos,2}]),
	[ets:insert(TId,Element) || Element <- Genotype],
	ets:tab2file(TId,FileName).
%The save_genotype/2 function expects that the Genotype is a list composed of the neuron, sensor, actuator, cortex and exoself elements. The function creates a new ets table, writes all the element representing tuples from the Genotype list to the ets table, and then writes the ets table to file.

save_to_file(Genotype,FileName) ->
	ets:tab2file(Genotype,FileName).
%The save_to_file/2 function saves the ets table bu the name Genotype to the file by the name FileName.

load_from_file(FileName) ->
	{ok,TId} = ets:file2tab(FileName),
	TId.
%The load_form_file/1 loads an ets representing file by the name FileName, returning the ets table id to the caller.

read(TId,key) ->
	[R] = ets:lookup(TId,Key),
	R.
%The read/2 function reads a record associated with Key from the ets table with the id TId, returning the record R to the caller. It expects that only a single record exists with the specified Key.

write(TId,R) ->
	ets:insert(TId,R).
%The function write/2 writes the record R to the ets table with the id TId.

print(FileName) ->
	Genotype = load_from_file(FileName),
	Cx = read(Genotype,cortex),
	SIds = Cx#cortex.sensor_ids,
	NIds = Cx#cortex.nids,
	AIds = Cx#cortex.actuator_ids,
	io:format("~p~n",[Cx]),
	[io:format("~p~n",[read(Genotype,Id)] || Id <- SIds]]),
	[io:format("~p~n",[read(Genotype,Id)] || Id <- NIds]]),
	[io:format("~p~n",[read(Genotype,Id)] || Id <- AIds]]).
%The function print/1 reads a stored Genotype from the file FileName, and then prints to console all the elements making up the NN's genotype.

