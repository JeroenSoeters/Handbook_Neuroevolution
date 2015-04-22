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
