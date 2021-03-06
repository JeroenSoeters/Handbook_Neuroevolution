-module(exoself).
-compile(export_all).
-include("records.hrl").

map() ->
	map(ffnn).
map(FileName) ->
	Genotype = genotype:load_from_file(FileName),
	spawn(exoself,map,[FileName,Genotype]).
map(FileName,Genotype) ->
	IdsNPIds = ets:new(idsNpids,[set,private]),
	Cx = genotype:read(Genotype,cortex),
	Sensor_Ids = Cx#cortex.sensor_ids,
	Actuator_Ids = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.nids,
	spawn_CerebralUnits(IdsNPIds,cortex,[Cx#cortex.id]),
	spawn_CerebralUnits(IdsNPIds,sensor,Sensor_Ids),
	spawn_CerebralUnits(IdsNPIds,actuator,Actuator_Ids),
	spawn_CerebralUnits(IdsNPIds,neuron,NIds),
	link_Sensors(Genotype,Sensor_Ids,IdsNPIds),
	link_Actuators(Genotype,Actuator_Ids,IdsNPIds),
	link_Neurons(Genotype,NIds,IdsNPIds),
	link_Cortex(Cx,IdsNPIds),
	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),
	genotype:save_to_file(FileName,Genotype).
%The map/1 function maps the tuple encoded genotype into a process based phenotype. The map function expects for the Cx record to be the leading tuple in the tuple list it reads from the FileName. We create an ets table to map Ids to PIds and back again. Since the Cortex element contains all the Sensor, Actuator and Neuron Ids, we are able to spawn each neuron using its own gen function, and in the process construct a map from Ids to PIds. We then use the link_CerebralUnits to link all non Cortex elements to each other by sending each spawned process the information contained in its record, but with Ids converted to PIds where appropriate. Finally, we provide the Cortex process with all the PIds in the NN system by executing the link_Cortex/2 function. Once the NN is up and running, exoself starts its wait until the NN has finished its job and is ready to backup. When the cortex initiates the backup process it sends exoself the updated Input_PIdPs from its neurons. Exoself uses the update_genotype/3 function to update the old genotype with new weights, and then stores the updated version back to its file.

	spawn_CerebralUnits(IdsNPIds,CerebralUnitType,[Id|Ids]) ->
		PId = CerebralUnitType:gen(self(),node()),
		ets:insert(IdsNPIds,{Id,PId}),
		ets:insert(IdsNPIds,{PId,Id}),
		spawn_CerebralUnits(IdsNPIds,CerebralUnitType,Ids);
	spawn_CerebralUnits(_IdsNPIds,_CerebralUnitType,[]) ->
		true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,PId} tuple into our ETS table for later use.

	link_Sensors(Genotype,[SId|Sensor_Ids],IdsNPIds) ->
		R = genotype:read(Genotype,SId),
		SPId = ets:lookup_element(IdsNPIds,SId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,R#sensor.cx_id,2),
		SName = R#sensor.name,
		Fanout_Ids = R#sensor.fanout_ids,
		Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],
		SPId ! {self(), {SId,Cx_PId,SName,R#sensor.vl,Fanout_PIds}},
		link_Sensors(Genotype,Sensor_Ids,IdsNPIds);
	link_Sensors(_Genotype,[],_IdsNPIds) ->
		ok.

	link_Actuators(Genotype,[AId|Actuator_Ids],IdsNPIds) ->
		R = genotype:read(Genotype,AId),
		APId = ets:lookup_element(IdsNPIds,AId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,R#actuator.cx_id,2),
		AName = R#actuator.name,
		Fanin_Ids = R#actuator.fanin_ids,
		Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],
		APId ! {self(),{AId,Cx_PId,AName,Fanin_PIds}},
		link_Actuators(Genotype,Actuator_Ids,IdsNPIds);
	link_Actuators(_Genotype,[],_IdsNPIds) ->
		ok.

	link_Neurons(Genotype,[NId|Neuron_Ids],IdsNPIds) ->
		R = genotype:read(Genotype,NId),
		NPId = ets:lookup_element(IdsNPIds,NId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,R#neuron.cx_id,2),
		AFName = R#neuron.af,
		Input_IdPs = R#neuron.input_idps,
		Output_Ids = R#neuron.output_ids,
		Input_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]),
		Output_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],
		NPId ! {self(),{NId,Cx_PId,AFName,Input_PIdPs,Output_PIds}},
		link_Neurons(Genotype,Neuron_Ids,IdsNPIds);
	link_Neurons(_Genotype,[],_IdsNPIds) ->
		ok.

	convert_IdPs2PIdPs(_IdsNPIds,[{bias,Bias}],Acc) ->
		lists:reverse([Bias|Acc]);
	convert_IdPs2PIdPs(IdsNPIds,[{Id,Weights}|Fanin_IdPs],Acc) ->
		convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),Weights}|Acc]).
%Rhw link_CerebralUnits/2 converts the Ids to PIds using the created IdsNPIds ETS table. At this point all the elements are spawned, and the processes are waiting for their inital states. convert_IdPs2PIdPs/3 converts the IdPs tuples into tuples that use PIds instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	link_Cortex(Cx,IdsNPIds) ->
		Cx_Id = Cx#cortex.id,
		Cx_PId = ets:lookup_element(IdsNPIds,Cx_Id,2),
		SIds = Cx#cortex.sensor_ids,
		AIds = Cx#cortex.actuator_ids,
		NIds = Cx#cortex.nids,
		SPIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- SIds],
		APIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- AIds],
		NPIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- NIds],
		Cx_PId ! {self(),{Cx_Id,SPIds,APIds,NPIds},1000},
		{SPIds,NPIds,APIds}.
%The cortex is initialized to its proper state just as other elements. Because we have not yet implemented a learning algorithm for our NN system, we need to specify when the NN should shutdown. We do this by specifying the total number of cycles the NN should execute before terminating, which is 1000 in this case.

backup_genotype(FileName,IdsNPIds,Genotype,NPIds)->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	update_Genotype(IdsNPIds,Genotype,Neuron_IdsNWeights),
	genotype:save_to_file(Genotype,FileName),
	io:format("Finished updating genotype to file:~p~n",[FileName]).
	get_backup([NPId|NPIds],Acc)->
		NPId ! {self(),get_backup},
			receive
				{NPId,NId,WeightTuples}->
					get_backup(NPIds,[{NId,WeightTuples}|Acc])
			end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/4 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, they are passed through the update_genotype/3 function to produce the genotype with updated weights, which is then saved to file.

	update_Genotype(IdsNPIds,Genotype,[{N_Id,PIdPs}|WeightPs]) ->
		N = genotype:read(Genotype,N_Id),
		io:format("PIdPs:~p~n",[PIdPs]),
		Updated_InputIdPs = convert_PIdPs2IdPs(IdsNPIds,PIdPs,[]),
		U_N = N#neuron{input_idps=Updated_InputIdPs},
		U_Genotype = genotype:write(Genotype,U_N),
		io:format("N:~p~n U_N:~p~n Genotype:~p~n U_Genotype:~p~n",[N,U_N,Genotype,U_Genotype]),
		update_Genotype(IdsNPIds,Genotype,WeightPs);
	update_Genotype(_IdsNPIds,Genotype,[]) ->
		Genotype.

	convert_PIdPs2IdPs(IdsNPIds,[{PId,Weights}|Input_PIdPs],Acc) ->
		convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),Weights}|Acc]);
	convert_PIdPs2IdPs(_IdsNPIds,[Bias],Acc) ->
		lists:reverse([{bias,Bias}|Acc]).
%For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id and updates its weights. The convert_PIdPs2IdPs/3 performs the conversion from PIdPs to Ids of every {PId,Weights} tuple in the Input_PIdPs list. The updated Genotype is then returned back to the caller.
