-module(dsl_graph).
-compile(export_all).

generate_all_possible_edges(Min_node, Max_node)	->
	[{X,Y} || X <- lists:seq(Min_node,Max_node), Y <- lists:seq(X+1,Max_node)].

build_candidate_edge_list([], _, _, Result)	-> Result;
build_candidate_edge_list([Head|Candidate_list], Edge_list, Directed, Result) ->
	New_result = append_edge(Head, Edge_list, Directed, Result),
	build_candidate_edge_list(Candidate_list, Edge_list, Directed, New_result).

sequential_katz_score(Arg) ->
	[Edge, Beta, Max_l, G, Directed] = Arg,
	{X,Y} = Edge,
	List = lists:seq(1,Max_l),
	lists:foldl(fun(L, Sum) -> 
				math:pow(Beta,L)*nb_path(X, Y, L, G, Directed) + Sum 
		    end, 
		    0.0, 
		    List
		   ).

compute_katz_score(Filename, NbProcess, Beta, Max_l) ->
  {G, Node_list, Edge_list,Directed} = load_graph_gml(Filename),
  Min_node = lists:min(Node_list),
  Max_node = lists:max(Node_list),
  All_possible_edge = generate_all_possible_edges(Min_node, Max_node),
  Candidate = build_candidate_edge_list(All_possible_edge, Edge_list, Directed, []),
  parallel_op(dsl_graph,sequential_katz_score,[Beta, Max_l, G, Directed],NbProcess,Candidate).


test_katz(Filename, NbProcess, Beta, Max_l, Parallel) ->
	{G, Node_list, Edge_list, Directed} = load_graph_gml(Filename),
	Min_node = lists:min(Node_list),
	Max_node = lists:max(Node_list),
	All_possible_edge = generate_all_possible_edges(Min_node, Max_node),
	Candidate = build_candidate_edge_list(All_possible_edge, Edge_list, Directed, []),
	NbEdge = length(Candidate),
	case Parallel of
		2	->
			parallel_katz_score_list(Candidate, NbEdge, NbProcess, Beta, Max_l, G, Directed);
		1	->
			parallel_op(dsl_graph,sequential_katz_score,[Beta, Max_l, G, Directed],NbProcess,Candidate);
		0	->
			sequential_katz_score_list(Candidate, Beta, Max_l, G, Directed, [])
	end.

test_parallel_op(Filename, NbProcess, Beta, Max_l) ->
	{G, Node_list, Edge_list, Directed} = load_graph_gml(Filename),
	Directed = is_directed_gml(Filename),
	Min_node = lists:min(Node_list),
	Max_node = lists:max(Node_list),
	All_possible_edge = generate_all_possible_edges(Min_node, Max_node),
	Candidate = build_candidate_edge_list(All_possible_edge, Edge_list, Directed, []),
	parallel_op(dsl_graph,sequential_katz_score,[Beta, Max_l, G, Directed],NbProcess,Candidate).

empty_function(Z)->
	[X,Y] = Z,
		io:format("~w ~w\n",[X,Y]).

autre_test(X)->
		dsl_graph:empty_function(X).	

parallel_op(Module,Function,Arg_list,NumberOfProcess,Job_list) ->
	Parent_PID = self(),
	Master_Arg = [Job_list,[],Parent_PID,0, length(Job_list)],
        Master_PID = spawn(?MODULE, generic_master, Master_Arg),
	PreArg = {Module,Function,Master_PID},
	Arg = {Arg_list},
	X =  [0|lists:append([PreArg],[Arg])],
	List = lists:seq(1, NumberOfProcess),
        lists:foldl(    fun(_, _) ->
				spawn(?MODULE, generic_slave,X)
                        end,
                        0,
                        List
                     ),	
	receive
		{master, Send_Result} ->
			Result = Send_Result
	end,
	Result.

generic_slave(0, PreArg, Arg) ->
	{_,_,Master_PID} = PreArg,
	Master_PID ! {need_job, {void, self()}},
	generic_slave(1, PreArg, Arg);

generic_slave(1, PreArg, Arg) ->
	{Module,Function,Master_PID} = PreArg,
	{Arg_list} = Arg,
	
        receive
                {master, FirstArg} ->
			X = [FirstArg|Arg_list],
			Partial_res = Module:Function(X),
			Master_PID ! {give_result,{Partial_res,self()}},
			generic_slave(1, PreArg, Arg)
	end.

generic_master(_,Result, Parent_Pid, N, N) ->
	Parent_Pid ! {master, Result};

generic_master([],Result,Parent_Pid,Cur_N,N) ->
	receive
		{give_result, {Katz_scr_res, _}}->
			generic_master([], [Katz_scr_res|Result], Parent_Pid, Cur_N+1, N)
	end;		

generic_master([Head|Tail],Result,Parent_Pid,Cur_N,N) ->
	receive
        	{need_job, {void, Slave_Pid}} ->
			Slave_Pid!{master,Head},
			generic_master(Tail, Result, Parent_Pid, Cur_N, N);

		{give_result, {Partial_rslt, Slave_Pid}}->
			Slave_Pid!{master,Head},
			generic_master(Tail, [Partial_rslt|Result], Parent_Pid, Cur_N+1, N)
	end.

slave_sblst(Master_Pid, Candidate, NbProcess, Beta, Max_l, G, Directed) ->
	NbEdg = length(Candidate),
	Partial_Result = parallel_katz_score_list(Candidate, NbEdg, NbProcess, Beta, Max_l, G, Directed),
	Master_Pid ! {slave, Partial_Result}.

par_katz_sblst(_,N, N,Result,_,_,_,_,_)->
	Result;
par_katz_sblst([],NbOfList, NbReceiv,Result,NbProcess, Beta, Max_l, G, Directed)->
	receive
		{slave, Y} ->
			par_katz_sblst([],NbOfList,NbReceiv+1,lists:append(Y,Result),NbProcess, Beta, Max_l, G, Directed)
	end;
par_katz_sblst(List, NbOfList, 0, Result, NbProcess, Beta, Max_l, G, Directed)->
	NbElt = length(List) div NbOfList,
	Rest  = length(List) rem NbOfList,
%	lists:sublist(List, 4*NbElt, NbElt).
	lists:foldl(fun(I, _) ->
			if Rest == 0 -> 
				X = lists:sublist(List, (I-1)*NbElt+1, NbElt);
			   true -> 
				if I =< Rest -> 
					X = lists:sublist(List, (I-1)*NbElt+I, NbElt+1);
			   	true -> 
					X = lists:sublist(List, (I-1)*NbElt+Rest+1, NbElt)
				end
			end,
			spawn(?MODULE, slave_sblst, [self(), X, NbProcess, Beta, Max_l, G, Directed])
                     end,
                       0,
                        lists:seq(1, NbOfList)
                     ),
	par_katz_sblst([],NbOfList, 0,Result,NbProcess, Beta, Max_l, G, Directed).
	

append_in_file(FileName, Text)->
	file:write_file(FileName, Text, [append]).

loop_execution(FileName, Beta, Max_l, Parallel, Result_file, Nb_process,Repeat) ->
        lists:foldl(    fun(_, _) ->
				io:format("~w process  ",[Nb_process]),
                                execution_time(FileName, Nb_process, Beta, Max_l, Parallel, Result_file)
                        end,
                        0,
                        lists:seq(1, Repeat)
                     ).

execution_time(FileName, Nb_process, Beta, Max_l, Parallel, Result_file)->
	{Time,_} = timer:tc(dsl_graph,test_katz,[FileName, Nb_process, Beta, Max_l, Parallel]),
	Text=lists:concat([FileName,"\t",Nb_process,"\t",Beta,"\t",Max_l,"\t",Parallel,"\t",Time,"\n"]),
	io:format("~w~n",[Time]),
	append_in_file(Result_file,Text).

optimize_execution_time(FileName, NbOfList, NbProcess, Beta, Max_l, Result_file)->
	{Time,_} = timer:tc(dsl_graph,optimize_test_katz,[FileName, NbOfList, NbProcess, Beta, Max_l]),
	Text=lists:concat([FileName,"\t",NbOfList,"\t",NbProcess,"\t",Beta,"\t",Max_l,"\t",Time,"\n"]),
	io:format("~w~n",[Time]),
	append_in_file(Result_file,Text).

optimize_test_katz(Filename, NbOfList, NbProcess, Beta, Max_l)->
	{Node_list, Edge_list, G, Directed} = load_graph_gml(Filename),
	Min_node = lists:min(Node_list),
	Max_node = lists:max(Node_list),
	All_possible_edge = generate_all_possible_edges(Min_node, Max_node),
	Candidate = build_candidate_edge_list(All_possible_edge, Edge_list, Directed, []),
	par_katz_sblst(Candidate, NbOfList, 0, [], NbProcess, Beta, Max_l, G, Directed).



sequential_katz_score_list([], _, _, _, _, Result) ->
	Result;

sequential_katz_score_list([Edge|Edge_list], Beta, Max_l, G, Directed, Result) ->
	Arg = [Edge, Beta, Max_l, G, Directed],
	Katz_score = sequential_katz_score(Arg),
	Ktz_scr_rsult = {Edge,Katz_score},
	sequential_katz_score_list(Edge_list, Beta, Max_l, G, Directed, [Ktz_scr_rsult|Result]).
	


	
append_edge(Edge, Edge_list, 0, Result) ->
	Edge2 = {element(2,Edge),element(1,Edge)},
	Member = lists:member(Edge, Edge_list) or lists:member(Edge2, Edge_list),
	case Member of
		true	-> 
			New_result1 = Result;

		false	->
			New_result1 = [Edge|Result]
	end,
	New_result1;
append_edge(Edge, Edge_list, 1, Result) ->
	Edge2 = {element(2,Edge),element(1,Edge)},
	Member = lists:member(Edge, Edge_list),
	Member2 = lists:member(Edge2,Edge_list),
	case Member of
		true	-> 
			New_result1 = Result;

		false	->
			New_result1 = [Edge|Result]
	end,
	
	case Member2 of
		true	->
			New_result2 = New_result1;
		false	->
			New_result2 = [Edge2|New_result1]
	end,

	New_result2.

     

parallel_katz_score_list(Edge_list, NbOfEdge, Max_process, Beta, Max_l, G, Directed) ->
        Master_Pid = spawn(?MODULE, katz_score_master, [Edge_list, [], self(), 0, NbOfEdge]),
        lists:foldl(    fun(_, _) ->
				spawn(?MODULE, katz_score_slave, [0, Master_Pid, Beta, Max_l, G, Directed])
                        end,
                        0,
                        lists:seq(1, Max_process)
                     ),	
	receive
		{master, Result} ->
			Katz_score_list = Result
	end,
	Katz_score_list.
%for the first time 
katz_score_slave(0, Master_Pid, Beta, Max_l, G, Directed) ->
	Master_Pid ! {need_job, {void, self()}},
	katz_score_slave(1, Master_Pid, Beta, Max_l, G, Directed);

katz_score_slave(1, Master_Pid, Beta, Max_l, G, Directed) ->
        receive
                {master, Edge} ->
			X = element(1,Edge),
			Y = element(2,Edge),
			Katz_scr = parallel_katz_score(X, Y, Beta, Max_l, G, Directed),
			Katz_scr_res = {Edge,Katz_scr},
			Master_Pid ! {give_result,{Katz_scr_res,self()}},
			katz_score_slave(1, Master_Pid, Beta, Max_l, G, Directed)
	end.

katz_score_master(_,Result, Parent_Pid, N, N) ->
		Parent_Pid ! {master, Result};
katz_score_master([],Result,Parent_Pid,Cur_N,N) ->
	receive
		{give_result, {Katz_scr_res, _}}->
			katz_score_master([], [Katz_scr_res|Result], Parent_Pid, Cur_N+1, N)
	end;		
katz_score_master([Edge|Next_Edges],Result,Parent_Pid,Cur_N,N) ->
	receive
                {need_job, {void, Slave_Pid}} ->
			Slave_Pid!{master,Edge},
			katz_score_master(Next_Edges, Result, Parent_Pid, Cur_N, N);

		{give_result, {Katz_scr_res, Slave_Pid}}->
			Slave_Pid!{master,Edge},
			katz_score_master(Next_Edges, [Katz_scr_res|Result], Parent_Pid, Cur_N+1, N)
	end.

slave(X, Y, L, Beta, G, Directed, Master_Pid) ->
		Partial_sum = math:pow(Beta,L)*nb_path(X, Y, L, G, Directed),
		Master_Pid ! {slave, {Partial_sum, self()}}.

master(N,N,Parent_Pid,Result) ->
		Parent_Pid ! {master, Result};

master(Num_process, Max_process, Parent_Pid, Result) ->
        receive
                {slave, {Y, _}} ->
                        master(Num_process+1, Max_process, Parent_Pid, lists:append([Y],Result))
        end.

parallel_katz_score(X, Y, Beta, Max_l, G, Directed) ->
        Master_PID = spawn(?MODULE, master, [0, Max_l, self(), []]),
	L_List = lists:reverse(lists:seq(1,Max_l)),
        lists:foldl(    fun(L, _) ->
                                spawn(?MODULE, slave, [X, Y, L, Beta, G, Directed, Master_PID])
                        end,
                        0,
                        L_List
                     ),	
	receive
		{master, Result} ->
			Katz_score = lists:sum(Result)
	end,
	Katz_score.



		



nb_path(X_id, Y_id, L, G, Directed) ->
	Node_list = path(X_id, Y_id, G, Directed, L, 0, []),
	occurence_count(Y_id, Node_list, 0).

occurence_count(Y_id, Node_list, Current_count) ->
	case Node_list of
		[H|T] -> 
			if H == Y_id -> occurence_count(Y_id, T, Current_count + 1);
			   true -> occurence_count(Y_id, T, Current_count)
			end;

		[]    ->
			Current_count
	end.     
					
			
path(X_id, Y_id, G, Directed, L, Current_L, Marqued_node)->
	Graph = {G,Directed},
	Node_list = node_neighbours(X_id, Graph),
	 	
	if (Current_L + 1) == L ->
				   Node_list;
	   true 		->
				   N_Marqued_node = lists:append([X_id], Marqued_node),
				   Filtered_node = filter_node(Node_list, [Y_id | N_Marqued_node], []),
				   lists:foldl(fun(N_X_id, Res) -> lists:append(path(N_X_id, Y_id, G, Directed, L, (Current_L + 1), N_Marqued_node), Res) end, [], Filtered_node)
				   
	end.
				   




filter_node(List1, List2, ListResult) -> 
	case List1 of
		[H|T] -> 
			Test1 = lists:member(H, List2),

			if Test1  -> 
					filter_node(T, List2, ListResult);
			   true ->
						 N_ListResult = lists:append([H],ListResult),
						 filter_node(T, List2, N_ListResult)
			end;
		[] ->
			ListResult
	end.


node_neighbours(X_id, G) ->
	{Graph, Directed} = G,
	Out_neig = digraph:out_neighbours(Graph, X_id),

	if Directed == 1 ->
			    Out_neig;
	   true 	 -> In_neig = digraph:in_neighbours(Graph, X_id),
		   	    lists:append(In_neig, Out_neig)
	end.

is_directed_gml(Filename) ->
	{ok, Device} = file:open(Filename, [read]),
	try find_directed_attribut_gml(Device)
		after file:close(Device)
	end.
	
find_directed_attribut_gml(Device) ->
	case io:get_line(Device,"") of
		eof -> 0;
		Line -> 
			Str = string:tokens(Line, " "),
			X = hd(Str),
			if X == "directed" ->
				list_to_integer(hd(string:tokens(second(Str),"\n")));
			   true -> find_directed_attribut_gml(Device)
			end
			
	end.

load_graph_gml(Filename)->
	{G, Node_list, Edge_list} = read_graph_gml(Filename),
	Directed = is_directed_gml(Filename),
	{G,Node_list,Edge_list,Directed}.

read_graph_gml(Filename) ->
	{ok, Device} = file:open(Filename, [read]),
	try get_all_lines_gml(Device,[],[],digraph:new())
		after file:close(Device)
	end.

get_all_lines_gml(Device, Node_list, Edge_list,G) ->
	case io:get_line(Device,"") of
		eof -> {G, Node_list, Edge_list};
		Line -> 
			Str = string:tokens(Line, " "),
			X = hd(Str),
			if X == "id" ->
				Node = list_to_integer(hd(string:tokens(second(Str),"\n"))),
				digraph:add_vertex(G,Node),
				get_all_lines_gml(Device,[Node|Node_list], Edge_list,G);				
			   X == "source" ->
				Source = list_to_integer(hd(string:tokens(second(Str),"\n"))),
				get_all_lines_gml(Device, Node_list,[{Source,Source}|Edge_list],G);
			   X == "target" ->
				Target = list_to_integer(hd(string:tokens(second(Str),"\n"))),
				{First_edge, Other_edges} = cut_list(Edge_list),
				Srce = element(1, First_edge),
				digraph:add_edge(G, Srce, Target),	
				get_all_lines_gml(Device, Node_list, [{Srce, Target}|Other_edges],G);
			   true -> get_all_lines_gml(Device,Node_list, Edge_list,G)
			end
			
	end.

generate_head_gml(FileName)->
	lists:concat(["Creator \"Generated by dsl_graph from ",FileName,"\"\ngraph\n[\n directed 0 \n"]).

generate_node_gml(Extremities)->
	[Min, Max] = Extremities,
	Node_num = lists:seq(Min,Max),
        lists:foldl(    fun(N, Edge_part) ->
				X = lists:concat([" node\n [\n  id ",N,"\n ]\n"]),
				[X|Edge_part] 
                        end,
                        [],
                        Node_num
                     ).	
	
is_a_csv_file(FileName)->
	[_,Str] = string:tokens(FileName, "."),
	if Str == "csv" -> true;
	
		   true -> false
	end.

convert_csv_to_gml(FileName,Sep,FirstLine) ->
	CsvFile = is_a_csv_file(FileName),
	if CsvFile == true ->
		{ok, Device} = file:open(FileName, [read]),
	io:format("C'est bon là ~w ~w\n",[FileName,Device]),
		try  convert_csv_to_gml(Device,[],[],FileName,Sep,FirstLine)
			after file:close(Device)
		end;
	   true -> ok
	end.
convert_csv_to_gml(Device,Edge_part,Extremities,FileName,Sep,FirstLine) ->
	case io:get_line(Device,"") of
		eof -> 
			Head_part = generate_head_gml(FileName),
			Node_part = generate_node_gml(Extremities),
			Body      = lists:append(Node_part,Edge_part),
			Text      = lists:concat([Head_part,Body,"]"]),
			[Str,_]   = string:tokens(FileName, "."),
			Gml_file  = lists:concat([Str,".gml"]),
			append_in_file(Gml_file, Text);		
		Line -> 
			if FirstLine == 0 -> 
				Str = string:tokens(Line, Sep),
				[Source, Target, Value] = Str,
				Src =list_to_integer(Source) ,
				Trgt=list_to_integer(Target),
				Min = lists:min([Src|[Trgt|Extremities]]),
				Max = lists:max([Src|[Trgt|Extremities]]),

				Elt = lists:concat([" edge\n [\n  source ",Source,
								       "\n  target ",Target,
						       		"\n  value ",Value,"\n ]\n"]),
				convert_csv_to_gml(Device,[Elt|Edge_part],[Min,Max],FileName,Sep,0);	
		   	true -> 
				convert_csv_to_gml(Device,Edge_part,Extremities,FileName,Sep,0)	
			end
	end.


head([H|_]) -> H.

second([_,X|_]) ->X.

cut_list([H|T]) -> {H, T}. 

same(X,X) -> true;
same(_,_) -> false.

%Primitives pour la manipulation des graphes
exist_edge(G,V1,V2)	->
	{_, _, Edge_list,Directed} = G,

        case Directed of
                0	->
			lists:member({V1,V2}, Edge_list) or lists:member({V2,V1}, Edge_list);

                1	->
			lists:member({V1,V2}, Edge_list)
        end.

exist_vertex(G,V)	->
	{_, Node_list,_,_} = G,
	lists:member(V, Node_list).

add_edge(G, V1, V2) ->
	{Gr, Node_list, Edge_list,Directed} = G,
	Exist = exist_edge(G,V1,V2),
        case Exist of
                true    ->
				New_result1 = {Gr, Node_list, Edge_list,Directed};

                false   ->
				digraph:add_edge(Gr, V1, V2),
				New_result1 = {Gr, Node_list, [{V1,V2}|Edge_list],Directed}
        end,
	New_result1.

add_vertex(G,V) ->
	{Gr, Node_list, Edge_list,Directed} = G,
	Exist = exist_vertex(G,V),
        case Exist of
                true    ->
				New_result1 = {Gr, Node_list, Edge_list,Directed};

                false   ->
				digraph:add_vertex(Gr,V),
				New_result1 = {Gr, [V|Node_list], Edge_list,Directed}
        end,
	New_result1.

edges(G) ->
	{_, _, Edge_list,_} = G,
	Edge_list.

vertices(G) ->
	{_, Node_list, _,_} = G,
	Node_list.
