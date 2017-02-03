-module(zoo_network).

-export([new/1, run/3, mutate/2, clone/1]).
-export_type([zoo_network/0]).

-record(zoo_network, {
          weights :: [[number()]],
          state :: [number()]
         }).

-opaque zoo_network() :: #zoo_network{}.

% @doc returns a new network with random weights
-spec new(pos_integer()) -> zoo_network().
new(Dimension) ->
    #zoo_network{
       weights = generate_weights(Dimension),
       state = blank_state(Dimension)
      }.

% @doc runs the network with specified input and returns the
% network with updated state and the outputs
-spec run([number()], pos_integer(), zoo_network()) -> {zoo_network(), [number()]}.
run(Inputs, OutputNum, Network = #zoo_network{weights = Weights, state = State}) ->
    UpdatedState = update_state(Weights, State, Inputs),
    Output = lists:nthtail(length(UpdatedState) - OutputNum, UpdatedState),
    {Network#zoo_network{state = UpdatedState}, Output}.

% @doc mutate a random weight in a network
-spec mutate(zoo_network(), number()) -> zoo_network().
mutate(Network = #zoo_network{weights = Weights}, Mutations) ->
    MutatedWeights = lists:foldl(fun(_, W) ->
                                         zoo_lists:modify_random(fun mutate_neuron_weights/1, W)
                                 end, Weights, lists:seq(1, Mutations)),
    Network#zoo_network{weights = MutatedWeights}.

% @doc mutate a random weight in a single row
-spec mutate_neuron_weights([number()]) -> [number()].
mutate_neuron_weights(Weights) ->
    zoo_lists:modify_random(fun (_) -> generate_weight() end, Weights).

% @doc make a copy of a network with a blank state
-spec clone(zoo_network()) -> zoo_network().
clone(Network = #zoo_network{weights = Weights}) ->
    Network#zoo_network{state = blank_state(length(Weights))}.

% PRIVATE

% @doc returns updated state from network's weights and previous state
-spec update_state([[number()]], [number()], [number()]) -> [number()].
update_state(Weights, State, Inputs) ->
    Signals = [neuron_signals(NeuronWeights, State) || NeuronWeights <- Weights],
    SignalsWithInputs = add_inputs(Signals, Inputs),
    [afn(lists:sum(NeuronSignals)) || NeuronSignals <- SignalsWithInputs].

% @doc returns neuron's input signals from its inputs' weights and state
-spec neuron_signals([number()], [number()]) -> [number()].
neuron_signals(NeuronWeights, State) ->
    [W * S || {W, S} <- lists:zip(NeuronWeights, State)].

-spec add_inputs([[number()]], [number()]) -> [[number()]].
add_inputs(Signals, Inputs) ->
    add_inputs(Signals, Inputs, []).

-spec add_inputs([[number()]], [number()], [[number()]]) -> [[number()]].
add_inputs([], [], Result) ->
    lists:reverse(Result);
add_inputs([NeuronSignals | Signals], [], Result) ->
    add_inputs(Signals, [], [NeuronSignals | Result]);
add_inputs([NeuronSignals | Signals], [Input | Inputs], Result) ->
    add_inputs(Signals, Inputs, [[Input | NeuronSignals] | Result]).

% @doc neuron activation function
-spec afn(number()) -> number().
afn(X) -> math:tanh(X).

% @doc returns a random weight matrix
-spec generate_weights(pos_integer()) -> [[number()]].
generate_weights(Dimension) ->
    [generate_neuron_weights(Dimension) || _ <- lists:seq(1, Dimension)].

% @doc returns a random weight list
-spec generate_neuron_weights(pos_integer()) -> [number()].
generate_neuron_weights(Dimension) ->
    [generate_weight() || _ <- lists:seq(1, Dimension)].

% @doc returns a random weight
-spec generate_weight() -> number().
generate_weight() ->
    math:pi() * (rand:uniform() * 2 - 1).

% @doc returns a blank state with a given dimension
-spec blank_state(pos_integer()) -> [0].
blank_state(Dimension) ->
    [0 || _ <- lists:seq(1, Dimension)].
