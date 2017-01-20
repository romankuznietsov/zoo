-module(zoo_network).

-export([new/3, run/2, mutate/1, clone/1]).
-export_type([zoo_network/0]).

-record(zoo_network, {
          inputs :: pos_integer(),
          outputs :: pos_integer(),
          weights :: [[number()]],
          state :: [number()]
         }).

-opaque zoo_network() :: #zoo_network{}.

% @doc returns a new network with random weights
-spec new(pos_integer(), pos_integer(), pos_integer()) -> zoo_network().
new(Inputs, Outputs, Dimension) ->
    #zoo_network{
       inputs = Inputs,
       outputs = Outputs,
       weights = generate_weights(Dimension),
       state = blank_state(Dimension)
      }.

% @doc runs the network with specified input and returns the
% network with updated state and the outputs
-spec run([number()], zoo_network()) -> {zoo_network(), [number()]}.
run(InputValues, Network = #zoo_network{inputs = Inputs, outputs = Outputs,
                                        weights = Weights, state = State}) ->
    StateWithInputs = set_input_values(InputValues, Inputs, State),
    UpdatedState = update_state(Weights, StateWithInputs),
    OutputValues = lists:sublist(UpdatedState, Inputs + 1, Outputs),
    {Network#zoo_network{state = UpdatedState}, OutputValues}.

% @doc mutate a random weight in a network
-spec mutate(zoo_network()) -> zoo_network().
mutate(Network = #zoo_network{weights = Weights}) ->
    MutatedWeights = zoo_lists:modify_random(fun mutate_neuron_weights/1, Weights),
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

% @doc returns the state with input elements replaced with input values
-spec set_input_values([number()], pos_integer(), [number()]) -> [number()].
set_input_values(InputValues, Inputs, State) ->
    % Inputs = lists:length(InputValues),
    InputValues ++ lists:nthtail(Inputs, State).

% @doc returns updated state from network's weights and previous state
-spec update_state([[number()]], [number()]) -> [number()].
update_state(Weights, State) ->
    Signals = [neuron_signals(NeuronWeights, State) || NeuronWeights <- Weights],
    [afn(lists:sum(NeuronSignals)) || NeuronSignals <- Signals].

% @doc returns neuron's input signals from its inputs' weights and state
-spec neuron_signals([number()], [number()]) -> [number()].
neuron_signals(NeuronWeights, State) ->
    [W * S || {W, S} <- lists:zip(NeuronWeights, State)].

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
