-module(zoo_network).

-export([new/3, run/2]).

-record(zoo_network, {
          inputs :: pos_integer(),
          outputs :: pos_integer(),
          weights :: [[float()]],
          state :: [float()]
         }).

% @doc returns a new network with either predefined or random weights
-spec new(pos_integer(), pos_integer(), [float()] | pos_integer()) -> #zoo_network{}.
new(Inputs, Outputs, Weights) when is_list(Weights) ->
    State = [0 || _ <- lists:seq(1, length(Weights))],
    #zoo_network{
       inputs = Inputs,
       outputs = Outputs,
       weights = Weights,
       state = State
      };
new(Inputs, Outputs, Dimension) ->
    new(Inputs, Outputs, generate_weights(Dimension)).

% @doc runs the network with specified input and returns the
% network with updated state and the outputs
-spec run([float()], #zoo_network{}) -> {#zoo_network{}, [float()]}.
run(InputValues, Network = #zoo_network{inputs = Inputs, outputs = Outputs,
                                        weights = Weights, state = State}) ->
    StateWithInputs = set_input_values(InputValues, Inputs, State),
    UpdatedState = update_state(Weights, StateWithInputs),
    OutputValues = lists:sublist(UpdatedState, Inputs + 1, Outputs),
    {Network#zoo_network{state = UpdatedState}, OutputValues}.

% PRIVATE

% @doc returns the state with input elements replaced with input values
-spec set_input_values([float()], pos_integer(), [float()]) -> [float()].
set_input_values(InputValues, Inputs, State) ->
    % Inputs = lists:length(InputValues),
    InputValues ++ lists:nthtail(Inputs, State).

% @doc returns updated state from network's weights and previous state
-spec update_state([[float()]], [float()]) -> [float()].
update_state(Weights, State) ->
    Signals = [neuron_signals(NeuronWeights, State) || NeuronWeights <- Weights],
    [afn(lists:sum(NeuronSignals)) || NeuronSignals <- Signals].

% @doc returns neuron's input signals from its inputs' weights and state
-spec neuron_signals([float()], [float()]) -> [float()].
neuron_signals(NeuronWeights, State) ->
    [W * S || {W, S} <- lists:zip(NeuronWeights, State)].

% @doc neuron activation function
-spec afn(float()) -> float().
afn(X) -> math:tanh(X).

% @doc returns a random weight matrix
generate_weights(Dimension) ->
    [generate_neuron_weights(Dimension) || _ <- lists:seq(1, Dimension)].

% @doc returns a random weight list
generate_neuron_weights(Dimension) ->
    [generate_weight() || _ <- lists:seq(1, Dimension)].

% @doc returns a random weight
generate_weight() ->
    math:pi() * (rand:uniform() * 2 - 1).
