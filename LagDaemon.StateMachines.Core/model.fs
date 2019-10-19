namespace LagDaemon.StateMachines.Core

module Model =
    open System

    /// represents a state table used to generate a state machine
    type StateTable<'a,'b> = StateTable of (string * ('a -> 'a) * (string * string * 'b * ('a -> 'a) * string) list) list

    /// Represents the User State being processed by the state machine
    type UserState<'U> = UserState of 'U

    /// Represents a function that will be called when the state is entered
    type StateFunction<'U> = StateFunction of (UserState<'U> -> UserState<'U>)

    /// Represents a function that is called upon tranitioning from one state to another
    type TransitionFunction<'U, 'M> = TransitionFunction of (UserState<'U> -> Message<'M> -> UserState<'U>)

    /// Represents a transition from one state to another with a message that activates the
    /// transition
    and Transition<'U, 'M> = Transition of (string * Message<'M> * TransitionFunction<'U,'M> * State<'U, 'M>)

    /// Represents a particular state with a list of possible transitions
    and  State<'U,'M> = State of (string * StateFunction<'U> * Transition<'U,'M> list) 

    /// Represents a Message that activates a transition
    and Message<'M> = Message of (string * 'M)

    type StateMachineResult<'U,'M> =
        | StateChange of (UserState<'U> * State<'U,'M>)
        | FailureState of (UserState<'U> * State<'U,'M> * string)


    let createState<'U,'M> name (func:StateFunction<'U>) =
        let (trans:Transition<'U,'M> list) = []
        State (name, func, trans)

    let createTransition<'U,'M> name (message:Message<'M>) (transFunc:TransitionFunction<'U,'M>) newState =
        Transition (name, message, transFunc, newState)

    let addTransitionToState state transition =
        let (State (name, sfunc, transList)) = state in State (name, sfunc, transition :: transList)


    let testStateTable : StateTable<'a,'b> = StateTable [
        ("state1", (fun us -> us), 
            [ 
                ("trans1", "message2", "", (fun us -> us), "state2") 
                ("trans2", "message3", "", (fun us -> us), "state3") 
                ("trans3", "message4", "", (fun us -> us), "state4")
                ("trans4", "message5", "", (fun us -> us), "state5") 
            ]
        )
        ("state2", (fun us -> us), 
            [
                ("trans5", "message1", "", (fun us -> us), "state1")
            ]
        )
        ("state3", (fun us -> us), [("trans6", "message1", "", (fun us -> us), "state1")])
        ("state4", (fun us -> us), [("trans7", "message1", "", (fun us -> us), "state1")])
        ("state6", (fun us -> us), [("trans8", "message1", "", (fun us -> us), "state1")])
    ]

    let private makeStates (StateTable st) =
        let rec inner (StateTable rest) map =
            match rest with
            | [] -> map
            | (name, func, _)::tail -> inner (StateTable tail) (Map.add name (createState name (StateFunction func) ) map ) 

        inner (StateTable st) Map.empty
    
    let private makeMessages (StateTable st) =
        let rec inner (StateTable rest) map =
            match rest with
            | [] -> map
            | (_, _, trans)::tail -> 
                let rec inner2 rest2 map =
                    match rest2 with 
                    | [] -> map
                    | (_,mname,msg,_,_)::tail2 ->  inner2 tail2 (Map.add mname (Message (mname,msg)) map )
                inner2 trans map
        inner (StateTable st) Map.empty
                

    let loadStateTable (st:StateTable<'a,'b>) =
        

        ()

    let run<'U, 'M> (state:State<'U, 'M>) (message:Message<'M>) userState =
        let (State (sname, (StateFunction sfunc), transList)) = state
        let (Message (mname, msg)) = message
        let rec loop rest =
            match rest with
            | [] -> FailureState (
                                 userState, state, sprintf "Message is invalid for state '%s', no transition found for message '%A'." mname message   
                                )
            | (Transition (tname, msg, (TransitionFunction tfunc), newState))::tail -> 
                if tname = mname 
                then 
                    let s1 = tfunc userState msg |> sfunc

                    StateChange (s1, newState)        
                else
                    loop tail
        loop transList
