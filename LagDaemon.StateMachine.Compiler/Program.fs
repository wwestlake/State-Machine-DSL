// Learn more about F# at http://fsharp.org

open System
open LagDaemon.StateMachines.Core.Model

[<EntryPoint>]
let main argv =

    let s1 = createState<string,string> "ConnectedState" (StateFunction (fun us -> let (UserState s) = us in printfn "%s" s; UserState <| s  + " state 1 ")) 
    let s2 = createState<string,string> "LoginState" (StateFunction (fun us -> let (UserState s) = us in  printfn "%s" s; UserState <| s + " state 2 "))
    let msg = Message ("ToLogin", "test")
    let trans = createTransition<string,string> "ToLogin" msg (TransitionFunction (fun us msg ->  let (UserState s) = us in  printfn "%s" s; UserState <| s + " transition ")) s2
    let state = addTransitionToState s1 trans

    let newState = run state msg (UserState <| "test")

    printfn "%A" newState
    0 // return an integer exit code
