// Simple events

let myEvent = Event<string>()
let myStream = myEvent.Publish

myStream
|> Observable.subscribe(fun x -> printfn "%A" x)

myEvent.Trigger("hello")
