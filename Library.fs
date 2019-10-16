module Library

open System
open System.IO
open System.Text.RegularExpressions
open System.Net

// Utils

let private reverse (word:string) =
    new string(word
    |> Seq.toList
    |> List.rev
    |> List.toArray)

let private convertToString = List.map (sprintf "%i") >> String.concat ""

// -------------------------------------------------

// Collatz function: n => even n -> n / 2, odd n -> n * 3 + 1. Conjecture: will always return to 1.
// The longest progression for any starting number less than 100 billion is 75,128,138,247, which has 1228 steps.
let Collatz x =
    let rec loop acc counter seq =
        match acc with
        | acc when acc < 1I -> (0I, [])
        | acc when acc = 1I -> (counter, List.rev (1I :: seq))
        | acc when acc % 2I = 0I -> loop (acc / 2I) (counter + 1I) (acc :: seq)
        | _ -> loop (acc * 3I + 1I) (counter + 1I) (acc :: seq)
    loop x 0I []

let steps, numbers = Collatz 75128138247I

printfn "%A" (Collatz 75128138247I)

// -------------------------------------------------

let FizzBuzz upper =
    {1 .. upper}
    |> Seq.map (fun x ->
        match x with
        | x when x % 3 = 0 && x % 5 = 0 -> "FizzBuzz"
        | x when x % 3 = 0 -> "Fizz"
        | x when x % 5 = 0 -> "Buzz"
        | _ -> x.ToString())

// pass 100 into FizzBuzz then iterate through FizzBuzz
// list Reversing and then printing each element
1000 |> FizzBuzz |> Seq.iter (reverse >> printfn "%s")

// -------------------------------------------------

let IsPalindrome word =
    word = reverse word

// -------------------------------------------------

let CountVowels (word:string) =
    word.ToUpper()
    |> Seq.toList
    |> List.fold (fun acc -> function
        | 'A' | 'E' | 'I' | 'O' | 'U' -> acc + 1
        | _ -> acc) 0

// -------------------------------------------------

let private rotateString ls step =
    List.splitAt step (ls |> List.ofSeq)
    |> fun (x,y) -> List.append y x
    |> Array.ofList
    |> String

let private alph = "abcdefghijklmnopqrstuvwxyz"

let private transformedAlpha letter =
    rotateString alph (alph |> Seq.findIndex(fun x -> x = letter))

let private applyCipher (key : string) (message : string) =
    message
    |> String.mapi(fun i c ->
        let index = i % key.Length
        let newAlph = transformedAlpha (key.ToCharArray().[index])
        newAlph.ToCharArray().[alph.IndexOf(c)])

let private removeCipher (key : string) (message : string) =
    message
    |> String.mapi(fun i c ->
        let index = i % key.Length
        let newAlph = transformedAlpha (key.ToCharArray().[index])
        alph.ToCharArray().[newAlph.IndexOf(c)])

let encrypt (word : string) =
    let args = word.ToLower().Split(' ')
    applyCipher args.[0] args.[1]

let decrypt (word : string) =
    let args = word.ToLower().Split(' ')
    removeCipher args.[0] args.[1]

let encryptTests = ["snitch thepackagehasbeendelivered";
    "bond theredfoxtrotsquietlyatmidnight";
    "train murderontheorientexpress";
    "garden themolessnuckintothegardenlastnight"]

let encryptAnswers = ["lumicjcnoxjhkomxpkwyqogywq";
    "uvrufrsryherugdxjsgozogpjralhvg";
    "flrlrkfnbuxfrqrgkefckvsa";
    "zhvpsyksjqypqiewsgnexdvqkncdwgtixkx"]

let decryptTests = ["cloak klatrgafedvtssdwywcyty";
    "python pjphmfamhrcaifxifvvfmzwqtmyswst";
    "moore rcfpsgfspiecbcc"]

let decryptAnswers = ["iamtheprettiestunicorn";
    "alwayslookonthebrightsideoflife";
    "foryoureyesonly"]

// encryptTests |> Seq.iteri(fun i x -> printfn "%b" (encrypt x = encryptAnswers.[i]))
// decryptTests |> Seq.iteri(fun i x -> printfn "%b" (decrypt x = decryptAnswers.[i]))

// -------------------------------------------------

let fasterPaperfold steps =
    let rec result arr acc =
        if acc = 0 then
            arr
        else
            let mutable r = [1]
            arr |> List.iteri(fun i e -> if i % 2 = 0 then r <- List.append r [e; 0] else r <- List.append r [e; 1])
            result r (acc - 1)
    (result [1] steps) |> (List.map (sprintf "%i") >> String.concat "")

let private iterator (list : int list) =
    let rec loop (index : int) (value : int list) (acc : int list) =
        match index with
        | index when index = value.Length -> acc
        | index when index % 2 = 0 -> loop (index + 1) value (List.append acc [value.[index]; 0])
        | index when index % 2 = 1 -> loop (index + 1) value (List.append acc [value.[index]; 1])
        | _ -> acc
    loop 0 list [1]

let slowerPaperfold steps =
    let rec result arr acc =
        match acc with
        | 0 -> arr
        | _ -> result (iterator arr) (acc - 1)
    (result [1] steps) |> (List.map (sprintf "%i") >> String.concat "")

// convertToString (paperfold 8) = "1101100111001001110110001100100111011001110010001101100011001001110110011100100111011000110010001101100111001000110110001100100111011001110010011101100011001001110110011100100011011000110010001101100111001001110110001100100011011001110010001101100011001001110110011100100111011000110010011101100111001000110110001100100111011001110010011101100011001000110110011100100011011000110010001101100111001001110110001100100111011001110010001101100011001000110110011100100111011000110010001101100111001000110110001100100"

// -------------------------------------------------

let data =
    File.ReadAllLines "FSNetCore/src/Library/NCAA.txt"
    |> Seq.map(fun s ->
        let line = Regex.Split ((s.Substring (12, 56)).Replace ("@", " "), @"\d+")
        (line.[0].Trim(), line.[1].Trim()))

let testData = [
    ("Some Random", "Arizona Chr");
    ("Walsh", "Westmont");
    ("Westmont", "Card Stritch");
    ("Arizona Chr", "Card Stritch");
    ("Card Stritch", "Maranatha Bap")]

let findTeamsThatBeat team =
    data
    |> Seq.fold(fun acc teams ->
        if (snd teams = team) then
            (fst teams) :: acc
        else
            acc) []
    |> set
    |> List.ofSeq

//findTeamsThatBeat "Card Stritch"

// Needs work... looping is super slow
// Should probs be doing a depth first search on a directed graph
let getAllWinners team  =
    let rec findTeams allTeams newTeams =
        let winners =
            newTeams
            |> List.fold(fun ac v ->
                List.append ac (findTeamsThatBeat v)
                |> set
                |> List.ofSeq) []
            |> List.filter(fun x -> not (allTeams |> List.contains x))
        if (winners.Length = 0) then
            allTeams
        else
            findTeams (List.append allTeams winners) winners
    findTeams [] [team]

let res = getAllWinners "Card Stritch"

// -------------------------------------------------

// recaman sequence
// a(0) = 0; for n > 0, a(n) = a(n-1) - n if positive and not already in the sequence, otherwise a(n) = a(n-1) + n.
let recaman n =
    let rec loop index acc value =
        match index with
        | index when index = n + 1 -> value
        | index when value - index > 0 && not (List.contains (value - index) acc) ->
            loop (index + 1) ((value - index) :: acc) (value - index)
        | _ -> loop (index + 1) ((value + index) :: acc) (value + index)
    loop 1 [0] 0

// recaman 10000

// -------------------------------------------------

let lightOn (data : string) =
    let sorted =
        data.Split('\n')
        |> Array.map(fun x -> (int (x.Split(' ').[0]), int (x.Split(' ').[1])))
        |> Array.sort
    let seed = [0; fst sorted.[0]; 0]
    let result =
        sorted
        |> Array.fold(fun (acc : int list) inp ->
            let enter = max (fst inp) acc.[2]
            let leave = max (snd inp) acc.[2]
            let total = acc.[0] + leave - enter
            [total; enter; leave]) seed
    result.[0]

// lightOn "15 18
// 13 16
// 9 12
// 3 4
// 17 20
// 9 11
// 17 18
// 4 5
// 5 6
// 4 5
// 5 6
// 13 16
// 2 3
// 15 17
// 13 14"

//shorter solution from /r/DailyProgrammer
let tupList1 = [(2, 4);(3, 6);(1, 3);(6, 8)]
let tupList2 = [(6, 8);(5, 8);(8, 9);(5, 7);(4, 7)]
let tupList3 = [(15, 18);(13, 16);(9 ,12);(3 ,4);(17, 20);(9 ,11);(17, 18);(4 ,5);(5 ,6);(4 ,5);(5 ,6);(13, 16);(2 ,3);(15, 17);(13, 14)]

let calc x =
    x
    |> List.collect(fun (a,b) -> [a .. (b-1)])
    |> List.distinct

// [(calc tupList1).Length; (calc tupList2).Length; (calc tupList3).Length]

// -------------------------------------------------

type LightSwitch =
    | On
    | Off
    | Adjust of float

let light = Adjust(2.5)

type Thing =
    { Time: string;
        TODO: int }

type Other(name: string, jobId: int) =
    let secret = name + (string jobId)
    member __.Name = name
    member __.JobId = jobId
    member __.SayHiTo someName =
        "Hello " + someName + " my name is " + secret

let hello =
    { Time = "1";
        TODO = 5 }

// -------------------------------------------------

type FlightData =
    { Time: int;
        States: string List List }

type Flight(data:string list) =
    member __.Icao24 = data.[0]
    member __.Callsign = data.[1].Trim()
    member __.OriginCountry = data.[2]
    member __.TimePosition = int data.[3]
    member __.LastContact = int data.[4]
    member __.Longitude = if isNull data.[5] then None else Some (float data.[5])
    member __.Latitude = if isNull data.[6] then None else Some (float data.[6])
    member __.GeoAltitude = if isNull data.[7] then None else Some (float data.[7])
    member __.OnGround = Convert.ToBoolean data.[8]
    member __.Velocity = float data.[9]
    member __.Heading = float data.[10]
    member __.VerticalRate = float data.[11]
    member __.Sensors = data.[12] //should be int list but we're probably not going to use it anyway...
    member __.BaroAltitude = float data.[13]
    member __.Squawk = data.[14]
    member __.Spi = Convert.ToBoolean data.[15]
    member __.PositionSource = int data.[16]

let deserialiseFlightData data =
    (Newtonsoft.Json.JsonConvert.DeserializeObject<FlightData> data).States
    |> List.map(fun x -> Flight(x))

let fetchUrl url =
    let stream = WebRequest.Create(Uri(url)).GetResponse().GetResponseStream()
    let reader = new StreamReader(stream)
    reader.ReadToEnd()

let allFlights =
    let data = fetchUrl @"https://opensky-network.org/api/states/all"
    File.WriteAllText ("FSNetCore/src/Library/flightdata.json", data)
    deserialiseFlightData data

// let allFlights =
//     File.ReadAllText "FSNetCore/src/Library/flightdata.json"
//     |> deserialiseFlightData

let radians angle =
    Math.PI * angle / 180.0

let distance (lat1:float, lon1:float) (lat2:float, lon2:float) =
    let r = 6371e3
    let thi1 = radians lat1
    let thi2 = radians lat2
    let dThi = radians (lat2 - lat1)
    let dLam = radians (lon2 - lon1)
    let a = Math.Sin (dThi/2.0) * Math.Sin (dThi/2.0) + Math.Cos (thi1) * Math.Cos (thi2) * Math.Sin (dLam/2.0) * Math.Sin (dLam/2.0)
    let c = 2.0 * Math.Atan2 (Math.Sqrt a, Math.Sqrt (1.0-a))
    r * c

let eifelTower = (48.8584, 2.2945)

let JFKAirport = (40.6413, 73.7781)

let findClosest (flights:Flight list) (coords:float * float) =
    flights
    |> List.map(fun x ->
        if x.Latitude.IsNone || x.Longitude.IsNone then
            (x, float Int32.MaxValue)
        else
            (x, distance coords (x.Latitude.Value, x.Longitude.Value)))
    |> List.reduce(fun acc item -> if snd acc < snd item then acc else item)

let printResult =
    let closest = findClosest allFlights JFKAirport

    let callsign = if (fst closest).Callsign = "" then "??????" else (fst closest).Callsign
    let lat = (fst closest).Latitude.Value
    let long = (fst closest).Longitude.Value
    let geoAlt = if (fst closest).GeoAltitude.IsNone then 0.0 else (fst closest).GeoAltitude.Value

    sprintf "The closest plane to JFK Airport is:
    Distance: %fm
    Callsign: %s
    Position: %f N %f E
    Altitude: %fm
    Origin: %s
    ICAO24 ID: %s" (snd closest) callsign lat long geoAlt (fst closest).OriginCountry (fst closest).Icao24

// -------------------------------------------------

let countScore (scores:string) =
    scores.ToLower()
    |> Set
    |> Seq.map(fun c ->
        scores
        |> Seq.fold(fun acc x ->
            if x = c then
                (c, snd acc + 1)
            else if x = (System.Char.ToUpper c) then
                (c, snd acc - 1)
            else
                acc) (c, 0))
    |> Seq.sortByDescending snd
    |> Seq.map(fun x -> sprintf "%c:%i" (fst x) (snd x))
    |> String.concat " "

// countScore "EbAAdbBEaBaaBBdAccbeebaec"
