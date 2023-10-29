// cronota Version 0.2.0
// https://github.com/taidalog/cronota
// Copyright (c) 2023 taidalog
// This software is licensed under the MIT License.
// https://github.com/taidalog/cronota/blob/main/LICENSE
namespace Cronota

open System
open Browser.Dom
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fermata

module App =
    type RunningStatus =
        | NotStarted = 0
        | Running = 1
        | Stopping = 2
        | Finished = 4

    let status = [ "NotStarted"; "Running"; "Stopping"; ""; "Finished" ]

    [<Emit("setInterval($0, $1)")>]
    let setInterval (callback: unit -> unit) (interval: int) : int = jsNative

    [<Emit("clearInterval($0)")>]
    let clearInterval (intervalID: int) : unit = jsNative

    let mutable startTimeStop = DateTime.MinValue
    let mutable startTimeNext = DateTime.MinValue
    let mutable intervalId = -1
    let mutable timeAccStop = TimeSpan.Zero
    let mutable timeAccNext = TimeSpan.Zero
    let mutable notes: (int * string) list = []
    let mutable runningStatus = RunningStatus.NotStarted

    let timeSpanToDisplay (timeSpan: TimeSpan) =
        let h = timeSpan.Hours |> string |> String.padLeft 2 '0'
        let m = timeSpan.Minutes |> string |> String.padLeft 2 '0'
        let s = timeSpan.Seconds |> string |> String.padLeft 2 '0'
        let ms = timeSpan.Milliseconds |> string |> String.padLeft 3 '0'
        $"%s{h}:%s{m}:%s{s}.%s{ms}"

    let countUp () =
        intervalId <-
            setInterval
                (fun _ ->
                    let elapsedTime = DateTime.Now - startTimeStop + timeAccStop
                    document.getElementById("timer").innerText <- timeSpanToDisplay elapsedTime)
                10

    let logsTableHeader =
        """
        <tr>
            <th class="logs-table-no" translate="no">No.</th>
            <th class="logs-table-time" translate="no">Time</th>
            <th class="logs-table-note" translate="no">Note</th>
        </tr>
        """

    let start event =
        let notesEl = document.getElementById "notes" :?> HTMLInputElement

        let notesArray =
            notesEl.value
            |> String.split '\n'
            |> List.filter ((<>) "")
            |> List.mapi (fun i x -> i + 1, x)

        if List.length notesArray = 0 then
            notesEl.focus ()
        else
            match runningStatus with
            | RunningStatus.NotStarted
            | RunningStatus.Finished ->
                timeAccStop <- TimeSpan.Zero
                timeAccNext <- TimeSpan.Zero
                notes <- notesArray

                (document.getElementById "timer").innerText <- timeSpanToDisplay TimeSpan.Zero
                (document.getElementById "logsTable").innerHTML <- logsTableHeader
                document.getElementById("currNote").innerText <- $"%d{fst (List.head notes)}, %s{snd (List.head notes)}"

                if List.length notes > 1 then
                    document.getElementById("nextNote").innerText <-
                        $"%d{fst (List.item 1 notes)}, %s{snd (List.item 1 notes)}"
                else
                    ()

                notesEl.disabled <- true

                [ ("mainButton", true); ("stopButton", false); ("nextButton", false) ]
                |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

                runningStatus <- RunningStatus.Running
                printfn "%s" $"""runningStatus: %s{List.item (int runningStatus) status}"""

                startTimeStop <- DateTime.Now
                startTimeNext <- DateTime.Now
                countUp ()
            | RunningStatus.Running -> ()
            | RunningStatus.Stopping ->
                notesEl.disabled <- true

                [ ("mainButton", true); ("stopButton", false); ("nextButton", false) ]
                |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

                runningStatus <- RunningStatus.Running
                printfn "%s" $"""runningStatus: %s{List.item (int runningStatus) status}"""

                startTimeStop <- DateTime.Now
                startTimeNext <- DateTime.Now
                countUp ()
            | _ -> ()

    document.getElementById("mainButton").onclick <- start

    let stop event =
        match runningStatus with
        | RunningStatus.Running ->
            clearInterval intervalId
            timeAccStop <- timeAccStop + (DateTime.Now - startTimeStop)
            timeAccNext <- timeAccNext + (DateTime.Now - startTimeNext)

            [ ("notes", false)
              ("mainButton", false)
              ("stopButton", true)
              ("nextButton", true) ]
            |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

            runningStatus <- RunningStatus.Stopping
            printfn "%s" $"""runningStatus: %s{List.item (int runningStatus) status}"""
        | _ -> ()

    document.getElementById("stopButton").onclick <- stop

    let reset event =
        match runningStatus with
        | RunningStatus.Running -> stop ()
        | RunningStatus.Stopping
        | RunningStatus.Finished ->
            [ ("currNote", "")
              ("nextNote", "")
              ("timer", timeSpanToDisplay TimeSpan.Zero) ]
            |> List.iter (fun (x, y) -> (document.getElementById x).innerText <- y)

            (document.getElementById "logsTable").innerHTML <- logsTableHeader

            timeAccStop <- TimeSpan.Zero
            timeAccNext <- TimeSpan.Zero

            (document.getElementById "mainButton" :?> HTMLButtonElement).disabled <- false
            runningStatus <- RunningStatus.NotStarted
            printfn "%s" $"""runningStatus: %s{List.item (int runningStatus) status}"""
        | _ -> ()

    document.getElementById("resetButton").onclick <- reset

    let next event =
        match runningStatus with
        | RunningStatus.NotStarted -> start ()
        | RunningStatus.Running ->
            let td x y z =
                $"""
                <tr>
                    <td class="logs-table-no">%s{x}</td>
                    <td class="logs-table-time">%s{y}</td>
                    <td class="logs-table-note">%s{z}</td>
                </tr>
                """

            let logsTable = document.getElementById "logsTable" :?> HTMLTableElement

            logsTable.innerHTML <-
                logsTable.innerHTML
                + (td
                    (string (fst (List.head notes)))
                    (timeSpanToDisplay (timeAccNext + (DateTime.Now - startTimeNext)))
                    (snd (List.head notes)))

            startTimeNext <- DateTime.Now
            timeAccNext <- TimeSpan.Zero

            if List.length notes = 1 then
                logsTable.innerHTML <-
                    logsTable.innerHTML
                    + (td "TOTAL" (document.getElementById("timer").innerText) "END")

                document.getElementById("currNote").innerText <- ""
                stop ()
                (document.getElementById "mainButton" :?> HTMLButtonElement).disabled <- false

                runningStatus <- RunningStatus.Finished
                printfn "%s" $"""runningStatus: %s{List.item (int runningStatus) status}"""
            else
                notes <- List.tail notes

                document.getElementById("currNote").innerText <-
                    $"""%d{fst (List.head notes)}, %s{snd (List.head notes)}"""

                document.getElementById("nextNote").innerText <-
                    if List.length notes > 1 then
                        $"""%d{fst (List.item 1 notes)}, %s{snd (List.item 1 notes)}"""
                    else
                        ""
        | _ -> ()

    document.getElementById("nextButton").onclick <- next

    window.addEventListener (
        "keydown",
        fun (event: Event) ->
            let event = event :?> KeyboardEvent
            let key = event.key
            printfn "%s" key

            let notesEl = document.getElementById ("notes") :?> HTMLInputElement
            let helpWindow = document.getElementById "helpWindow"

            let isHelpWindowActive =
                helpWindow.classList
                |> (fun x -> JS.Constructors.Array?from(x))
                |> Array.contains "active"

            if document.activeElement = notesEl then
                if key = "Escape" then notesEl.blur () else ()
            else if isHelpWindowActive && key = "Escape" then
                helpWindow.classList.toggle "active" |> ignore
            else if not isHelpWindowActive && key = "?" then
                helpWindow.classList.toggle "active" |> ignore
            else
                match key with
                | "Enter" ->
                    if notesEl.value = "" then
                        notesEl.focus ()
                        event.preventDefault ()
                    else
                        start ()
                | "Escape" -> stop ()
                | "Delete" -> reset ()
                | "ArrowRight" -> next ()
                | "ArrowLeft" -> ()
                | "\\" ->
                    notesEl.focus ()
                    event.preventDefault ()
                | _ -> ()
    )

    (document.getElementById "timer").innerText <- timeSpanToDisplay TimeSpan.Zero

    [ ("mainButton", "Start watch (Enter)")
      ("stopButton", "Stop watch (Escape)")
      ("resetButton", "Reset watch and logs (Delete)")
      ("prevButton", "Previous note (<)")
      ("nextButton", "Next note (>)")
      ("helpButton", "Help (?)")
      ("helpClose", "Close help (Escape)")
      ("notes", "Type or paste notes to see while speaking or something. (\\)") ]
    |> List.iter (fun (x, y) -> (document.getElementById x).title <- y)

    [ ("stopButton", true); ("prevButton", true); ("nextButton", true) ]
    |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

    (document.getElementById "logsTable").innerHTML <- logsTableHeader

    [ "helpButton"; "helpClose" ]
    |> List.iter (fun x ->
        (document.getElementById x :?> HTMLButtonElement).onclick <-
            fun _ -> (document.getElementById "helpWindow").classList.toggle "active" |> ignore)

    printfn "%s" $"""runningStatus: %s{List.item (int runningStatus) status}"""
