// cronota Version 0.3.1
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

    type TimeAcc = { StartTime: DateTime; Acc: TimeSpan }

    type State =
        { Stop: TimeAcc
          Next: TimeAcc
          mutable IntervalId: int
          Notes: (int * string) list
          RunningStatus: RunningStatus }

    let status = [ "NotStarted"; "Running"; "Stopping"; ""; "Finished" ]

    [<Emit("setInterval($0, $1)")>]
    let setInterval (callback: unit -> unit) (interval: int) : int = jsNative

    [<Emit("clearInterval($0)")>]
    let clearInterval (intervalID: int) : unit = jsNative

    let mutable state =
        { Stop =
            { StartTime = DateTime.MinValue
              Acc = TimeSpan.Zero }
          Next =
            { StartTime = DateTime.MinValue
              Acc = TimeSpan.Zero }
          IntervalId = -1
          Notes = []
          RunningStatus = RunningStatus.NotStarted }

    let timeSpanToDisplay (timeSpan: TimeSpan) =
        let h = timeSpan.Hours |> string |> String.padLeft 2 '0'
        let m = timeSpan.Minutes |> string |> String.padLeft 2 '0'
        let s = timeSpan.Seconds |> string |> String.padLeft 2 '0'
        let ms = timeSpan.Milliseconds |> string |> String.padLeft 3 '0'
        $"%s{h}:%s{m}:%s{s}.%s{ms}"

    let countUp () =
        state.IntervalId <-
            setInterval
                (fun _ ->
                    let elapsedTime = DateTime.Now - state.Stop.StartTime + state.Stop.Acc
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
            let now = DateTime.Now

            match state.RunningStatus with
            | RunningStatus.NotStarted
            | RunningStatus.Finished ->
                state <-
                    { state with
                        Stop =
                            { state.Stop with
                                StartTime = now
                                Acc = TimeSpan.Zero }
                        Next =
                            { state.Next with
                                StartTime = now
                                Acc = TimeSpan.Zero }
                        Notes = notesArray
                        RunningStatus = RunningStatus.Running }

                (document.getElementById "timer").innerText <- timeSpanToDisplay TimeSpan.Zero
                (document.getElementById "logsTable").innerHTML <- logsTableHeader

                document.getElementById("currNote").innerText <-
                    $"%d{fst (List.head state.Notes)}, %s{snd (List.head state.Notes)}"

                if List.length state.Notes > 1 then
                    document.getElementById("nextNote").innerText <-
                        $"%d{fst (List.item 1 state.Notes)}, %s{snd (List.item 1 state.Notes)}"
                else
                    ()

                notesEl.disabled <- true

                [ ("mainButton", true)
                  ("stopButton", false)
                  ("cutinButton", false)
                  ("nextButton", false) ]
                |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

                printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
                countUp ()
            | RunningStatus.Running -> ()
            | RunningStatus.Stopping ->
                notesEl.disabled <- true

                [ ("mainButton", true)
                  ("stopButton", false)
                  ("cutinButton", false)
                  ("nextButton", false) ]
                |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

                state <-
                    { state with
                        Stop = { state.Stop with StartTime = now }
                        Next = { state.Next with StartTime = now }
                        RunningStatus = RunningStatus.Running }

                printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
                countUp ()
            | _ -> ()

    document.getElementById("mainButton").onclick <- start

    let stop event =
        match state.RunningStatus with
        | RunningStatus.Running ->
            let now = DateTime.Now
            clearInterval state.IntervalId

            state <-
                { state with
                    Stop =
                        { state.Stop with
                            Acc = state.Stop.Acc + (now - state.Stop.StartTime) }
                    Next =
                        { state.Next with
                            Acc = state.Next.Acc + (now - state.Next.StartTime) }
                    RunningStatus = RunningStatus.Stopping }

            [ ("mainButton", false)
              ("stopButton", true)
              ("cutinButton", true)
              ("nextButton", true) ]
            |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

            printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
        | _ -> ()

    document.getElementById("stopButton").onclick <- stop

    let reset event =
        match state.RunningStatus with
        | RunningStatus.Running -> stop ()
        | RunningStatus.Stopping
        | RunningStatus.Finished ->
            [ ("currNote", "")
              ("nextNote", "")
              ("timer", timeSpanToDisplay TimeSpan.Zero) ]
            |> List.iter (fun (x, y) -> (document.getElementById x).innerText <- y)

            (document.getElementById "logsTable").innerHTML <- logsTableHeader

            (document.getElementById "mainButton" :?> HTMLButtonElement).disabled <- false
            (document.getElementById "notes" :?> HTMLInputElement).disabled <- false

            state <-
                { state with
                    Stop = { state.Stop with Acc = TimeSpan.Zero }
                    Next = { state.Next with Acc = TimeSpan.Zero }
                    RunningStatus = RunningStatus.NotStarted }

            printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
        | _ -> ()

    document.getElementById("resetButton").onclick <- reset

    let next event =
        match state.RunningStatus with
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

            let now = DateTime.Now
            let logsTable = document.getElementById "logsTable" :?> HTMLTableElement

            logsTable.innerHTML <-
                logsTable.innerHTML
                + (td
                    (string (fst (List.head state.Notes)))
                    (timeSpanToDisplay (state.Next.Acc + (now - state.Next.StartTime)))
                    (snd (List.head state.Notes)))

            state <-
                { state with
                    Next = { StartTime = now; Acc = TimeSpan.Zero } }

            if List.length state.Notes = 1 then
                logsTable.innerHTML <-
                    logsTable.innerHTML
                    + (td "TOTAL" (document.getElementById("timer").innerText) "END")

                document.getElementById("currNote").innerText <- ""
                stop ()

                (document.getElementById "mainButton" :?> HTMLButtonElement).disabled <- false
                (document.getElementById "notes" :?> HTMLInputElement).disabled <- false

                state <-
                    { state with
                        RunningStatus = RunningStatus.Finished }

                printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
            else
                state <-
                    { state with
                        Notes = List.tail state.Notes }

                document.getElementById("currNote").innerText <-
                    $"""%d{fst (List.head state.Notes)}, %s{snd (List.head state.Notes)}"""

                document.getElementById("nextNote").innerText <-
                    if List.length state.Notes > 1 then
                        $"""%d{fst (List.item 1 state.Notes)}, %s{snd (List.item 1 state.Notes)}"""
                    else
                        ""
        | _ -> ()

    document.getElementById("nextButton").onclick <- next

    let cutin event =
        match state.RunningStatus with
        | RunningStatus.Running ->
            let td x y z =
                $"""
                <tr>
                    <td class="logs-table-no">%s{x}</td>
                    <td class="logs-table-time">%s{y}</td>
                    <td class="logs-table-note">%s{z}</td>
                </tr>
                """

            let now = DateTime.Now
            let logsTable = document.getElementById "logsTable" :?> HTMLTableElement

            logsTable.innerHTML <-
                logsTable.innerHTML
                + (td
                    (string (fst (List.head state.Notes)))
                    (timeSpanToDisplay (state.Next.Acc + (now - state.Next.StartTime)))
                    "CUT_IN")

            state <-
                { state with
                    Next = { StartTime = now; Acc = TimeSpan.Zero }
                    Notes = state.Notes |> List.map (fun (i, x) -> i + 1, x) }

            document.getElementById("currNote").innerText <-
                $"""%d{fst (List.head state.Notes)}, %s{snd (List.head state.Notes)}"""

            document.getElementById("nextNote").innerText <-
                if List.length state.Notes > 1 then
                    $"""%d{fst (List.item 1 state.Notes)}, %s{snd (List.item 1 state.Notes)}"""
                else
                    ""
        | _ -> ()

    document.getElementById("cutinButton").onclick <- cutin

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
                | "@" -> cutin ()
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
      ("cutinButton", "Cut in (@)")
      ("nextButton", "Next note (>)")
      ("helpButton", "Help (?)")
      ("helpClose", "Close help (Escape)")
      ("notes", "Type or paste notes to see while speaking or something. (\\)") ]
    |> List.iter (fun (x, y) -> (document.getElementById x).title <- y)

    [ ("stopButton", true)
      ("prevButton", true)
      ("cutinButton", true)
      ("nextButton", true) ]
    |> List.iter (fun (x, b) -> (document.getElementById x :?> HTMLButtonElement).disabled <- b)

    (document.getElementById "logsTable").innerHTML <- logsTableHeader

    [ "helpButton"; "helpClose" ]
    |> List.iter (fun x ->
        (document.getElementById x :?> HTMLButtonElement).onclick <-
            fun _ -> (document.getElementById "helpWindow").classList.toggle "active" |> ignore)

    printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
