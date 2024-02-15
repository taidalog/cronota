// cronota Version 1.0.1
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

    type Notes =
        { Finished: (int * string) list
          NotFinished: (int * string) list }

    type State =
        { Stop: TimeAcc
          Next: TimeAcc
          IntervalId: int
          Notes: Notes
          RunningStatus: RunningStatus }

    let status = [ "NotStarted"; "Running"; "Stopping"; ""; "Finished" ]

    [<Emit("setInterval($0, $1)")>]
    let setInterval (callback: unit -> unit) (interval: int) : int = jsNative

    [<Emit("clearInterval($0)")>]
    let clearInterval (intervalID: int) : unit = jsNative

    let transfer (transferor: 'T list -> 'T list * 'T list) (list: 'T list * 'T list) : 'T list * 'T list =
        let fh, sh = list
        let fh1, fh2 = transferor fh
        fh1, fh2 @ sh

    let transferBack (transferor: 'T list -> 'T list * 'T list) (list: 'T list * 'T list) : 'T list * 'T list =
        let fh, sh = list
        let sh1, sh2 = transferor sh
        fh @ sh1, sh2

    let nextNotes (notes: 'T list * 'T list) =
        transferBack (fun list -> [ List.head list ], List.tail list) notes

    let prevNotes (notes: 'T list * 'T list) =
        transfer (fun list -> (List.rev >> List.tail >> List.rev) list, [ List.last list ]) notes

    let initState =
        { Stop =
            { StartTime = DateTime.MinValue
              Acc = TimeSpan.Zero }
          Next =
            { StartTime = DateTime.MinValue
              Acc = TimeSpan.Zero }
          IntervalId = -1
          Notes = { Finished = []; NotFinished = [] }
          RunningStatus = RunningStatus.NotStarted }

    let mutable state =
        { Stop =
            { StartTime = DateTime.MinValue
              Acc = TimeSpan.Zero }
          Next =
            { StartTime = DateTime.MinValue
              Acc = TimeSpan.Zero }
          IntervalId = -1
          Notes = { Finished = []; NotFinished = [] }
          RunningStatus = RunningStatus.NotStarted }

    let timeSpanToDisplay (timeSpan: TimeSpan) =
        let h = timeSpan.Hours |> string |> String.padLeft 2 '0'
        let m = timeSpan.Minutes |> string |> String.padLeft 2 '0'
        let s = timeSpan.Seconds |> string |> String.padLeft 2 '0'
        let ms = timeSpan.Milliseconds |> string |> String.padLeft 3 '0'
        $"%s{h}:%s{m}:%s{s}.%s{ms}"

    let td x y z =
        $"""
        <tr>
            <td class="logs-table-no">%s{x}</td>
            <td class="logs-table-time">%s{y}</td>
            <td class="logs-table-note">%s{z}</td>
        </tr>
        """

    let countUp () =
        let intervalId =
            setInterval
                (fun _ ->
                    let elapsedTime = DateTime.Now - state.Stop.StartTime + state.Stop.Acc
                    document.getElementById("timer").innerText <- timeSpanToDisplay elapsedTime)
                10

        state <- { state with IntervalId = intervalId }

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
                    { initState with
                        Stop =
                            { initState.Stop with
                                StartTime = now
                                Acc = TimeSpan.Zero }
                        Next =
                            { initState.Next with
                                StartTime = now
                                Acc = TimeSpan.Zero }
                        Notes =
                            { initState.Notes with
                                NotFinished = notesArray }
                        RunningStatus = RunningStatus.Running }

                (document.getElementById "timer").innerText <- timeSpanToDisplay TimeSpan.Zero
                (document.getElementById "logsTable").innerHTML <- logsTableHeader

                document.getElementById("currNote").innerText <-
                    $"%d{fst (List.head state.Notes.NotFinished)}, %s{snd (List.head state.Notes.NotFinished)}"

                if List.length state.Notes.NotFinished > 1 then
                    document.getElementById("nextNote").innerText <-
                        $"%d{fst (List.item 1 state.Notes.NotFinished)}, %s{snd (List.item 1 state.Notes.NotFinished)}"
                else
                    ()

                notesEl.disabled <- true

                [ ("mainButton", true)
                  ("stopButton", false)
                  ("prevButton", true)
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
                  ("prevButton", List.length state.Notes.Finished = 0)
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
              ("prevButton", true)
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
            let now = DateTime.Now
            let logsTable = document.getElementById "logsTable" :?> HTMLTableElement

            logsTable.innerHTML <-
                logsTable.innerHTML
                + (td
                    (string (fst (List.head state.Notes.NotFinished)))
                    (timeSpanToDisplay (state.Next.Acc + (now - state.Next.StartTime)))
                    (snd (List.head state.Notes.NotFinished)))

            state <-
                { state with
                    Next = { StartTime = now; Acc = TimeSpan.Zero } }

            if List.length state.Notes.NotFinished = 1 then
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
                        Notes =
                            { Finished = state.Notes.Finished @ [ List.head state.Notes.NotFinished ]
                              NotFinished = List.tail state.Notes.NotFinished } }

                document.getElementById("currNote").innerText <-
                    $"""%d{fst (List.head state.Notes.NotFinished)}, %s{snd (List.head state.Notes.NotFinished)}"""

                document.getElementById("nextNote").innerText <-
                    if List.length state.Notes.NotFinished > 1 then
                        $"""%d{fst (List.item 1 state.Notes.NotFinished)}, %s{snd (List.item 1 state.Notes.NotFinished)}"""
                    else
                        ""

                (document.getElementById "prevButton" :?> HTMLButtonElement).disabled <-
                    List.length state.Notes.Finished = 0
        | _ -> ()

    document.getElementById("nextButton").onclick <- next

    let prev event =
        match state.RunningStatus with
        | RunningStatus.Running ->
            printfn "List.length state.Notes.Finished: %d" (List.length state.Notes.Finished)

            if List.length state.Notes.Finished = 0 then
                ()
            else
                let now = DateTime.Now
                let logsTable = document.getElementById "logsTable" :?> HTMLTableElement

                logsTable.innerHTML <-
                    logsTable.innerHTML
                    + (td
                        (string (fst (List.last state.Notes.Finished)))
                        (timeSpanToDisplay (state.Next.Acc + (now - state.Next.StartTime)))
                        (snd (List.last state.Notes.Finished)))

                state <-
                    { state with
                        Next = { StartTime = now; Acc = TimeSpan.Zero }
                        Notes =
                            { Finished = (List.rev >> List.tail >> List.rev) state.Notes.Finished
                              NotFinished = List.last state.Notes.Finished :: state.Notes.NotFinished } }

                document.getElementById("currNote").innerText <-
                    $"""%d{fst (List.head state.Notes.NotFinished)}, %s{snd (List.head state.Notes.NotFinished)}"""

                document.getElementById("nextNote").innerText <-
                    if List.length state.Notes.NotFinished > 1 then
                        $"""%d{fst (List.item 1 state.Notes.NotFinished)}, %s{snd (List.item 1 state.Notes.NotFinished)}"""
                    else
                        ""

                (document.getElementById "prevButton" :?> HTMLButtonElement).disabled <-
                    List.length state.Notes.Finished = 0

                printfn "%s" $"""runningStatus: %s{List.item (int state.RunningStatus) status}"""
        | _ -> ()

    document.getElementById("prevButton").onclick <- prev

    let cutin event =
        match state.RunningStatus with
        | RunningStatus.Running ->
            let now = DateTime.Now
            let logsTable = document.getElementById "logsTable" :?> HTMLTableElement

            logsTable.innerHTML <-
                logsTable.innerHTML
                + (td
                    (string (fst (List.head state.Notes.NotFinished)))
                    (timeSpanToDisplay (state.Next.Acc + (now - state.Next.StartTime)))
                    "CUT_IN")

            state <-
                { state with
                    Next = { StartTime = now; Acc = TimeSpan.Zero }
                    Notes =
                        { state.Notes with
                            NotFinished = state.Notes.NotFinished |> List.map (fun (i, x) -> i + 1, x) } }

            document.getElementById("currNote").innerText <-
                $"""%d{fst (List.head state.Notes.NotFinished)}, %s{snd (List.head state.Notes.NotFinished)}"""

            document.getElementById("nextNote").innerText <-
                if List.length state.Notes.NotFinished > 1 then
                    $"""%d{fst (List.item 1 state.Notes.NotFinished)}, %s{snd (List.item 1 state.Notes.NotFinished)}"""
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
                | "ArrowLeft" -> prev ()
                | "@" -> cutin ()
                | "\\" ->
                    notesEl.focus ()
                    event.preventDefault ()
                | _ -> ()
    )

    (document.getElementById "timer").innerText <- timeSpanToDisplay TimeSpan.Zero

    [ ("mainButton", "開始 (Enter)")
      ("stopButton", "停止 (Escape)")
      ("resetButton", "リセット (Delete)")
      ("prevButton", "戻る (<)")
      ("cutinButton", "割り込み (@)")
      ("nextButton", "次へ (>)")
      ("helpButton", "ヘルプ (?)")
      ("helpClose", "閉じる (Escape)")
      ("notes", "「ノート」を入力するか貼り付けてください (\\)") ]
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
