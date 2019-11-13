module App

open Elmish
open Elmish.React
open Feliz
open Thoth.Json
open Fable.SimpleHttp

type HackernewsItem = {
  id: int
  title: string
  itemType: string
  url: string
  score : int
}

[<RequireQualifiedAccess>]
type Stories =
  | New
  | Best
  | Top
  | Ask
  | Show
  | Job

type State =
  { CurrentStories: Stories
    StoryItems : Deferred<Result<HackernewsItem list, string>> }

type Msg =
  | LoadStoryItems of AsyncOperationEvent<Result<HackernewsItem list, string>>
  | ChangeStories of Stories

let init() =
  { CurrentStories = Stories.New
    StoryItems = HasNotStartedYet }, Cmd.ofMsg (LoadStoryItems Started)

let itemDecoder : Decoder<HackernewsItem> =
  Decode.object (fun fields -> {
    id = fields.Required.At [ "id" ] Decode.int
    title = fields.Required.At [ "title" ] Decode.string
    itemType = fields.Required.At [ "type" ] Decode.string
    url = fields.Required.At [ "url" ] Decode.string
    score = fields.Required.At [ "score" ] Decode.int })

let storiesEndpoint stories =
  let fromBaseUrl = sprintf "https://hacker-news.firebaseio.com/v0/%sstories.json"
  match stories with
  | Stories.Ask -> fromBaseUrl "ask"
  | Stories.Best -> fromBaseUrl "best"
  | Stories.Top -> fromBaseUrl "top"
  | Stories.New -> fromBaseUrl "new"
  | Stories.Show -> fromBaseUrl "show"
  | Stories.Job -> fromBaseUrl "job"

let loadStoryItem (id: int) = async {
  let endpoint = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id
  let! (status, responseText) = Http.get endpoint
  match status with
  | 200 ->
    match Decode.fromString itemDecoder responseText with
    | Ok storyItem -> return Some storyItem
    | Error _ -> return None
  | _ ->
    return None
}

let (|HttpOk|HttpError|) status =
  match status with
  | 200 -> HttpOk
  | _ -> HttpError

let loadStoryItems stories = async {
  do! Async.Sleep 1500
  let endpoint = storiesEndpoint stories
  let! (status, responseText) = Http.get endpoint
  match status with
  | HttpOk ->
    // parse the response text as a list of IDs (ints)
    let storyIds = Decode.fromString (Decode.list Decode.int) responseText
    match storyIds with
    | Ok storyIds ->
        // take the first 10 IDs
        // load the item from each ID in parallel
        // aggregate the results into a single list
        let! storyItems =
          storyIds
          |> List.truncate 10
          |> List.map loadStoryItem
          |> Async.Parallel
          |> Async.map (List.ofArray >> List.choose id)

        printfn "LoadStoryItems Finished %A" (Encode.Auto.toString(4, storyItems))
        return LoadStoryItems (Finished (Ok storyItems))

    | Error errorMsg ->
        // could not parse the array of story ID's
        return LoadStoryItems (Finished (Error errorMsg))
  | HttpError ->
    // non-OK response goes finishes with an error
    return LoadStoryItems (Finished (Error responseText))
}

let startLoading (state: State) =
  { state with StoryItems = InProgress }

let update (msg: Msg) (state: State) =
  match msg with
  | ChangeStories stories ->
      let nextState = { startLoading state with CurrentStories = stories }
      let nextCmd = Cmd.fromAsync (loadStoryItems stories)
      nextState, nextCmd

  | LoadStoryItems Started ->
      let nextState = startLoading state
      let nextCmd = Cmd.fromAsync (loadStoryItems state.CurrentStories)
      nextState, nextCmd

  | LoadStoryItems (Finished items) ->
      let nextState = { state with StoryItems = Resolved items }
      nextState, Cmd.none

let storiesName = function
  | Stories.New -> "New"
  | Stories.Ask -> "Ask"
  | Stories.Best -> "Best"
  | Stories.Job -> "Job"
  | Stories.Top -> "Top"
  | Stories.Show -> "Show"

let renderTab currentStories stories dispatch =
  Html.li [
    prop.className [ currentStories = stories, "is-active" ]
    prop.onClick (fun _ -> if (currentStories <> stories) then dispatch (ChangeStories stories))
    prop.children [
      Html.a [ Html.span (storiesName stories) ]
    ]
  ]

let stories = [
  Stories.New
  Stories.Top
  Stories.Best
  Stories.Show
  Stories.Ask
  Stories.Job
]

let renderTabs currentStories dispatch =
  Html.div [
    prop.className [ "tabs"; "is-toggle"; "is-fullwidth" ]
    prop.children [
      Html.ul [
        for story in stories ->
        renderTab currentStories story dispatch
      ]
    ]
  ]

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
  ]

let renderItem item =
  Html.div [
    prop.className "box"
    prop.style [
      style.marginTop 15
      style.marginBottom 15
    ]
    prop.children [
      div [ "columns"; "is-mobile" ] [
        div [ "column"; "is-narrow" ] [
          Html.div [
            prop.className [ "icon" ]
            prop.style [ style.marginLeft 20 ]
            prop.children [
              Html.i [prop.className "fa fa-poll fa-2x"]
              Html.span [
                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                prop.text item.score
              ]
            ]
          ]
        ]

        div [ "column" ] [
          Html.a [
            prop.style [ style.textDecoration.underline ]
            prop.custom("target", "_blank")
            prop.href item.url
            prop.text item.title
          ]
        ]
      ]
    ]
  ]

let renderItems (items: HackernewsItem list) =
  Html.fragment [
    for item in items ->
      renderItem item
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderStories items =
  match items with
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> renderItems items

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hacker News"
      ]

      renderTabs state.CurrentStories dispatch
      renderStories state.StoryItems
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run