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
  url: string option
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

type DeferredStoryItem = Deferred<Result<HackernewsItem, string>>

type State =
  { CurrentStories: Stories
    StoryItems : Deferred<Result<Map<int, DeferredStoryItem>, string>> }

type Msg =
  | LoadStoryIds of AsyncOperationStatus<Result<int list, string>>
  | LoadedStoryItem of int * Result<HackernewsItem, string>
  | ChangeStories of Stories

let init() =
  { CurrentStories = Stories.New
    StoryItems = HasNotStartedYet }, Cmd.ofMsg (LoadStoryIds Started)

let itemDecoder : Decoder<HackernewsItem> =
  Decode.object (fun fields -> {
    id = fields.Required.At [ "id" ] Decode.int
    title = fields.Required.At [ "title" ] Decode.string
    itemType = fields.Required.At [ "type" ] Decode.string
    url = fields.Optional.At [ "url" ] Decode.string
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


let (|HttpOk|HttpError|) status =
  match status with
  | 200 -> HttpOk
  | _ -> HttpError

let rnd = System.Random()  

let loadStoryItem (id: int) = async {
    // simulate high network latency
    do! Async.Sleep (rnd.Next(1000, 3000))
    let endpoint = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id
    let! (status, responseText) = Http.get endpoint
    match status with
    | HttpOk ->
        match Decode.fromString itemDecoder responseText with
        | Ok storyItem -> return LoadedStoryItem (id, Ok storyItem)
        | Error parseError -> return LoadedStoryItem (id, Error parseError)
    | HttpError ->
        return LoadedStoryItem (id, Error ("Http error while loading " + string id))
}



let loadStoryIds stories = async {
 
  let endpoint = storiesEndpoint stories
  let! (status, responseText) = Http.get endpoint
  match status with
  | HttpOk ->
    // parse the response text as a list of IDs (ints)
    let storyIds = Decode.fromString (Decode.list Decode.int) responseText
    match storyIds with
    | Ok storyIds ->
      
        let firstTenIds =
          storyIds
          |> List.truncate 10

        return LoadStoryIds (Finished (Ok firstTenIds))

    | Error errorMsg ->
        // could not parse the array of story ID's
        return LoadStoryIds (Finished (Error errorMsg))
  | HttpError ->
    // non-OK response goes finishes with an error
    return LoadStoryIds (Finished (Error responseText))
}

let startLoading (state: State) =
  { state with StoryItems = InProgress }

let update (msg: Msg) (state: State) =
  match msg with
  | ChangeStories stories ->
      let nextState = { startLoading state with CurrentStories = stories }
      let nextCmd = Cmd.fromAsync (loadStoryIds stories)
      nextState, nextCmd

  | LoadStoryIds Started ->
      let nextState = startLoading state
      let nextCmd = Cmd.fromAsync (loadStoryIds state.CurrentStories)
      nextState, nextCmd

  | LoadStoryIds (Finished (Ok storyIds)) ->
      let initialItems = Map.ofList [ for id in storyIds -> (id, InProgress) ]
      let nextState = { state with StoryItems = Resolved (Ok initialItems) }
      nextState, Cmd.batch [ for id in storyIds -> Cmd.fromAsync (loadStoryItem id) ]

  | LoadStoryIds (Finished (Error error)) ->
      let nextState = { state with StoryItems = Resolved (Error error) }
      nextState, Cmd.none

  | LoadedStoryItem (itemId, Ok item) ->
      match state.StoryItems with
      | Resolved (Ok storiesMap) ->
          let modifiedStoriesMap =
            storiesMap
            |> Map.remove itemId
            |> Map.add itemId (Resolved (Ok item))

          let nextState = { state with StoryItems = Resolved (Ok modifiedStoriesMap) }
          nextState, Cmd.none

      | _ ->
          state, Cmd.none

  | LoadedStoryItem (itemId, Error error) ->
      match state.StoryItems with
      | Resolved (Ok storiesMap) ->
          let modifiedStoriesMap =
            storiesMap
            |> Map.remove itemId
            |> Map.add itemId (Resolved (Error error))

          let nextState = { state with StoryItems = Resolved (Ok modifiedStoriesMap) }
          nextState, Cmd.none

      | _ ->
          state, Cmd.none        


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

let renderItemContent (item: HackernewsItem) =
  Html.div [
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
        match item.url with
        | Some url ->
            Html.a [
              prop.style [ style.textDecoration.underline ]
              // prop.target.blank
              prop.href url
              prop.text item.title
            ]

        | None ->  Html.p item.title
      ]
    ]
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

let renderStoryItem (itemId: int) storyItem =
  let renderedItem =
      match storyItem with
      | HasNotStartedYet -> Html.none
      | InProgress -> spinner
      | Resolved (Error error) -> renderError error
      | Resolved (Ok storyItem) -> renderItemContent storyItem

  Html.div [
    prop.key itemId
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15]
    prop.children [ renderedItem ]
  ]

let renderStories items =
  match items with
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> 
      items
      |> Map.toList
      |> List.map (fun (id, storyItem) -> renderStoryItem id storyItem)
      |> Html.div


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