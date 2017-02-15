module App exposing (..)

import Html exposing (Html)
import Html.Attributes
import Time
import Process
import Task
import RemoteData exposing (WebData, RemoteData(..))


type alias Post =
    { header : String
    , description : String
    }


type alias Model =
    { posts : WebData (List Post) }


type Animation
    = Blink
    | Fade


type Msg
    = ReceivePosts (WebData (List Post))


init : ( Model, Cmd Msg )
init =
    Model Loading
        |> withCmd getPosts


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivePosts newPosts ->
            model
                |> setPosts newPosts
                |> withoutCmd


setPosts : WebData (List Post) -> Model -> Model
setPosts newPosts model =
    { model | posts = newPosts }


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd =
    flip (,)


withoutCmd : Model -> ( Model, Cmd Msg )
withoutCmd =
    withCmd Cmd.none


view : Model -> Html Msg
view { posts } =
    viewPosts posts


viewPosts : WebData (List Post) -> Html Msg
viewPosts postsData =
    case postsData of
        NotAsked ->
            Html.text ""

        Loading ->
            List.range 1 3
                |> List.map (always viewPostSkeleton)
                |> Html.div []

        Success posts ->
            posts
                |> List.map viewPost
                |> Html.div []

        Failure error ->
            Html.text <| toString error


viewPostLayout : Animation -> Html Msg -> Html Msg -> Html Msg
viewPostLayout animation header description =
    let
        animationClass =
            case animation of
                Blink ->
                    "blink"

                Fade ->
                    "fade"
    in
        Html.div
            [ Html.Attributes.class ("post " ++ animationClass) ]
            [ Html.h2
                []
                [ header ]
            , Html.p
                []
                [ description ]
            ]


spacer : Int -> Html Msg
spacer width =
    let
        widthInPercent =
            toString width ++ "%"
    in
        Html.span
            [ Html.Attributes.class "spacer"
            , Html.Attributes.style
                [ ( "width", widthInPercent ) ]
            ]
            []


viewPost : Post -> Html Msg
viewPost { header, description } =
    let
        header_ =
            Html.text header

        description_ =
            Html.text description
    in
        viewPostLayout Blink header_ description_


viewPostSkeleton : Html Msg
viewPostSkeleton =
    let
        header =
            spacer 100

        description =
            Html.span
                []
                [ spacer 100
                , spacer 80
                ]
    in
        viewPostLayout Fade header description


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getPosts : Cmd Msg
getPosts =
    let
        posts =
            [ Post
                "My First Post"
                """
                Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                """
            , Post
                "Second short post"
                "Hello world"
            , Post
                "Third long post"
                """
                Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                Sint modo partes vitae beatae.
                """
            , Post
                "Some extra post"
                """
                Hello
                """
            ]

        response =
            Success posts
    in
        Time.second
            |> Process.sleep
            |> Task.andThen (always <| Task.succeed response)
            |> Task.perform ReceivePosts
