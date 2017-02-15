module App exposing (..)

import Html exposing (Html)
import Html.Attributes
import Time exposing (Time)
import Process
import Task
import RemoteData exposing (WebData, RemoteData(..))


type alias Post =
    { header : String
    , description : String
    }


type Animation
    = Blink
    | Fade


type alias Model =
    { posts : WebData (List Post) }


init : Time -> ( Model, Cmd Msg )
init delay =
    ( Model Loading
    , getPosts ReceivePosts delay
    )


type Msg
    = ReceivePosts (WebData (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivePosts newPosts ->
            ( { model | posts = newPosts }
            , Cmd.none
            )


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


layoutPost : Animation -> Html Msg -> Html Msg -> Html Msg
layoutPost animation header description =
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


withBlink : List (Html Msg) -> Html Msg
withBlink =
    Html.span
        [ Html.Attributes.class "blink" ]


withFade : List (Html Msg) -> Html Msg
withFade =
    Html.span
        [ Html.Attributes.class "fade" ]


viewPost : Post -> Html Msg
viewPost { header, description } =
    layoutPost
        Blink
        (Html.text header)
        (Html.text description)


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
        layoutPost
            Fade
            header
            description


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getPosts : (WebData (List Post) -> Msg) -> Time -> Cmd Msg
getPosts msg delay =
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
        delay
            |> Process.sleep
            |> Task.andThen (always <| Task.succeed response)
            |> Task.perform msg
