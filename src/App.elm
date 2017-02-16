module App exposing (..)

import Ports
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg
import Svg.Attributes
import String
import Json.Decode
import Http
import Time
import Process
import Task
import List.Split
import Scroll
import RemoteData exposing (WebData, RemoteData(..))


type alias Package =
    { name : String
    , summary : String
    , author : String
    , version : String
    }


type SearchStatus
    = Entering String
    | SearchingAndEntering String String


type alias Model =
    { packages : WebData (List Package)
    , search : SearchStatus
    , isShadowVisible : Bool
    }


type Animation
    = Blink
    | Fade


type Msg
    = ReceivePackages (WebData (List Package))
    | ReceivePackagesChunk (List Package)
    | UpdateSearchEntering String
    | StartSearch
    | CancelSearch
    | UpdateShadowState Scroll.Move
    | NoOp


init : ( Model, Cmd Msg )
init =
    Model Loading (Entering "") False
        |> withCmd getPackages


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                |> withoutCmd

        UpdateShadowState ( _, offset ) ->
            model
                |> setShadowState (offset /= 0)
                |> withoutCmd

        ReceivePackages (Success newPackages) ->
            let
                sendChunk chunk index =
                    Task.perform
                        (always <| ReceivePackagesChunk chunk)
                        (Process.sleep <| 100 * toFloat index * Time.millisecond)

                batchChunk ( index, chunk ) cmd =
                    Cmd.batch
                        [ cmd
                        , sendChunk chunk index
                        ]

                sendChunks =
                    newPackages
                        |> List.Split.chunksOfLeft 10
                        |> List.indexedMap (,)
                        |> List.foldl batchChunk Cmd.none
            in
                model
                    |> setPackagesData Loading
                    |> withCmd sendChunks

        ReceivePackages newPackagesData ->
            model
                |> setPackagesData newPackagesData
                |> withoutCmd

        ReceivePackagesChunk newPackages ->
            model
                |> consPackages newPackages
                |> withoutCmd

        UpdateSearchEntering newQuery ->
            model
                |> setSearchEnteringQuery newQuery
                |> normalizeSearchingStatus
                |> withoutCmd

        StartSearch ->
            model
                |> startSearch
                |> normalizeSearchingStatus
                |> withoutCmd

        CancelSearch ->
            model
                |> cancelSearch
                |> withoutCmd


setShadowState : Bool -> Model -> Model
setShadowState newState model =
    { model | isShadowVisible = newState }


normalizeSearchingStatus : Model -> Model
normalizeSearchingStatus ({ search } as model) =
    let
        newNormalizedSearch =
            case search of
                SearchingAndEntering "" query ->
                    Entering query

                _ ->
                    search
    in
        { model | search = newNormalizedSearch }


setSearchEnteringQuery : String -> Model -> Model
setSearchEnteringQuery newQuery ({ search } as model) =
    let
        newSearch =
            case search of
                Entering _ ->
                    Entering newQuery

                SearchingAndEntering searchedQuery _ ->
                    SearchingAndEntering searchedQuery newQuery
    in
        { model | search = newSearch }


startSearch : Model -> Model
startSearch ({ search } as model) =
    let
        query =
            getQuery search

        newSearch =
            SearchingAndEntering query query
    in
        { model | search = newSearch }


cancelSearch : Model -> Model
cancelSearch ({ search } as model) =
    let
        newSearch =
            Entering ""
    in
        { model | search = newSearch }


setPackagesData : WebData (List Package) -> Model -> Model
setPackagesData newPackagesData model =
    { model | packages = newPackagesData }


consPackages : List Package -> Model -> Model
consPackages newPackages ({ packages } as model) =
    let
        newPackagesData =
            case packages of
                Success oldPackages ->
                    Success (oldPackages ++ newPackages)

                _ ->
                    Success newPackages
    in
        { model | packages = newPackagesData }


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd =
    flip (,)


withoutCmd : Model -> ( Model, Cmd Msg )
withoutCmd =
    withCmd Cmd.none


view : Model -> Html Msg
view { search, packages, isShadowVisible } =
    Html.div
        []
        [ viewSearch search isShadowVisible
        , viewPackages packages search
        ]


icon : String -> Html Msg
icon name =
    Svg.svg
        [ Svg.Attributes.class "icon" ]
        [ Svg.use
            [ Svg.Attributes.xlinkHref ("#" ++ name) ]
            []
        ]


getQuery : SearchStatus -> String
getQuery status =
    case status of
        Entering query ->
            query

        SearchingAndEntering _ query ->
            query


getSearchedQuery : SearchStatus -> String
getSearchedQuery status =
    case status of
        Entering query ->
            ""

        SearchingAndEntering searchedQuery _ ->
            searchedQuery


viewSearch : SearchStatus -> Bool -> Html Msg
viewSearch searchStatus isShadowVisible =
    Html.form
        [ Html.Attributes.classList
            [ ( "search", True )
            , ( "search--shadow", isShadowVisible )
            ]
        , Html.Events.onSubmit StartSearch
        ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.class "search__input"
            , Html.Attributes.value (getQuery searchStatus)
            , Html.Attributes.placeholder "Search"
            , Html.Events.onInput UpdateSearchEntering
            ]
            []
        , viewSearchButton searchStatus
        ]


viewSearchButton : SearchStatus -> Html Msg
viewSearchButton searchStatus =
    let
        button iconName type_ onClick =
            Html.button
                [ Html.Attributes.class "search__button"
                , Html.Attributes.type_ type_
                , Html.Events.onClick onClick
                ]
                [ icon iconName ]
    in
        case searchStatus of
            Entering "" ->
                Html.text ""

            Entering _ ->
                button "search" "submit" NoOp

            SearchingAndEntering _ _ ->
                button "cancel" "button" CancelSearch


isPackageContains : String -> Package -> Bool
isPackageContains query { name, author } =
    (name ++ "/" ++ author)
        |> String.toLower
        |> String.contains query


viewPackages : WebData (List Package) -> SearchStatus -> Html Msg
viewPackages packagesData searchStatus =
    case packagesData of
        NotAsked ->
            Html.text ""

        Loading ->
            List.range 1 5
                |> List.map (always viewPackageSkeleton)
                |> Html.div []

        Success packages ->
            packages
                |> List.filter
                    (isPackageContains <|
                        getSearchedQuery searchStatus
                    )
                |> List.map viewPackage
                |> Html.div []

        Failure error ->
            Html.text <| toString error


viewPackageLayout :
    { animation : Animation
    , name : Html Msg
    , summary : Html Msg
    , author : Html Msg
    , version : Html Msg
    , avatar : Html Msg
    }
    -> Html Msg
viewPackageLayout { animation, name, summary, author, version, avatar } =
    let
        animationClass =
            case animation of
                Blink ->
                    "blink"

                Fade ->
                    "fade"
    in
        Html.div
            [ Html.Attributes.class ("package " ++ animationClass) ]
            [ Html.div
                [ Html.Attributes.class "package__image" ]
                [ avatar ]
            , Html.div
                [ Html.Attributes.class "package__info" ]
                [ Html.h2
                    [ Html.Attributes.class "package__header" ]
                    [ name
                    , version
                    ]
                , Html.h4
                    [ Html.Attributes.class "package__author" ]
                    [ author ]
                , Html.p
                    [ Html.Attributes.class "package__summary" ]
                    [ summary ]
                ]
            ]


viewPackage : Package -> Html Msg
viewPackage { name, summary, author, version } =
    let
        packageUrl =
            "http://package.elm-lang.org/packages/"
                ++ author
                ++ "/"
                ++ name
                ++ "/"

        packageLatestUrl =
            packageUrl ++ version

        githubUrl =
            "http://github.com/" ++ author

        avatarUrl =
            "http://avatars.githubusercontent.com/" ++ author
    in
        viewPackageLayout
            { animation = Blink
            , name =
                Html.a
                    [ Html.Attributes.href packageLatestUrl
                    , Html.Attributes.target "_blank"
                    ]
                    [ Html.text name ]
            , summary =
                Html.span
                    []
                    [ Html.text summary ]
            , author =
                Html.span
                    []
                    [ Html.text "by "
                    , Html.a
                        [ Html.Attributes.href githubUrl
                        , Html.Attributes.target "_blank"
                        ]
                        [ Html.text author ]
                    ]
            , version =
                Html.a
                    [ Html.Attributes.class "package__version"
                    , Html.Attributes.href packageUrl
                    , Html.Attributes.target "_blank"
                    ]
                    [ Html.text version ]
            , avatar =
                Html.img
                    [ Html.Attributes.src avatarUrl ]
                    []
            }


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


viewPackageSkeleton : Html Msg
viewPackageSkeleton =
    viewPackageLayout
        { animation = Fade
        , name = spacer 70
        , summary =
            Html.span
                []
                [ spacer 100
                , spacer 80
                ]
        , author = spacer 30
        , version = spacer 20
        , avatar =
            Html.span
                [ Html.Attributes.class "spacer-bg" ]
                []
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.scroll UpdateShadowState


decodePackage : Json.Decode.Decoder Package
decodePackage =
    let
        extractName fullName =
            case String.split "/" fullName of
                [ author, name ] ->
                    Json.Decode.succeed ( name, author )

                _ ->
                    Json.Decode.fail ("invalid name: " ++ fullName)

        extractVersion versions =
            case List.head versions of
                Just version ->
                    Json.Decode.succeed version

                Nothing ->
                    Json.Decode.fail "invalid versions list"

        nameDecoder =
            Json.Decode.string
                |> Json.Decode.andThen extractName

        versionsDecoder =
            Json.Decode.list Json.Decode.string
                |> Json.Decode.andThen extractVersion

        createPackage ( name, author ) summary version =
            Package name summary author version
    in
        Json.Decode.map3 createPackage
            (Json.Decode.field "name" nameDecoder)
            (Json.Decode.field "summary" Json.Decode.string)
            (Json.Decode.field "versions" versionsDecoder)


decodePackages : Json.Decode.Decoder (List Package)
decodePackages =
    Json.Decode.list decodePackage


getPackages : Cmd Msg
getPackages =
    let
        -- "https://crossorigin.me/"
        proxy =
            "https://cors-anywhere.herokuapp.com/"

        url =
            "http://package.elm-lang.org/all-packages"

        packages =
            [ { name = "elm-list-ndet"
              , summary = """
              Nondeterministic values / cartesian combinable lists
              """
              , author = "Apanatshka"
              , version = "1.0.1"
              }
            ]

        response =
            Success packages
    in
        Http.get (proxy ++ url) decodePackages
            |> RemoteData.sendRequest
            |> Cmd.map ReceivePackages
