Object subclass: Shape [
    | name type|

    Shape class >> new [
        | r |
        r := super new.
        ^r
    ]

    setName: setName setType: setType[
        name := setName.
        type := setType.
    ]

    printOn: stream [
        super printOn: stream.
    ]

]
