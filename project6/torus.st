Shape subclass: Torus [
    | smallRadius bigRadius PI area volume|

    init: newName smallRadius: newSmallRadius bigRadius: newBigRadius type: newType [
        smallRadius := newSmallRadius.
        bigRadius := newBigRadius.
        PI := 3.14159265358979323846.
        area := (2 * PI * bigRadius) * (2 * PI * smallRadius).
        volume := (PI * (smallRadius * smallRadius)) * (2 * PI * bigRadius).
        ^ super setName: newName setType: newType.
    ]

    printOn: stream [
        super printOn: stream.
    ]

    printData [
        | tempSR tempBR tempA tempV |
        tempSR := ((smallRadius + 0.005) truncateTo: 0.001) asString.
        tempBR := ((bigRadius + 0.005) truncateTo: 0.001) asString.
        Transcript show: 'Torus: ', name, ', Small Radius=', (tempSR copyFrom: 1 to: ((tempSR size) - 1)), ', Big Radius=', (tempBR copyFrom: 1 to: ((tempBR size) - 1)); cr.
        tempA := ((area + 0.005) truncateTo: 0.001) asString.
        tempV := ((volume + 0.005) truncateTo: 0.001) asString.
        Transcript show: '        Surface Area: ', (tempA copyFrom: 1 to: ((tempA size) - 1)), ', Volume: ', (tempV copyFrom: 1 to: ((tempV size) - 1)); cr.
    ]

    getArea [
        ^ area.
    ]
    getVolume [
        ^ volume.
    ]

    test: condition bool: keep [
        | line tokenizer arg1 argB arg2 |
        tokenizer := condition subStrings.
        arg1 := tokenizer at: 1.
        argB := tokenizer at: 2.
        arg2 := tokenizer at: 3.
        
        argB = '=='
            ifTrue:[
                arg1 = 'type'
                    ifTrue:[
                        arg2 = type
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'area'
                    ifTrue:[
                        (arg2 asNumber) = area
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'volume'
                    ifTrue:[
                        (arg2 asNumber) = volume
                            ifFalse:[
                                ^ false.
                            ].
                    ].
            ].
        argB = '!='
            ifTrue:[
                arg1 = 'type'
                    ifTrue:[
                        arg2 ~= type
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'area'
                    ifTrue:[
                        (arg2 asNumber) ~= area
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'volume'
                    ifTrue:[
                        (arg2 asNumber) ~= volume
                            ifFalse:[
                                ^ false.
                            ].
                    ].
            ].
            argB = '>='
            ifTrue:[
                arg1 = 'type'
                    ifTrue:[
                        type >= argB
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'area'
                    ifTrue:[
                        area >= (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'volume'
                    ifTrue:[
                        volume >= (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
            ].
        argB = '<='
            ifTrue:[
                arg1 = 'type'
                    ifTrue:[
                        type <= arg2
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'area'
                    ifTrue:[
                        area <= (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'volume'
                    ifTrue:[
                        volume <= (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
            ].
        argB = '>'
            ifTrue:[
                arg1 = 'type'
                    ifTrue:[
                        type > arg2
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'area'
                    ifTrue:[
                        area > (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'volume'
                    ifTrue:[
                        volume > (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
            ].
        argB = '<'
            ifTrue:[
                arg1 = 'type'
                    ifTrue:[
                        type < arg2
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'area'
                    ifTrue:[
                        area < (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
                arg1 = 'volume'
                    ifTrue:[
                        volume < (arg2 asNumber)
                            ifFalse:[
                                ^ false.
                            ].
                    ].
            ].
        ^ true.
    ]
]
