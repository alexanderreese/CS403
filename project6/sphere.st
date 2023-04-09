Shape subclass: Sphere [
    | radius PI area volume |

    init: newName radius: newRadius type: newType [
        radius := newRadius.
        PI := 3.14159265358979323846.
        area := radius * radius * 4 * PI.
        volume := (radius * radius * radius) * (1.333333333) * PI.     
        ^ super setName: newName setType: newType.
    ]

    printOn: stream [
        super printOn: stream.
    ]

    printData [
        | tempR tempA tempV |
        tempR := ((radius + 0.005) truncateTo: 0.001) asString.
        Transcript show: 'Sphere: ', name, ', Radius=', (tempR copyFrom: 1 to: ((tempR size) - 1)); cr.
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
