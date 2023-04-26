FileStream fileIn: 'shape.st'.
FileStream fileIn: 'sphere.st'.
FileStream fileIn: 'box.st'.
FileStream fileIn: 'cylinder.st'.
FileStream fileIn: 'torus.st'.

sphereDict := Dictionary new.
boxDict := Dictionary new.
cylinderDict := Dictionary new.
torusDict := Dictionary new.

argSize := (Smalltalk arguments size).
(argSize < 1) ifTrue: [
    Transcript show: 'Incorrect number of arguments.'; cr.
    ObjectMemory quit.
].

fileName := Smalltalk arguments first.
(File exists: fileName) ifTrue: [
    dataFile := FileStream open: fileName mode: #read.
] ifFalse: [
    Transcript show: 'Unable to open '.
    Transcript show: fileName.
    Transcript show: ' for reading'; cr.
    ObjectMemory quit.
].

[dataFile atEnd] whileFalse: [
    | line tokenizer shapeName shapeType |
    line := dataFile nextLine.
    tokenizer := line subStrings.
    shapeName := tokenizer at: 1.
    shapeType := tokenizer at: 2.

    shapeType = 'sphere'
        ifTrue:[
            newRadius := (tokenizer at: 3) asNumber.
            sphereDict at: ((sphereDict size)+1) put: (Sphere new init: shapeName radius: newRadius type: shapeType).
        ].
    shapeType = 'box'
        ifTrue:[
            newLength := (tokenizer at: 3) asNumber.
            newWidth := (tokenizer at: 4) asNumber.
            newHeight := (tokenizer at: 5) asNumber.
            boxDict at: ((boxDict size)+1) put: (Box new init: shapeName length: newLength width: newWidth height: newHeight type: shapeType).
        ].
    shapeType = 'cylinder'
        ifTrue:[
            newRadius := (tokenizer at: 3) asNumber.
            newHeight := (tokenizer at: 4) asNumber.
            cylinderDict at: ((cylinderDict size)+1) put: (Cylinder new init: shapeName radius: newRadius height: newHeight type: shapeType).
        ].
    shapeType = 'torus'
        ifTrue:[
            newSmallRadius := (tokenizer at: 3) asNumber.
            newBigRadius := (tokenizer at: 4) asNumber.
            torusDict at: ((torusDict size)+1) put: (Torus new init: shapeName smallRadius: newSmallRadius bigRadius: newBigRadius type: shapeType).
        ].
].
dataFile close.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
| endVar userInput word number |
endVar := 'start'.
[endVar = 'quit'] whileFalse: [
    Transcript cr.
    Transcript show: 'Enter a command: '.
    userInput := stdin nextLine.
    userInput = 'quit'
        ifTrue:[
            ^endVar
        ].
    word := userInput select: [:char | char isLetter].
    number := (userInput select: [:char | char isDigit]) asNumber.
    conditionDict := Dictionary new.
    finalDict := Dictionary new.

    number > 0
        ifTrue:[
            index := 1.
            [index <= number] whileTrue: [
                Transcript show: 'Enter condition #', (index asString), ':'.
                userInput := stdin nextLine.
                conditionDict at: index put: userInput.
                index := index + 1.
            ].
        ].

    index := 1.
    [index <= (sphereDict size)] whileTrue:[
        | keep |
        keep := true.
        index2 := 1.
        [index2 <= (conditionDict size)] whileTrue: [
            tempKeep := ((sphereDict at: index) test: (conditionDict at: index2) bool: keep).
            tempKeep = false
                ifTrue:[
                    keep := tempKeep.
                ].
            index2 := index2 + 1.
        ].
        keep = true
            ifTrue:[
                finalDict at: ((finalDict size)+1) put: (sphereDict at: index).
            ].
        index := index + 1.
    ].
    index := 1.
    [index <= (boxDict size)] whileTrue:[
        | keep |
        keep := true.
        index2 := 1.
        [index2 <= (conditionDict size)] whileTrue: [
            tempKeep := ((boxDict at: index) test: (conditionDict at: index2) bool: keep).
            tempKeep = false
                ifTrue:[
                    keep := tempKeep.
                ].
            index2 := index2 + 1.
        ].
        keep = true
            ifTrue:[
                finalDict at: ((finalDict size)+1) put: (boxDict at: index).
            ].
        index := index + 1.
    ].
    index := 1.
    [index <= (cylinderDict size)] whileTrue:[
        | keep |
        keep := true.
        index2 := 1.
        [index2 <= (conditionDict size)] whileTrue: [
            tempKeep := ((cylinderDict at: index) test: (conditionDict at: index2) bool: keep).
            tempKeep = false
                ifTrue:[
                    keep := tempKeep.
                ].
            index2 := index2 + 1.
        ].
        keep = true
            ifTrue:[
                finalDict at: ((finalDict size)+1) put: (cylinderDict at: index).
            ].
        index := index + 1.
    ].
    index := 1.
    [index <= (torusDict size)] whileTrue:[
        | keep |
        keep := true.
        index2 := 1.
        [index2 <= (conditionDict size)] whileTrue: [
            tempKeep := ((torusDict at: index) test: (conditionDict at: index2) bool: keep).
            tempKeep = false
                ifTrue:[
                    keep := tempKeep.
                ].
            index2 := index2 + 1.
        ].
        keep = true
            ifTrue:[
                finalDict at: ((finalDict size)+1) put: (torusDict at: index).
            ].
        index := index + 1.
    ].
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Final Print"
    word = 'print'
        ifTrue:[
            index := 1.
                [index <= (finalDict size)] whileTrue: [
                    (finalDict at: index) printData.
                    index := index + 1.
                ].
        ].
    word = 'count'
        ifTrue:[
            Transcript show: 'There are ', ((finalDict size) asString), ' shapes.'; cr.
        ].
    word = 'min'
        ifTrue:[
            | minIndexA minIndexV index tempA tempV |
            minIndexA := 1.
            minIndexV := 1.
            index := 2.
            [index <= (finalDict size)] whileTrue: [
                    ((finalDict at: minIndexA) getArea) > ((finalDict at: index) getArea)
                        ifTrue:[
                            minIndexA := index.
                        ].
                    ((finalDict at: minIndexV) getVolume) > ((finalDict at: index) getVolume)
                        ifTrue:[
                            minIndexV := index.
                        ].
                    index := index + 1.
                ].
            (finalDict size) = 0
                ifFalse:[
                    tempA := ((((finalDict at: minIndexA) getArea) + 0.005) truncateTo: 0.001) asString.
                    tempV := ((((finalDict at: minIndexV) getVolume) + 0.005) truncateTo: 0.001) asString.
                    Transcript show: 'min(Surface Area)=', (tempA copyFrom: 1 to: ((tempA size) - 1)); cr.
                    Transcript show: 'min(Volume)=', (tempV copyFrom: 1 to: ((tempV size) - 1)); cr.
                ].
        ].
    word = 'max'
        ifTrue:[
            | maxIndexA maxIndexV index tempA tempV |
            maxIndexA := 1.
            maxIndexV := 1.
            index := 2.
            [index <= (finalDict size)] whileTrue: [
                    ((finalDict at: maxIndexA) getArea) < ((finalDict at: index) getArea)
                        ifTrue:[
                            maxIndexA := index.
                        ].
                    ((finalDict at: maxIndexV) getVolume) < ((finalDict at: index) getVolume)
                        ifTrue:[
                            maxIndexV := index.
                        ].
                    index := index + 1.
                ].
            (finalDict size) = 0
                ifFalse:[
                    tempA := ((((finalDict at: maxIndexA) getArea) + 0.005) truncateTo: 0.001) asString.
                    tempV := ((((finalDict at: maxIndexV) getVolume) + 0.005) truncateTo: 0.001) asString.
                    Transcript show: 'max(Surface Area)=', (tempA copyFrom: 1 to: ((tempA size) - 1)); cr.
                    Transcript show: 'max(Volume)=', (tempV copyFrom: 1 to: ((tempV size) - 1)); cr.
                ].
        ].
    word = 'total'
        ifTrue:[
            | totalA totalV index |
            totalA := 0.00.
            totalV := 0.00.
            index := 1.
            [index <= (finalDict size)] whileTrue: [
                    totalA := totalA + ((finalDict at: index) getArea).

                    totalV := totalV + ((finalDict at: index) getVolume).
                            
                    index := index + 1.
                ].
            tempA := ((totalA + 0.005) truncateTo: 0.001) asString.
            tempV := ((totalV + 0.005) truncateTo: 0.001) asString.
            Transcript show: 'total(Surface Area)=', (tempA copyFrom: 1 to: ((tempA size) - 1)); cr.
            Transcript show: 'total(Volume)=', (tempV copyFrom: 1 to: ((tempV size) - 1)); cr.
        ].
    word = 'avg'
        ifTrue:[
            | totalA totalV index avgA avgV |
            totalA := 0.00.
            totalV := 0.00.
            index := 1.
            [index <= (finalDict size)] whileTrue: [
                    totalA := totalA + ((finalDict at: index) getArea).

                    totalV := totalV + ((finalDict at: index) getVolume).
                            
                    index := index + 1.
                ].

            avgA := ((totalA / (finalDict size) + 0.005) truncateTo: 0.001) asString.
            avgV := ((totalV / (finalDict size) + 0.005) truncateTo: 0.001) asString.
            Transcript show: 'avg(Surface Area)=', (avgA copyFrom: 1 to: ((avgA size) - 1)); cr.
            Transcript show: 'avg(Volume)=', (avgV copyFrom: 1 to: ((avgV size) - 1)); cr.
        ].
].