module NGrams

let moduleTok =
    [['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false;
     [' '], true, false]

let identifierTok =
    [['a'..'z'] @ ['A'..'Z'] @ ['_'], true, false;
     ['0'..'9'] @ ['_'] @ ['$'] @ ['a'..'z'] @ ['A'..'Z'], true, true;
     [' '], true, true]

let punctuationTok = 
    [['('; ')'; '['; ']'; '{'; '}'; ':'; ';'; ','], false, false;
     [' '], true, true]

let andTok = 
    [['a'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     [' '], true, false]

let orTok = 
    [['o'], false, false;
     ['r'], false, false;
     [' '], true, false]

let notTok = 
    [['n'], false, false;
     ['o'], false, false;
     ['t'], false, false;
     [' '], true, false]

let inputTok = 
    [['i'], false, false;
     ['n'], false, false;
     ['p'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     [' '], true, false]

let outputTok = 
    [['o'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     ['p'], false, false;
     ['u'], false, false;
     ['t'], false, false;
     [' '], true, false]

let wireTok = 
    [['w'], false, false;
     ['i'], false, false;
     ['r'], false, false;
     ['e'], false, false;
     [' '], true, false]

let endModuleTok =
    [['e'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     ['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false]

let emptyLine = 
    [['\010'], false, false;
     [' '], true, true]